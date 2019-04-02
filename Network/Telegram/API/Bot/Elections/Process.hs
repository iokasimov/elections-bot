module Network.Telegram.API.Bot.Elections.Process (initiate, conduct, participate, vote) where

import "base" Control.Applicative (pure, (<*>), (*>))
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad ((>>=))
import "base" Data.Foldable (foldr, length)
import "base" Data.Function (const, flip, id, (.), ($), (&))
import "base" Data.Functor (fmap, void, (<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.List (zip)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Semigroup ((<>))
import "base" Prelude ((*))
import "base" System.IO (print)
import "base" Text.Read (readMaybe)
import "base" Text.Show (show)
import "lens" Control.Lens ((^.))
import "stm" Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, modifyTVar', readTVar, writeTVar)
import "text" Data.Text (Text, pack, unpack)
import "telega" Network.Telegram.API.Bot (Telegram, ask')
import "telega" Network.Telegram.API.Bot.Object (Callback, Message (Textual), Button (Button), Pressed (Callback), Keyboard (Inline))
import "telega" Network.Telegram.API.Bot.Object.From (From (User, Bot), firstname, lastname)
import "telega" Network.Telegram.API.Bot.Capacity (Editable (edit), Postable (post), Purgeable (purge))
import "telega" Network.Telegram.API.Bot.Property (Identifiable (identificator))
import "transformers" Control.Monad.Trans.Class (lift)

import Network.Telegram.API.Bot.Elections.Configuration (Environment)
import Network.Telegram.API.Bot.Elections.State (Scores, Votes, nomination, consider)

initiate :: From -> Telegram Environment ()
initiate from = ask' >>= \(chat_id, _, votes) -> (atomically' $ readTVar votes) >>=
	maybe (show_candidates chat_id votes) (const $ already_initiated chat_id) where

	already_initiated :: Int64 -> Telegram Environment ()
	already_initiated chat_id = void $ post @Message
		(chat_id, "Идёт голосование...", Nothing)

	show_candidates :: Int64 -> TVar Votes -> Telegram Environment ()
	show_candidates chat_id votes = do
		msg <- post @Message (chat_id, start_voting, Just $
			Inline . pure . pure $ button (0, (from, [])))
		let Textual keyboard_msg_id _ _ _ = msg
		atomically' . writeTVar votes . Just $
			(keyboard_msg_id, [(from, [])])

conduct :: Telegram Environment ()
conduct = ask' >>= \(chat_id, election_duration, votes) -> do
	lift . lift . threadDelay $ election_duration * 60000000
	atomically' (readTVar votes) >>= maybe (pure ()) (finish_election chat_id votes) where

	finish_election :: Int64 -> TVar Votes -> (Int, Scores) -> Telegram Environment ()
	finish_election chat_id votes (keyboard_msg_id, scores) = do
		void $ purge @Message (chat_id, keyboard_msg_id)
		void $ post @Message (chat_id, end_voting scores, Nothing)
		atomically' . writeTVar votes $ Nothing

participate :: From -> Telegram Environment ()
participate from = ask' >>= \(chat_id, _, votes) -> atomically' (readTVar votes) >>= \case
	Nothing -> void $ post @Message (chat_id, "Голосование не иницировано...", Nothing)
	Just (keyboard_msg_id, scores) -> flip (maybe (pure ())) (nomination from scores) $ \upd -> do
		let new_keyboard = Inline $ pure . button <$> zip [0..] upd
		atomically' $ writeTVar votes $ Just (keyboard_msg_id, upd)
		void $ edit @Keyboard (chat_id, keyboard_msg_id, new_keyboard)

vote :: From -> Text -> Telegram Environment ()
vote _ (readMaybe @Int . unpack -> Nothing) = pure ()
vote from (readMaybe @Int . unpack -> Just cnd_idx) = ask' >>= \(chat_id, _, votes) -> do
	let considering = modifyTVar' votes (fmap . fmap $ consider cnd_idx from) *> readTVar votes
	atomically' considering >>= \case
		Nothing -> lift . lift $ print "Very strange situation"
		Just (keyboard_msg_id, scores) -> void $ edit @Keyboard
			(chat_id, keyboard_msg_id, Inline $ pure . button <$> zip [0..] scores)

atomically' :: STM a -> Telegram Environment a
atomically' = lift . lift . atomically

button :: (Int, (From, [From])) -> Button
button (idx, (from, n)) = flip Button (Callback . pack . show $ idx) $
	from ^. firstname <> " " <> maybe "" id (from ^. lastname)
		<> " : " <> (pack . show . length $ n)

start_voting :: Text
start_voting = "Голосование началось - в течении следующих 5 минут "
	<> "вы можете указать игроков, с которыми вы хотели бы поиграть."

end_voting :: Scores -> Text
end_voting scores = "Голосование окончилось, результаты: \n" <>
	foldr (\x acc -> line x <> acc) "" scores where

	line :: (From, [From]) -> Text
	line (from, voters) = "* " <> from ^. firstname
		<> " " <> maybe "" id (from ^. lastname)
		<> " : " <> (pack . show . length $ voters) <> "\n"
