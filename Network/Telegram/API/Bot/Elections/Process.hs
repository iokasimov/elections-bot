module Network.Telegram.API.Bot.Elections.Process (initiate, conduct, participate, vote) where

import "base" Control.Applicative (pure, (<*>), (*>))
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad ((>>=))
import "base" Data.Foldable (foldr, length)
import "base" Data.Function (flip, id, (.), ($), (&))
import "base" Data.Functor (fmap, void, (<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.List (zip)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Semigroup ((<>))
import "base" System.IO (print)
import "base" Text.Read (readMaybe)
import "base" Text.Show (show)
import "stm" Control.Concurrent.STM (TVar, atomically, newTVarIO, modifyTVar', readTVar, writeTVar)
import "text" Data.Text (Text, pack, unpack)
import "telega" Network.Telegram.API.Bot (Telegram, ask')
import "telega" Network.Telegram.API.Bot.Capacity (Editable (edit), Postable (post), Purgeable (purge))
import "telega" Network.Telegram.API.Bot.Object (Callback, From (User, Bot), Message (Textual), Button (Button), Pressed (Callback), Keyboard (Inline))
import "transformers" Control.Monad.Trans.Class (lift)

import Network.Telegram.API.Bot.Elections.State (Scores, Votes, nomination, consider)

initiate :: Telegram (Int64, TVar Votes) ()
initiate = ask' >>= \(chat_id, votes) ->
	(lift . lift . atomically $ readTVar votes) >>= \case
		Just _ -> void $ post @Message (chat_id, "Идёт голосование...", Nothing)
		Nothing -> do
			msg <- post @Message (chat_id, start_voting, Just $ Inline [])
			let Textual keyboard_msg_id _ _ _ = msg
			lift . lift . atomically . writeTVar votes . Just $ (keyboard_msg_id, []) where

conduct :: Telegram (Int64, TVar Votes) ()
conduct = ask' >>= \(chat_id, votes) -> do
	lift . lift $ threadDelay 60000000 -- wait for 10 minutes
	(lift . lift . atomically $ readTVar votes) >>= \case
		Nothing -> lift . lift $ print "Very strange situation"
		Just (keyboard_msg_id, scores) -> do
			void $ purge @Message (chat_id, keyboard_msg_id)
			void $ post @Message (chat_id, end_voting scores, Nothing)
			lift . lift . atomically . writeTVar votes $ Nothing

participate :: From -> Telegram (Int64, TVar Votes) ()
participate from = ask' >>= \(chat_id, votes) ->
	(lift . lift . atomically $ modifyTVar' votes (nomination from) *> readTVar votes) >>= \case
		Nothing -> void $ post @Message (chat_id, "Голосование не иницировано...", Nothing)
		Just (keyboard_msg_id, scores) -> void $ edit @Keyboard
			(chat_id, keyboard_msg_id, Inline $ pure . button <$> zip [0..] scores)

vote :: From -> Text -> Telegram (Int64, TVar Votes) ()
vote _ (readMaybe @Int . unpack -> Nothing) = pure ()
vote from (readMaybe @Int . unpack -> Just cnd_idx) = ask' >>= \(chat_id, votes) -> do
	let considering = modifyTVar' votes (fmap . fmap $ consider cnd_idx from) *> readTVar votes
	(lift . lift . atomically $ considering) >>= \case
		Nothing -> lift . lift $ print "Very strange situation"
		Just (keyboard_msg_id, scores) -> void $ edit @Keyboard
			(chat_id, keyboard_msg_id, Inline $ pure . button <$> zip [0..] scores)

button :: (Int, (From, [From])) -> Button
button (idx, (User _ _ fn ln _, n)) = flip Button (Callback . pack . show $ idx)
	(fn <> " " <> maybe "" id ln <> " : " <> (pack . show . length $ n))
button (idx, (Bot _ _ fn ln _, n)) = flip Button (Callback . pack . show $ idx)
	(fn <> " " <> maybe "" id ln <> " : " <> (pack . show . length $ n))

start_voting :: Text
start_voting = "Голосование началось - в течении следующих 5 минут "
	<> "вы можете указать игроков, с которыми вы хотели бы поиграть."

end_voting :: Scores -> Text
end_voting scores = "Голосование окончилось, результаты: \n" <>
	foldr (\x acc -> line x <> acc) "" scores where

	line :: (From, [From]) -> Text
	line (User _ _ fn ln _, voters) = "* " <> fn <> " " <> (maybe "" id ln)
		<> " : " <> (pack . show . length $ voters) <> "\n"
	line (Bot _ _ fn ln _, voters) = "* " <> fn <> " " <> (maybe "" id ln)
		<> " : " <> (pack . show . length $ voters) <> "\n"
