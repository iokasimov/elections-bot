module Network.API.Telegram.Bot.Elections.Process (initiate, conduct, participate, vote) where

import "base" Control.Applicative (pure, (*>))
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad ((>>=))
import "base" Data.Foldable (foldr, length)
import "base" Data.Function (const, flip, id, (.), ($))
import "base" Data.Functor (fmap, (<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.List (zip)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Semigroup ((<>))
import "base" Prelude ((*))
import "base" Text.Read (readMaybe)
import "base" Text.Show (show)
import "lens" Control.Lens (view, (^.))
import "stm" Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', readTVar, writeTVar)
import "text" Data.Text (Text, pack, unpack)
import "telega" Network.API.Telegram.Bot (Telegram, ask')
import "telega" Network.API.Telegram.Bot.Field.Name (Name, First, Last)
import "telega" Network.API.Telegram.Bot.Object (Button (Button), Content (Textual), Notification, Pressed (Callback), Keyboard (Inline))
import "telega" Network.API.Telegram.Bot.Object.Sender (Sender)
import "telega" Network.API.Telegram.Bot.Object.Update.Callback (Trigger (Trigger), Notification)
import "telega" Network.API.Telegram.Bot.Object.Update.Message (Message (Direct), Send (Send), Edit (Edit), Delete (Delete))
import "telega" Network.API.Telegram.Bot.Property (Accessible (access), Persistable (persist, persist_))
import "transformers" Control.Monad.Trans.Class (lift)
import "with" Data.With (type (:&:)((:&:)))

import Network.API.Telegram.Bot.Elections.Configuration (Environment)
import Network.API.Telegram.Bot.Elections.Locales (Locale
	, Status (Started, Absented, Proceeded, Considered, Ended), message)
import Network.API.Telegram.Bot.Elections.State (Scores, Votes, nomination, consider)

-- Initiate elections, the initiator becomes a candidate automatically
initiate :: Sender -> Telegram Environment ()
initiate sender = ask' >>= \(locale, chat_id, _, votes) -> atomically' (readTVar votes) >>=
	maybe (show_candidates locale chat_id votes) (const $ already_initiated locale chat_id) where

	already_initiated :: Locale -> Int64 -> Telegram Environment ()
	already_initiated locale chat_id = persist_ . Send chat_id $ message locale Proceeded

	show_candidates :: Locale -> Int64 -> TVar Votes -> Telegram Environment ()
	show_candidates locale chat_id votes = do
		let keyboard = Inline . pure . pure $ button (0, (sender, []))
		msg <- persist . Send chat_id $ start_voting locale :&: keyboard
		let Direct keyboard_msg_id _ (Textual _) = msg
		atomically' . writeTVar votes . Just $
			(keyboard_msg_id, [(sender, [])])

	start_voting :: Locale -> Text
	start_voting = flip message Started

-- After some election period summarize all scores for each candidate
conduct :: Telegram Environment ()
conduct = ask' >>= \(locale, chat_id, election_duration, votes) -> do
	lift . lift . threadDelay $ election_duration * 60000000
	atomically' (readTVar votes) >>= maybe (pure ()) (finish_election locale chat_id votes) where

	finish_election :: Locale -> Int64 -> TVar Votes -> (Int, Scores) -> Telegram Environment ()
	finish_election locale chat_id votes (keyboard_msg_id, scores) = do
		persist_ $ Delete @Message chat_id keyboard_msg_id
		persist_ . Send chat_id $ end_voting locale scores
		atomically' . writeTVar votes $ Nothing

	end_voting :: Locale -> Scores -> Text
	end_voting locale scores = message locale Ended <> "\n" <>
		foldr (\x acc -> line x <> acc) "" scores where

		line :: (Sender, [Sender]) -> Text
		line (sender, voters) = "* " <> (sender ^. access @(First Name) . access @Text)
			<> " " <> maybe "" (view $ access @Text) (sender ^. access @(Maybe (Last Name)))
			<> " : " <> (pack . show . length $ voters) <> "\n"

-- Become a candidate
participate :: Sender -> Telegram Environment ()
participate sender = ask' >>= \(locale, chat_id, _, votes) -> atomically' (readTVar votes) >>= \case
	Nothing -> persist_ $ Send chat_id $ message locale Absented
	Just (keyboard_msg_id, scores) -> flip (maybe (pure ())) (nomination sender scores) $ \upd -> do
		let new_keyboard = Inline $ pure . button <$> zip [0..] upd
		atomically' $ writeTVar votes $ Just (keyboard_msg_id, upd)
		persist_ $ Edit chat_id keyboard_msg_id new_keyboard

-- 👍 or 👎 for some candidate
vote :: Text -> Sender -> Text -> Telegram Environment ()
vote _ _ (readMaybe @Int . unpack -> Nothing) = pure ()
vote cbq_id sender (readMaybe @Int . unpack -> Just cnd_idx) = ask' >>= \(locale, chat_id, _, votes) -> do
	let considering = modifyTVar' votes (fmap . fmap $ consider cnd_idx sender) *> readTVar votes
	atomically' considering >>= maybe (pure ()) (adjust_scores locale chat_id) where

	adjust_scores :: Locale -> Int64 -> (Int, Scores) -> Telegram Environment ()
	adjust_scores locale chat_id (keyboard_msg_id, scores) = do
		persist . Trigger @Notification cbq_id $ message locale Considered
		persist_ . Edit @Keyboard chat_id keyboard_msg_id . Inline $
			pure . button <$> zip [0..] scores

button :: (Int, (Sender, [Sender])) -> Button
button (idx, (sender, n)) = flip Button (Callback . pack . show $ idx) $
	(sender ^. access @(First Name) . access @Text) <> " "
	<> maybe "" (view $ access @Text) (sender ^. access @(Maybe (Last Name)))
	<> " : " <> (pack . show . length $ n)

atomically' :: STM a -> Telegram Environment a
atomically' = lift . lift . atomically
