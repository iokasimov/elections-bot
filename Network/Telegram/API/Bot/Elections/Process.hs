module Network.Telegram.API.Bot.Elections.Process (initiate, conduct, participate, vote) where

import "base" Control.Applicative (pure, (*>))
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad ((>>=))
import "base" Data.Foldable (foldr, length)
import "base" Data.Function (const, flip, id, (.), ($))
import "base" Data.Functor (fmap, void, (<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.List (zip)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Semigroup ((<>))
import "base" Prelude ((*))
import "base" Text.Read (readMaybe)
import "base" Text.Show (show)
import "lens" Control.Lens ((^.))
import "stm" Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', readTVar, writeTVar)
import "text" Data.Text (Text, pack, unpack)
import "telega" Network.Telegram.API.Bot (Telegram, ask')
import "telega" Network.Telegram.API.Bot.Object (Button (Button), Content (Textual), Notification, Pressed (Callback), Keyboard (Inline))
import "telega" Network.Telegram.API.Bot.Object.Sender (Sender, firstname, lastname)
import "telega" Network.Telegram.API.Bot.Object.Update.Message (Message (Direct), Messaging (Directly))
import "telega" Network.Telegram.API.Bot.Property.Persistable (Persistable (request), Capacity (Edit, Post, Purge), PL (PL))
import "transformers" Control.Monad.Trans.Class (lift)

import Network.Telegram.API.Bot.Elections.Configuration (Environment)
import Network.Telegram.API.Bot.Elections.Locales (Locale
	, Status (Started, Absented, Proceeded, Considered, Ended), message)
import Network.Telegram.API.Bot.Elections.State (Scores, Votes, nomination, consider)

-- Initiate elections, the initiator becomes a candidate automatically
initiate :: Sender -> Telegram Environment ()
initiate sender = ask' >>= \(locale, chat_id, _, votes) -> atomically' (readTVar votes) >>=
	maybe (show_candidates locale chat_id votes) (const $ already_initiated locale chat_id) where

	already_initiated :: Locale -> Int64 -> Telegram Environment ()
	already_initiated locale chat_id = request @Directly @Message @()
		$ PL (chat_id, message locale Proceeded)

	show_candidates :: Locale -> Int64 -> TVar Votes -> Telegram Environment ()
	show_candidates locale chat_id votes = do
		let keyboard = Inline . pure . pure $ button (0, (sender, []))
		let content = (chat_id, start_voting locale, keyboard)
		msg <- request @Post @Keyboard @Message $ PL content
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
		request @Purge @Message @() $ PL (chat_id, keyboard_msg_id)
		request @Directly @Message @() $ PL (chat_id, end_voting locale scores)
		atomically' . writeTVar votes $ Nothing

	end_voting :: Locale -> Scores -> Text
	end_voting locale scores = message locale Ended <> "\n" <>
		foldr (\x acc -> line x <> acc) "" scores where

		line :: (Sender, [Sender]) -> Text
		line (sender, voters) = "* " <> sender ^. firstname
			<> " " <> maybe "" id (sender ^. lastname)
			<> " : " <> (pack . show . length $ voters) <> "\n"

-- Become a candidate
participate :: Sender -> Telegram Environment ()
participate sender = ask' >>= \(locale, chat_id, _, votes) -> atomically' (readTVar votes) >>= \case
	Nothing -> request @Directly @Message $ PL (chat_id, message locale Absented)
	Just (keyboard_msg_id, scores) -> flip (maybe (pure ())) (nomination sender scores) $ \upd -> do
		let new_keyboard = Inline $ pure . button <$> zip [0..] upd
		atomically' $ writeTVar votes $ Just (keyboard_msg_id, upd)
		request @Edit @Keyboard @() $ PL (chat_id, keyboard_msg_id, new_keyboard)

-- 👍 or 👎 for some candidate
vote :: Text -> Sender -> Text -> Telegram Environment ()
vote _ _ (readMaybe @Int . unpack -> Nothing) = pure ()
vote cbq_id sender (readMaybe @Int . unpack -> Just cnd_idx) = ask' >>= \(locale, chat_id, _, votes) -> do
	let considering = modifyTVar' votes (fmap . fmap $ consider cnd_idx sender) *> readTVar votes
	atomically' considering >>= maybe (pure ()) (adjust_scores locale chat_id) where

	adjust_scores :: Locale -> Int64 -> (Int, Scores) -> Telegram Environment ()
	adjust_scores locale chat_id (keyboard_msg_id, scores) = do
		request @Post @Notification @() $ PL (cbq_id, message locale Considered)
		request @Edit @Keyboard $ PL (chat_id, keyboard_msg_id
			, Inline $ pure . button <$> zip [0..] scores)

button :: (Int, (Sender, [Sender])) -> Button
button (idx, (sender, n)) = flip Button (Callback . pack . show $ idx) $
	sender ^. firstname <> " " <> maybe "" id (sender ^. lastname)
		<> " : " <> (pack . show . length $ n)

atomically' :: STM a -> Telegram Environment a
atomically' = lift . lift . atomically
