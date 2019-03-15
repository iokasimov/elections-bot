module Mafia.Bot.Voting (initiate, participate, vote) where

import "base" Data.Eq (Eq ((==)))
import "base" Control.Applicative (pure, (*>))
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad (void, (>>=))
import "base" Data.Int (Int)
import "base" Data.Either (Either (Left, Right))
import "base" Data.Function (const, flip, id, (.), ($), (&))
import "base" Data.Functor ((<$>))
import "base" Data.Traversable (traverse)
import "base" Data.Tuple (uncurry)
import "base" Data.List (length, zip)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" System.IO (IO, print)
import "base" Text.Show (show)
import "stm" Control.Concurrent.STM (atomically, modifyTVar', readTVar, writeTVar)
import "telegram-api" Web.Telegram.API.Bot.API (runClient)
import "telegram-api" Web.Telegram.API.Bot.API.Edit (deleteMessageM, editMessageReplyMarkup)
import "telegram-api" Web.Telegram.API.Bot.API.Messages (sendMessage)
import "telegram-api" Web.Telegram.API.Bot.Data (Chat (..), Message (..), MessageEntity (..)
	, User (..), InlineKeyboardButton (..), InlineKeyboardMarkup (..))
import "telegram-api" Web.Telegram.API.Bot.Requests (ChatId (ChatId), ReplyKeyboard (..)
	, DeleteMessageRequest (..), EditMessageReplyMarkupRequest (..), SendMessageRequest (SendMessageRequest))
import "telegram-api" Web.Telegram.API.Bot.Responses (Response (..))
import "text" Data.Text (Text, pack)

import Mafia.Bot.Environment (Settings (Settings))
import Mafia.Bot.State (Scores, start, consider, nomination)

-- Voting takes place in two stages for 5 minutes:
-- 1) Primaries: someone should initiate it, when putting `/vote` command
-- 2) Participate: candidate should put `/participate` command in bot PM
-- 3) Vote: all candidates can click on buttons with other candidates

initiate :: Settings -> IO ()
initiate settings@(Settings token chatid manager votes) = atomically (readTVar votes) >>= \case
	Just _ -> void $ sendMessage token (text_message chatid "Идёт голосование...") manager
	Nothing -> sendMessage token start_voting_message manager >>= \case
		Left err -> print err
		Right res -> do
			let keyboard_msg_id = message_id . result $ res
			atomically . writeTVar votes $ start keyboard_msg_id
			threadDelay 300000000 -- Wait all votes for 5 minutes
			atomically $ modifyTVar' votes (const Nothing) -- Voting is over
			void $ sendMessage token (text_message chatid "Голосование завершено.") manager
	where

	start_voting_message :: SendMessageRequest
	start_voting_message = SendMessageRequest chatid
		"Голосование началось - в течении следующих 5 минут вы можете указать игроков, с которыми вы хотели бы поиграть."
			Nothing Nothing Nothing Nothing (Just . ReplyInlineKeyboardMarkup . candidates_table $ [])

vote :: Settings -> Int -> User -> Int -> IO ()
vote (Settings token group_chatid manager votes) msg_id voter candidate_index = do
	atomically $ modifyTVar' votes (consider candidate_index voter)
	atomically (readTVar votes) >>= void . traverse
		(flip (editMessageReplyMarkup token) manager . uncurry (update_keyboard group_chatid))

participate :: Settings -> Int -> User -> IO ()
participate settings@(Settings token group_chatid manager votes) msgid user = do
	atomically (modifyTVar' votes (nomination user) *> readTVar votes) >>=
		void . traverse update_keyboard_and_delete_message where

	update_keyboard_and_delete_message :: (Int, Scores) -> IO ()
	update_keyboard_and_delete_message (keyboard_msg_id, scores) = do
		void $ editMessageReplyMarkup token (update_keyboard group_chatid keyboard_msg_id scores) manager
		delete_message settings msgid

candidates_table :: [(User, [User])] -> [[InlineKeyboardButton]]
candidates_table scores = pure . button <$> zip [0..] scores where

	button :: (Int, (User, [User])) -> InlineKeyboardButton
	button (idx, (User uid _ fn ln _ _, n)) = InlineKeyboardButton
		(fn <> " " <> maybe "" id ln <> " : " <> (pack . show . length $ n))
		Nothing (Just . pack . show $ idx) Nothing Nothing Nothing Nothing

update_keyboard :: ChatId -> Int -> [(User, [User])] -> EditMessageReplyMarkupRequest
update_keyboard group_chatid keyboard_msg_id scores = EditMessageReplyMarkupRequest
	(Just group_chatid) (Just keyboard_msg_id ) Nothing . Just . InlineKeyboardMarkup
		$ candidates_table scores

text_message :: ChatId -> Text -> SendMessageRequest
text_message chatid txt = SendMessageRequest
	chatid txt Nothing Nothing Nothing Nothing Nothing

delete_message :: Settings -> Int -> IO ()
delete_message (Settings token group_chatid manager _) msgid = void $ runClient
	(deleteMessageM $ DeleteMessageRequest group_chatid msgid) token manager
