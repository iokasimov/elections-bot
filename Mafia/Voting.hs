module Mafia.Voting (initiate, participate, vote) where

import "base" Data.Eq (Eq ((==)))
import "base" Control.Applicative (pure)
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad (void, (>>=))
import "base" Data.Int (Int)
import "base" Data.Either (Either (Left, Right))
import "base" Data.Foldable (find)
import "base" Data.Function (const, id, (.), ($), (&))
import "base" Data.Functor (fmap, (<$>))
import "base" Data.List (delete, length, zip)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" System.IO (IO, print)
import "base" Text.Show (show)
import "lens" Control.Lens (element, _2, (%~))
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

import Mafia.Configuration (Settings (Settings))

instance Eq User where
	User uid _ _ _ _ _ == User uid' _ _ _ _ _ = uid == uid

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
			atomically . writeTVar votes . Just $ (keyboard_msg_id, [])
			threadDelay 30000000 -- wait for 5 minutes
			atomically $ modifyTVar' votes (const Nothing)
			void $ sendMessage token (text_message chatid "Голосование завершено.") manager
	where

	start_voting_message :: SendMessageRequest
	start_voting_message = SendMessageRequest chatid
		"Голосование началось - в течении следующих 5 минут вы можете указать игроков, с которыми вы хотели бы поиграть."
		Nothing Nothing Nothing Nothing (Just . ReplyInlineKeyboardMarkup . candidates_table $ [])

vote :: Settings -> Int -> User -> Int -> IO ()
vote (Settings token group_chatid manager votes) msg_id voter candidate_index = do
	atomically $ modifyTVar' votes $ (fmap . fmap) (cast_vote candidate_index voter)
	atomically (readTVar votes) >>= \case
		Nothing -> print "Very strange situation"
		Just (keyboard_msg_id, voters) -> void $ editMessageReplyMarkup token
			(update_scores voters group_chatid keyboard_msg_id) manager

participate :: Settings -> Int -> User -> IO ()
participate (Settings token group_chatid manager votes) msgid user = do
	atomically (readTVar votes) >>= \case
		Nothing -> void $ sendMessage token (text_message group_chatid "Голосование не инициировано в чате мафии.") manager
		Just (keyboard_msg_id, voters) -> do
			let new_candidate = (user, []) : voters
			atomically . writeTVar votes . Just . (,) keyboard_msg_id $ new_candidate
			void $ editMessageReplyMarkup token (update_scores new_candidate group_chatid keyboard_msg_id) manager
			void $ runClient (deleteMessageM $ DeleteMessageRequest group_chatid msgid) token manager

candidates_table :: [(User, [User])] -> [[InlineKeyboardButton]]
candidates_table scores = pure . button <$> zip [0..] scores where

	button :: (Int, (User, [User])) -> InlineKeyboardButton
	button (idx, (User uid _ fn ln _ _, n)) = InlineKeyboardButton
		(fn <> " " <> maybe "" id ln <> " : " <> (pack . show . length $ n))
		Nothing (Just . pack . show $ idx) Nothing Nothing Nothing Nothing

-- If you already voted for this candidate, your vote will be removed
cast_vote :: Int -> User -> [(User, [User])] -> [(User, [User])]
cast_vote candidate_index voter votes = votes & element candidate_index . _2 %~
	(\scores -> maybe (voter : scores) (const $ delete voter scores) . find ((==) (user_id voter) . user_id) $ scores)

update_scores :: [(User, [User])] -> ChatId -> Int -> EditMessageReplyMarkupRequest
update_scores voters group_chatid keyboard_msg_id = EditMessageReplyMarkupRequest
	(Just group_chatid) (Just keyboard_msg_id ) Nothing . Just . InlineKeyboardMarkup
		$ candidates_table voters

text_message :: ChatId -> Text -> SendMessageRequest
text_message chatid txt = SendMessageRequest
	chatid txt Nothing Nothing Nothing Nothing Nothing
