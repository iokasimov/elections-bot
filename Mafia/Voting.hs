module Mafia.Voting (primaries, vote) where

import Control.Lens (element, _2, (%~))
import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Data.Function ((&))
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text, pack)
import Web.Telegram.API.Bot.Data (Chat (..), Message (..), MessageEntity (..)
	, User (..), InlineKeyboardButton (..), InlineKeyboardMarkup (..))
import Web.Telegram.API.Bot.API.Edit (editMessageReplyMarkup)
import Web.Telegram.API.Bot.API.Messages (sendMessage)
import Web.Telegram.API.Bot.Requests (ChatId (ChatId), ReplyKeyboard (..)
	, EditMessageReplyMarkupRequest (..), SendMessageRequest (SendMessageRequest))

import Mafia.Configuration (Settings (Settings))

-- Voting takes place in two stages:
-- 1) Someone should initiate it, when putting `/vote` command and mention all who will take participatation
-- 2) Mentioned users have 5 minutes click on buttons with candidates, keyboard is updating on every vote

primaries :: Settings -> ChatId -> [MessageEntity] -> IO ()
primaries settings@(Settings token (ChatId chatid) _ votes) (ChatId chatid') es =
	if chatid /= chatid' then pure () else -- Voting command in group starts voting
		background settings . foldr (\x acc -> maybe acc (flip (:) acc) x) [] . map me_user $ es

background :: Settings -> [User] -> IO ()
background (Settings token chatid manager votes) us = atomically (readTVar votes) >>= \case
	Just _ -> void $ sendMessage token taking_place_massage manager
	Nothing -> do
		atomically $ modifyTVar' votes (const . Just . map (flip (,) 0) $ us)
		void $ sendMessage token starting_message manager
		threadDelay 300000000 -- wait for 5 minutes
		decisions <- atomically (readTVar votes)
		decisions & maybe (print "Very strange situation")
			(void . flip (sendMessage token) manager . results chatid)
		atomically $ modifyTVar' votes (const Nothing)
	where

	starting_message, taking_place_massage :: SendMessageRequest
	starting_message = SendMessageRequest chatid
		"Голосование началось - в течении следующих 5 минут вы можете указать игроков, с которыми вы хотели бы поиграть."
		Nothing Nothing Nothing Nothing (Just . ReplyInlineKeyboardMarkup . candidates_table $ (flip (,) 0) <$> us )
	taking_place_massage = SendMessageRequest chatid "Идёт голосование..." Nothing Nothing Nothing Nothing Nothing

results :: ChatId -> [(User, Int)] -> SendMessageRequest
results chatid decisions = SendMessageRequest chatid
	("Голосование окончилось, результаты: \n" <> foldr (\x acc -> line x <> acc) "" decisions)
		Nothing Nothing Nothing Nothing (Just $ ReplyKeyboardRemove True Nothing) where

	line :: (User, Int) -> Text
	line (u, n) = "* " <> user_first_name u <> " "
		<> (maybe "" id . user_last_name $ u)
		<> " : " <> (pack . show $ n) <> "\n"

candidates_table :: [(User, Int)] -> [[InlineKeyboardButton]]
candidates_table scores = pure . button <$> zip [0..] scores where

	button :: (Int, (User, Int)) -> InlineKeyboardButton
	button (idx, (User uid fn ln _ _, n)) = InlineKeyboardButton
		(fn <> " " <> maybe "" id ln <> " : " <> (pack . show $ n))
		Nothing (Just . pack . show $ idx) Nothing Nothing Nothing Nothing

vote :: Settings -> Int -> Int -> Int -> IO ()
vote (Settings token chatid manager votes) msg_id _ candidate_index = do
	atomically $ modifyTVar' votes $ fmap (element candidate_index . _2 %~ (+1))
	atomically (readTVar votes) >>= \case
		Nothing -> print "Very strange situation"
		Just scores -> void $ flip (editMessageReplyMarkup token) manager .
			EditMessageReplyMarkupRequest (Just chatid) (Just msg_id) Nothing .
				Just . InlineKeyboardMarkup $ candidates_table scores
