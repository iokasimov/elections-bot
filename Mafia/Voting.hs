module Mafia.Voting (vote) where

import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text, pack)
import Web.Telegram.API.Bot.Data (Chat (..), Message (..), MessageEntity (..), User (..))
import Web.Telegram.API.Bot.API.Messages (sendMessage)
import Web.Telegram.API.Bot.Requests (ChatId (ChatId), SendMessageRequest, sendMessageRequest)

import Mafia.Configuration (Settings (Settings))

-- Voting takes place in two stages:
-- 1) Someone should initiate it, when putting `/vote` command and mention all who
--    will take participate voting in group chat, then members of chat have 5 minutes to vote
-- 2) Actual voting: members of the group chat can send vote command to bot directly,
--    bot should return the keyboard with members of this chat

vote :: Settings -> ChatId -> [MessageEntity] -> IO ()
vote settings@(Settings token (ChatId chatid) _ votes) (ChatId chatid') es =
	if chatid == chatid' -- Voting command in group starts voting
		then background settings . foldr (\x acc -> maybe acc (flip (:) acc) x) [] . map me_user $ es
		else undefined -- TODO: When every member is voting (personal message to the bot)
	where
	-- TODO: Send every member keyboard with checkboxes on other members
	background :: Settings -> [User] -> IO ()
	background (Settings token chatid manager votes) us =  atomically (readTVar votes) >>= \case
		Just _ -> void $ sendMessage token taking_place_massage manager
		Nothing -> do
			atomically $ modifyTVar' votes (const . Just . map (flip (,) 0) $ us)
			void $ sendMessage token starting_message manager
			threadDelay 300000000 -- wait for 5 minutes
			decisions <- atomically (readTVar votes)
			decisions & maybe (print "Very strange situation")
				(void . flip (sendMessage token) manager . results)
			atomically $ modifyTVar' votes (const Nothing)

	starting_message, taking_place_massage :: SendMessageRequest
	starting_message = sendMessageRequest (ChatId chatid)
		"Голосование началось - в течении следующих 5 минут вы можете указать игроков, с которыми вы хотели бы поиграть."
	taking_place_massage = sendMessageRequest (ChatId chatid) "Идёт голосование..."

	results :: [(User, Int)] -> SendMessageRequest
	results decisions = sendMessageRequest (ChatId chatid) $
		"Голосование окончилось, результаты: \n" <>
		foldr (\x acc -> line x <> acc) "" decisions where

		line :: (User, Int) -> Text
		line (u, n) = "* " <> user_first_name u <> " "
			<> (maybe "" id . user_last_name $ u)
			<> " : " <> (pack . show $ n) <> "\n"
