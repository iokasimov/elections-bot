module Mafia.Voting (vote) where

import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Web.Telegram.API.Bot.Data (Chat (..), Message (..), Update (..))
import Web.Telegram.API.Bot.API.Messages (sendMessage)
import Web.Telegram.API.Bot.Requests (ChatId (ChatId), SendMessageRequest, sendMessageRequest)

import Mafia.Configuration (Settings (Settings))

-- Voting takes place in two stages:
-- 1) Someone should initiate it, when putting `/vote` command in group chat,
--    then members of chat have 5 minutes to vote
-- 2) Actual voting: members of the group chat can send vote command to bot directly,
--    bot should return the keyboard with members of this chat

vote :: Settings -> Update -> IO ()
vote settings@(Settings token (ChatId chatid) _ votes) u =
	if (fmap (chat_id . chat) . message) u == Just chatid
		then background settings else pure () where

	background :: Settings -> IO ()
	background (Settings token chatid manager votes) = atomically (readTVar votes) >>= \case
		Just _ -> void $ sendMessage token taking_place_massage manager
		Nothing -> do
			atomically $ modifyTVar' votes (const $ Just [])
			void $ sendMessage token starting_message manager
			threadDelay 300000000 -- wait for 5 minutes
			atomically $ modifyTVar' votes (const Nothing)
			void $ sendMessage token ending_message manager

	starting_message, ending_message, taking_place_massage :: SendMessageRequest
	starting_message = sendMessageRequest (ChatId chatid) "Голосование началось - в течении следующих 5 минут вы можете указать игроков, с которыми вы хотели бы поиграть."
	ending_message = sendMessageRequest (ChatId chatid) "Голосование окончилось."
	taking_place_massage = sendMessageRequest (ChatId chatid) "Идёт голосование..."
