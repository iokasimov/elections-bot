module Main where

import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, ReqBody, Proxy (Proxy), Server, JSON, Get, Post, type (:>), serve, err403, throwError)
import Text.Read (readMaybe)
import Web.Telegram.API.Bot.API (Token (Token))
import Web.Telegram.API.Bot.Data (CallbackQuery (..), Chat (..), Message (..), Update (..), User (..))
import Web.Telegram.API.Bot.Requests (ChatId (ChatId))

import qualified Data.Text as T (take, unpack)

import Mafia.Configuration (Settings (Settings), settings)
import Mafia.Voting (initiate, participate, vote)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

server :: Settings -> Server API
server settings@(Settings token _ _ _) secret update = if secret == token
	then liftIO $ webhook settings update else throwError err403

webhook :: Settings -> Update -> IO ()
webhook settings u@(Update { message = Just (Message { text = Just "/participate" , chat = Chat { chat_id = cid }, from = Just user }) }) =
	void . async $ participate settings (ChatId cid) user
webhook settings@(Settings _ (ChatId cid') _ _) u@(Update { message = Just (Message { text = Just "/vote", chat = Chat { chat_id = cid } }) }) =
		if cid' == cid then void . async $ initiate settings (ChatId cid) else pure ()
webhook settings u@(Update { callback_query = Just (CallbackQuery { cq_from = user, cq_data = Just candidate, cq_message = Just (Message { message_id = mid }) }) }) =
	maybe (pure ()) (void . async . vote settings mid user) $ readMaybe (T.unpack candidate)
webhook _ u = print u

main = settings >>= run 8080 . serve (Proxy :: Proxy API) . server
