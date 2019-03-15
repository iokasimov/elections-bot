module Main where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure)
import "base" Control.Monad (void, (>>=))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Eq (Eq ((==)))
import "base" Data.Function ((.), ($))
import "base" Data.Maybe (Maybe (Just), maybe)
import "base" System.IO (IO, print)
import "base" Text.Read (readMaybe)
import "servant-server" Servant (Capture, ReqBody, Proxy (Proxy), Server, JSON, Get, Post, type (:>), serve, err403, throwError)
import "telegram-api" Web.Telegram.API.Bot.API (Token (Token))
import "telegram-api" Web.Telegram.API.Bot.Data (CallbackQuery (..), Chat (..), Message (..), Update (..), User (..))
import "telegram-api" Web.Telegram.API.Bot.Requests (ChatId (ChatId))
import "warp" Network.Wai.Handler.Warp (run)

import qualified Data.Text as T (take, unpack)

import Mafia.Bot.Environment (Settings (Settings), settings)
import Mafia.Bot.Voting (initiate, participate, vote)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

server :: Settings -> Server API
server settings@(Settings token _ _ _) secret update = if secret == token
	then liftIO $ webhook settings update else throwError err403

webhook :: Settings -> Update -> IO ()
webhook settings@(Settings _ (ChatId cid') _ _) u@(Update { message = Just (Message { message_id = msgid, text = Just "/participate" , chat = Chat { chat_id = cid }, from = Just user }) }) =
	if cid' == cid then void . async $ participate settings msgid user else pure ()
webhook settings@(Settings _ (ChatId cid') _ _) u@(Update { message = Just (Message { text = Just "/vote", chat = Chat { chat_id = cid } }) }) =
	if cid' == cid then void . async $ initiate settings else pure ()
webhook settings u@(Update { callback_query = Just (CallbackQuery { cq_from = user, cq_data = Just candidate, cq_message = Just (Message { message_id = mid }) }) }) =
	maybe (pure ()) (void . async . vote settings mid user) $ readMaybe (T.unpack candidate)
webhook _ u = print u

main = settings >>= run 8080 . serve (Proxy :: Proxy API) . server
