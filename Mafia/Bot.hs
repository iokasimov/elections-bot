module Main where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure)
import "base" Control.Monad (void, (>>=))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Eq (Eq ((==), (/=)))
import "base" Data.Function ((.), ($))
import "base" Data.Maybe (Maybe (Just), maybe)
import "base" System.IO (IO, print)
import "base" Text.Read (readMaybe)
import "servant-server" Servant (Capture, ReqBody, Proxy (Proxy)
	, Server, JSON, Get, Post, type (:>), serve, err403, throwError)
import "telega" Network.Telegram.API.Bot.Update (Update (..))
import "telega" Network.Telegram.API.Bot (Telegram, Token (Token))
import "warp" Network.Wai.Handler.Warp (run)

import qualified Data.Text as T (take, unpack)

import Mafia.Configuration (Settings (Settings), settings)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

server :: Settings -> Server API
server settings@(Settings token group_chatid manager _) secret update =
	if secret /= token then throwError err403 else do
		liftIO $ webhook settings update

webhook :: Settings -> Update -> IO ()
webhook _ (Incoming _ u) = print u

main = settings >>= run 8080 . serve (Proxy :: Proxy API) . server
