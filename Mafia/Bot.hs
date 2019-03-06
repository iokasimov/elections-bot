module Main where

import Control.Monad.IO.Class (liftIO)
-- import Data.Text.IO (putStrLn)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, ReqBody, Proxy (Proxy), Server, JSON, Get, Post, type (:>), serve, err403, throwError)
import Web.Telegram.API.Bot.API (Token (Token))
import Web.Telegram.API.Bot.Data (Chat (..), Message (..), Update (..))
import Web.Telegram.API.Bot.Requests (ChatId (ChatId))

import Mafia.Configuration (Settings (Settings), settings)
import Mafia.Voting (vote)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

server :: Settings -> Server API
server settings@(Settings token _ _ _) secret update = if secret == token
	then liftIO $ webhook settings update else throwError err403

webhook :: Settings -> Update -> IO ()
webhook settings u@(Update { message = Just (Message { text = Just "/vote" }) }) = vote settings u
webhook _ _ = pure ()

main = settings >>= run 8080 . serve (Proxy :: Proxy API) . server
