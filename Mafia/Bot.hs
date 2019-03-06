module Main where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Options.Applicative (Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, ReqBody, Proxy (Proxy), Server, JSON, Get, Post, type (:>), serve, err403, throwError)
import Web.Telegram.API.Bot.API (Token (Token))
import Web.Telegram.API.Bot.Data (Chat (..), Message (..), Update (..))
import Web.Telegram.API.Bot.Requests (ChatId (ChatId))

import qualified Data.Text as T

data Arguments = Arguments Token ChatId deriving Show

options :: Parser Arguments
options = Arguments
	<$> (Token . T.pack <$> argument str (metavar "TOKEN"))
	<*> (ChatId . negate <$> argument auto (metavar "CHATID"))

data Settings = Settings Token ChatId Manager (TVar [(Int, Int)])

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

server :: Settings -> Server API
server settings@(Settings token _ _ _) secret update = if secret == token
	then liftIO $ webhook settings update
	else throwError err403

webhook :: Settings -> Update -> IO ()
webhook _ _ = pure ()

main = do
	Arguments token chaid <- execParser $ info options fullDesc
	settings <- Settings token chaid <$> newManager tlsManagerSettings <*> newTVarIO []
	run 8080 . serve (Proxy :: Proxy API) $ server settings
