module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Options.Applicative (Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, ReqBody, Proxy (Proxy), Server, JSON, Get, Post, type (:>), serve, err403, throwError)
import Web.Telegram.API.Bot.API (Token (Token), sendMessage)
import Web.Telegram.API.Bot.Data (Chat (..), Message (..), Update (..))
import Web.Telegram.API.Bot.Requests (ChatId (ChatId), SendMessageRequest (..), ReplyKeyboard (ReplyKeyboardRemove))

import qualified Data.Text as T

data Options = Options Token ChatId deriving Show

options :: Parser Options
options = Options
	<$> (Token . T.pack <$> argument str (metavar "TOKEN"))
	<*> (ChatId <$> argument auto (metavar "CHATID"))

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

server :: Manager -> Options -> Server API
server manager opts@(Options token _) secret update = if secret == token
	then liftIO $ webhook manager opts update
	else throwError err403

webhook :: Manager -> Options -> Update -> IO ()
webhook _ _ _ = pure ()

main = do
	opts <- execParser $ info options fullDesc
	manager <- newManager tlsManagerSettings
	run 8080 (serve api $ server manager opts)
		where api = Proxy :: Proxy API
