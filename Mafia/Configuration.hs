module Mafia.Configuration (Settings (..), settings) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Text (pack)
import Options.Applicative (Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot.API (Token (Token))
import Web.Telegram.API.Bot.Requests (ChatId (ChatId))

data Arguments = Arguments Token ChatId deriving Show

options :: Parser Arguments
options = Arguments
	<$> (Token . pack <$> argument str (metavar "TOKEN"))
	<*> (ChatId . negate <$> argument auto (metavar "CHATID"))

data Settings = Settings Token ChatId Manager (TVar [(Int, Int)])

settings :: IO Settings
settings = do
	Arguments token chaid <- execParser $ info options fullDesc
	Settings token chaid <$> newManager tlsManagerSettings <*> newTVarIO []
