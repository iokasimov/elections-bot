module Mafia.Configuration (Settings (..), settings) where

import "stm" Control.Concurrent.STM (TVar, newTVarIO)
import "optparse-applicative" Options.Applicative (Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import "http-client" Network.HTTP.Client (Manager, newManager)
import "http-client-tls" Network.HTTP.Client.TLS (tlsManagerSettings)
import "telegram-api" Web.Telegram.API.Bot.API (Token (Token))
import "telegram-api" Web.Telegram.API.Bot.Data (User (..))
import "telegram-api" Web.Telegram.API.Bot.Requests (ChatId (ChatId))
import "text" Data.Text (pack)

data Arguments = Arguments Token ChatId deriving Show

options :: Parser Arguments
options = Arguments
	<$> (Token . pack <$> argument str (metavar "TOKEN"))
	<*> (ChatId . negate <$> argument auto (metavar "CHATID"))

type Votes = TVar (Maybe (Int, [(User, [User])]))

data Settings = Settings Token ChatId Manager Votes

settings :: IO Settings
settings = do
	Arguments token chaid <- execParser $ info options fullDesc
	Settings token chaid <$> newManager tlsManagerSettings <*> newTVarIO Nothing
