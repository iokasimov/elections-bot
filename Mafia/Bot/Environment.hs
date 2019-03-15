module Mafia.Bot.Environment (Settings (..), settings) where

import "base" Data.Int (Int)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Control.Applicative ((<*>))
import "base" Data.Maybe (Maybe (Nothing))
import "base" System.IO (IO)
import "base" Prelude (negate)
import "stm" Control.Concurrent.STM (TVar, newTVarIO)
import "optparse-applicative" Options.Applicative (Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import "http-client" Network.HTTP.Client (Manager, newManager)
import "http-client-tls" Network.HTTP.Client.TLS (tlsManagerSettings)
import "telegram-api" Web.Telegram.API.Bot.API (Token (Token))
import "telegram-api" Web.Telegram.API.Bot.Data (User (..))
import "telegram-api" Web.Telegram.API.Bot.Requests (ChatId (ChatId))
import "text" Data.Text (pack)

import Mafia.Bot.State (Votes)

data Arguments = Arguments Token ChatId

options :: Parser Arguments
options = Arguments
	<$> (Token . pack <$> argument str (metavar "TOKEN"))
	<*> (ChatId . negate <$> argument auto (metavar "CHATID"))

data Settings = Settings Token ChatId Manager (TVar Votes)

settings :: IO Settings
settings = do
	Arguments token chaid <- execParser $ info options fullDesc
	Settings token chaid <$> newManager tlsManagerSettings <*> newTVarIO Nothing
