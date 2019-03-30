module Network.Telegram.API.Bot.Elections.Configuration (Settings (..), settings) where

import "base" Data.Int (Int64)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Control.Applicative ((<*>))
import "base" Data.Maybe (Maybe (Nothing))
import "base" System.IO (IO)
import "base" Prelude (negate)
import "optparse-applicative" Options.Applicative (Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import "stm" Control.Concurrent.STM (TVar, newTVarIO)
import "telega" Network.Telegram.API.Bot (Token (Token))
import "text" Data.Text (pack)
import "wreq" Network.Wreq.Session (Session, newAPISession)

import Network.Telegram.API.Bot.Elections.State (Votes)

data Arguments = Arguments Token Int64

options :: Parser Arguments
options = Arguments <$> token <*> chat_id where

	token :: Parser Token
	token = Token . pack <$> argument str (metavar "TOKEN")

	chat_id :: Parser Int64
	chat_id = negate <$> argument auto (metavar "CHATID")

data Settings = Settings Token Int64 Session (TVar Votes)

settings :: IO Settings
settings = do
	Arguments token chaid <- execParser $ info options fullDesc
	Settings token chaid <$> newAPISession <*> newTVarIO Nothing
