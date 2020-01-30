module Network.API.Telegram.Bot.Elections.Configuration
	(Environment, Settings (..), settings) where

import "base" Data.Int (Int)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Control.Applicative ((<*>))
import "base" Data.Maybe (Maybe (Nothing))
import "base" System.IO (IO)
import "base" Prelude (negate)
import "optparse-applicative" Options.Applicative
	(Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import "stm" Control.Concurrent.STM (TVar, newTVarIO)
import "telega" Network.API.Telegram.Bot (Token (Token))
import Network.API.Telegram.Bot.Object.Chat (Chat, ID (CHAT))
import "text" Data.Text (pack)

import Network.API.Telegram.Bot.Elections.Locales (Locale)
import Network.API.Telegram.Bot.Elections.State (Votes)

type Environment = (Locale, ID Chat, Int, TVar Votes)

data Arguments = Arguments Locale Token (ID Chat) Int

options :: Parser Arguments
options = Arguments <$> locale <*> token <*> chat_id <*> election_duration where

	locale :: Parser Locale
	locale = argument auto (metavar "LOCALE_LANGUAGE")

	token :: Parser Token
	token = Token . pack <$> argument str (metavar "TELEGRAM_TOKEN")

	chat_id :: Parser (ID Chat)
	chat_id = CHAT . negate <$> argument auto (metavar "CHAT_ID")

	election_duration :: Parser Int
	election_duration = argument auto (metavar "ELECTION_DURATION")

data Settings = Settings Locale Token (ID Chat) Int (TVar Votes)

settings :: IO Settings
settings = do
	Arguments locale token chat_id election_duration <- execParser $ info options fullDesc
	Settings locale token chat_id election_duration <$> newTVarIO Nothing
