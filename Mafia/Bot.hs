import Options.Applicative (Parser, argument, metavar, str)
import Web.Telegram.API.Bot.API (Token (..))

import qualified Data.Text as T

data Options = Options Token deriving Show

options :: Parser Options
options = fmap Options $ fmap (Token . T.pack) $
	argument str $ metavar "TOKEN"

main = print "typechecked"
