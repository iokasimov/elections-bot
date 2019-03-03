import Control.Monad.IO.Class (liftIO)
import Options.Applicative (Parser, execParser, argument, info, fullDesc, metavar, str)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, ReqBody, Proxy (Proxy), Server, JSON, Get, Post, type (:>), serve, err403, throwError)
import Web.Telegram.API.Bot.API (Token (Token))
import Web.Telegram.API.Bot.Data (Update (..))

import qualified Data.Text as T

data Options = Options Token deriving Show

options :: Parser Options
options = fmap Options $ fmap (Token . T.pack) $
	argument str $ metavar "TOKEN"

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

server :: Token -> Server API
server _ _ _ = liftIO $ print "REACHED!"

main = do
	Options token <- execParser $ info options fullDesc
	manager <- newManager tlsManagerSettings
	run 8080 (serve api $ server token)
		where api = Proxy :: Proxy API
