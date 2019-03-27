module Main where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure, (*>))
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad (void, (>>=))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Eq (Eq ((==), (/=)))
import "base" Data.Function ((.), ($))
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" System.IO (IO, print)
import "base" Text.Read (readMaybe)
import "servant-server" Servant (Capture, ReqBody, Proxy (Proxy), Server, JSON, PlainText, Get, Post
	, FromHttpApiData, ToHttpApiData, type (:>), serve, err403, throwError)
import "telega" Network.Telegram.API.Bot.Capacity.Editable (Editable (edit), Substitution)
import "telega" Network.Telegram.API.Bot.Capacity.Postable (Postable (post), Initial)
import "telega" Network.Telegram.API.Bot.Capacity.Purgeable (Purgeable (purge), Marking)
import "telega" Network.Telegram.API.Bot.Object.Button (Button (Button), Pressed (Open, Callback))
import "telega" Network.Telegram.API.Bot.Object.From (From)
import "telega" Network.Telegram.API.Bot.Object.Callback (Callback (Datatext))
import "telega" Network.Telegram.API.Bot.Object.Keyboard (Keyboard (Inline), Substitution)
import "telega" Network.Telegram.API.Bot.Object.Message (Message (Message), Initial, Marking)
import "telega" Network.Telegram.API.Bot.Object.Update (Update (Incoming, Query))
import "telega" Network.Telegram.API.Bot (Telegram, Token (Token), telegram)
import "transformers" Control.Monad.Trans.Class (lift)
import "warp" Network.Wai.Handler.Warp (run)

import Mafia.Configuration (Settings (Settings), settings)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

deriving instance ToHttpApiData Token
deriving instance FromHttpApiData Token

server :: Settings -> Server API
server settings@(Settings token group_chatid manager _) secret update =
	if secret /= token then throwError err403 else do
		liftIO $ webhook settings update

webhook :: Settings -> Update -> IO ()
webhook _ (Query _ u) = print u
webhook _ (Incoming _ u) = print u

test_inline_keyboard :: Keyboard
test_inline_keyboard = Inline . pure $ Button "click me" (Callback "!") :
	Button "do not click me" (Open "http://www.nooooooooooooooo.com") : []

main = do
	Settings token chat_id session votes <- settings
	telegram session token chat_id $ do
		post @Message (chat_id, "hello there", Just test_inline_keyboard)
		lift . lift . run 8080 . serve (Proxy :: Proxy API) . server $
			Settings token chat_id session votes
