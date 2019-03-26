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
import "servant-server" Servant (Capture, ReqBody, Proxy (Proxy)
	, Server, JSON, Get, Post, type (:>), serve, err403, throwError)
import "telega" Network.Telegram.API.Bot.Capacity.Postable (Postable (post), Initial)
import "telega" Network.Telegram.API.Bot.Capacity.Purgeable (Purgeable (purge), Marking)
import "telega" Network.Telegram.API.Bot.Object.Chat (Chat (Group))
import "telega" Network.Telegram.API.Bot.Object.Keyboard (Keyboard (Inline), Button (Button), Pressed (Open))
import "telega" Network.Telegram.API.Bot.Object.Message (Message (Message), Initial, Marking)
import "telega" Network.Telegram.API.Bot.Object.Update (Update (Incoming))
import "telega" Network.Telegram.API.Bot (Telegram, Token (Token), telegram)
import "transformers" Control.Monad.Trans.Class (lift)
import "warp" Network.Wai.Handler.Warp (run)

import Mafia.Configuration (Settings (Settings), settings)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

server :: Settings -> Server API
server settings@(Settings token group_chatid manager _) secret update =
	if secret /= token then throwError err403 else do
		liftIO $ webhook settings update

webhook :: Settings -> Update -> IO ()
webhook _ (Incoming _ u) = print u

test_inline_keyboard :: Keyboard
test_inline_keyboard = Inline . pure $
	Button "click me" (Open "http://www.nooooooooooooooo.com") :
	Button "do not click me" (Open "http://www.nooooooooooooooo.com") : []

main = do
	Settings token chatid session _ <- settings
	-- Post and purge the message with timeout
	telegram session token chatid $ do
		msg <- post @Message (chatid, "hello there", Just test_inline_keyboard)
		let Message msg_id (Group chat_id _) _ _ = msg
		lift . lift $ print msg *> print "wait..." *> threadDelay 5000000
		purge @Message (chat_id, msg_id)
