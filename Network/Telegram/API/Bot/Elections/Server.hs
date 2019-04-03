module Network.Telegram.API.Bot.Elections.Server (API, server) where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure, (*>))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Bool ((||))
import "base" Data.Eq ((/=))
import "base" Data.Function ((.), ($))
import "base" Data.Functor (void)
import "lens" Control.Lens ((^.))
import "servant-server" Servant (Capture, ReqBody, Server, JSON, Post, FromHttpApiData, ToHttpApiData, type (:>), err403, throwError)
import "telega" Network.Telegram.API.Bot (Telegram, Token (Token), telegram)
import "telega" Network.Telegram.API.Bot.Capacity (purge)
import "telega" Network.Telegram.API.Bot.Property (identificator)
import "telega" Network.Telegram.API.Bot.Object (Callback (Datatext), Chat (Group), Message (Textual, Command))
import "telega" Network.Telegram.API.Bot.Object.Update (Update (Incoming, Query), chat)

import Network.Telegram.API.Bot.Elections.Configuration (Environment, Settings (Settings))
import Network.Telegram.API.Bot.Elections.Process (initiate, conduct, participate, vote)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

deriving instance ToHttpApiData Token
deriving instance FromHttpApiData Token

server :: Settings -> Server API
server (Settings locale token chat_id election_duration session votes) secret update =
	if secret /= token || (identificator $ update ^. chat) /= chat_id then throwError err403 else
		liftIO . void . async . telegram session token (chat_id, election_duration, votes) $ webhook update

webhook :: Update -> Telegram Environment ()
webhook (Query _ (Datatext cbq_id (Textual _ _ from _) dttxt)) = vote cbq_id from dttxt
webhook (Incoming _ (Command msg_id (Group chat_id _) from "initiate")) = initiate from *> purge @Message (chat_id, msg_id) *> conduct
webhook (Incoming _ (Command msg_id (Group chat_id _) from "participate")) = participate from *> purge @Message (chat_id, msg_id)
webhook _ = pure ()
