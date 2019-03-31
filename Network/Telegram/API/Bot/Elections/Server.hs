module Network.Telegram.API.Bot.Elections.Server (API, server) where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure, (*>))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Eq ((/=))
import "base" Data.Int (Int64)
import "base" Data.Function ((.), ($))
import "base" Data.Functor (void)
import "servant-server" Servant (Capture, ReqBody, Server, JSON, Post, FromHttpApiData, ToHttpApiData, type (:>), err403, throwError)
import "stm" Control.Concurrent.STM (TVar)
import "telega" Network.Telegram.API.Bot (Telegram, Token (Token), telegram)
import "telega" Network.Telegram.API.Bot.Capacity (purge)
import "telega" Network.Telegram.API.Bot.Object (Callback (Datatext), Chat (Group), Message (Command), Update (Incoming, Query))
import "wreq" Network.Wreq.Session (Session)

import Network.Telegram.API.Bot.Elections.Process (initiate, conduct, participate, vote)
import Network.Telegram.API.Bot.Elections.State (Votes)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

deriving instance ToHttpApiData Token
deriving instance FromHttpApiData Token

server :: Session -> Token -> Int64 -> TVar Votes -> Server API
server session token chat_id votes secret update = if secret /= token then throwError err403
	else liftIO . void . async . telegram session token (chat_id, votes) $ webhook update

webhook :: Update -> Telegram (Int64, TVar Votes) ()
webhook (Query _ (Datatext from _ txt)) = vote from txt
webhook (Incoming _ (Command msg_id (Group chat_id _) from "initiate")) = initiate from *> purge @Message (chat_id, msg_id) *> conduct
webhook (Incoming _ (Command msg_id (Group chat_id _) from "participate")) = participate from *> purge @Message (chat_id, msg_id)
webhook _ = pure ()
