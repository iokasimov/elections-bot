{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.API.Telegram.Bot.Elections.Server (API, server) where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure, (*>))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Eq ((/=))
import "base" Data.Function ((.), ($))
import "base" Data.Functor (void)
import "servant-server" Servant (Capture, ReqBody, Server, JSON, Post, FromHttpApiData, ToHttpApiData, type (:>), err403, throwError)
import "telega" Network.API.Telegram.Bot (Telegram, Token (Token), telegram)
import "telega" Network.API.Telegram.Bot.Property (Persistable (persist))
import "telega" Network.API.Telegram.Bot.Object (Callback (Datatext), Origin (Group), Content (Command))
import "telega" Network.API.Telegram.Bot.Object.Update.Message (Message (Direct), Delete (Delete))
import "telega" Network.API.Telegram.Bot.Object.Update (Update (Incoming, Query))

import Network.API.Telegram.Bot.Elections.Configuration (Environment, Settings (Settings))
import Network.API.Telegram.Bot.Elections.Process (initiate, conduct, participate, vote)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

deriving instance ToHttpApiData Token
deriving instance FromHttpApiData Token

server :: Settings -> Server API
server (Settings locale token chat_id election_duration session votes) secret update =
	if secret /= token then throwError err403 else
		liftIO . void . async . telegram session token (locale, chat_id, election_duration, votes) $ webhook update

webhook :: Update -> Telegram Environment ()
webhook (Query _ (Datatext cbq_id sender _ dttxt)) = vote cbq_id sender dttxt
webhook (Incoming _ (Direct msg_id (Group chat_id _ sender) (Command "initiate"))) =
	initiate sender *> persist (Delete @Message chat_id msg_id) *> conduct
webhook (Incoming _ (Direct msg_id (Group chat_id _ sender) (Command "participate"))) =
	participate sender *> persist (Delete @Message chat_id msg_id)
webhook _ = pure ()
