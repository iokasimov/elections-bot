{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.API.Telegram.Bot.Elections.Server (API, server) where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure, (*>))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Bool ((||))
import "base" Data.Eq ((/=))
import "base" Data.Function ((.), ($))
import "base" Data.Functor (void)
import "base" System.IO (print)
import "tagged" Data.Tagged (Tagged (Tagged))
import "transformers" Control.Monad.Trans.Class (lift)
import "lens" Control.Lens ((^.))
import "servant-server" Servant (Capture, ReqBody, Server, JSON, Post, FromHttpApiData, ToHttpApiData, type (:>), err403, throwError)
import "telega" Network.API.Telegram.Bot (Telegram, Token (Token), telegram)
import "telega" Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (identificator), Persistable (request), Capacity (Purge))
import "telega" Network.API.Telegram.Bot.Object (Callback (Datatext), Origin (Group), Content (Textual, Command), Message (Direct))
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
webhook (Query _ (Datatext cbq_id (Direct _ (Group chat_id _ sender) (Textual _)) dttxt)) = vote cbq_id sender dttxt
webhook (Incoming _ (Direct msg_id (Group chat_id _ sender) (Command "initiate"))) = initiate sender *> request @Purge @Message @() (Tagged (chat_id, msg_id)) *> conduct
webhook (Incoming _ (Direct msg_id (Group chat_id _ sender) (Command "participate"))) = participate sender *> request @Purge @Message @() (Tagged (chat_id, msg_id))
webhook _ = pure ()
