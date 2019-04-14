{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Telegram.API.Bot.Elections.Server (API, server) where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure, (*>))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Bool ((||))
import "base" Data.Eq ((/=))
import "base" Data.Function ((.), ($))
import "base" Data.Functor (void)
import "base" System.IO (print)
import "transformers" Control.Monad.Trans.Class (lift)
import "lens" Control.Lens ((^.))
import "servant-server" Servant (Capture, ReqBody, Server, JSON, Post, FromHttpApiData, ToHttpApiData, type (:>), err403, throwError)
import "telega" Network.Telegram.API.Bot (Telegram, Token (Token), telegram)
import "telega" Network.Telegram.API.Bot.Property (Accessible (access), Identifiable (identificator), Persistable (request), Capacity (Purge'), PL (PL))
import "telega" Network.Telegram.API.Bot.Object (Callback (Datatext), Origin (Group), Content (Textual, Command), Message (Direct, Forward, Reply))
import "telega" Network.Telegram.API.Bot.Object.Update (Update (Incoming, Query))

import Network.Telegram.API.Bot.Elections.Configuration (Environment, Settings (Settings))
import Network.Telegram.API.Bot.Elections.Process (initiate, conduct, participate, vote)

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

deriving instance ToHttpApiData Token
deriving instance FromHttpApiData Token

server :: Settings -> Server API
server (Settings locale token chat_id election_duration session votes) secret update =
	if secret /= token then throwError err403 else
		liftIO . void . async . telegram session token (locale, chat_id, election_duration, votes) $ webhook update

webhook :: Update -> Telegram Environment ()
webhook (Query _ (Datatext cbq_id (Direct _ (Group chat_id _ sender) (Textual _)) dttxt)) = vote cbq_id sender dttxt
webhook (Incoming _ (Direct msg_id (Group chat_id _ sender) (Command "initiate"))) = initiate sender *> request @'Purge' @Message @() (PL (chat_id, msg_id)) *> conduct
webhook (Incoming _ (Direct msg_id (Group chat_id _ sender) (Command "participate"))) = participate sender *> request @'Purge' @Message @() (PL (chat_id, msg_id))
webhook _ = pure ()
