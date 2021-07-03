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
import "joint" Control.Joint (adapt)
import "lens" Control.Lens ((^.))
import "servant-server" Servant (Capture, ReqBody, Server, JSON, Post, FromHttpApiData, ToHttpApiData, type (:>), err403, throwError)
import "telega" Network.API.Telegram.Bot (Telegram, Token (Token), telegram)
import "telega" Network.API.Telegram.Bot.Object (Callback (Datatext), Origin (Group), Content (Command))
import "telega" Network.API.Telegram.Bot.Object.Chat (Chat)
import "telega" Network.API.Telegram.Bot.Object.Update.Message (Message (Direct), Delete (Delete))
import "telega" Network.API.Telegram.Bot.Object.Update (Update (Incoming, Query))
import "telega" Network.API.Telegram.Bot.Property (Accessible (access), Persistable (persist), ID)

import Network.API.Telegram.Bot.Elections.Configuration (Environment, Settings (Settings))
import Network.API.Telegram.Bot.Elections.Process (initiate, conduct, participate, vote)

type API = "webhook" :> ReqBody '[JSON] Update :> Post '[JSON] ()

deriving instance ToHttpApiData Token
deriving instance FromHttpApiData Token

server :: Settings -> Server API
server (Settings locale token chat_id election_duration votes) update = do
	if update ^. access /= chat_id then pure () else
		liftIO . void . async . telegram token (locale, chat_id, election_duration, votes) $ webhook update

webhook :: Update -> Telegram Environment ()
webhook (Query _ (Datatext _ sender _ dttxt)) = vote sender dttxt
webhook (Incoming _ (Direct msg_id (Group chat_id _ sender) (Command cmd))) = case cmd of
	"initiate" -> adapt (print "INITIATE!") *> initiate sender *> del_cmd chat_id msg_id *> conduct
	"participate" -> adapt (print "PARTICIPATE!") *> participate sender *> del_cmd chat_id msg_id
	x -> adapt (print x)
webhook x = adapt (print x)

del_cmd :: ID Chat -> ID Message -> Telegram Environment ()
del_cmd chat_id msg_id = persist $ Delete @Message chat_id msg_id

