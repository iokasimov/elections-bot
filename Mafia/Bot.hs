module Main where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure, (<*>), (*>))
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad (void, (>>=))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Eq (Eq ((==), (/=)))
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Prelude (negate)
import "base" System.IO (IO, print)
import "base" Text.Read (readMaybe)
import "optparse-applicative" Options.Applicative (Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import "servant-server" Servant (Capture, ReqBody, Proxy (Proxy), Server, JSON
	, Get, Post, FromHttpApiData, ToHttpApiData, type (:>), serve, err403, throwError)
import "stm" Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import "telega" Network.Telegram.API.Bot.Capacity.Editable (Editable (edit), Substitution)
import "telega" Network.Telegram.API.Bot.Capacity.Postable (Postable (post), Initial)
import "telega" Network.Telegram.API.Bot.Capacity.Purgeable (Purgeable (purge), Marking)
import "telega" Network.Telegram.API.Bot.Object.Button (Button (Button), Pressed (Open, Callback))
import "telega" Network.Telegram.API.Bot.Object.From (From)
import "telega" Network.Telegram.API.Bot.Object.Callback (Callback (Datatext))
import "telega" Network.Telegram.API.Bot.Object.Keyboard (Keyboard (Inline), Substitution)
import "telega" Network.Telegram.API.Bot.Object.Message (Message (Textual, Command), Initial, Marking)
import "telega" Network.Telegram.API.Bot.Object.Update (Update (Incoming, Query))
import "telega" Network.Telegram.API.Bot (Telegram, Token (Token), telegram, ask')
import "transformers" Control.Monad.Trans.Class (lift)
import "warp" Network.Wai.Handler.Warp (run)
import "wreq" Network.Wreq.Session (Session, newAPISession)

import qualified "text" Data.Text as T (Text, pack)
import qualified "text" Data.Text.IO as T (putStrLn)

type Scores = [(From, [From])]

type Votes = Maybe (Int, Scores)

start_voting :: T.Text
start_voting = "Голосование началось - в течении следующих 5 минут вы можете указать игроков, с которыми вы хотели бы поиграть."

initiate :: Telegram (Int64, TVar Votes) ()
initiate = ask' >>= \(chat_id, votes) ->
	(lift . lift . atomically $ readTVar votes) >>= \case
		Just _ -> void $ post @Message (chat_id, "Идёт голосование...", Nothing)
		Nothing -> do
			msg <- post @Message (chat_id, start_voting, Just $ Inline [])
			let Textual keyboard_msg_id _ _ _ = msg
			lift . lift . atomically . writeTVar votes . Just $ (keyboard_msg_id, [])

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

deriving instance ToHttpApiData Token
deriving instance FromHttpApiData Token

server :: Session -> Token -> Int64 -> TVar Votes -> Server API
server session token chat_id votes secret update = if secret /= token then throwError err403
	else liftIO . void . telegram session token (chat_id, votes) $ webhook update

webhook :: Update -> Telegram (Int64, TVar Votes) ()
webhook (Query _ u) = lift . lift $ print u
webhook (Incoming _ (Textual _ _ _ txt)) = lift . lift $ T.putStrLn txt
webhook (Incoming _ (Command _ _ _ "vote")) = initiate

test_inline_keyboard :: Keyboard
test_inline_keyboard = Inline . pure $ Button "click me" (Callback "!") :
	Button "do not click me" (Open "http://www.nooooooooooooooo.com") : []

data Arguments = Arguments Token Int64

options :: Parser Arguments
options = Arguments
	<$> (Token . T.pack <$> argument str (metavar "TOKEN"))
	<*> (negate <$> argument auto (metavar "CHAT_ID"))

main = do
	Arguments token chat_id <- execParser $ info options fullDesc
	(session, votes) <- (,) <$> newAPISession <*> newTVarIO Nothing
	run 8080 . serve (Proxy :: Proxy API) $ server session token chat_id votes
