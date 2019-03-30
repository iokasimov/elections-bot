module Main where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure, (<*>), (*>))
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad (void, (>>=))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Eq (Eq ((==), (/=)))
import "base" Data.Foldable (find, length)
import "base" Data.Function (const, id, (.), ($), (&))
import "base" Data.Functor (fmap, (<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Tuple (fst)
import "base" Prelude (negate)
import "base" System.IO (IO, print)
import "base" Text.Read (readMaybe)
import "base" Text.Show (show)
import "optparse-applicative" Options.Applicative (Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import "servant-server" Servant (Proxy (Proxy), serve)
import "stm" Control.Concurrent.STM (newTVarIO)
import "telega" Network.Telegram.API.Bot.Capacity.Editable (Editable (edit), Substitution)
import "telega" Network.Telegram.API.Bot.Capacity.Postable (Postable (post), Initial)
import "telega" Network.Telegram.API.Bot.Capacity.Purgeable (Purgeable (purge), Marking)
import "telega" Network.Telegram.API.Bot.Object.Button (Button (Button), Pressed (Open, Callback))
import "telega" Network.Telegram.API.Bot.Object.Chat (Chat (Group))
import "telega" Network.Telegram.API.Bot.Object.From (From (User))
import "telega" Network.Telegram.API.Bot.Object.Callback (Callback (Datatext))
import "telega" Network.Telegram.API.Bot.Object.Keyboard (Keyboard (Inline), Substitution)
import "telega" Network.Telegram.API.Bot.Object.Message (Message (Textual, Command), Initial, Marking)
import "telega" Network.Telegram.API.Bot.Object.Update (Update (Incoming, Query))
import "telega" Network.Telegram.API.Bot (Telegram, Token (Token), telegram, ask')
import "transformers" Control.Monad.Trans.Class (lift)
import "warp" Network.Wai.Handler.Warp (run)
import "wreq" Network.Wreq.Session (Session, newAPISession)

import qualified "text" Data.Text as T (Text, pack, unpack)
import qualified "text" Data.Text.IO as T (putStrLn)

import Network.Telegram.API.Bot.Elections.Server (API, server)

data Arguments = Arguments Token Int64

options :: Parser Arguments
options = Arguments
	<$> (Token . T.pack <$> argument str (metavar "TOKEN"))
	<*> (negate <$> argument auto (metavar "CHAT_ID"))

main = do
	Arguments token chat_id <- execParser $ info options fullDesc
	(session, votes) <- (,) <$> newAPISession <*> newTVarIO Nothing
	run 8080 . serve (Proxy :: Proxy API) $ server session token chat_id votes
