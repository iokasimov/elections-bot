module Main where

import "base" Control.Monad ((>>=))
import "base" Data.Function ((.))
import "servant-server" Servant (Proxy (Proxy), serve)
import "warp" Network.Wai.Handler.Warp (run)

import Network.Telegram.API.Bot.Elections.Configuration (settings)
import Network.Telegram.API.Bot.Elections.Server (API, server)

main = settings >>= run 8080 . serve (Proxy :: Proxy API) . server
