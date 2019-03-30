module Main where

import "base" Data.Function ((.), ($))
import "servant-server" Servant (Proxy (Proxy), serve)
import "warp" Network.Wai.Handler.Warp (run)

import Network.Telegram.API.Bot.Elections.Configuration (Settings (..), settings)
import Network.Telegram.API.Bot.Elections.Server (API, server)

main = do
	Settings token chat_id session votes <- settings
	run 8080 . serve (Proxy :: Proxy API) $ server session token chat_id votes
