module Mafia.Bot.State (Votes, Scores, start, consider, nomination) where

import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (find)
import "base" Data.Function (const, (.), ($), (&))
import "base" Data.Functor (fmap)
import "base" Data.Int (Int)
import "base" Data.List (delete)
import "base" Data.Maybe (Maybe (Just), maybe)
import "base" Data.Tuple (fst)
import "lens" Control.Lens (element, _2, (%~))
import "telegram-api" Web.Telegram.API.Bot.Data (User (User, user_id))

type Scores = [(User, [User])]

type Votes = Maybe (Int, Scores)

instance Eq User where
	lu == ru = user_id lu == user_id ru

start :: Int -> Votes
start keyboard_msg_id = Just (keyboard_msg_id, [])

-- If you already voted for this candidate, your vote will be removed
consider :: Int -> User -> Votes -> Votes
consider candidate_index voter = fmap . fmap $ \votes -> votes & element candidate_index . _2 %~
	(\scores -> maybe (voter : scores) (const $ delete voter scores) . find ((==) (user_id voter) . user_id) $ scores)

-- Application for participation
nomination :: User -> Votes -> Votes
nomination user = fmap . fmap $ \us ->
	maybe ((user, []) : us) (const us) . find ((==) user . fst) $ us
