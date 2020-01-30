module Network.API.Telegram.Bot.Elections.State (Scores, Votes, nomination, consider) where

import "base" Data.Eq ((==))
import "base" Data.Foldable (find)
import "base" Data.Function (const, (.), ($), (&))
import "base" Data.Int (Int)
import "base" Data.List (delete)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Tuple (fst)
import "lens" Control.Lens (element, _2, (%~))
import "telega" Network.API.Telegram.Bot.Object.Sender (Sender)
import "telega" Network.API.Telegram.Bot.Object.Update.Message (Message, ID)

type Scores = [(Sender, [Sender])]

type Votes = Maybe (ID Message, Scores)

-- Application for participation
nomination :: Sender -> Scores -> Maybe Scores
nomination user scores = scores & find ((==) user . fst)
	& maybe (Just $ (user, []) : scores) (const $ Nothing)

-- If you already voted for this candidate, your vote will be removed
consider :: Int -> Sender -> Scores -> Scores
consider candidate_index voter votes = votes & element candidate_index . _2 %~
	(\scores -> maybe (voter : scores) (const $ delete voter scores) . find (== voter) $ scores)
