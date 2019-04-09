module Network.Telegram.API.Bot.Elections.Scores (Scores, nomination, consider) where

import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (find)
import "base" Data.Function (const, (.), ($), (&))
import "base" Data.Int (Int)
import "base" Data.List (delete)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Tuple (fst)
import "lens" Control.Lens (element, _2, (%~))

type Scores subject object = [(subject, [object])]

-- Application for participation
nomination :: Eq subject => subject -> Scores subject object -> Maybe (Scores subject object)
nomination x scores = scores & find ((==) x . fst) & maybe (Just $ (x, []) : scores) (const $ Nothing)

-- If you already voted for this candidate, your vote will be removed
consider :: Eq object => Int -> object -> Scores subject object -> Scores subject object
consider subject_index x xs = xs & element subject_index . _2 %~
	(\scores -> maybe (x : scores) (const $ delete x scores) . find (== x) $ scores)
