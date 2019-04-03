module Network.Telegram.API.Bot.Elections.Locales
	(Locale (..), Status (..), message) where

import "text" Data.Text (Text)

data Locale = EN | RU

data Status = Started | Absented | Proceeded | Considered | Ended

message :: Locale -> Status -> Text
message EN Started = "Voting has begun - during the next 5 minutes you can vote for the candidates."
message RU Started = "Голосование началось - в течении следующих 5 минут вы можете голосовать за кандидатов."
message EN Absented = "Elections are not initiated..."
message RU Absented = "Голосование не иницировано..."
message EN Proceeded = "Elections are in progress..."
message RU Proceeded = "Идёт голосование..."
message EN Considered = "Your vote has been counted!"
message RU Considered = "Ваш голос был учтен!"
message EN Ended = "Elections are over, results:"
message RU Ended = "Голосование окончилось, результаты:"
