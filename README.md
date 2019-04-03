# Elections bot for group chats

How to run
--------------------------------------------------------------------------------
`stack exec bot <LOCALE> <TELEGRAM_TOKEN> <CHAT_ID (without minus)> <ELECTION_DURATION (in minutes)>`

For example, elections will be held within 5 minutes, english messages:

`stack exec bot EN "bot111111111:tkn" 111111111 5`

How it works
--------------------------------------------------------------------------------
* Someone should initiate elections with `/initiate` command.
* Then anybody can become a candidate with `/participate` command.
* During election period anybody can vote! (with keyboard)

Supported locales: 🇬🇧 EN | 🇷🇺 RU
