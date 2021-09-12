module OutputFormatter.Colors where

backgroundError :: [Char]
backgroundError = "\x1b[41m"

backgroundWarning :: [Char]
backgroundWarning = "\x1b[43m " ++ textBlack

backgroundInfo :: [Char]
backgroundInfo = "\x1b[44m \x1b[37;1m"

backgroundSuccess :: [Char]
backgroundSuccess = "\x1b[42;1m "

textError :: [Char]
textError = "\x1b[31m"

textWarning :: [Char]
textWarning = "\x1b[33m"

textInfo :: [Char]
textInfo = "\x1b[36m"

textBlack :: [Char]
textBlack = "\x1b[30m"

textSuccess :: [Char]
textSuccess = "\x1b[32m"

reset :: [Char]
reset = "\x1b[0m"