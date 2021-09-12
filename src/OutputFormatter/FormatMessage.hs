module OutputFormatter.FormatMessage where

import OutputFormatter.Colors
    ( backgroundError,
      backgroundInfo,
      backgroundSuccess,
      backgroundWarning,
      reset,
      textError,
      textInfo,
      textSuccess,
      textWarning )

_error :: Show a => a -> [Char]
_error msg = backgroundError ++ " [Error] "
     ++ reset ++ " " ++ textError ++ " " ++ show msg ++ " " ++ reset

_warning :: Show a => a -> [Char]
_warning msg = backgroundWarning ++ " [Warning] "
    ++ reset ++ " " ++ textWarning ++ " " ++ show msg ++ " " ++ reset

_success :: Show a => a -> [Char]
_success msg = backgroundSuccess ++ " [Success] "
    ++ reset ++ " " ++ textSuccess ++ " " ++ show msg ++ " " ++ reset

_info :: Show a => a -> [Char]
_info msg = textInfo ++ " " ++ show msg ++ " " ++ reset

_backgroundedInfo :: [Char] -> [Char]
_backgroundedInfo txt = backgroundInfo ++ txt ++ reset