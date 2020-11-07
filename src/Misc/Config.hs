module Misc.Config where

import Data.Either.Utils
import Data.ConfigFile

greetUser configParser = putStrLn $ "Hello, " ++ (forceEither $ get configParser "MISC" "callout") ++"!"

getConfigPath :: ConfigParser -> String -> String
getConfigPath cp opt = forceEither $ get cp "PATHS" opt