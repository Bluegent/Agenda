module Misc.Menu where
import Data.Time

dummyMenu = do
    putStrLn "What would you like to do today?"
    putStrLn "a - tell me what 2 + 2 is"
    putStrLn "b - tell me the date"
    line <- getLine
    case line of
        "a" -> print $ 2+2
        "b" -> print =<< getZonedTime
        _ -> putStrLn "That was not an option..."