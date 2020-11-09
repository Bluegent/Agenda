module Misc.Menu where
import Data.Time
import Agenda.Utils

{-|
searchContactByName :: int
searchContactByName = do
    putStr("Input name:")
    line <- getLine
    return 0
    
searchContactMenu :: int
searchContactMenu = do
    putStrLn "Search by what?"
    putStrLn "[n]ame"
    putStrLn "[p]hone"
    putStrLn "[e]-mail"
    putStrLn "[q]uit"
    input <- getChar
    case input of
        'n' -> return searchContactByName
        'p' -> return searchContactByName
        'e' -> return searchContactByName
        'q' -> return welcomeMenu
        _   -> return searchContactMenu
    return 1

searchAppointmentMenu :: int
searchAppointmentMenu = do
    putStrLn "Search by what?"
    return 0
    
printTodayAppointments :: int
printTodayAppointments = do
    putStrLn "Your appointments are:"
    return 0

-}
invalidChoice :: (IO()) -> IO ()
invalidChoice func = do
    putStrLn ""
    putStrLn "================Invalid choice================="
    putStrLn ""
    func



printSeparator = putStrLn "==============================================="

searchContactMenu :: IO()
searchContactMenu = do
    printSeparator
    putStrLn "Search by what?"
    putStrLn "[n]ame"
    putStrLn "[p]hone"
    putStrLn "[e]-mail"
    putStrLn "[q]uit"
    input <- readChar
    case input of
        'n' -> do 
            putStrLn "This is where you would search if it worked..."
        'q' -> return ()
        _ -> invalidChoice searchContactMenu

welcomeMenu :: IO Int
welcomeMenu = do
    printSeparator
    putStrLn "What would you like to do?"
    putStrLn "search [c]ontacts"
    putStrLn "search [a]ppointments"
    putStrLn "[m]anage agenda"
    putStrLn "[l]ist today's appointments"
    putStrLn "[q]uit"
    line <- readChar
    case line of
        'c' -> do
                searchContactMenu 
                return 0
--        'a' -> return searchAppointmentMenu
--        't' -> return searchContactMenu
        'q' -> return 1
        _ -> return 0