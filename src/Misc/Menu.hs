module Misc.Menu where
import Data.Time
import Agenda.Utils
import Agenda.Contact
import Agenda.Appointment

data Database = 
    Database { ctcs     :: [Contact]
             , appointments :: [Appointment]
            }

invalidChoice :: IO ()
invalidChoice  = do
    putStrLn ""
    putStrLn "================Invalid choice================="
    putStrLn ""




printSeparator = putStrLn "==============================================="


searchContactByFunc :: Database -> (Contact->String) -> IO()
searchContactByFunc db func = do
    line <- getLine
    searchContactByString (ctcs db) func line
    searchContactMenu db

searchContactMenu :: Database -> IO()
searchContactMenu db = do
    printSeparator
    putStrLn "Search by what?"
    putStrLn "[n]ame"
    putStrLn "[p]hone"
    putStrLn "[e]-mail"
    putStrLn "[q]uit"
    input <- readChar
    case input of
        'n' -> do
            putStrLn "Input name:" 
            searchContactByFunc db Agenda.Contact.name
        'p' -> do
            putStrLn "Input phone:" 
            searchContactByFunc db Agenda.Contact.phone
        'e' -> do
            putStrLn "Input e-mail:" 
            searchContactByFunc db Agenda.Contact.email
        'q' -> return ()
        _ -> do 
            invalidChoice 
            searchContactMenu db

welcomeMenu :: Database -> IO Int
welcomeMenu db = do
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
                searchContactMenu db
                return 0
--        'a' -> return searchAppointmentMenu
--        't' -> return searchContactMenu
        'q' -> return 1
        _ -> return 0