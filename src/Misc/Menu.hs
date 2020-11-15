module Misc.Menu where
import Data.Time
import Agenda.Utils
import Agenda.Contact
import Agenda.Appointment
import System.Exit

data Database = 
    Database { ctcs         :: [Contact]
             , appointments :: [Appointment]
             }

invalidChoice  = putStrLn "\n================Invalid choice=================\n"

printSeparator = putStrLn "==============================================="


searchContactByFunc :: Database -> (Contact->String) -> IO()
searchContactByFunc db func = do
    line <- getLine
    putStrLn "\nResults:"
    searchContactByString (ctcs db) func line
    searchContactMenu db

searchContactMenu :: Database -> IO()
searchContactMenu db = do
    printSeparator
    putStrLn "Search by what? - [n]ame, [p]hone, [e]-mail, [q]uit"
    input <- readChar
    case input of
        'n' -> do
            putStr "Input name:" 
            searchContactByFunc db Agenda.Contact.name
        'p' -> do
            putStr "Input phone:" 
            searchContactByFunc db Agenda.Contact.phone
        'e' -> do
            putStr "Input e-mail:" 
            searchContactByFunc db Agenda.Contact.email
        'q' -> return ()
        _ -> do 
            invalidChoice 
            searchContactMenu db

searchAppointmentByFunc :: Database -> (Appointment->String) -> IO()
searchAppointmentByFunc db func = do
    line <- getLine
    putStrLn "\nResults:"
    searchApptByString (appointments db) func line
    searchAppointmentMenu db



searchAppointmentByExactDate :: Database -> IO()
searchAppointmentByExactDate db = do
    putStrLn "Accepted date formats are \"dd/mm/yy hh:mm\" or \"mm/dd/yyyy\" or \"hh:mm\"."
    putStr "Enter date:"
    line <- getLine
    putStrLn "\nResults:"
    case (parseDateMaybe line) :: Maybe LocalTime of 
        Just time -> searchApptByExactDate  (appointments db) time
        Nothing -> putStrLn "Invalid input."


searchAppointmentMenu :: Database -> IO()
searchAppointmentMenu db = do
    printSeparator
    putStrLn "Search by what? - [n]ame, [d]etails, [e]xact date, [r]ange of dates, [q]uit"
    input <- readChar
    case input of
        'n' -> do
            putStr "Input name:" 
            searchAppointmentByFunc db Agenda.Appointment.name
        'd' -> do
            putStr "Input details search term:" 
            searchAppointmentByFunc db Agenda.Appointment.details
        'e' -> do
            searchAppointmentByExactDate db
        'r' -> do
            putStrLn "Not supported yet" 
        'q' -> return ()
        _ -> do 
            invalidChoice 
            searchContactMenu db

menuLoop :: Database -> IO()
menuLoop db = do
    let loop = do 
            printSeparator
            putStrLn "What would you like to do?"
            putStrLn "search [c]ontacts"
            putStrLn "search [a]ppointments"
            putStrLn "[m]anage agenda"
            putStrLn "[l]ist today's appointments"
            putStrLn "[q]uit"
            line <- readChar
            case line of
                'c' -> searchContactMenu db
                'a' -> searchAppointmentMenu db
                'm' -> putStrLn "Not supported yet"
                'l' -> putStrLn "Not supported yet"
                'q' -> exitWith ExitSuccess
                _ -> invalidChoice
            loop
    loop
    