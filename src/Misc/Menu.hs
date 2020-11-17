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
    putStrLn "Accepted date formats are \"dd/mm/yy hh:mm\" or \"dd/mm/yyyy\" or \"dd/mm\"(will use current year) or \"hh:mm\"(will use current date)."
    putStr "Enter date:"
    line <- getLine
    putStrLn "\nResults:"
    current <- getCurrentTime
    baseDate <- utcToLocal current
    case (parseDateMaybe line baseDate) :: Maybe LocalTime of 
        Just time -> do
            putStrLn $ "Searching for appointments that contain date " ++ show time
            utcTime <- localToUtc time
            searchApptByExactDate  (appointments db) utcTime
        Nothing -> putStrLn "Invalid input."

searchAppointmentByDateRange :: Database -> IO()
searchAppointmentByDateRange db = do
    putStrLn "Accepted date formats are \"dd/mm/yy hh:mm\" or \"dd/mm/yyyy\" or \"dd/mm\"(will use current year) or \"hh:mm\"(will use current date)."
    putStr "Enter start date:"
    startLine <- getLine
    current <- getCurrentTime
    baseDate <- utcToLocal current
    case (parseDateMaybe startLine baseDate) :: Maybe LocalTime of 
        Just startTime -> do
            putStr "Enter end date:"
            endLine <- getLine
            case (parseDateMaybe endLine baseDate) :: Maybe LocalTime of 
                Just endTime -> do
                    putStrLn $ "Searching for appointments between " ++ show startTime ++" and " ++ show endTime
                    putStrLn "\nResults:"
                    startUtc <- localToUtc startTime
                    endUtc <- localToUtc endTime
                    searchApptByDateRange  (appointments db) startUtc endUtc
                Nothing -> putStrLn "Invalid input."
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
            searchAppointmentByDateRange db
        'q' -> return ()
        _ -> do 
            invalidChoice 
            searchAppointmentMenu db

printToday :: [Appointment] -> IO()
printToday list = do
    putStrLn "\nToday's appointments:"
    current <- getCurrentTime
    let start = UTCTime {utctDay = utctDay current, utctDayTime = secondsToDiffTime 0}
    let end = addUTCTime nominalDay start
    searchApptByDateRange list start end

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
                'l' -> printToday (appointments db)
                'q' -> exitWith ExitSuccess
                _ -> invalidChoice
            loop
    loop
    