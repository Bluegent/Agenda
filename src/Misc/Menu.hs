module Misc.Menu where
import Data.Time
import Agenda.Utils
import Agenda.Contact
import Agenda.Appointment
import System.Exit
import Misc.Config
import Data.ConfigFile
import System.Console.ANSI
import Control.Monad
import qualified Data.Vector as V

data Database = 
    Database { contacts     :: V.Vector Contact
             , appointments :: V.Vector Appointment
             , cfg          :: ConfigParser
             }

invalidChoice  = putMenuStrLn "\nInvalid choice!\n"


searchContactByFunc :: Database -> (Contact->String) -> IO()
searchContactByFunc db func = do
    line <- getLine
    putMenuStrLn "Results:"
    searchContactByString (contacts db) func line
    waitForUserRead
    searchContactMenu db

searchContactMenu :: Database -> IO()
searchContactMenu db = do
    resetScreen
    putMenuStrLn "Search by what? - [n]ame, [p]hone, [e]-mail, [b]ack"
    input <- readChar
    case input of
        'n' -> do
            putMenuStr "Input name:" 
            searchContactByFunc db Agenda.Contact.name
        'p' -> do
            putMenuStr "Input phone:" 
            searchContactByFunc db Agenda.Contact.phone
        'e' -> do
            putMenuStr "Input e-mail:" 
            searchContactByFunc db Agenda.Contact.email
        'b' -> return ()
        _ -> do 
            invalidChoice 
            searchContactMenu db

searchAppointmentByFunc :: Database -> (Appointment->String) -> IO()
searchAppointmentByFunc db func = do
    line <- getLine
    putMenuStrLn "Results:"
    searchApptByString (appointments db) func line
    waitForUserRead
    searchAppointmentMenu db

searchAppointmentByExactDate :: Database -> IO()
searchAppointmentByExactDate db = do
    putMenuStrLn "Accepted date formats are \"dd/mm/yy hh:mm\" or \"dd/mm/yyyy\" or \"dd/mm\"(will use current year) or \"hh:mm\"(will use current date)."
    putMenuStr "Enter date:"
    line <- getLine
    putMenuStrLn "Results:"
    current <- getCurrentTime
    baseDate <- utcToLocal current
    case (parseDateMaybe line baseDate) :: Maybe LocalTime of 
        Just time -> do
            putMenuStrLn $ "Searching for appointments that contain date " ++ show time
            utcTime <- localToUtc time
            searchApptByExactDate  (appointments db) utcTime
        Nothing -> putMenuStrLn "Invalid input."
    waitForUserRead

searchAppointmentByDateRange :: Database -> IO()
searchAppointmentByDateRange db = do
    putMenuStrLn "Accepted date formats are \"dd/mm/yy hh:mm\" or \"dd/mm/yyyy\" or \"dd/mm\"(will use current year) or \"hh:mm\"(will use current date)."
    putMenuStr "Enter start date:"
    startLine <- getLine
    current <- getCurrentTime
    baseDate <- utcToLocal current
    case (parseDateMaybe startLine baseDate) :: Maybe LocalTime of 
        Just startTime -> do
            putMenuStr "Enter end date:"
            endLine <- getLine
            case (parseDateMaybe endLine baseDate) :: Maybe LocalTime of 
                Just endTime -> do
                    putMenuStrLn $ "Searching for appointments between " ++ show startTime ++" and " ++ show endTime
                    putMenuStrLn "\nResults:"
                    startUtc <- localToUtc startTime
                    endUtc <- localToUtc endTime
                    searchApptByDateRange  (appointments db) startUtc endUtc
                Nothing -> putMenuStrLn "Invalid input."
        Nothing -> putMenuStrLn "Invalid input."
    waitForUserRead


searchAppointmentMenu :: Database -> IO()
searchAppointmentMenu db = do
    resetScreen
    putMenuStrLn "Search by what? - [n]ame, [d]etails, [e]xact date, [r]ange of dates, [b]ack"
    input <- readChar
    showCursor
    case input of
        'n' -> do
            putMenuStr "Input name:" 
            searchAppointmentByFunc db Agenda.Appointment.name
        'd' -> do
            putMenuStr "Input details search term:" 
            searchAppointmentByFunc db Agenda.Appointment.details
        'e' -> do
            searchAppointmentByExactDate db
        'r' -> do
            searchAppointmentByDateRange db
        'b' -> return ()
        _ -> do 
            invalidChoice 
            searchAppointmentMenu db

printToday :: V.Vector Appointment -> IO()
printToday list = do
    resetScreen
    putMenuStrLn "Today's appointments:"
    current <- getCurrentTime
    let start = UTCTime {utctDay = utctDay current, utctDayTime = secondsToDiffTime 0}
    let end = addUTCTime nominalDay start
    searchApptByDateRange list start end
    waitForUserRead


addContactMenu :: V.Vector Contact -> IO (V.Vector Contact)
addContactMenu contacts = do
    resetScreen
    showCursor
    contact <- readContact
    return $ V.snoc contacts contact

removeContactMenu :: V.Vector Contact -> IO (V.Vector Contact)
removeContactMenu contacts = do
    resetScreen
    showCursor
    putMenuStr "Input the exact name of the contact you wish to remove:"
    line <- getLine
    let found = searchContactsByName line contacts
    if length found == 0 then do
        putMenuStrLn "No contacts found with that name."
        waitForUserRead
        return contacts
    else do
        putMenuStrLn "Are you sure you want to delete the following contact(s):\n"
        printContacts found
        confirmation <- getConfirmation
        if confirmation then do
            putMenuStrLn $ "Deleting " ++ (show (length found)) ++ " contact(s)..."
            waitForUserRead
            return (removeAll contacts found)
        else return contacts

editContact :: Contact -> IO Contact
editContact contact = do
    resetScreen
    putMenuStrLn "Editing contact:" 
    printContact contact
    putMenuStrLn "Edit options: edit [n]ame, edit [p]hone, edit [e]-mail, [b]ack"
    input <- readChar
    case input of
        'n' -> do
            putMenuStr "Input new name:"
            line <- getLine
            let newContact = Contact { Agenda.Contact.name = line, phone = phone contact, email = email contact}
            editContact newContact
        'p' -> do
            line <- readPhone
            let newContact = Contact { Agenda.Contact.name = Agenda.Contact.name contact, phone = line, email = email contact}
            editContact newContact   
        'e' -> do
            line <- readEmail
            let newContact = Contact { Agenda.Contact.name = Agenda.Contact.name contact, phone = phone contact, email =  line}
            editContact newContact
        'b' -> return contact
        _ -> do 
            invalidChoice
            editContact contact


editContactMenu :: V.Vector Contact -> IO (V.Vector Contact)
editContactMenu contacts = do
    resetScreen
    showCursor
    putMenuStr "Input the exact name of the contact you would like to edit:"
    line <- getLine
    let found = searchContactsByName line contacts
    
    if length found == 0 then do
        putMenuStrLn "No contacts found with that name."
        waitForUserRead
        return contacts
    else do
        when(length found > 1) $ putMenuStrLn "Multiple contacts found with that name, editing the first one."
        newContact <- editContact (found V.! 0)
        let remaining = removeElem contacts (found V.! 0)
        return (V.snoc remaining newContact)

manageContactsMenu :: V.Vector Contact -> IO (V.Vector Contact)
manageContactsMenu contacts = do
    resetScreen
    putMenuStrLn "Manage contacts: [a]dd contact, [r]emove contact, [e]dit contact , [b]ack"
    line <- readChar
    case line of
        'a' -> addContactMenu contacts  
        'r' -> removeContactMenu contacts
        'e' -> editContactMenu contacts
        'b' -> return contacts
        _ -> do 
            invalidChoice 
            manageContactsMenu contacts

addAppointmentMenu :: V.Vector Appointment -> IO (V.Vector Appointment)
addAppointmentMenu appointments = do
    resetScreen
    appointment <- readAppointment
    return $ V.snoc appointments appointment


getConfirmation :: IO Bool
getConfirmation = do
    putMenuStr "y/n:"
    line <- getLine
    case line of
        "y" -> return True
        "n" -> return False
        _ -> getConfirmation

removeAppointmentMenu :: V.Vector Appointment -> IO (V.Vector Appointment)
removeAppointmentMenu appointments = do
    resetScreen
    showCursor
    putMenuStr "Input the exact name of the appointment you wish to remove:"
    line <- getLine
    let found = searchAppointmentsByName line appointments
    if length found == 0 then do
        putMenuStrLn "No appointments found with that name."
        waitForUserRead
        return appointments
    else do
        putMenuStrLn "Are you sure you want to delete the following appointments(s):\n"
        printAppointments found
        confirmation <- getConfirmation
        if confirmation then do
            putMenuStrLn $ "Deleting " ++ (show (length found)) ++ " appointment(s)..."
            waitForUserRead
            return (removeAll appointments found)
        else return appointments

editAppointment :: Appointment -> IO Appointment
editAppointment appointment = do
    resetScreen
    putMenuStrLn "Editing appointment:" 
    printAppointment appointment
    putMenuStrLn "Edit options: edit [n]ame, edit [d]etails, edit [s]tart date, edit [e]nd date, [b]ack"
    input <- readChar
    case input of
        'n' -> do
            putMenuStr "Input new name:"
            line <- getLine
            let newAppointment = Appointment { Agenda.Appointment.name = line, details = details appointment, startDate = startDate appointment, endDate = endDate appointment}
            editAppointment newAppointment
        'd' -> do
            putMenuStr "Input new details:"
            line <- getLine
            let newAppointment = Appointment { Agenda.Appointment.name = Agenda.Appointment.name appointment, details = line, startDate = startDate appointment, endDate = endDate appointment}
            editAppointment newAppointment
        's' -> do
            putMenuStr "Input new start date:"
            date <- readDate
            let newAppointment = Appointment { Agenda.Appointment.name = Agenda.Appointment.name appointment, details = details appointment, startDate = date, endDate = endDate appointment}
            editAppointment newAppointment
        'e' -> do
            putMenuStr "Input new end date:"
            date <- readDate
            let newAppointment = Appointment { Agenda.Appointment.name = Agenda.Appointment.name appointment, details = details appointment, startDate = startDate appointment, endDate = date}
            editAppointment newAppointment
        'b' -> return appointment
        _ -> do 
            invalidChoice
            editAppointment appointment

editAppointmentMenu :: V.Vector Appointment -> IO (V.Vector Appointment)
editAppointmentMenu appointments = do
    resetScreen
    showCursor
    putMenuStr "Input the exact name of the appointment you would like to edit:"
    line <- getLine
    let found = searchAppointmentsByName line appointments
    
    if length found == 0 then do
        putMenuStrLn "No appointments found with that name."
        waitForUserRead
        return appointments
    else do
        when(length found > 1) $ putMenuStrLn "Multiple appointments found with that name, editing the first one."
        newAppointment <- editAppointment $ found V.! 0
        let remaining = removeElem appointments (found V.! 0)
        return (V.snoc remaining newAppointment)

manageAppointmentMenu :: V.Vector Appointment -> IO (V.Vector Appointment)
manageAppointmentMenu appointments = do
    resetScreen
    putMenuStrLn "Manage appointments: [a]dd appointment, [r]emove appointment, [e]dit appointment , [b]ack"
    line <- readChar
    case line of
        'a' -> addAppointmentMenu appointments
        'r' -> removeAppointmentMenu appointments
        'e' -> editAppointmentMenu appointments
        'b' -> return appointments
        _ -> do 
            invalidChoice 
            manageAppointmentMenu appointments

manageMenu :: Database -> IO Database
manageMenu db = do
    resetScreen
    putMenuStrLn "Manage Agenda: [c]ontacts, [a]ppointments, [s]ave to file, [b]ack"
    line <- readChar
    case line of
        'c' -> do
            newContacts <- manageContactsMenu $ contacts db
            manageMenu Database { contacts = newContacts , appointments = appointments db, cfg = cfg db}
        'a' -> do
            newAppointments <- manageAppointmentMenu $ appointments db
            manageMenu Database { contacts = contacts db , appointments = newAppointments, cfg = cfg db}
        's' -> do
            let ctcPath = getConfigPath (cfg db) contactsPath
            let apptPath = getConfigPath  (cfg db) appointmentsPath
            
            writeContactList ctcPath (contacts db)
            writeAppointmentList apptPath (appointments db)
            return db
        'b' -> return db
        _ -> do 
            invalidChoice 
            manageMenu db


menuLoop :: Database -> IO()
menuLoop db = do
    resetScreen
    putStrLnCursor (menuCol - 5) "What would you like to do?"
    putMenuStrLn "search [c]ontacts"
    putMenuStrLn "search [a]ppointments"
    putMenuStrLn "[m]anage agenda"
    putMenuStrLn "[l]ist today's appointments"
    putMenuStrLn "list all c[o]ntacts"
    putMenuStrLn "list all a[p]pointments"
    putMenuStrLn "[q]quit"
    hideCursor
    line <- readChar
    case line of
        'c' -> do 
            searchContactMenu db
            menuLoop db
        'a' -> do 
            searchAppointmentMenu db
            menuLoop db
        'm' -> do
            newDb <- manageMenu db
            menuLoop newDb
        'l' -> do 
            printToday (appointments db)
            menuLoop db
        'o' -> do
            resetScreen
            printContacts (contacts db)
            waitForUserRead
            menuLoop db
        'p' -> do 
            resetScreen
            printAppointments (appointments db)
            waitForUserRead
            menuLoop db
        'q' -> exitWith ExitSuccess
        _ -> do
            invalidChoice
            menuLoop db
    