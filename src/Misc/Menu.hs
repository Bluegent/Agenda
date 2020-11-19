module Misc.Menu where
import Data.Time
import Agenda.Utils
import Agenda.Contact
import Agenda.Appointment
import System.Exit
import Misc.Config
import Data.ConfigFile
import qualified Data.Vector as V

data Database = 
    Database { contacts     :: V.Vector Contact
             , appointments :: V.Vector Appointment
             , cfg          :: ConfigParser
             }

invalidChoice  = putStrLn "\n================Invalid choice=================\n"

printSeparator = putStrLn "==============================================="


searchContactByFunc :: Database -> (Contact->String) -> IO()
searchContactByFunc db func = do
    line <- getLine
    putStrLn "\nResults:"
    searchContactByString (contacts db) func line
    searchContactMenu db

searchContactMenu :: Database -> IO()
searchContactMenu db = do
    printSeparator
    putStrLn "Search by what? - [n]ame, [p]hone, [e]-mail, [b]ack"
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
        'b' -> return ()
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
    putStrLn "Search by what? - [n]ame, [d]etails, [e]xact date, [r]ange of dates, [b]ack"
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
        'b' -> return ()
        _ -> do 
            invalidChoice 
            searchAppointmentMenu db

printToday :: V.Vector Appointment -> IO()
printToday list = do
    putStrLn "\nToday's appointments:"
    current <- getCurrentTime
    let start = UTCTime {utctDay = utctDay current, utctDayTime = secondsToDiffTime 0}
    let end = addUTCTime nominalDay start
    searchApptByDateRange list start end


addContactMenu :: V.Vector Contact -> IO (V.Vector Contact)
addContactMenu contacts = do
    contact <- readContact
    return $ V.snoc contacts contact

removeContactMenu :: V.Vector Contact -> IO (V.Vector Contact)
removeContactMenu contacts = do
    putStr "Input the index of the contact you would like to remove:"
    line <- getLine
    if isValidInt line then do
        let num = stringToInt line
        if num >= 0 && num < V.length contacts then do 
            putStrLn $ "Removing contact with index " ++ show num
            let firstHalf = V.take num contacts :: V.Vector Contact
            let lastHalf = V.drop (num+1) contacts :: V.Vector Contact
            let combined = firstHalf V.++ lastHalf
            return combined
            else do
                putStrLn "Invalid input."
                removeContactMenu contacts
    else do
        putStrLn "Invalid input."
        removeContactMenu contacts

editContact :: Contact -> IO Contact
editContact contact = do
    putStr $ "Editing contact:" 
    printContact contact
    putStrLn "Edit options: edit [n]ame, edit [p]hone, edit [e]-mail, [b]ack"
    input <- readChar
    case input of
        'n' -> do
            putStr "Input new name:"
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
    putStr "Input the index of the contact you would like to edit:"
    line <- getLine
    if isValidInt line then do
        let num = stringToInt line
        if num >= 0 && num < V.length contacts then do 
            newContact <- editContact $ contacts V.! num
            let firstHalf = V.snoc (V.take num contacts :: V.Vector Contact) newContact
            let lastHalf = V.drop (num+1) contacts :: V.Vector Contact
            let combined = firstHalf V.++ lastHalf
            return combined
            else do
                putStrLn "Invalid input."
                editContactMenu contacts
    else do
        putStrLn "Invalid input."
        editContactMenu contacts


manageContactsMenu :: V.Vector Contact -> IO (V.Vector Contact)
manageContactsMenu contacts = do
    printSeparator
    putStrLn "Manage contacts: [a]dd contact, [r]emove contact, [e]dit contact , [b]ack"
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
    appointment <- readAppointment
    return $ V.snoc appointments appointment

removeAppointmentMenu :: V.Vector Appointment -> IO (V.Vector Appointment)
removeAppointmentMenu appointments = do
    putStr "Input the index of the appointment you would like to remove:"
    line <- getLine
    if isValidInt line then do
        let num = stringToInt line
        if num >= 0 && num < V.length appointments then do 
            putStrLn $ "Removing appointment with index " ++ show num
            let firstHalf = V.take num appointments :: V.Vector Appointment
            let lastHalf = V.drop (num+1) appointments :: V.Vector Appointment
            let combined = firstHalf V.++ lastHalf
            return combined
            else do
                putStrLn "Invalid input."
                removeAppointmentMenu appointments
    else do
        putStrLn "Invalid input."
        removeAppointmentMenu appointments

editAppointment :: Appointment -> IO Appointment
editAppointment appointment = do
    putStr $ "Editing appointment:" 
    printAppointment appointment
    putStrLn "Edit options: edit [n]ame, edit [d]etails, edit [s]tart date, edit [e]nd date, [b]ack"
    input <- readChar
    case input of
        'n' -> do
            putStr "Input new name:"
            line <- getLine
            let newAppointment = Appointment { Agenda.Appointment.name = line, details = details appointment, startDate = startDate appointment, endDate = endDate appointment}
            editAppointment newAppointment
        'd' -> do
            putStr "Input new details:"
            line <- getLine
            let newAppointment = Appointment { Agenda.Appointment.name = Agenda.Appointment.name appointment, details = line, startDate = startDate appointment, endDate = endDate appointment}
            editAppointment newAppointment
        's' -> do
            putStr "Input new start date:"
            date <- readDate
            let newAppointment = Appointment { Agenda.Appointment.name = Agenda.Appointment.name appointment, details = details appointment, startDate = date, endDate = endDate appointment}
            editAppointment newAppointment
        'e' -> do
            putStr "Input new end date:"
            date <- readDate
            let newAppointment = Appointment { Agenda.Appointment.name = Agenda.Appointment.name appointment, details = details appointment, startDate = startDate appointment, endDate = date}
            editAppointment newAppointment
        'b' -> return appointment
        _ -> do 
            invalidChoice
            editAppointment appointment

editAppointmentMenu :: V.Vector Appointment -> IO (V.Vector Appointment)
editAppointmentMenu appointments = do
    putStr "Input the index of the appointment you would like to edit:"
    line <- getLine
    if isValidInt line then do
        let num = stringToInt line
        if num >= 0 && num < V.length appointments then do 
            newAppointment <- editAppointment $ appointments V.! num
            let firstHalf = V.snoc (V.take num appointments :: V.Vector Appointment) newAppointment
            let lastHalf = V.drop (num+1) appointments :: V.Vector Appointment
            let combined = firstHalf V.++ lastHalf
            return combined
            else do
                putStrLn "Invalid input."
                editAppointmentMenu appointments
    else do
        putStrLn "Invalid input."
        editAppointmentMenu appointments

manageAppointmentMenu :: V.Vector Appointment -> IO (V.Vector Appointment)
manageAppointmentMenu appointments = do
    printSeparator
    putStrLn "Manage appointments: [a]dd appointment, [r]emove appointment, [e]dit appointment , [b]ack"
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
    printSeparator
    putStrLn "Manage Agenda: [c]ontacts, [a]ppointments, [s]ave to file, [b]ack"
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
    printSeparator
    putStrLn "What would you like to do?"
    putStrLn "search [c]ontacts"
    putStrLn "search [a]ppointments"
    putStrLn "[m]anage agenda"
    putStrLn "[l]ist today's appointments"
    putStrLn "[q]quit"
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
        'q' -> exitWith ExitSuccess
        _ -> do
            invalidChoice
            menuLoop db
    