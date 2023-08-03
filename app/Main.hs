import Lib
import Text.Read (readMaybe)

getAddressesFromUser :: IO [Address]
getAddressesFromUser = do
  putStrLn "Do you want to enter an address? (yes/no)"
  response <- getLine
  case response of
    "yes" -> do
      addr <- getAddressFromUser
      addrs <- getAddressesFromUser
      return (addr : addrs)
    _ -> return []

editAddress :: [Address] -> IO [Address]
editAddress addrs = do
  putStrLn "Select an id to edit any addresses input (or press enter to quit):"
  printAddressesWithIndices addrs
  idxStr <- getLine
  case readMaybe idxStr of
    Just idx ->
      if idx > 0 && idx <= length addrs
        then do
          putStrLn "Enter new address:"
          newAddr <- getAddressFromUser
          let (ys, zs) = splitAt (idx - 1) addrs
          return (ys ++ newAddr : tail zs)
        else do
          putStrLn "Invalid index. No changes made."
          return addrs
    Nothing -> do
      putStrLn "Invalid input. No changes made."
      return addrs

main :: IO ()
main = do
  putStrLn "Welcome to the International Address Book!"
  addrs <- runAddressBook []
  printAddresses addrs
  where
    runAddressBook :: [Address] -> IO [Address]
    runAddressBook addrs = do
      putStrLn "Please select an option: 1 - Add an address, 2 - Edit an address, 3 - List all addresses, 4 - Quit"
      response <- getLine
      case response of
        "1" -> do
          addr <- getAddressFromUser
          runAddressBook (addr : addrs)
        "2" -> do
          addrs' <- editAddress addrs
          runAddressBook addrs'
        "3" -> do
          printAddresses addrs
          runAddressBook addrs
        "4" -> do
          return addrs
        _ -> do
          putStrLn "Invalid option, please try again."
          runAddressBook addrs
