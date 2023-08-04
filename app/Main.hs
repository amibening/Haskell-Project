import Lib

main :: IO ()
main = do
  putStrLn "Welcome to My International Address Book!"
  addrs <- myiAddressBook []
  printAddresses addrs
  where
    myiAddressBook :: [Address] -> IO [Address]
    myiAddressBook addrs = do
      putStrLn "Please select an option: 1 - Add an address, 2 - Edit an address, 3 - List all addresses, 4 - Quit"
      response <- getLine
      case response of
        "1" -> do
          addr <- inputAddressFromUser
          myiAddressBook (addr : addrs)
        "2" -> do
          addrs' <- editAddress addrs
          myiAddressBook addrs'
        "3" -> do
          printAddresses addrs
          myiAddressBook addrs
        "4" -> do
          return addrs
        _ -> do
          putStrLn "Invalid option, please try again."
          myiAddressBook addrs
