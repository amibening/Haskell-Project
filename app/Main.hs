import Lib

main :: IO ()
main = do
  putStrLn "Welcome to My International Address Book!"
  addrs <- myAddressBook []
  printAddresses addrs
  where
    myAddressBook :: [Address] -> IO [Address]
    myAddressBook addrs = do
      putStrLn "Please select an option: 1 - Add an address, 2 - Edit an address, 3 - List all addresses, 4 - Quit"
      response <- getLine
      case response of
        "1" -> do
          addr <- inputAddressFromUser
          myAddressBook (addr : addrs)
        "2" -> do
          addrs' <- editAddress addrs
          myAddressBook addrs'
        "3" -> do
          printAddresses addrs
          myAddressBook addrs
        "4" -> do
          return addrs
        _ -> do
          putStrLn "Invalid option, please try again."
          myAddressBook addrs
