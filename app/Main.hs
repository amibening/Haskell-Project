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
  putStrLn "Please select an address to edit (enter the number):"
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
  putStrLn "Please enter addresses:"
  addrs <- getAddressesFromUser
  addrs' <- editAddress addrs
  putStrLn "You entered these addresses:"
  printAddresses addrs'
