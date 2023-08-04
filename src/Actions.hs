module Actions where

import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))
import Types

postcodeFormat :: Country -> String
postcodeFormat country = case country of
  US -> "US - 5 digits (e.g., 12345)"
  UK -> "UK - AN NAA or ANN NAA or AAN NAA or AANN NAA or ANA NAA or AANA NAA (e.g., M1 1AE)"
  NL -> "NL - 4 digits + 2 letters (e.g., 1234 AB)"
  HK -> "No postcode."
  IN -> "IN - 6 digits (e.g., 110001)"

validatePostcode :: String -> Country -> Bool
validatePostcode postcode country = case country of
  US -> postcode =~ ("^[0-9]{5}$" :: String) :: Bool
  UK -> postcode =~ ("^[A-Z]{1,2}[0-9R][0-9A-Z]? [0-9][ABD-HJLNP-UW-Z]{2}$" :: String) :: Bool
  NL -> postcode =~ ("^[0-9]{4} [A-Z]{2}$" :: String) :: Bool
  HK -> True -- HK has no postcode
  IN -> postcode =~ ("^[0-9]{6}$" :: String) :: Bool

inputAddressFromUser :: IO Address
inputAddressFromUser = do
  putStrLn "Select country: 1 - US, 2 - UK, 3 - NL, 4 - HK, 5 - IN"
  countryStr <- getLine
  let country = case countryStr of
        "1" -> Just US
        "2" -> Just UK
        "3" -> Just NL
        "4" -> Just HK
        "5" -> Just IN
        _ -> Nothing
  case country of
    Just c -> do
      putStrLn "Enter house no. and street:"
      street <- getLine
      putStrLn "Enter city or town:"
      city <- getLine
      putStrLn "Enter state or county:"
      state <- getLine
      putStrLn "Enter zip or post code:"
      zipCode <- getLine
      if validatePostcode zipCode c
        then return (Address street city state zipCode c)
        else do
          putStrLn $ "Invalid postcode format, please try again. The format for the " ++ postcodeFormat c
          inputAddressFromUser
    _ -> do
      putStrLn "Invalid country selection, please try again."
      inputAddressFromUser

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
          newAddr <- inputAddressFromUser
          let (ys, zs) = splitAt (idx - 1) addrs
          return (ys ++ newAddr : tail zs)
        else do
          putStrLn "Invalid index. No changes made."
          return addrs
    Nothing -> do
      putStrLn "Invalid input. No changes made."
      return addrs

printAddress :: Address -> IO ()
printAddress addr = putStrLn $ street addr ++ ", " ++ city addr ++ ", " ++ state addr ++ ", " ++ zipCode addr ++ ", " ++ show (country addr)

printAddresses :: [Address] -> IO ()
printAddresses = mapM_ printAddress

printAddressesWithIndices :: [Address] -> IO ()
printAddressesWithIndices addrs = mapM_ (putStrLn . format) (zip [1 ..] addrs)
  where
    format (i, addr) = show i ++ ": " ++ street addr ++ ", " ++ city addr ++ ", " ++ state addr ++ " " ++ zipCode addr ++ ", " ++ show (country addr)
