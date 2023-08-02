module Types where

data Country = US | UK | NL | HK | IN deriving (Show)

data Address = Address
  { street :: String,
    city :: String,
    state :: String,
    zipCode :: String,
    country :: Country
  }
  deriving (Show)
