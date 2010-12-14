module Main where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Validator
import Data.Validator.Combinators


type Params = Map ByteString ByteString

data User = User 
  { usrAge :: Int 
  , usrName :: ByteString } deriving (Show, Eq, Read)


ex1 :: Consumer Params User
ex1 = User <$> v1 <*> v2

v1 = isPresent "Age" >+> isNumber "Age" 
v2 = isPresent "Name" >+> nonBlank "Name" >+> isUnique "Name"


isPresent :: ByteString -> Consumer Params ByteString
isPresent fname = Consumer step
  where
    step m = return $ maybe errorval return $ Map.lookup fname m
    errorval = ferror fname Nothing "Must be present"


nonBlank :: ByteString -> Consumer ByteString ByteString
nonBlank fname = Consumer step
  where
    step m = return $ if B.length m > 0 then return m else errorval
      where errorval = ferror fname (Just m) "Must be at least 1 character"


isNumber :: ByteString -> Consumer ByteString Int
isNumber fname = Consumer step
  where
    step n = return $ maybe errorval return $ readMay . B.unpack $ n
      where errorval = ferror fname (Just n) "Must be a number"


atLeast :: (Num a, Ord a) => a -> Consumer a a
atLeast = undefined


isUnique :: ByteString -> Consumer ByteString ByteString
isUnique fname = Consumer step
  where step m = do
                  let look = True
                  putStrLn "I'm looking shit up from DB"
                  return $ case look of
                    True -> return m
                    False -> ferror fname (Just m) "Must be unique"
