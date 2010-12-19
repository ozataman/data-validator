module Test where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Validator
import Safe


data User = User 
  { usrAge :: Maybe Int 
  , usrName :: ByteString 
  , usrLogin :: ByteString 
  } deriving (Show, Eq, Read)


ps1 = Map.fromList 
  [ ("login", ["ozataman"])
  , ("name", ["ahmet"])
  , ("age", ["21"])
  ]


usrForm ps = runCons $ User 
  <$> field 
    ((isPresent >>= isNonBlank >>= isNum >>= isAtLeast 18 >>= maybeThere) 
      <|> canbeBlank) 
    (paramv "age" ps)
  <*> field (isPresent >>= isNonBlank) (paramv "name" ps)
  <*> field (isPresent >>= isNonBlank) (paramv "login" ps)
  






