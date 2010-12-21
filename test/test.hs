module Test where

import Control.Applicative
import Data.Maybe (fromJust)
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
  , usrPass :: ByteString
  } deriving (Show, Eq, Read)


ps1 :: Map ByteString [ByteString]
ps1 = Map.fromList 
  [ ("login", ["ozataman"])
  , ("name", ["as"])
  , ("age", ["21"])
  , ("pass", ["eben"])
  , ("pass_conf", ["eben"])
  ]


usrForm ps = runCons $ User 
  <$> field (paramv "age" ps)
        (     canbeBlank 
          <|> (isPresent >>= isNonBlank >>= isNum >>= isAtLeast 18 >>= maybeThere)
        )
  <*> field (paramv "name" ps) (isPresent >>= isNonBlank >>= hasMinLen 5) 
  <*> field (paramv "login" ps) (isPresent >>= isNonBlank) 
  <*> passField ps


passField ps = liftM fromJust $ bindC pt "pass" areSame
  where
    pt :: Consumer IO [ByteString]
    pt = liftM (:) (field (paramv "pass" ps) isPresent) 
         <*> 
         liftM return (field (paramv "pass_conf" ps) isPresent)



