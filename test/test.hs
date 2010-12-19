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
  , usrPass :: ByteString
  } deriving (Show, Eq, Read)


ps1 = Map.fromList 
  [ ("login", ["ozataman"])
  , ("name", ["ahmet"])
  , ("age", ["21"])
  , ("pass", ["eben"])
  , ("pass_conf", ["eben"])
  ]


usrForm ps = runCons $ User 
  <$> field (paramv "age" ps)
        ((isPresent >>= isNonBlank >>= isNum >>= isAtLeast 18 >>= maybeThere) 
          <|> canbeBlank) 
  <*> field (paramv "name" ps) (isPresent >>= isNonBlank) 
  <*> field (paramv "login" ps) (isPresent >>= isNonBlank) 
  <*> passField ps


passField ps = bindC pt "pass" passvalid
  where
    pt :: Consumer IO (ByteString, ByteString)
    pt = liftM2 (,) (field (paramv "pass" ps) isPresent)
                    (field (paramv "pass_conf" ps) isPresent)

  
passvalid :: FieldValidator IO (ByteString, ByteString) ByteString
passvalid = do
  Just (p1,p2) <- asks vOrig
  if p1 == p2 then return p1 else errval p1
  where
    errval p1 = do
      fname <- asks vField
      lift . Consumer . return . Error $ Map.fromList
        [(fname, (Just p1, [("Must be same as its confirmation", [])]))]






