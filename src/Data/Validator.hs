module Data.Validator 
(
  -- * Types
    FieldVal(..)
  , FieldValidator
  , Consumer(..)
  , Result(..)
  , ErrorMap
  , ErrorInfo
  , ErrorList
  , ErrorItem

  -- * High Level Combinators
  -- | These are meant to be used in your modules.
  , field
  , ferror
  , paramv

  -- * Validators
  -- | A variety of built-in validator combinators for your convenience.
  , isPresent
  , isNonBlank
  , isNum
  , isAtLeast
  , canbeBlank
  , maybeThere
  

) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.String
import Data.ListLike (StringLike, toString)

import Safe


------------------------------------------------------------------------------
-- Validators
------------------------------------------------------------------------------


isPresent :: (Monad m) => FieldValidator m ByteString
isPresent = 
  let errval = ferror ("Must be present", [])
  in do
    v <- asks vOrig
    maybe errval return v


isNonBlank :: (Eq a, IsString a, Monad m) => a -> FieldValidator m a
isNonBlank v = if v == "" then errval else return v
  where errval = ferror ("Must be non-blank", [])


isAtLeast :: (Monad m, Ord a) => a -> a -> FieldValidator m a
isAtLeast limit val = if val < limit then errval else return val
  where errval = ferror ("Must be at least", [])


isNum :: (StringLike a, Monad m, Num b, Read b) 
      => a 
      -> FieldValidator m b
isNum val = maybe errval return n
  where 
    sval = toString val :: String
    n = readMay sval
    errval = ferror ("Must be numeric", [])


canbeBlank :: (Monad m) => FieldValidator m (Maybe a)
canbeBlank = check
  where
    errval = ferror ("Must be a valid entry or can be left blank", [])
    check = asks vOrig >>= maybe (return Nothing) blank 
    blank v = if v == "" then return Nothing else errval
    

maybeThere :: (Monad m) => a -> FieldValidator m (Maybe a)
maybeThere = return . Just 

------------------------------------------------------------------------------
-- Combinators
------------------------------------------------------------------------------


field :: FieldValidator m a -> FieldVal -> Consumer m a
field r v = runReaderT r v


paramv :: ByteString -> Map ByteString [ByteString] -> FieldVal
paramv k m = FV k val 
  where val = Map.lookup k m >>= headMay 


ferror :: (Monad m) 
       => (ByteString, [(ByteString, ByteString)]) 
       -> FieldValidator m a
ferror e = do
  fname <- asks vField
  vorig <- asks vOrig
  lift . Consumer . return . Error $ Map.fromList [(fname, (vorig, [e]))]



------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type FieldValidator m a = ReaderT FieldVal (Consumer m) a


data FieldVal = FV 
  { vField :: ByteString
  , vOrig :: Maybe ByteString
  }



newtype Consumer m a = Consumer
  { runCons :: m (Result a) }


instance (Applicative m, Monad m) => Alternative (Consumer m) where
  empty = Consumer . return $ Error Map.empty

  (Consumer f) <|> (Consumer g) = Consumer new
    where
      new = do
        r1 <- f
        r2 <- g
        return $ r1 <|> r2

instance (Applicative m, Monad m) => Applicative (Consumer m) where
  pure = return

  (Consumer f) <*> (Consumer g) = Consumer new
    where
      new = do
        r1 <- f
        r2 <- g
        return $ r1 <*> r2


instance (Functor m) => Functor (Consumer m) where
  fmap f (Consumer g) = Consumer $ fmap (fmap f) g


instance (Monad m) => Monad (Consumer m) where
  return x = Consumer $ return . return $ x

  (Consumer f) >>= g = Consumer step
    where 
      g' (Ok x) = g x
      g' (Error x) = Consumer $ return $ Error x
      step = do
        r <- f
        runCons $ g' r


type ErrorMap = Map ByteString ErrorInfo


type ErrorInfo = (Maybe ByteString, ErrorList)


type ErrorList = [ErrorItem]


type ErrorItem = (ByteString, [(ByteString, ByteString)])


data Result ok 
  = Error ErrorMap
  | Ok ok 
  deriving (Show, Eq)


instance Functor Result where
    fmap _ (Error x) = Error x
    fmap f (Ok x) = Ok (f x)


instance Monad Result where
    return = Ok
    Error x >>= _ = Error x
    Ok x >>= f = f x


instance Applicative Result where
    pure = Ok
    Error x <*> Error y = 
      Error $ Map.unionWith (\(l,as) (_,bs) -> (l,as++bs)) x y
    Error x <*> Ok _ = Error x
    Ok _ <*> Error y = Error y
    Ok x <*> Ok y = Ok $ x y


instance Alternative Result where
  empty = Error Map.empty
  Ok x <|> _ = Ok x
  Error _ <|> p = p

