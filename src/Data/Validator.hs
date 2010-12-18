module Data.Validator where


import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Safe

ferror :: ByteString -> Maybe ByteString -> ErrorItem -> Result t
ferror fname val e = Error $ Map.fromList [(fname, (val, [e]))]


newtype (MonadIO m) => Consumer m a t = Consumer 
  { runCons :: a -> m (Result t) }


instance (Functor m, MonadIO m) => Functor (Consumer m a) where
  fmap f (Consumer g) = Consumer $ \m -> fmap (fmap f) (g m)


(>+>) :: (MonadIO m) => Consumer m a b -> Consumer m b c -> Consumer m a c
(Consumer f) >+> (Consumer g) = Consumer step
  where
    g' (Ok x) = g x
    g' (Error x) = return $ Error x
    step m = do
              r <- f m
              g' r


instance (Applicative m, MonadIO m) => Applicative (Consumer m a) where
  pure x = Consumer $ \m -> return . return $ x
  Consumer f <*> Consumer g = Consumer step
    where
      step m = do
                r1 <- f m
                r2 <- g m
                return $ r1 <*> r2
  

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


