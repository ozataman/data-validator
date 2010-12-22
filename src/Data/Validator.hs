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
  , ferror'
  , paramv
  , bindC

  -- * Validators
  -- | A variety of built-in validator combinators for your convenience. 
  , isPresent
  , isNonBlank
  , hasMinLen
  , isNum
  , isAtLeast
  , canbeBlank
  , maybeThere
  , areSame
  , areSame2

  -- * Access To Error Values
  , errsFor
  , errVal
  , fmapM

) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.String

import Safe


------------------------------------------------------------------------------
-- Validators
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Field is present
isPresent :: (Monad m) => FieldValidator m ByteString ByteString
isPresent = 
  let errval = ferror ("Must be present", [])
  in do
    v <- asks vOrig
    maybe errval return v


------------------------------------------------------------------------------
-- | Field is not blank
isNonBlank :: (Eq a, IsString a, Monad m) => a -> FieldValidator m ByteString a
isNonBlank v = if v == "" then errval else return v
  where errval = ferror ("Must be non-blank", [])


------------------------------------------------------------------------------
-- | Field has minimum length
hasMinLen :: (Show a, Show b, Monad m) => Int -> a -> FieldValidator m b a
hasMinLen n v = if (length . show $ v) >= n then return v else errval
  where errval = ferror' ("Must have a minimum length of x", [])


------------------------------------------------------------------------------
-- | Field is greater than the given value
isAtLeast :: (Monad m, Ord a) => a -> a -> FieldValidator m ByteString a
isAtLeast limit val = if val < limit then errval else return val
  where errval = ferror ("Must be at least", [])


------------------------------------------------------------------------------
-- | Field is numeric
isNum :: (Monad m, Num b, Read b, Show a) 
      => a 
      -> FieldValidator m ByteString b
isNum val = maybe errval return n
  where 
    sval = show val
    n = readMay sval
    errval = ferror ("Must be numeric", [])


------------------------------------------------------------------------------
-- | Field can be blank
canbeBlank :: (Monad m) => FieldValidator m ByteString (Maybe a)
canbeBlank = check
  where
    errval = ferror ("Must be a valid entry or can be left blank", [])
    check = asks vOrig >>= maybe (return Nothing) blank 
    blank v = if v == "" then return Nothing else errval
    

------------------------------------------------------------------------------
-- | Turn a value into Just value
maybeThere :: (Monad m) => a -> FieldValidator m ByteString (Maybe a)
maybeThere = return . Just 


------------------------------------------------------------------------------
-- | A given list of fields are all the same
areSame :: (Monad m, Eq a, Show a) => FieldValidator m [a] (Maybe a)
areSame = do
  Just ps <- asks vOrig
  if same ps then return (headMay ps) else errval 
  where
    same xs = let (r, _) = foldr step (True, Nothing) xs in r
    step x (c, Just l) = (c && (l == x), Just x)
    step x (c, Nothing) = (c, Just x)
    errval = do
      fname <- asks vField
      ferror' ("Must be same as its confirmation", [])


------------------------------------------------------------------------------
-- | A given tuple has the same element twice
areSame2 :: (Monad m, Eq a, Show a) => FieldValidator m (a, a) a
areSame2 = do
  Just (p1,p2) <- asks vOrig
  if p1 == p2 then return p1 else errval 
  where
    errval = do
      fname <- asks vField
      ferror' ("Must be same as its confirmation", [])

------------------------------------------------------------------------------
-- Combinators
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Given a 'FieldVal' and a validating 'FieldValidator', return a 'Consumer'
--
-- See examples for usage.
field :: FieldVal a -> FieldValidator m a b -> Consumer m b
field v r = runReaderT r v


------------------------------------------------------------------------------
-- | Convenience function to work with Snap's 'Params' type
paramv :: ByteString -> Map ByteString [ByteString] -> FieldVal ByteString
paramv k m = FV k val 
  where val = Map.lookup k m >>= headMay 


------------------------------------------------------------------------------
-- | Convenience function to quickly generate error messages in the
-- 'FieldValidator' monad. See the supplied examples.
ferror :: (Monad m) 
       => (ByteString, [(ByteString, ByteString)])
       -- ^ Message, substitution pairs mapping. If in doubt, just supply []
       -- for the pairs.
       -> FieldValidator m ByteString a
ferror e = do
  fname <- asks vField
  vorig <- asks vOrig
  lift . Consumer . return . Error $ Map.fromList [(fname, (vorig, [e]))]


------------------------------------------------------------------------------
-- | Just like 'ferror' but operates on any input value that is an instance of
-- 'Show'. This is needed for the implicit conversion of the input to
-- ByteString in the returned 'ErrorMap'.
ferror' :: (Monad m, Show a)
        => (ByteString, [(ByteString, ByteString)])
        -> FieldValidator m a b
ferror' e = do
  fname <- asks vField
  vorig <- liftM (liftM (fromString . show)) $ asks vOrig
  lift . Consumer . return . Error $ Map.fromList [(fname, (vorig, [e]))]


------------------------------------------------------------------------------
-- | Take a consumer and make in the input for a 'FieldValidator'.
bindC :: (Monad m) 
      => Consumer m a 
      -> ByteString 
      -> FieldValidator m a b 
      -> Consumer m b
bindC (Consumer c) label rv = Consumer step
  where 
    step = do
      r <- c
      case r of
        Error x -> return $ Error x
        Ok x -> runCons $ runReaderT rv (FV label (Just x))


------------------------------------------------------------------------------
-- Easy Access To Result Values
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Errors for the given field
errsFor :: ByteString -> Result ok -> Maybe ErrorInfo
errsFor f (Error m) = Map.lookup f m
errsFor f _ = Nothing


------------------------------------------------------------------------------
-- | The supplied rejected value for the field
errVal :: ByteString -> Result ok -> Maybe ByteString
errVal f (Error m) = Map.lookup f m >>= \(v, _) -> v
errVal f _ = Nothing


------------------------------------------------------------------------------
-- | Lift the given function inside 'Result', get the result in a different
-- monad. fail (Nothing) if result was 'Error'. 'Maybe' is a typical
-- application.
fmapM :: (Monad m) => (a -> b) -> Result a -> m b
fmapM f (Error _) = fail "Can't lift a function inside a Result with errors"
fmapM f (Ok x) = return $ f x


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type FieldValidator m a b = ReaderT (FieldVal a) (Consumer m) b



------------------------------------------------------------------------------
-- | Environment / input data for the validation session. Later wrapped around
-- the 'Consumer' with 'ReaderT' when the validation is run.
data FieldVal a = FV 
  { vField :: ByteString  -- ^ Specified name for this field/validated entity.
  , vOrig :: Maybe a      -- ^ Original/initial value being validated.
  }


------------------------------------------------------------------------------
-- | A container to hold the validated data. 
--
-- It lives in a monad, so you can put it in 'IO' if that is needed. It wraps
-- around 'Result', which means it can intrinsically express failure.
--
-- Consumer is itself a 'Monad', a 'Functor', an 'Applicative' and an
-- 'Alternative' which really is what makes this library possible. See examples
-- for how these instances are used.
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


instance MonadIO m => MonadIO (Consumer m) where
  liftIO a = Consumer run
    where run = liftIO a >>= return . return


instance MonadTrans Consumer where
  lift a = Consumer run
    where run = a >>= return . return


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

