{- |
A 'UID' is a unique identifier, generated from "Data.UUID.V4". 

These identifiers are designed for easy presentation as Base32, using "Codec.Binary.Base32", and
for transport in a JSON format using "Data.Aeson".

The primary constructor, 'newUID', runs in the 'IO' monad, primarily because of its
use of a random number generator during construction. If all you need is a unique string,
then 'newUIDString' is just a wrapper that creates a new UID and then invokes 'toBase32' on it.

-}

{-# LANGUAGE DeriveDataTypeable #-}

module Data.UID (
  UID,
  newUID,
  newUIDString,
  fromBase32,
  toBase32) 
       where

import Codec.Binary.Base32 as B32

import Control.Applicative (pure)
import Control.Exception

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower,toUpper)
import Data.Maybe
import Data.Serialize
import Data.Text (pack,unpack)
import Data.Typeable
import Data.UUID
import Data.UUID.V4

{- |
A new, randomly generated identifier
-}
data UID = UID UUID deriving (Eq,Typeable)

instance Serialize UID where
  put (UID uuid) = do 
    put $ toByteString uuid
    
  get = do
    bs <- getByteString 16 -- size of UUID representation
    return $ UID $ fromJust $ fromByteString $ BL.fromStrict bs
    
{- |
Convert the 'UID' into a base32 'String' representation'.
-}
toBase32 :: UID -> String
toBase32 (UID uuid) = let encoding = B32.encode $ BL.unpack $ toByteString uuid
                          except c = c /= '='
                          in map toLower $ filter except encoding 

data InvalidUIDException = InvalidUIDException String deriving (Show,Typeable)

instance Exception InvalidUIDException

{- |
Convert a base32 'String' representation of the 'UID' back into a UID instance.
-}
fromBase32 :: String -> UID
fromBase32 s = let extended = map toUpper $ s ++ (replicate paddingSize '=')
                   paddingSize = if (length s) < 32 then (32 - (length s)) else 0
                   decoded = B32.decode $ extended
                   bs = fromByteString $ BL.pack $ 
                        if (isJust decoded)
                        then fromJust decoded
                        else (throw $ InvalidUIDException s)
               in UID $ 
                  if (isJust bs)
                  then fromJust bs
                  else throw $ InvalidUIDException s

instance Show UID where    
  show u = "UID " ++ toBase32 u
  
instance ToJSON UID where  
  toJSON u = String $  pack $ toBase32 u
  
instance FromJSON UID where  
  parseJSON = withText "Base32" $ pure . fromBase32 . unpack

{- |
Construct a new unique identifier.
-}
newUID :: IO UID
newUID = do
  uuid <- nextRandom
  return $ UID uuid
  
  
{- |  
Construct a new identifier and immediately convert to its base32 representation using 'toBase32'
-}
newUIDString :: IO String  
newUIDString = do
  uid <- newUID
  return $ toBase32 uid