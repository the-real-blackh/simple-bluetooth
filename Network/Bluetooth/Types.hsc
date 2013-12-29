{-# LANGUAGE DeriveDataTypeable #-}
module Network.Bluetooth.Types where

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <stddef.h>

import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Typeable
import Foreign
import Foreign.C
import Numeric


foreign import ccall unsafe "strerror" strerror
    :: CInt -> CString

data Adapter = Adapter CInt CInt deriving (Eq, Ord, Show)

data BluetoothException = BluetoothException String deriving (Show, Typeable)
instance Exception BluetoothException

newtype BDAddr = BDAddr ByteString deriving (Eq, Ord)

instance Show BDAddr where
    show (BDAddr bs) = intercalate ":" $ map (\x -> dig2 $ showHex x "") $ reverse $ B.unpack bs
      where
        dig2 = map toUpper . reverse . take 2 . reverse . ('0':) 

instance Read BDAddr where
    readsPrec _ t = go t (6 :: Int) []
      where
        go t n acc = case readHex t of
            (x, t'):_ -> case (n, t') of
                (1, _)       -> [(BDAddr (B.pack (x:acc)), t')]
                (_, ':':t'') -> go t'' (n-1) (x:acc)
                _ -> []
            _ -> []

instance Storable BDAddr where
    sizeOf _ = (#const sizeof(bdaddr_t))
    alignment _ = error "BDAddr.alignment not defined"
    peek p = BDAddr . B.pack <$> peekArray (#const sizeof(bdaddr_t)) (castPtr p)
    poke _ _ = fail "BDAddr.poke not defined"

