{-# LANGUAGE DeriveDataTypeable #-}
module Network.Bluetooth.Types where

#if defined(mingw32_HOST_OS)
#include <windows.h>
#else
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#endif
#include <stddef.h>

import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.Char
import Data.List
import Data.Typeable
import Foreign
import Foreign.C
import Numeric


foreign import ccall unsafe "strerror" strerror
    :: CInt -> CString

#if defined(mingw32_HOST_OS)
data Adapter = Adapter deriving (Eq, Ord, Show)
#else
data Adapter = Adapter CInt CInt deriving (Eq, Ord, Show)
#endif

data BluetoothException = BluetoothException String String deriving (Show, Typeable)
instance Exception BluetoothException

newtype BluetoothAddr = BluetoothAddr ByteString deriving (Eq, Ord)

instance Show BluetoothAddr where
    show (BluetoothAddr bs) = intercalate ":" $ map (\x -> dig2 $ showHex x "") $ reverse $ B.unpack bs
      where
        dig2 = map toUpper . reverse . take 2 . reverse . ('0':) 

instance Read BluetoothAddr where
    readsPrec _ t = go t (6 :: Int) []
      where
        go t n acc = case readHex t of
            (x, t'):_ -> case (n, t') of
                (1, _)       -> [(BluetoothAddr (B.pack (x:acc)), t')]
                (_, ':':t'') -> go t'' (n-1) (x:acc)
                _ -> []
            _ -> []

instance Storable BluetoothAddr where
#if defined(mingw32_HOST_OS)
    sizeOf _ = 8
#else
    sizeOf _ = (#const sizeof(bdaddr_t))
#endif
    alignment _ = alignment (undefined :: Word64)
    peek p = BluetoothAddr . B.pack <$> peekArray 6 (castPtr p)
    poke p (BluetoothAddr bs) = do
        BI.memset (castPtr p) 0 (fromIntegral $ sizeOf (undefined :: BluetoothAddr)) 
        pokeArray (castPtr p) (B.unpack bs)

