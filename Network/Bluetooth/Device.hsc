{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Bluetooth.Device (
        Device(..),
        deviceName,
        module Network.Bluetooth.Types
    ) where

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <stddef.h>

import Network.Bluetooth.Types

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

import Foreign
import Foreign.C


data Device = Device Adapter BDAddr deriving (Eq, Ord, Show)

foreign import ccall safe "hci_read_remote_name" hci_read_remote_name
    :: CInt -> Ptr BDAddr -> CInt -> Ptr CChar -> CInt -> IO CInt

deviceName :: Device -> IO ByteString
deviceName (Device (Adapter _ dd) (BDAddr bs)) =
    B.createUptoN maxLen $ \buf -> do
        ret <- B.unsafeUseAsCString bs $ \cs ->
            hci_read_remote_name dd (castPtr cs) (fromIntegral maxLen) (castPtr buf) 0
        when (ret < 0) $ do
            Errno errno <- getErrno
            err <- peekCString (strerror errno)
            throwIO $ BluetoothException err
        fromIntegral <$> B.c_strlen (castPtr buf) 
  where
    maxLen = 255

