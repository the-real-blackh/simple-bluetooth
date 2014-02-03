{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Network.Bluetooth.Adapter (
        allAdapters,
        defaultAdapter,
        discover,

        Adapter,
        BluetoothException(..),
        BluetoothAddr(..)
    ) where

#if defined(mingw32_HOST_OS)
#include <windows.h>
#else
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#endif
#include <stddef.h>

import Network.Bluetooth.Device
import Network.Bluetooth.Types
#if defined(mingw32_HOST_OS)
import Network.Bluetooth.Win32
#endif
import qualified Data.ByteString.Internal as BI

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Word
import Foreign
import Foreign.C
#if defined(mingw32_HOST_OS)
import System.Win32.Types
#endif

#if !defined(mingw32_HOST_OS)
foreign import ccall safe "hci_for_each_dev" hci_for_each_dev
    :: CInt -> FunPtr (CInt -> CInt -> CLong -> IO CInt) -> CLong -> IO ()

foreign import ccall safe "wrapper" mkVisitDev
    ::            (CInt -> CInt -> CLong -> IO CInt) ->
       IO (FunPtr (CInt -> CInt -> CLong -> IO CInt))

foreign import ccall safe "hci_open_dev" hci_open_dev
    :: CInt -> IO CInt
#endif

#if !defined(mingw32_HOST_OS)
openDev :: CInt -> IO Adapter
openDev dev_id = do
    ret <- hci_open_dev dev_id
    if ret < 0 then do
        errno@(Errno errno_) <- getErrno
        if errno == eINTR
            then openDev dev_id
            else do
                err <- peekCString (strerror errno_)
                throwIO $ BluetoothException "openDev" err
      else
        pure $ Adapter dev_id ret
#endif

allAdapters :: IO [Adapter]
#if defined(mingw32_HOST_OS)
allAdapters = return [Adapter]
#else
allAdapters = do
    devsRef <- newIORef []
    cb <- mkVisitDev $ \_ dev_id _ -> do
        modifyIORef devsRef (dev_id:)
        pure 0
    hci_for_each_dev (#const HCI_UP) cb 0
      `finally`
        freeHaskellFunPtr cb
    dev_ids <- reverse <$> readIORef devsRef
    mapM openDev dev_ids
#endif

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "hci_get_route" hci_get_route
    :: Ptr BluetoothAddr -> IO CInt
#endif

defaultAdapter :: IO (Maybe Adapter)
#if defined(mingw32_HOST_OS)
defaultAdapter = return $ Just Adapter
#else
defaultAdapter = do
    ret <- hci_get_route nullPtr
    if ret < 0 then do
        errno@(Errno errno_) <- getErrno
        if errno == eINTR
            then defaultAdapter
          else if errno == eNODEV
            then pure Nothing
            else do
                err <- peekCString (strerror errno_)
                throwIO $ BluetoothException "defaultAdapter" err
      else
        Just <$> openDev ret
#endif

#if !defined(mingw32_HOST_OS)
foreign import ccall safe "hci_inquiry" hci_inquiry
    :: CInt -> CInt -> CInt -> Ptr Word8 -> Ptr (Ptr InquiryInfo) -> CLong -> IO CInt

data InquiryInfo = InquiryInfo {
        iiAddr :: BluetoothAddr
    }
    deriving Show

instance Storable InquiryInfo where
    sizeOf _ = (#const sizeof(inquiry_info))
    alignment _ = alignment (undefined :: Word64)
    peek p = InquiryInfo <$> peek (p `plusPtr` (#const offsetof(inquiry_info, bdaddr)))
    poke _ _ = fail "InquiryInfo.poke not defined"
#endif

discover :: Adapter -> IO [Device]
#if defined(mingw32_HOST_OS)
discover a = map (Device a . fst) <$> discover' a flags
  where
    flags = (#const LUP_RETURN_ADDR) .|.
            (#const LUP_FLUSHCACHE)
#else
discover a@(Adapter dev_id _) = go 0  -- (#const IREQ_CACHE_FLUSH)
  where
    go flags = do
        mDevices <- allocaArray 255 $ \ppDevs -> do
            poke ppDevs nullPtr
            n <- hci_inquiry dev_id 8 255 nullPtr ppDevs flags
            if n < 0 then do
                errno@(Errno errno_) <- getErrno
                if errno == eINTR
                    then pure Nothing
                    else do
                        err <- peekCString (strerror errno_)
                        throwIO $ BluetoothException "discover" err
              else do
                pDevs <- peek ppDevs
                iis <- peekArray (fromIntegral n) pDevs
                free pDevs
                pure $ Just $ map (Device a . iiAddr) iis
        case mDevices of
            Just devices -> pure devices
            Nothing      -> go 0  -- eINTR ? Retry.
#endif

