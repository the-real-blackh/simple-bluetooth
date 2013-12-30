{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Network.Bluetooth.Adapter (
        allAdapters,
        defaultAdapter,
        discover,
        module Network.Bluetooth.Types
    ) where

#if defined(mingw32_HOST_OS)
#include <windows.h>
#include "Adapter_win.h"
#else
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#endif
#include <stddef.h>

import Network.Bluetooth.Device
import Network.Bluetooth.Types
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

#if defined(mingw32_HOST_OS)
data SOCKADDR_BTH = SOCKADDR_BTH {
    saFamily :: USHORT,
    saAddr   :: BluetoothAddr,
    saPort   :: LONG
  }
  deriving Show

instance Storable SOCKADDR_BTH where
    sizeOf _ = (#const sizeof(SockAddrBTH))
    alignment _ = alignment (undefined :: Word64)
    poke _ _ = fail "SOCKADDR_BTH.poke not defined"
    peek p = SOCKADDR_BTH <$> peek (p `plusPtr` (#const offsetof(SockAddrBTH,addressFamily)))
                          <*> peek (p `plusPtr` (#const offsetof(SockAddrBTH,btAddr)))
                          <*> peek (p `plusPtr` (#const offsetof(SockAddrBTH,port)))

data WSAQUERYSET = WSAQUERYSET {
    qsSize                :: DWORD,
    qsNameSpace           :: DWORD,
    qsNumberOfCsAddrs     :: DWORD,
    qsCsAddrs             :: Ptr SOCKADDR_BTH,
    qsBlob                :: Ptr Word8
  }
  deriving Show

instance Storable WSAQUERYSET where
    sizeOf _ = (#const sizeof(WSAQUERYSET))
    alignment _ = alignment (undefined :: Word64)
    poke p qs = do
        BI.memset (castPtr p) 0 (fromIntegral $ sizeOf qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,dwSize))) (qsSize qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,dwNameSpace))) (qsNameSpace qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,dwNumberOfCsAddrs))) (qsNumberOfCsAddrs qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,lpcsaBuffer))) (qsCsAddrs qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,lpBlob))) (qsBlob qs)
    peek p =
        WSAQUERYSET <$> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,dwSize)))
                    <*> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,dwNameSpace))) 
                    <*> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,dwNumberOfCsAddrs)))
                    <*> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,lpcsaBuffer)))
                    <*> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,lpBlob)))

foreign import stdcall safe "WSALookupServiceBeginA" wsaLookupServiceBegin
    :: Ptr WSAQUERYSET -> DWORD -> Ptr HANDLE -> IO CInt
foreign import stdcall safe "WSALookupServiceNextA" wsaLookupServiceNext
    :: HANDLE -> DWORD -> Ptr DWORD -> Ptr WSAQUERYSET -> IO CInt
foreign import stdcall safe "WSALookupServiceEnd" wsaLookupServiceEnd
    :: HANDLE -> IO CInt
#else
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

-- | The Bluetooth namespace
nS_BTH :: DWORD
nS_BTH = 16

discover :: Adapter -> IO [Device]
#if defined(mingw32_HOST_OS)
discover a = alloca $ \pqs -> alloca $ \ph -> do
    poke pqs $ WSAQUERYSET {
        qsSize            = fromIntegral $ sizeOf (undefined :: WSAQUERYSET),
        qsNameSpace       = nS_BTH,
        qsNumberOfCsAddrs = 0,
        qsCsAddrs         = nullPtr,
        qsBlob            = nullPtr
      }
    ret <- wsaLookupServiceBegin pqs ((#const LUP_CONTAINERS) {- .|. (#const LUP_RETURN_ADDR) -}) ph
    when (ret < 0) $ do
        errno@(Errno errno_) <- getErrno
        err <- peekCString (strerror errno_)
        throwIO $ BluetoothException "discover" err
    h <- peek ph
    do
        let bufSize = 4096
        alloca $ \pResults -> allocaBytes bufSize $ \buf -> alloca $ \pdwSize -> do
            poke pdwSize (fromIntegral bufSize)
            poke pResults $ WSAQUERYSET {
                qsSize            = fromIntegral $ sizeOf (undefined :: WSAQUERYSET),
                qsNameSpace       = nS_BTH,
                qsNumberOfCsAddrs = 0,
                qsCsAddrs         = nullPtr,
                qsBlob            = nullPtr
              }
            let loop acc = do
                    ret <- wsaLookupServiceNext h (#const LUP_RETURN_ADDR) pdwSize pResults
                    when (ret < 0) $ do
                        errno@(Errno errno_) <- getErrno
                        err <- peekCString (strerror errno_)
                        throwIO $ BluetoothException "discover" err
                    results <- peek pResults
                    addrs <- peekArray (fromIntegral $ qsNumberOfCsAddrs results) (qsCsAddrs results)
                    return $ reverse (map (Device a . saAddr) addrs) ++ acc
            reverse <$> loop []
      `finally`
        wsaLookupServiceEnd h
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

