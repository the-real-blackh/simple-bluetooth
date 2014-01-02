module Network.Bluetooth.Win32 where

#include <windows.h>

import Network.Bluetooth.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI
import Control.Applicative
import Control.Exception
import Foreign
import Foreign.C
import System.Win32.Types


data SockAddrBTH = SockAddrBTH {
    bthFamily :: USHORT,
    bthAddr   :: BluetoothAddr,
    bthPort   :: LONG
  }
  deriving Show

sockAddrBTH :: BluetoothAddr -> Word8 -> SockAddrBTH
sockAddrBTH addr port = SockAddrBTH aF_BTH addr (fromIntegral port)

instance Storable SockAddrBTH where
    sizeOf _ = 30
    alignment _ = alignment (undefined :: Word64)
    poke p bth = do
        BI.memset (castPtr p) 0 (fromIntegral $ sizeOf bth)
        poke (p `plusPtr` 0) (bthFamily bth)
        poke (p `plusPtr` 2) (bthAddr bth)
        poke (p `plusPtr` 26) (bthPort bth)
    peek p = SockAddrBTH <$> peek (p `plusPtr` 0)
                         <*> peek (p `plusPtr` 2)
                         <*> peek (p `plusPtr` 26)

data SOCKET_ADDRESS sa = SOCKET_ADDRESS {
    saSockaddr :: Ptr sa,
    saSockaddrLength :: INT
  }
  deriving Show

instance Storable (SOCKET_ADDRESS sa) where
    sizeOf _ = (#const sizeof(SOCKET_ADDRESS))
    alignment _ = alignment (undefined :: Word64)
    poke _ _ = fail "SOCKET_ADDRESS.poke not defined"
    peek p = SOCKET_ADDRESS <$> peek (p `plusPtr` (#const offsetof(SOCKET_ADDRESS,lpSockaddr)))
                            <*> peek (p `plusPtr` (#const offsetof(SOCKET_ADDRESS,iSockaddrLength)))

data CSADDR_INFO sa = CSADDR_INFO {
    csaLocalAddr  :: SOCKET_ADDRESS sa,
    csaRemoteAddr :: SOCKET_ADDRESS sa
  }
  deriving Show

instance Storable sa => Storable (CSADDR_INFO sa) where
    sizeOf _ = (#const sizeof(CSADDR_INFO))
    alignment _ = alignment (undefined :: Word64)
    poke _ _ = fail "CSADDR_INFO.poke not defined"
    peek p = CSADDR_INFO <$> peek (p `plusPtr` (#const offsetof(CSADDR_INFO,LocalAddr)))
                         <*> peek (p `plusPtr` (#const offsetof(CSADDR_INFO,RemoteAddr)))

data WSAQUERYSET sa = WSAQUERYSET {
    qsSize                :: DWORD,
    qsServiceInstanceName :: CString,
    qsNameSpace           :: DWORD,
    qsNumberOfCsAddrs     :: DWORD,
    qsCsAddrs             :: Ptr (CSADDR_INFO sa),
    qsBlob                :: Ptr Word8
  }
  deriving Show

instance Storable (WSAQUERYSET sa) where
    sizeOf _ = (#const sizeof(WSAQUERYSET))
    alignment _ = alignment (undefined :: Word64)
    poke p qs = do
        BI.memset (castPtr p) 0 (fromIntegral $ sizeOf qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,dwSize))) (qsSize qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,lpszServiceInstanceName))) (qsServiceInstanceName qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,dwNameSpace))) (qsNameSpace qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,dwNumberOfCsAddrs))) (qsNumberOfCsAddrs qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,lpcsaBuffer))) (qsCsAddrs qs)
        poke (p `plusPtr` (#const offsetof(WSAQUERYSET,lpBlob))) (qsBlob qs)
    peek p =
        WSAQUERYSET <$> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,dwSize)))
                    <*> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,lpszServiceInstanceName)))
                    <*> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,dwNameSpace))) 
                    <*> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,dwNumberOfCsAddrs)))
                    <*> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,lpcsaBuffer)))
                    <*> peek (p `plusPtr` (#const offsetof(WSAQUERYSET,lpBlob)))

foreign import stdcall safe "WSALookupServiceBeginA" wsaLookupServiceBegin
    :: Ptr (WSAQUERYSET SockAddrBTH) -> DWORD -> Ptr HANDLE -> IO CInt
foreign import stdcall safe "WSALookupServiceNextA" wsaLookupServiceNext
    :: HANDLE -> DWORD -> Ptr DWORD -> Ptr (WSAQUERYSET SockAddrBTH) -> IO CInt
foreign import stdcall safe "WSALookupServiceEnd" wsaLookupServiceEnd
    :: HANDLE -> IO CInt

-- | The Bluetooth namespace
nS_BTH :: DWORD
nS_BTH = 16

aF_BTH :: USHORT
aF_BTH = 32

wsaServiceNotFound :: ErrCode
wsaServiceNotFound = 10108

wsaENoMore :: ErrCode
wsaENoMore = 10110

bTHPROTO_RFCOMM :: CInt
bTHPROTO_RFCOMM = 0x0003

discover' :: Adapter -> DWORD -> IO [(BluetoothAddr, Maybe ByteString)]
discover' a flags = alloca $ \pqs -> alloca $ \ph -> do
    poke pqs $ WSAQUERYSET {
        qsSize            = fromIntegral $ sizeOf (undefined :: WSAQUERYSET SockAddrBTH),
        qsServiceInstanceName = nullPtr,
        qsNameSpace       = nS_BTH,
        qsNumberOfCsAddrs = 0,
        qsCsAddrs         = nullPtr,
        qsBlob            = nullPtr
      }
    ret <- wsaLookupServiceBegin pqs flags ph
    none <- if ret < 0 then do
        err <- getLastError
        if err == wsaServiceNotFound  -- This error means that there are no devices
            then pure True
            else throwIO =<< BluetoothException "discover" <$> (peekTString =<< getErrorMessage err)
      else
        pure False
    if none then
        return []
      else do
        h <- peek ph
        do
            let bufSize = 5000
            alloca $ \pResults -> allocaBytes bufSize $ \buf -> alloca $ \pdwSize -> do
                poke pResults $ WSAQUERYSET {
                    qsSize            = fromIntegral $ sizeOf (undefined :: WSAQUERYSET SockAddrBTH),
                    qsServiceInstanceName = nullPtr,
                    qsNameSpace       = nS_BTH,
                    qsNumberOfCsAddrs = 0,
                    qsCsAddrs         = nullPtr,
                    qsBlob            = nullPtr
                  }
                let loop acc = do
                        poke pdwSize (fromIntegral bufSize)
                        ret <- wsaLookupServiceNext h flags pdwSize pResults
                        if ret < 0 then do
                            err <- getLastError
                            if err == wsaENoMore
                                then pure $ reverse acc
                                else throwIO =<< BluetoothException "discover" <$> (peekTString =<< getErrorMessage err)
                          else do
                            results <- peek pResults
                            csAddrs <- peekArray (fromIntegral $ qsNumberOfCsAddrs results) (qsCsAddrs results)
                            addrs <- mapM (peek . saSockaddr . csaRemoteAddr) csAddrs
                            serviceName <- if qsServiceInstanceName results == nullPtr
                                then pure Nothing
                                else Just . C.pack <$> peekCString (qsServiceInstanceName results)
                            loop $ reverse (map (\addr -> (bthAddr addr, serviceName)) addrs) ++ acc
                loop []
          `finally`
            wsaLookupServiceEnd h

