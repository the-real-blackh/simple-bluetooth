module Network.Bluetooth.Win32 where

#include <windows.h>

import Network.Bluetooth.Types

import qualified Data.ByteString.Internal as BI
import Control.Applicative
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
