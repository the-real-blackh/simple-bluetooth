{-# LANGUAGE ForeignFunctionInterface #-}
module Network.Bluetooth.Device (
        Device(..),
        deviceName,
        RFCOMMSocket,
        openRFCOMM,
        recvRFCOMM,
        sendRFCOMM,
        sendAllRFCOMM,
        closeRFCOMM,
        module Network.Bluetooth.Types
    ) where

#if defined(mingw32_HOST_OS)
#include <windows.h>
#else
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/rfcomm.h>
#endif
#include <stddef.h>

import Network.Bluetooth.Types
#if defined(mingw32_HOST_OS)
import Network.Bluetooth.Win32
#endif
import Network.Socket
import qualified Network.Socket.ByteString as NB

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import Data.List (lookup)
import Data.Word
import Foreign
import Foreign.C
import GHC.Conc (threadWaitWrite)
#if defined(mingw32_HOST_OS)
import System.Win32.Types
#endif


data Device = Device Adapter BluetoothAddr deriving (Eq, Ord, Show)

foreign import ccall safe "hci_read_remote_name" hci_read_remote_name
    :: CInt -> Ptr BluetoothAddr -> CInt -> Ptr CChar -> CInt -> IO CInt

deviceName :: Device -> IO ByteString
#if defined(mingw32_HOST_OS)
deviceName dev@(Device a addr) = do
    devs <- discover' a flags
    case join $ lookup addr devs of
        Just name -> return name
        Nothing -> throwIO $ BluetoothException "deviceName" "device has no name"
  where
    flags = (#const LUP_CONTAINERS) .|.
            (#const LUP_RETURN_ADDR) .|.
            (#const LUP_RETURN_NAME)
#else
deviceName dev@(Device (Adapter _ dd) (BluetoothAddr bs)) = do
    retRef <- newIORef 0
    bs0 <- B.create maxLen $ \buf -> do
        ret <- B.unsafeUseAsCString bs $ \cs ->
            hci_read_remote_name dd (castPtr cs) (fromIntegral maxLen) (castPtr buf) 0
        writeIORef retRef ret
    ret <- readIORef retRef
    if ret < 0 then do
        errno@(Errno errno_) <- getErrno
        if errno == eINTR
            then deviceName dev
            else do
                err <- peekCString (strerror errno_)
                throwIO $ BluetoothException "deviceName" err
      else
        return $ B.takeWhile (/= 0) bs0
  where
    maxLen = 255
#endif

data RFCOMMSocket = RFCOMMSocket Socket

#if !defined(mingw32_HOST_OS)
data SockAddrBTH = SockAddrBTH Word16 ByteString Word8

sockAddrBTH :: BluetoothAddr -> Word8 -> SockAddrBTH
sockAddrBTH (BluetoothAddr bs) port = SockAddrBTH (#const AF_BLUETOOTH) bs port

instance Storable SockAddrBTH where
    sizeOf _ = (#const sizeof(struct sockaddr_rc))
    alignment _ = alignment (undefined :: Word64)
    peek _ = fail "SockAddrBTH.peek not defined"
    poke p (SockAddrBTH family bdaddr channel) = do
        let p_family = p `plusPtr` (#const offsetof(struct sockaddr_rc, rc_family)) :: Ptr ()
            p_bdaddr = p `plusPtr` (#const offsetof(struct sockaddr_rc, rc_bdaddr))
            p_channel = p `plusPtr` (#const offsetof(struct sockaddr_rc, rc_channel))
        case (#const sizeof(sa_family_t)) of
            1 -> poke (castPtr p_family) (fromIntegral family :: Word8)
            2 -> poke (castPtr p_family) (fromIntegral family :: Word16)
            4 -> poke (castPtr p_family) (fromIntegral family :: Word32)
            sz -> fail $ "SockAddrBTH.poke can't handle size "++show sz
        B.unsafeUseAsCString bdaddr $ \c_bdaddr ->
            B.memcpy p_bdaddr (castPtr c_bdaddr) (B.length bdaddr)
        poke p_channel channel
#endif

#if defined(mingw32_HOST_OS)
foreign import stdcall unsafe "connect"
#else
foreign import ccall unsafe "connect"
#endif
  c_connect :: CInt -> Ptr SockAddrBTH -> CInt -> IO CInt

#if defined(mingw32_HOST_OS)
foreign import stdcall unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt
#endif

openRFCOMM :: Device -> Word8 -> IO RFCOMMSocket
openRFCOMM dev@(Device _ addr) channel = do
#if defined(mingw32_HOST_OS)
    s <- do
        fd <- c_socket (fromIntegral aF_BTH) (#const SOCK_STREAM) bTHPROTO_RFCOMM
        mkSocket fd AF_BLUETOOTH Stream bTHPROTO_RFCOMM NotConnected
#else
    s <- socket AF_BLUETOOTH Stream (#const BTPROTO_RFCOMM)
#endif
    connect s `onException` sClose s
  where
    connect s = do
        let fd = fdSocket s
        ret <- alloca $ \p_sarc -> do
            poke p_sarc (sockAddrBTH addr channel)
            c_connect fd p_sarc (fromIntegral $ sizeOf (undefined :: SockAddrBTH))
        if ret < 0 then do
            errno@(Errno errno_) <- getErrno
            case errno of
                _ | errno == eINTR -> connect s
                _ | errno == eOK -> return $ RFCOMMSocket s
                _ | errno == eINPROGRESS -> do
                    threadWaitWrite (fromIntegral fd)
                    errno@(Errno errno_) <- Errno . fromIntegral <$> getSocketOption s SoError
                    if errno == eOK
                        then return $ RFCOMMSocket s
                        else do
#if defined(mingw32_HOST_OS)
                            err <- getLastError
                            throwIO =<< BluetoothException "openRFCOMM" <$> (peekTString =<< getErrorMessage err)
#else
                            err <- peekCString (strerror errno_)
                            throwIO $ BluetoothException "openRFCOMM" err
#endif
                _ -> do
#if defined(mingw32_HOST_OS)
                    err <- getLastError
                    throwIO =<< BluetoothException "openRFCOMM" <$> (peekTString =<< getErrorMessage err)
#else
                    err <- peekCString (strerror errno_)
                    throwIO $ BluetoothException "openRFCOMM" err
#endif
          else
            return $ RFCOMMSocket s

recvRFCOMM :: RFCOMMSocket -> Int -> IO ByteString
recvRFCOMM (RFCOMMSocket s) n = NB.recv s n
  `catch` \exc ->
     throwIO (BluetoothException "recvRFCOMM" (show (exc :: IOException)))

sendRFCOMM :: RFCOMMSocket -> ByteString -> IO Int
sendRFCOMM (RFCOMMSocket s) txt = NB.send s txt
  `catch` \exc ->
     throwIO (BluetoothException "sendRFCOMM" (show (exc :: IOException)))

sendAllRFCOMM :: RFCOMMSocket -> ByteString -> IO ()
sendAllRFCOMM (RFCOMMSocket s) txt = NB.sendAll s txt
  `catch` \exc ->
     throwIO (BluetoothException "sendAllRFCOMM" (show (exc :: IOException)))

closeRFCOMM :: RFCOMMSocket -> IO ()
closeRFCOMM (RFCOMMSocket s) = sClose s
  `catch` \exc ->
     throwIO (BluetoothException "close" (show (exc :: IOException)))
