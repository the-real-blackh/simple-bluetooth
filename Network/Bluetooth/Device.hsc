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

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/rfcomm.h>
#include <stddef.h>

import Network.Bluetooth.Types
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
import Data.Word
import Foreign
import Foreign.C
import GHC.Conc (threadWaitWrite)


data Device = Device Adapter BDAddr deriving (Eq, Ord, Show)

foreign import ccall safe "hci_read_remote_name" hci_read_remote_name
    :: CInt -> Ptr BDAddr -> CInt -> Ptr CChar -> CInt -> IO CInt

deviceName :: Device -> IO ByteString
deviceName dev@(Device (Adapter _ dd) (BDAddr bs)) = do
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

data RFCOMMSocket = RFCOMMSocket Socket

data SockAddrRC = SockAddrRC Word16 ByteString Word8

instance Storable SockAddrRC where
    sizeOf _ = (#const sizeof(struct sockaddr_rc))
    alignment _ = alignment (undefined :: Word64)  -- be conservative
    peek _ = fail "SockAddrRC.peek not defined"
    poke p (SockAddrRC family bdaddr channel) = do
        let p_family = p `plusPtr` (#const offsetof(struct sockaddr_rc, rc_family)) :: Ptr ()
            p_bdaddr = p `plusPtr` (#const offsetof(struct sockaddr_rc, rc_bdaddr))
            p_channel = p `plusPtr` (#const offsetof(struct sockaddr_rc, rc_channel))
        case (#const sizeof(sa_family_t)) of
            1 -> poke (castPtr p_family) (fromIntegral family :: Word8)
            2 -> poke (castPtr p_family) (fromIntegral family :: Word16)
            4 -> poke (castPtr p_family) (fromIntegral family :: Word32)
            sz -> fail $ "SockAddrRC.poke can't handle size "++show sz
        B.unsafeUseAsCString bdaddr $ \c_bdaddr ->
            B.memcpy p_bdaddr (castPtr c_bdaddr) (B.length bdaddr)
        poke p_channel channel

foreign import ccall unsafe "connect"
  c_connect :: CInt -> Ptr SockAddrRC -> CInt -> IO CInt

openRFCOMM :: Device -> Word8 -> IO RFCOMMSocket
openRFCOMM dev@(Device _ (BDAddr bdaddr)) channel = do
    s <- socket AF_BLUETOOTH Stream (#const BTPROTO_RFCOMM)
    connect s `onException` sClose s
  where
    connect s = do
        let fd = fdSocket s
        ret <- alloca $ \p_sarc -> do
            poke p_sarc (SockAddrRC (#const AF_BLUETOOTH) bdaddr channel)
            c_connect fd p_sarc (fromIntegral $ sizeOf (undefined :: SockAddrRC))
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
                            err <- peekCString (strerror errno_)
                            throwIO $ BluetoothException "openRFCOMM" err
                _ -> do
                    err <- peekCString (strerror errno_)
                    throwIO $ BluetoothException "openRFCOMM" err
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
