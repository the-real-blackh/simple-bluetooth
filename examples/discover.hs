{-# LANGUAGE ScopedTypeVariables #-}
import Network.Bluetooth
import Network.Socket (withSocketsDo)
import Control.Applicative
import Control.Exception
import Control.Monad (mapM_)
import System.IO

main = withSocketsDo $ do
    mAdapter <- defaultAdapter
    case mAdapter of
        Just adapter -> do
            devs <- discover adapter
            mapM_ printDev devs
        Nothing -> hPutStrLn stderr "no local bluetooth adapter found"
  where
    printDev dev@(Device _ addr) = do
        putStr $ show addr ++ " "
        hFlush stdout
        {-
        name <- (Just <$> deviceName dev)
            `catch` \(exc :: BluetoothException) -> return Nothing
        print name
        -}
        putStrLn ""
        hFlush stdout

