import Network.Bluetooth
import Network.Socket (withSocketsDo)
import Control.Monad (mapM_)
import System.IO

main = withSocketsDo $ do
    mAdapter <- defaultAdapter
    case mAdapter of
        Just adapter -> do
            devs <- discover adapter
            mapM_ print devs
        Nothing -> hPutStrLn stderr "no local bluetooth adapter found"
