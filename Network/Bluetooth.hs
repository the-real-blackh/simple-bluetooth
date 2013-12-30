-- | Simple Bluetooth API for Windows and Linux (bluez).
--
-- You must use 'Network.withSocketsDo' at the start of your program
-- for Windows compatibility.
module Network.Bluetooth (
        module Network.Bluetooth.Adapter,
        module Network.Bluetooth.Device
    ) where

import Network.Bluetooth.Adapter
import Network.Bluetooth.Device
import qualified Network (withSocketsDo)

