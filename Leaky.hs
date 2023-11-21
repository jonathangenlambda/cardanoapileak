module Main (main) where

import Cardano.Api
import Control.Monad
import Data.String
import Data.Word
import System.Environment

main :: IO ()
main = do
    [networkIdStr, socketPath] <- getArgs
    putStrLn "Running Leaky LocalTxMonitoringMempoolInformation"

    forever $ do
        -- With this approach the application memory grows unbounded reaching 680MiB after 10 minutes
        -- according to Ubuntu 22.04 System Monitor
        res <-
            queryTxMonitoringLocal
                (connectInfo (read networkIdStr) socketPath)
                LocalTxMonitoringMempoolInformation
        case res of
            LocalTxMonitoringMempoolSizeAndCapacity cap _slot ->
                print $ "Leaky LocalTxMonitoringMempoolInformation: " ++ show cap
            _ -> print "Unexpected response in Leaky LocalTxMonitoringMempoolInformation"
  where
    connectInfo ::
        Word32 ->
        String ->
        LocalNodeConnectInfo CardanoMode
    connectInfo nwId socketPath =
        LocalNodeConnectInfo
            { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
            , localNodeNetworkId = Testnet $ NetworkMagic nwId
            , localNodeSocketPath = fromString socketPath
            }
