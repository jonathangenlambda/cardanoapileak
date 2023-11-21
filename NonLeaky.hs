{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Cardano.Api
import Control.Concurrent
import Control.Monad
import Data.String
import Data.Word
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
import System.Environment

main :: IO ()
main = do
    [networkIdStr, socketPath] <- getArgs
    putStrLn "Running NonLeaky LocalTxMonitoringMempoolInformation"

    -- With this approach the application memory consumption tops out at ~131MiB on local machine
    -- according to the Ubuntu 22.04 System Monitor
    client <- mkMempoolClient $ connectInfo (read networkIdStr) socketPath
    forever $ do
        mpc <- getMempoolSizeAndCapacity client
        print $ "NonLeaky LocalTxMonitoringMempoolInformation: " ++ show mpc
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

data MempoolClient = MempoolClient
    { mpcRequestVar :: !(MVar ())
    , mpcResultVar :: !(MVar MempoolSizeAndCapacity)
    }

getMempoolSizeAndCapacity ::
    MempoolClient ->
    IO MempoolSizeAndCapacity
getMempoolSizeAndCapacity mpc = do
    putMVar (mpcRequestVar mpc) ()
    takeMVar (mpcResultVar mpc)

mkMempoolClient ::
    LocalNodeConnectInfo CardanoMode ->
    IO MempoolClient
mkMempoolClient nodeConn = do
    requestVar <- newEmptyMVar
    resultVar <- newEmptyMVar
    let prot =
            LocalNodeClientProtocols
                { localChainSyncClient = NoLocalChainSyncClient
                , localTxSubmissionClient = Nothing
                , localStateQueryClient = Nothing
                , localTxMonitoringClient = Just $ LocalTxMonitorClient $ pure (clientIdle requestVar resultVar)
                }
    void $ forkIO $ connectToLocalNode nodeConn prot
    pure $ MempoolClient requestVar resultVar
  where
    clientIdle ::
        MVar () ->
        MVar MempoolSizeAndCapacity ->
        Ouroboros.Network.Protocol.LocalTxMonitor.Client.ClientStIdle txid (TxInMode era) SlotNo IO ()
    clientIdle requestVar resultVar = do
        SendMsgAcquire $ \slotNo -> do
            void $ takeMVar requestVar
            pure $ localTxMonitorMempoolInfo requestVar resultVar slotNo

    localTxMonitorMempoolInfo ::
        MVar () ->
        MVar MempoolSizeAndCapacity ->
        SlotNo ->
        ClientStAcquired txid (TxInMode era) SlotNo IO ()
    localTxMonitorMempoolInfo requestVar resultVar _slotNo = SendMsgGetSizes $ \mpc -> do
        putMVar resultVar mpc
        pure $ SendMsgRelease $ pure (clientIdle requestVar resultVar)
