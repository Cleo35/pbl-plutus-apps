{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Main entry points to the chain index.
-}
module Plutus.ChainIndex.App(main, runMain) where

import Control.Exception (throwIO)
import Data.Aeson qualified as A
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Yaml qualified as Y
import Options.Applicative (execParser)
import Prettyprinter (Pretty (pretty))

import Cardano.BM.Configuration.Model qualified as CM

import Cardano.BM.Setup (setupTrace_)
import Cardano.BM.Trace (Trace)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, waitAny, withAsync)
import Control.Concurrent.STM (TChan, dupTChan, tryPeekTChan, tryReadTChan, unGetTChan, writeTChan)
import Control.Concurrent.STM.TChan (newBroadcastTChanIO)
import Control.Monad (forM_, join, void)
import Control.Monad.Freer (Eff, LastMember, Member)
import Control.Monad.Freer.Extras (LogMsg)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Plutus.ChainIndex (ChainSyncBlock)
import Plutus.ChainIndex.CommandLine (AppConfig (AppConfig, acCLIConfigOverrides, acCommand, acConfigPath, acLogConfigPath, acMinLogLevel),
                                      Command (DumpDefaultConfig, DumpDefaultLoggingConfig, StartChainIndex),
                                      applyOverrides, cmdWithHelpParser)
import Plutus.ChainIndex.Compatibility (fromCardanoBlockNo)
import Plutus.ChainIndex.Config qualified as Config
import Plutus.ChainIndex.Effects (appendBlocks, resumeSync, rollback)
import Plutus.ChainIndex.Lib (ChainSyncEvent (Resume, RollBackward, RollForward), RunRequirements,
                              defaultChainSyncHandler, getTipSlot, runChainIndexDuringSync, storeFromBlockNo,
                              syncChainIndex, withRunRequirements, writeChainSyncEventToChan)
import Plutus.ChainIndex.Logging qualified as Logging
import Plutus.ChainIndex.Server qualified as Server
import Plutus.ChainIndex.SyncStats (SyncLog, SyncStats, convertEventToSyncStats, logProgress)
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog, runLogEffects)

main :: IO ()
main = do
  -- Parse comand line arguments.
  cmdConfig@AppConfig{acLogConfigPath, acConfigPath, acMinLogLevel, acCommand, acCLIConfigOverrides} <- execParser cmdWithHelpParser

  case acCommand of
    DumpDefaultConfig path ->
      A.encodeFile path Config.defaultConfig

    DumpDefaultLoggingConfig path ->
      Logging.defaultConfig >>= CM.toRepresentation >>= Y.encodeFile path

    StartChainIndex {} -> do
      -- Initialise logging
      logConfig <- maybe Logging.defaultConfig Logging.loadConfig acLogConfigPath
      for_ acMinLogLevel $ \ll -> CM.setMinSeverity logConfig ll

      -- Reading configuration file
      config <- applyOverrides acCLIConfigOverrides <$> case acConfigPath of
        Nothing -> pure Config.defaultConfig
        Just p  -> A.eitherDecodeFileStrict p >>=
          either (throwIO . Config.DecodeConfigException) pure

      putStrLn "\nCommand line config:"
      print cmdConfig

      putStrLn "\nLogging config:"
      CM.toRepresentation logConfig >>= print

      putStrLn "\nChain Index config:"
      print (pretty config)

      runMain logConfig config

runMain :: CM.Configuration -> Config.ChainIndexConfig -> IO ()
runMain logConfig config = do
  withRunRequirements logConfig config $ \runReq -> do
    putStr "\nThe tip of the local node: "
    slotNo <- getTipSlot config
    print slotNo

    -- Channel for broadcasting 'ChainSyncEvent's
    chanLog <- newBroadcastTChanIO
    chanBlocks <- newBroadcastTChanIO
    syncHandler
      <- defaultChainSyncHandler runReq
        & storeFromBlockNo (fromCardanoBlockNo $ Config.cicStoreFrom config)
        -- & writeChainSyncEventToChan convertEventToSyncStats chanLog
        -- & (\x -> x >>= writeChainSyncEventToChan id chanBlocks)
        & writeChainSyncEventToChan id chanBlocks

    putStrLn $ "Connecting to the node using socket: " <> Config.cicSocketPath config
    syncChainIndex config runReq syncHandler

    (trace :: Trace IO (PrettyObject SyncLog), _) <- setupTrace_ logConfig "chain-index"
    (trace2 :: Trace IO (PrettyObject SyncLog), _) <- setupTrace_ logConfig "chain-index"
    withAsync (runLogEffects (convertLog PrettyObject trace) $ logProgress chanLog) $ \a1 -> do
        withAsync (runLogEffects (convertLog PrettyObject trace2) $ processEvents runReq chanLog chanBlocks) $ \a2 -> void $ waitAny [a1, a2]
    -- (trace2 :: Trace IO (PrettyObject SyncLog), _) <- setupTrace_ logConfig "chain-index"
    -- withAsync (runLogEffects (convertLog PrettyObject trace2) $ processEvents runReq chanBlocks) $ \a2 -> void $ wait a2

    let port = show (Config.cicPort config)
    putStrLn $ "Starting webserver on port " <> port
    putStrLn $ "A Swagger UI for the endpoints are available at "
            <> "http://localhost:" <> port <> "/swagger/swagger-ui"
    Server.serveChainIndexQueryServer (Config.cicPort config) runReq

processEvents :: forall effs.
    ( LastMember IO effs
    )
    => RunRequirements
    -> TChan SyncStats
    -> TChan ChainSyncEvent
    -> Eff effs ()
processEvents runReq chanLog broadcastChan = do
    chan <- liftIO $ atomically $ dupTChan broadcastChan
    go chan
  where
    go chan = do
        events <- liftIO $ readRollFowardInBatch chan
        liftIO $ print $ length events
        case events of
          [] -> do
            liftIO $ putStrLn "No events... delaying"
            liftIO $ threadDelay 30_000_000
            go chan
          _ -> do
            void $ liftIO $ runChainIndexDuringSync runReq $ case events of
                [RollBackward point _] -> do
                    rollback point
                [Resume point] -> do
                    resumeSync point
                e -> do
                    appendBlocks $ getRollForwards e
            forM_ events $ \e -> liftIO $ atomically $ writeTChan chanLog (convertEventToSyncStats e)
            go chan

getRollForwards :: [ChainSyncEvent] -> [ChainSyncBlock]
getRollForwards ((RollForward block _):events) = block : getRollForwards events
getRollForwards _                              = []

readRollFowardInBatch :: TChan ChainSyncEvent -> IO [ChainSyncEvent]
readRollFowardInBatch tchan = do
    eventM <- atomically $ tryReadTChan tchan
    case eventM of
        Nothing                   -> pure []
        Just event@RollForward {} -> go [event]
        Just event                -> pure [event]
    where
        go combined = do
            if length combined == 1000
            then
                pure combined
            else do
                elementM <- atomically $ tryReadTChan tchan
                case elementM of
                  Nothing      -> pure combined
                  Just event@RollForward {} -> go (combined <> [event])
                  Just event -> do
                      atomically $ unGetTChan tchan event
                      pure combined
