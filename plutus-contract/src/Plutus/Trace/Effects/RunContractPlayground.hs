{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | A version of 'Plutus.Trace.Effects.RunContract' for use in the
--   playground.
module Plutus.Trace.Effects.RunContractPlayground(
    RunContractPlayground
    , callEndpoint
    , launchContract
    , handleRunContractPlayground
    ) where

import Cardano.Api (NetworkId)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.Coroutine (Yield (..))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg (..))
import Control.Monad.Freer.Reader (ask)
import Control.Monad.Freer.State (State, gets, modify)
import Control.Monad.Freer.TH (makeEffect)
import Data.Aeson qualified as JSON
import Data.Map (Map)
import Plutus.Contract (Contract (..), ContractInstanceId, EndpointDescription (..))
import Plutus.Contract.Effects (PABResp (ExposeEndpointResp))
import Plutus.Trace.Effects.ContractInstanceId (ContractInstanceIdEff, nextId)
import Plutus.Trace.Effects.RunContract (startContractThread)
import Plutus.Trace.Emulator.ContractInstance (EmulatorRuntimeError, getThread)
import Plutus.Trace.Emulator.Types (ContractConstraints, ContractHandle (..), EmulatorMessage (..),
                                    EmulatorRuntimeError (..), EmulatorThreads, walletInstanceTag)
import Plutus.Trace.Scheduler (EmSystemCall, MessageCall (Message), Priority (..), ThreadId, fork, mkSysCall)
import Wallet.Emulator.MultiAgent (EmulatorEvent' (..), MultiAgentEffect)
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Types (EndpointValue (..))

{- Note [Wallet contract instances]

In the Playground we have a single 'Contract' that we are testing, and each
wallet runs exactly one instance of this contract. As a result,

1. The 'RunContractPlayground' effect, which governs interactions with contract
   instances, only needs a 'Wallet' to identify the contract instance.
2. We don't need an @ActivateContract@ action, we can just start all the
   instances at the beginning of the simulation, using 'launchContract'

-}

data RunContractPlayground r where
    LaunchContract :: Wallet -> RunContractPlayground ()
    CallEndpoint :: Wallet -> String -> JSON.Value -> RunContractPlayground ()

makeEffect ''RunContractPlayground

-- | Handle the 'RunContractPlayground' effect.
handleRunContractPlayground ::
    forall w s e effs effs2 a.
    ( ContractConstraints s
    , Show e
    , JSON.ToJSON e
    , JSON.ToJSON w
    , Monoid w
    , Member ContractInstanceIdEff effs
    , Member (Yield (EmSystemCall effs2 EmulatorMessage a) (Maybe EmulatorMessage)) effs
    , Member (LogMsg EmulatorEvent') effs2
    , Member (Error EmulatorRuntimeError) effs2
    , Member (State EmulatorThreads) effs2
    , Member MultiAgentEffect effs2
    , Member (State (Map Wallet ContractInstanceId)) effs2
    , Member (State (Map Wallet ContractInstanceId)) effs
    )
    => NetworkId
    -> Contract w s e ()
    -> RunContractPlayground
    ~> Eff effs
handleRunContractPlayground networkId contract = \case
    CallEndpoint wallet ep vl -> handleCallEndpoint @effs @effs2 @a wallet ep vl
    LaunchContract wllt       -> handleLaunchContract @w @s @e @effs @effs2 @a networkId contract wllt

handleLaunchContract ::
    forall w s e effs effs2 a.
    ( ContractConstraints s
    , Show e
    , JSON.ToJSON e
    , JSON.ToJSON w
    , Monoid w
    , Member (Yield (EmSystemCall effs2 EmulatorMessage a) (Maybe EmulatorMessage)) effs
    , Member ContractInstanceIdEff effs
    , Member (LogMsg EmulatorEvent') effs2
    , Member (Error EmulatorRuntimeError) effs2
    , Member (State EmulatorThreads) effs2
    , Member MultiAgentEffect effs2
    , Member (State (Map Wallet ContractInstanceId)) effs
    )
    => NetworkId
    -> Contract w s e ()
    -> Wallet
    -> Eff effs ()
handleLaunchContract networkId contract wllt = do
    i <- nextId
    let handle = ContractHandle{chContract=contract, chInstanceId = i, chInstanceTag = walletInstanceTag wllt, chNetworkId = networkId}
    void $ startContractThread @w @s @e @effs @effs2 @a wllt handle
    modify @(Map Wallet ContractInstanceId) (set (at wllt) (Just i))

handleCallEndpoint ::
    forall effs effs2 a.
    ( Member (State (Map Wallet ContractInstanceId)) effs2
    , Member (Error EmulatorRuntimeError) effs2
    , Member (Yield (EmSystemCall effs2 EmulatorMessage a) (Maybe EmulatorMessage)) effs
    , Member (State EmulatorThreads) effs2
    )
    => Wallet
    -> String
    -> JSON.Value
    -> Eff effs ()
handleCallEndpoint wllt endpointName endpointValue = do
    let desc = EndpointDescription endpointName
        epJson = JSON.toJSON $ ExposeEndpointResp desc $ EndpointValue endpointValue
        thr = do
            threadId <- getInstance wllt >>= getThread
            ownId <- ask @ThreadId
            void $ mkSysCall @effs2 @EmulatorMessage @_ @a Normal (Left $ Message threadId $ EndpointCall ownId (EndpointDescription endpointName) epJson)
    void $ fork @effs2 @EmulatorMessage @_ @a "call endpoint" Normal thr

getInstance ::
    ( Member (State (Map Wallet ContractInstanceId)) effs
    , Member (Error EmulatorRuntimeError) effs
    )
    => Wallet
    -> Eff effs ContractInstanceId
getInstance wllt = do
    r <- gets @(Map Wallet ContractInstanceId) (view (at wllt))
    case r of
        Nothing -> throwError (InstanceIdNotFound wllt)
        Just i  -> pure i
