{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module DemoContract(
    DemoContract(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as OpenApi
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)
import Ledger (PaymentPubKeyHash, StakePubKeyHash, Value)
import Ledger.Constraints (adjustUnbalancedTx, mustPayToPubKeyAddress)
import Playground.Types (FunctionSchema)
import Plutus.Contract (ContractError, Endpoint, Promise, endpoint, logInfo, mkTxConstraints, yieldUnbalancedTx)
import Plutus.Contracts.Game qualified as Contracts.Game
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions, SomeBuiltin (SomeBuiltin))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Prettyprinter (Pretty, pretty, viaShow)
import Schema (FormSchema, ToSchema)

data DemoContract = PayToWallet | Game
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty DemoContract where
    pretty = viaShow

instance HasPSTypes DemoContract where
    psTypes =
        [ equal . genericShow . argonaut $ mkSumType @DemoContract
        ]

instance HasDefinitions DemoContract where
    getDefinitions = [ PayToWallet
                     , Game
                     ]
    getContract = getDemoContract
    getSchema = getDemoContractSchema

getDemoContractSchema :: DemoContract -> [FunctionSchema FormSchema]
getDemoContractSchema = \case
    PayToWallet -> Builtin.endpointsToSchemas @PayToWalletSchema
    Game        -> Builtin.endpointsToSchemas @Contracts.Game.GameSchema

getDemoContract :: DemoContract -> SomeBuiltin
getDemoContract = \case
    PayToWallet -> SomeBuiltin payToWallet
    Game        -> SomeBuiltin (Contracts.Game.contract @Text)

data PayToWalletParams =
    PayToWalletParams
        { amount :: Value
        , pkh    :: PaymentPubKeyHash
        , skh    :: StakePubKeyHash
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

type PayToWalletSchema = Endpoint "PayToWallet" PayToWalletParams

payToWallet :: Promise () PayToWalletSchema ContractError ()
payToWallet = endpoint @"PayToWallet" $ \PayToWalletParams{amount, pkh, skh} -> do
    utx <- mkTxConstraints @Void mempty (mustPayToPubKeyAddress pkh skh amount)
    logInfo @String $ show utx
    yieldUnbalancedTx $ adjustUnbalancedTx utx
