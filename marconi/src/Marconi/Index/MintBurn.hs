{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Marconi.Index.MintBurn where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Short qualified as Short
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Word (Word64)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Cardano.Ledger.Core qualified as LCore
import Cardano.Ledger.Crypto qualified as LCrypto
import Cardano.Ledger.Era qualified as LEra

import Cardano.Ledger.Mary qualified as LM hiding (Value)
import Cardano.Ledger.ShelleyMA.TxBody qualified as LMA

import Cardano.Ledger.Alonzo qualified as LA
import Cardano.Ledger.Alonzo.Data qualified as LA
import Cardano.Ledger.Alonzo.Scripts qualified as LA
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Alonzo.TxWitness qualified as LA
import Cardano.Ledger.Babbage.Tx qualified as LB

import Ouroboros.Consensus.Shelley.Eras qualified as OEra

import Cardano.Ledger.Mary.Value qualified as LM

data TxMintEvent = TxMintEvent
  { txMintEventSlot         :: C.SlotNo
  , txMintEventBlockNo      :: C.BlockNo
  , txMintEventTxId         :: C.TxId
  , txMintEventPolicyId     :: C.PolicyId
  , txMintEventAssetName    :: C.AssetName
  , txMintEventQuantity     :: C.Quantity
  , txMintEventRedeemerIdx  :: Word64
  , txMintEventRedeemerData :: C.ScriptData
  }


-- * Event

toUpdate :: C.BlockInMode C.CardanoMode -> [TxMintEvent]
toUpdate (C.BlockInMode (C.Block (C.BlockHeader slotNo _ blockNo) txs :: C.Block era) _ :: C.BlockInMode C.CardanoMode) = do
  tx@(C.Tx txb@(C.TxBody txbc) _) <- txs

  let mkTxMintEvent policyId assetName quantity redeemerIx redeemerData = TxMintEvent
        { txMintEventSlot = slotNo
        , txMintEventBlockNo = blockNo
        , txMintEventTxId = C.getTxId txb
        , txMintEventPolicyId = policyId
        , txMintEventAssetName = assetName
        , txMintEventQuantity = quantity
        , txMintEventRedeemerIdx = redeemerIx
        , txMintEventRedeemerData = redeemerData
        }

  case txb of
    C.ShelleyTxBody era shelleyTx _ _ _ _ -> case era of
      C.ShelleyBasedEraShelley -> []
      C.ShelleyBasedEraAllegra -> []
      C.ShelleyBasedEraMary -> []
      C.ShelleyBasedEraAlonzo -> do
        (policyId, assetName, quantity, index, redeemer) <- getPolicyData txb $ LA.mint shelleyTx
        pure $ mkTxMintEvent policyId assetName quantity index redeemer
      C.ShelleyBasedEraBabbage -> do
        (policyId, assetName, quantity, index, redeemer) <- getPolicyData txb $ LB.mint shelleyTx
        pure $ mkTxMintEvent policyId assetName quantity index redeemer
    _ -> [] -- ByronTxBody is not exported but as it's the only other data constructor then _ matches it.

-- * Helpers

txRedeemers :: C.TxBody era -> Map.Map LA.RdmrPtr (LA.Data (C.ShelleyLedgerEra era), LA.ExUnits)
txRedeemers (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
  C.TxBodyScriptData _proof datum redeemers -> LA.unRedeemers redeemers

mintRedeemers :: C.TxBody era -> [(Word64, (LA.Data (C.ShelleyLedgerEra era), LA.ExUnits))]
mintRedeemers txb = txRedeemers txb
  & Map.toList
  & filter (\(LA.RdmrPtr tag _, _) -> tag == LA.Mint)
  & map (\(LA.RdmrPtr _ w, a) -> (w, a))

getMaryOtherAssets :: LM.Value c -> Map.Map (LM.PolicyID c) (Map.Map LM.AssetName Integer)
getMaryOtherAssets (LM.Value _ m) = m

getPolicyData :: C.TxBody era -> LM.Value OEra.StandardCrypto -> [(C.PolicyId, C.AssetName, C.Quantity, Word64, C.ScriptData)]
getPolicyData txb value = do
  let
    policyIdList = Map.toList $ getMaryOtherAssets value
    getPolicyId index = policyIdList !! fromIntegral index
  ((maryPolicyID, assets), index, (redeemer, _)) <- map (\(index, data_) -> (getPolicyId index, index, data_)) $ mintRedeemers txb
  (assetName, quantity) :: (LM.AssetName, Integer) <- Map.toList assets
  pure $ (fromMaryPolicyID maryPolicyID, fromMaryAssetName assetName, C.Quantity quantity, index, fromAlonzoData redeemer)

-- ** Copy-paste

fromMaryPolicyID :: LM.PolicyID OEra.StandardCrypto -> C.PolicyId
fromMaryPolicyID (LM.PolicyID sh) = C.PolicyId (C.fromShelleyScriptHash sh) -- file:/home/iog/src/cardano-node/cardano-api/src/Cardano/Api/Value.hs::293

fromMaryAssetName :: LM.AssetName -> C.AssetName
fromMaryAssetName (LM.AssetName n) = C.AssetName $ Short.fromShort n -- file:/home/iog/src/cardano-node/cardano-api/src/Cardano/Api/Value.hs::296

fromAlonzoData :: LA.Data ledgerera -> C.ScriptData
fromAlonzoData = C.fromPlutusData . LA.getPlutusData -- file:/home/iog/src/cardano-node/cardano-api/src/Cardano/Api/ScriptData.hs::147

-- * Sqlite

sqliteCreateTable :: SQL.Connection -> IO ()
sqliteCreateTable c = do
  liftIO $ SQL.execute_ c
    "CREATE TABLE IF NOT EXISTS minting_policy_event_table (TxId BLOB NOT NULL, PolicyId BLOB NOT NULL, AssetName STRING?, Quantity INT NOT NULL, RedeemerData, RedeemerIdx, BlockNumber, Slot)"
  undefined

instance SQL.ToField C.SlotNo where
  toField f = undefined

instance SQL.ToField C.BlockNo where
  toField f = undefined

instance SQL.ToField C.TxId where
  toField f = undefined

instance SQL.ToField C.PolicyId where
  toField f = undefined

instance SQL.ToField C.AssetName where
  toField f = undefined

instance SQL.ToField C.Quantity where
  toField f = undefined

-- instance SQL.ToField Word64 where
--   toField f = undefined

instance SQL.ToField C.ScriptData where
  toField f = undefined

instance SQL.ToRow TxMintEvent where
  toRow e = undefined
    [ SQL.toField $ txMintEventSlot e
    , SQL.toField $ txMintEventBlockNo e
    , SQL.toField $ txMintEventTxId e
    , SQL.toField $ txMintEventPolicyId e
    , SQL.toField $ txMintEventAssetName e
    , SQL.toField $ txMintEventQuantity e
    , SQL.toField $ txMintEventRedeemerIdx e
    , SQL.toField $ txMintEventRedeemerData e
    ]

sqliteInsert :: SQL.Connection -> [TxMintEvent] -> IO ()
sqliteInsert c es = do
  SQL.executeMany c
    "INSERT INTO minting_policy_event_table \
    \        (txId, policyId, assetName, quantity, redeemerData, redeemerIdx, blockNumber, slot) \
    \ VALUES (?   , ?       , ?        , ?       ,             ,            ,            , ?   )"
    $ map SQL.toRow es
  undefined

--   TxId -- Transaction which executed the minting policy
-- | PolicyId -- Minting policy hash - Part of the AssetId
-- | AssetName -- Part of the AssetId
-- | Quantity -- Amount that was minted or burned (could be negative)
-- | RedeemerData -- Data associated with the redeemer
-- | RedeemerIdx -- The index of the redeemer pointer in the transaction
-- | BlockNumber -- Block number this transaction occured
-- | Slot -- Slot this transaction occured
