{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Marconi.Index.MintBurn where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Data qualified as L
import Cardano.Ledger.Alonzo.TxWitness qualified as L
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as Map
import Data.Word (Word64)
import Database.SQLite.Simple qualified as SQL

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

  -- AssetId and Quantity from txMintValue
  (assetId, quantity) :: (C.AssetId, C.Quantity) <- case C.txMintValue txbc of
    C.TxMintValue _ (v :: C.Value) _ -> C.valueToList v
  case assetId of
    C.AdaAssetId -> []
    C.AssetId policyId assetName -> pure $ TxMintEvent
      { txMintEventSlot = slotNo
      , txMintEventBlockNo = blockNo
      , txMintEventTxId = C.getTxId txb
      , txMintEventPolicyId = policyId
      , txMintEventAssetName = assetName
      , txMintEventQuantity = quantity
      -- , txMintEventRedeemerIdx :: Word64
      -- , txMintEventRedeemerData :: C.ScriptData
      }

  -- From TxOuts
  txOut <- C.txOuts txbc
  (policyId, assetName, quantity) <- txOutValue txOut
  pure $ TxMintEvent
    { txMintEventSlot = slotNo
    , txMintEventBlockNo = blockNo
    , txMintEventTxId = C.getTxId txb
    , txMintEventPolicyId = policyId
    , txMintEventAssetName = assetName
    , txMintEventQuantity = quantity
    -- , txMintEventRedeemerIdx :: Word64
    -- , txMintEventRedeemerData :: C.ScriptData
    }

  where

    txRedeemer :: C.TxBody era -> [L.Data (C.ShelleyLedgerEra era)]
    txRedeemer (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
      C.TxBodyScriptData _proof datum redeemers -> map fst $ Map.elems $ L.unRedeemers redeemers

    txOutValue :: C.TxOut ctx era -> [(C.PolicyId, C.AssetName, C.Quantity)]
    txOutValue txOut =  case txOut of
      C.TxOut _proof value' _datum _referenceScript -> case value' of
        C.TxOutAdaOnly _proof _lovelace -> undefined
        C.TxOutValue _proof value''     -> fromValue value''


--    f2 :: C.TxBodyContent build era -> [(C.AssetId, C.Quantity)]
    f2 txbc = case C.txMintValue txbc of
      C.TxMintValue _ value _ -> C.valueToList value

    fromValue :: C.Value -> [(C.PolicyId, C.AssetName, C.Quantity)]
    fromValue value = do
      (assetId, quantity) <- C.valueToList value
      case assetId of
        C.AdaAssetId                 -> []
        C.AssetId policyId assetName -> pure (policyId, assetName, quantity)



-- * Sqlite

sqliteCreateTable :: SQL.Connection -> IO ()
sqliteCreateTable c = do
  liftIO $ SQL.execute_ c
    "CREATE TABLE IF NOT EXISTS minting_policy_event_table (TxId BLOB NOT NULL, PolicyId BLOB NOT NULL, AssetName STRING?, Quantity INT NOT NULL, RedeemerData, RedeemerIdx, BlockNumber, Slot)"
  undefined

sqliteInsert :: SQL.Connection -> [TxMintEvent] -> IO ()
sqliteInsert c _ = do
  undefined

--   TxId -- Transaction which executed the minting policy
-- | PolicyId -- Minting policy hash - Part of the AssetId
-- | AssetName -- Part of the AssetId
-- | Quantity -- Amount that was minted or burned (could be negative)
-- | RedeemerData -- Data associated with the redeemer
-- | RedeemerIdx -- The index of the redeemer pointer in the transaction
-- | BlockNumber -- Block number this transaction occured
-- | Slot -- Slot this transaction occured
