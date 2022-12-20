{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Marconi.Index.MintBurn where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Word (Word64)
import Database.SQLite.Simple qualified as SQL

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Cardano.Ledger.Core qualified as LCore
import Cardano.Ledger.Crypto qualified as LCrypto
import Cardano.Ledger.Era qualified as LEra

import Cardano.Ledger.Alonzo qualified as LA
import Cardano.Ledger.Alonzo.Scripts qualified as LA
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Alonzo.TxWitness qualified as LA

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

  -- AssetId and Quantity from txMintValue
  -- (assetId, quantity) :: (C.AssetId, C.Quantity) <- case C.txMintValue txbc of
  --   C.TxMintValue _ (v :: C.Value) _ -> C.valueToList v
  -- case assetId of
  --   C.AdaAssetId                 -> []
  --   C.AssetId policyId assetName -> pure $ mkTxMintEvent policyId assetName quantity undefined undefined

  -- From TxBodyContent{txOuts}: But this is WRONG, as this only transfers already existing asset
  -- txOut <- C.txOuts txbc
  -- (policyId, assetName, quantity) <- txOutValue txOut
  -- pure $ mkTxMintEvent policyId assetName quantity undefined undefined

  -- From real TxBody
  -- let
  --   scripts@(s1 : _) = txScripts txb :: [LCore.Script (C.ShelleyLedgerEra era)]

  --   rm = txRedeemers txb
  --   rl = Map.assocs rm -- ptr word is index of script?
  --   _ = map (\((LA.RdmrPtr tag w), _) -> undefined) rl
  --   _ = txDatums txb

  -- pure $ mkTxMintEvent policyId assetName quantity undefined undefined

  -- From real Tx 2
  let
    _ = txb :: C.TxBody era
    result = case txb of
      C.ShelleyTxBody era tx _ _ _ _ -> let
        -- _ = era :: C.ShelleyBasedEra era
        -- _ = tx :: LCore.TxBody (C.ShelleyLedgerEra era)
        in case era of
          C.ShelleyBasedEraShelley -> []
          C.ShelleyBasedEraAllegra -> []
          C.ShelleyBasedEraMary -> []
          C.ShelleyBasedEraAlonzo -> let
              mintRedeemers :: [(Word64, (LA.Data (C.ShelleyLedgerEra C.AlonzoEra), LA.ExUnits))]
              mintRedeemers = txRedeemers txb
                & Map.toList
                & filter (\(LA.RdmrPtr tag _, _) -> tag == LA.Mint)
                & map (\(LA.RdmrPtr _ w, a) -> (w, a))
              LM.Value _ m = LA.mint tx :: LM.Value (LEra.Crypto (LA.AlonzoEra LCrypto.StandardCrypto))
              _ = m :: Map.Map (LM.PolicyID LCrypto.StandardCrypto) (Map.Map LM.AssetName Integer)
              policyIdList = Map.toList m
              policyRedeemers :: [((LM.PolicyID LCrypto.StandardCrypto, Map.Map LM.AssetName Integer), (LA.Data OEra.StandardAlonzo, LA.ExUnits))]
              policyRedeemers = map (\(index, data_) -> (policyIdList !! fromIntegral index, data_)) mintRedeemers

            in policyRedeemers

          C.ShelleyBasedEraBabbage -> []

      _ -> [] -- ByronTxBody is not exported but as it's the only other data constructor then _ matches it.

  ((LM.PolicyID scriptHash, _), (redeemer, _)) :: ((LM.PolicyID LCrypto.StandardCrypto, Map.Map LM.AssetName Integer), (LA.Data OEra.StandardAlonzo, LA.ExUnits)) <- result

  let _ = mkTxMintEvent policyId

  undefined
  where

    -- * TxBody part getters

    txRedeemers :: C.TxBody era -> Map.Map LA.RdmrPtr (LA.Data (C.ShelleyLedgerEra era), LA.ExUnits)
    txRedeemers (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
      C.TxBodyScriptData _proof datum redeemers -> LA.unRedeemers redeemers

    -- UNUSED
    -- txDatums :: C.TxBody era -> [LA.Data (C.ShelleyLedgerEra era)]
    -- txDatums (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
    --   C.TxBodyScriptData _proof datums redeemers -> Map.elems $ LA.unTxDats datums

    -- txScripts :: C.TxBody era -> [LCore.Script (C.ShelleyLedgerEra era)]
    -- txScripts (C.ShelleyTxBody _ _ scripts _ _ _) = scripts

    -- txOutValue :: C.TxOut ctx era -> [(C.PolicyId, C.AssetName, C.Quantity)]
    -- txOutValue txOut =  case txOut of
    --   C.TxOut _proof value' _datum _referenceScript -> case value' of
    --     C.TxOutAdaOnly _proof _lovelace -> undefined
    --     C.TxOutValue _proof value''     -> fromValue value''


--    f2 :: C.TxBodyContent build era -> [(C.AssetId, C.Quantity)]
    -- f2 txbc = case C.txMintValue txbc of
    --   C.TxMintValue _ value _ -> C.valueToList value

    -- fromValue :: C.Value -> [(C.PolicyId, C.AssetName, C.Quantity)]
    -- fromValue value = do
    --   (assetId, quantity) <- C.valueToList value
    --   case assetId of
    --     C.AdaAssetId                 -> []
    --     C.AssetId policyId assetName -> pure (policyId, assetName, quantity)

    -- * Real TxBody




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

getLedgerEraConstraint :: C.ShelleyBasedEra era -> (LEra.Era (C.ShelleyLedgerEra era) => a) -> a
getLedgerEraConstraint C.ShelleyBasedEraShelley f = f
getLedgerEraConstraint C.ShelleyBasedEraAllegra f = f
getLedgerEraConstraint C.ShelleyBasedEraMary f    = f
getLedgerEraConstraint C.ShelleyBasedEraAlonzo f  = f
getLedgerEraConstraint C.ShelleyBasedEraBabbage f = f
