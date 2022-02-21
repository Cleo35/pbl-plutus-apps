module Ledger.Contexts
    ( module Export
    , pubKeyHash
    , scriptCurrencySymbol
    ) where

import Ledger.Crypto (pubKeyHash)
import Ledger.Scripts (MintingPolicy, MintingPolicyHash (..), mintingPolicyHash)
import Plutus.V1.Ledger.Contexts as Export
import Plutus.V1.Ledger.Value (CurrencySymbol (..))
import Plutus.V1.Ledger.Value qualified as Value

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'
scriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
scriptCurrencySymbol scrpt = let (MintingPolicyHash hsh) = mintingPolicyHash scrpt in Value.CurrencySymbol hsh

{-# INLINABLE adaLockedBy #-}
-- | Get the total amount of 'Ada' locked by the given validator in this transaction.
adaLockedBy :: TxInfo -> ValidatorHash -> Ada
adaLockedBy ptx h = Ada.fromValue (valueLockedBy ptx h)

{-# INLINABLE signsTransaction #-}
-- | Check if the provided signature is the result of signing the pending
--   transaction (without witnesses) with the given public key.
signsTransaction :: Signature -> PubKey -> TxInfo -> Bool
signsTransaction (Signature sig) (PubKey (LedgerBytes pk)) TxInfo{txInfoId=TxId h} =
    verifySignature pk h sig
