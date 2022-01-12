module API.Wallet where

import Prologue
import AppM (Env)
import Cardano.Wallet.Nami as Nami
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk)
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Cardano.Transaction as Transaction
import Data.Cardano.TransactionWitnessSet as TransactionWitnessSet
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (attempt, throwError)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Halogen as H
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding as Encoding

-- | Given a partial transaction encoded in CBOR, balance, sign and submit it
-- using the Nami wallet api.
balanceSignAndSubmitTx ::
  forall m.
  MonadAsk Env m =>
  MonadAff m =>
  MonadThrow Error m =>
  String ->
  m String
balanceSignAndSubmitTx partialTxCbor = do
  balancedTxCbor <- H.liftAff $ Nami.balanceTx partialTxCbor
  balancedTxE <- mkFromCbor balancedTxCbor Transaction.fromBytes
  balancedTx <- either throwError pure balancedTxE
  txWitnessSetCborE <- H.liftAff $ attempt $ Nami.signTx balancedTxCbor Nothing
  txWitnessSetCbor <- either throwError pure txWitnessSetCborE
  txWitnessSetE <- mkFromCbor txWitnessSetCbor TransactionWitnessSet.fromBytes
  txWitnessSet <- either throwError pure txWitnessSetE
  finalTxE <- Transaction.new (Transaction.body balancedTx) txWitnessSet
  finalTx <- either throwError pure finalTxE
  finalTxCbor <- bytesToCbor $ Transaction.toBytes finalTx
  Nami.submitTx finalTxCbor

-- | Decode a CBOR string to the given type 'a'.
mkFromCbor ::
  forall m a.
  MonadEffect m =>
  String ->
  (Uint8Array -> m (Either Error a)) ->
  m (Either Error a)
mkFromCbor cbor mk = do
  bytes <-
    liftEffect $ (Buffer.fromString cbor Encoding.Hex :: Effect Buffer)
      >>= Buffer.toArrayBuffer
      >>= (\x -> ArrayBuffer.whole x :: Effect Uint8Array)
  mk bytes

-- | Convert a byte array to a CBOR string.
bytesToCbor ::
  forall m.
  MonadEffect m =>
  Uint8Array ->
  m String
bytesToCbor bytes =
  liftEffect $ (Buffer.fromArrayBuffer (ArrayBuffer.buffer bytes) :: Effect Buffer)
    >>= Buffer.toString Encoding.Hex
