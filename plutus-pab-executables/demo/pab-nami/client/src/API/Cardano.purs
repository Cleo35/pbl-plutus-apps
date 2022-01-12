module API.Cardano
  ( getAddressKeyHashes
  ) where

import Prologue
import AppM (Env)
import Data.Cardano.Address (Address)
import Control.Monad.Reader (class MonadAsk)
import Effect.Class (class MonadEffect, liftEffect)
import Data.Cardano.BaseAddress as BaseAddress
import Data.Cardano.Ed25519KeyHash as Ed25519KeyHash
import Data.Cardano.StakeCredential as StakeCredential
import Effect (Effect)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Data.ArrayBuffer.Typed as ArrayBuffer
import Node.Encoding as Encoding

-- | From a Cardano address, extract the payment key and stake key encoded in
-- CBOR.
getAddressKeyHashes ::
  forall m.
  MonadEffect m =>
  MonadAsk Env m =>
  Address ->
  m (Tuple String String)
getAddressKeyHashes address = do
  baseAddress <- BaseAddress.fromAddress address
  let
    paymentKeyHash =
      Ed25519KeyHash.toBytes
        $ StakeCredential.toKeyHash
        $ BaseAddress.paymentCred baseAddress
  paymentKeyHashHex <-
    liftEffect $ identity
      $ (Buffer.fromArrayBuffer (ArrayBuffer.buffer paymentKeyHash) :: Effect Buffer)
      >>= Buffer.toString Encoding.Hex
  let
    stakeKeyHash =
      Ed25519KeyHash.toBytes
        $ StakeCredential.toKeyHash
        $ BaseAddress.stakeCred baseAddress
  stakeKeyHashHex <-
    liftEffect $ identity
      $ (Buffer.fromArrayBuffer (ArrayBuffer.buffer stakeKeyHash) :: Effect Buffer)
      >>= Buffer.toString Encoding.Hex
  pure $ Tuple paymentKeyHashHex stakeKeyHashHex
