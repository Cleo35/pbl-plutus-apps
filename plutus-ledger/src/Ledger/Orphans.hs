{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Orphans where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Crypto.Wallet qualified as Crypto
import Cardano.Ledger.Crypto qualified as C
import Cardano.Ledger.Hashes qualified as Hashes
import Cardano.Ledger.SafeHash qualified as C
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Freer.Extras.Log (LogLevel, LogMessage)
import Crypto.Hash qualified as Crypto
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.Bifunctor (bimap)
import Data.ByteArray qualified as BA
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as Text
import Data.Typeable (Proxy (Proxy), Typeable)
import GHC.Exts (IsList (fromList))
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Ada (Ada (Lovelace))
import Plutus.V1.Ledger.Api (Address, BuiltinByteString, BuiltinData (BuiltinData), Credential,
                             CurrencySymbol (CurrencySymbol), Data, Datum (Datum), DatumHash (DatumHash), Extended,
                             Interval, LedgerBytes (LedgerBytes), LowerBound, MintingPolicy (MintingPolicy),
                             MintingPolicyHash (MintingPolicyHash), POSIXTime (POSIXTime), PubKeyHash (PubKeyHash),
                             Redeemer (Redeemer), RedeemerHash (RedeemerHash), Script, StakeValidator (StakeValidator),
                             StakeValidatorHash (StakeValidatorHash), StakingCredential, TokenName (TokenName),
                             TxId (TxId), TxOut, TxOutRef, UpperBound, Validator (Validator),
                             ValidatorHash (ValidatorHash), Value (Value), fromBytes)
import Plutus.V1.Ledger.Bytes (bytes)
import Plutus.V1.Ledger.Crypto (PrivateKey (PrivateKey, getPrivateKey), PubKey (PubKey), Signature (Signature))
import Plutus.V1.Ledger.Scripts (ScriptHash (..))
import Plutus.V1.Ledger.Slot (Slot (Slot))
import Plutus.V1.Ledger.Tx (RedeemerPtr, ScriptTag, Tx, TxIn, TxInType)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import PlutusCore (Kind, Some, Term, Type, ValueOf, Version)
import PlutusTx.AssocMap qualified as AssocMap
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

-- | Orphan instances for plutus-ledger-api data types.

-- | General
instance ToJSON BSS.ByteString where
    toJSON = JSON.String . JSON.encodeByteString

instance FromJSON BSS.ByteString where
    parseJSON v = JSON.decodeByteString v

instance ToJSON PlutusTx.BuiltinByteString where
    toJSON = JSON.String . JSON.encodeByteString . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
    parseJSON v = PlutusTx.toBuiltin <$> JSON.decodeByteString v

-- | Plutus.V1.Ledger.Address

deriving instance Hashable Address
deriving instance FromJSON Address
deriving instance FromJSONKey Address
deriving instance Serialise Address
deriving instance ToJSON Address
deriving instance ToJSONKey Address

deriving instance Hashable Credential
deriving instance FromJSON Credential
deriving instance Serialise Credential
deriving instance ToJSON Credential

deriving instance Hashable StakingCredential
deriving instance FromJSON StakingCredential
deriving instance Serialise StakingCredential
deriving instance ToJSON StakingCredential

-- | Plutus.V1.Ledger.Bytes

deriving instance FromJSONKey LedgerBytes
deriving instance Serialise LedgerBytes
deriving instance ToJSONKey LedgerBytes

instance FromJSON LedgerBytes where
    parseJSON v = fromBytes <$> JSON.decodeByteString v

instance ToJSON LedgerBytes where
    toJSON = JSON.String . JSON.encodeByteString . bytes

-- | Plutus.V1.Ledger.Crypto

deriving instance Newtype PubKeyHash
deriving instance FromJSON PubKeyHash
deriving instance FromJSONKey PubKeyHash
deriving instance Hashable PubKeyHash
deriving instance Serialise PubKeyHash
deriving instance ToJSON PubKeyHash
deriving instance ToJSONKey PubKeyHash

-- | Plutus.V1.Ledger.DCert

deriving instance Hashable DCert
deriving instance FromJSON DCert
deriving instance Serialise DCert
deriving instance ToJSON DCert

-- | Plutus.V1.Ledger.Interval

deriving instance Hashable Interval
deriving instance FromJSON Interval
deriving instance Serialise Interval
deriving instance ToJSON Interval

deriving instance Hashable Extended
deriving instance FromJSON Extended
deriving instance Serialise Extended
deriving instance ToJSON Extended

deriving instance Hashable UpperBound
deriving instance FromJSON UpperBound
deriving instance Serialise UpperBound
deriving instance ToJSON UpperBound

deriving instance Hashable LowerBound
deriving instance FromJSON LowerBound
deriving instance Serialise LowerBound
deriving instance ToJSON LowerBound

-- | Plutus.V1.Ledger.Scripts

deriving instance FromJSON ScriptError
deriving instance ToJSON ScriptError

{- Note [JSON instances for Script]
The JSON instances for Script are partially hand-written rather than going via the Serialise
instance directly. The reason for this is to *avoid* the size checks that are in place in the
Serialise instance. These are only useful for deserialisation checks on-chain, whereas the
JSON instances are used for e.g. transmitting validation events, which often include scripts
with the data arguments applied (which can be very big!).
-}

instance ToJSON Script where
    -- See note [JSON instances for Script]
    toJSON (Script p) = JSON.String $ JSON.encodeSerialise (SerialiseViaFlat p)

instance FromJSON Script where
    -- See note [JSON instances for Script]
    parseJSON v = do
        (SerialiseViaFlat p) <- JSON.decodeSerialise v
        Haskell.return $ Script p

deriving via (JSON.JSONViaSerialise PLC.Data) instance ToJSON PLC.Data
deriving via (JSON.JSONViaSerialise PLC.Data) instance FromJSON PLC.Data

deriving instance FromJSON Validator
deriving instance ToJSON Validator

instance BA.ByteArrayAccess Validator where
    length =
        BA.length . BSL.toStrict . serialise
    withByteArray =
        BA.withByteArray . BSL.toStrict . serialise

deriving instance FromJSON Datum
deriving instance Serialise Datum
deriving instance ToJSON Datum

instance BA.ByteArrayAccess Datum where
    length =
        BA.length . BSL.toStrict . serialise
    withByteArray =
        BA.withByteArray . BSL.toStrict . serialise

deriving instance FromJSON Redeemer
deriving instance Serialise Redeemer
deriving instance ToJSON Redeemer

instance BA.ByteArrayAccess Redeemer where
    length =
        BA.length . BSL.toStrict . serialise
    withByteArray =
        BA.withByteArray . BSL.toStrict . serialise

deriving instance FromJSON MintingPolicy
deriving instance ToJSON MintingPolicy

instance BA.ByteArrayAccess MintingPolicy where
    length =
        BA.length . BSL.toStrict . serialise
    withByteArray =
        BA.withByteArray . BSL.toStrict . serialise

deriving instance FromJSON StakeValidator
deriving instance ToJSON StakeValidator

instance BA.ByteArrayAccess StakeValidator where
    length =
        BA.length . BSL.toStrict . serialise
    withByteArray =
        BA.withByteArray . BSL.toStrict . serialise

deriving instance Hashable ScriptHash
deriving instance FromJSON ScriptHash
deriving instance FromJSONKey ScriptHash
deriving instance Serialise ScriptHash
deriving instance ToJSON ScriptHash
deriving instance ToJSONKey ScriptHash

deriving instance Hashable ValidatorHash
deriving instance FromJSON ValidatorHash
deriving instance FromJSONKey ValidatorHash
deriving instance Serialise ValidatorHash
deriving instance ToJSON ValidatorHash
deriving instance ToJSONKey ValidatorHash

deriving instance Hashable DatumHash
deriving instance FromJSON DatumHash
deriving instance FromJSONKey DatumHash
deriving instance Serialise DatumHash
deriving instance ToJSON DatumHash
deriving instance ToJSONKey DatumHash

deriving instance Hashable RedeemerHash
deriving instance FromJSON RedeemerHash
deriving instance FromJSONKey RedeemerHash
deriving instance Serialise RedeemerHash
deriving instance ToJSON RedeemerHash
deriving instance ToJSONKey RedeemerHash

deriving instance Hashable MintingPolicyHash
deriving instance FromJSON MintingPolicyHash
deriving instance FromJSONKey MintingPolicyHash
deriving instance Serialise MintingPolicyHash
deriving instance ToJSON MintingPolicyHash
deriving instance ToJSONKey MintingPolicyHash

deriving instance Hashable StakeValidatorHash
deriving instance FromJSON StakeValidatorHash
deriving instance FromJSONKey StakeValidatorHash
deriving instance Serialise StakeValidatorHash
deriving instance ToJSON StakeValidatorHash
deriving instance ToJSONKey StakeValidatorHash

deriving instance FromJSON Context
deriving instance ToJSON Context

-- | Plutus.V1.Ledger.Time

deriving instance Hashable DiffMilliSeconds
deriving instance FromJSON DiffMilliSeconds
deriving instance FromJSONKey DiffMilliSeconds
deriving instance Serialise DiffMilliSeconds
deriving instance ToJSON DiffMilliSeconds
deriving instance ToJSONKey DiffMilliSeconds

deriving instance FromJSONKey POSIXTime
deriving instance Serialise POSIXTime
deriving instance ToJSONKey POSIXTime

-- | Custom `FromJSON` instance which allows to parse a JSON number to a
-- 'POSIXTime' value. The parsed JSON value MUST be an 'Integer' or else the
-- parsing fails.
instance FromJSON POSIXTime where
  parseJSON v@(Number n) =
      either (\_ -> prependFailure "parsing POSIXTime failed, " (typeMismatch "Integer" v))
             (Haskell.return . POSIXTime)
             (floatingOrInteger n :: Either Haskell.Double Integer)
  parseJSON invalid =
      prependFailure "parsing POSIXTime failed, " (typeMismatch "Number" invalid)

-- | Custom 'ToJSON' instance which allows to simply convert a 'POSIXTime'
-- value to a JSON number.
instance ToJSON POSIXTime where
  toJSON (POSIXTime n) = Number $ scientific n 0

-- | Plutus.V1.Ledger.Tx

deriving instance FromJSON ScriptTag
deriving instance Serialise ScriptTag
deriving instance ToJSON ScriptTag

deriving instance FromJSON RedeemerPtr
deriving instance FromJSONKey RedeemerPtr
deriving instance Serialise RedeemerPtr
deriving instance ToJSON RedeemerPtr
deriving instance ToJSONKey RedeemerPtr

deriving instance FromJSON TxOutRef
deriving instance FromJSONKey TxOutRef
deriving instance Serialise TxOutRef
deriving instance ToJSON TxOutRef
deriving instance ToJSONKey TxOutRef

deriving instance FromJSON TxInType
deriving instance Serialise TxInType
deriving instance ToJSON TxInType

deriving instance FromJSON TxIn
deriving instance Serialise TxIn
deriving instance ToJSON TxIn

deriving instance FromJSON TxOut
deriving instance Serialise TxOut
deriving instance ToJSON TxOut

-- | Plutus.V1.Ledger.Value

deriving instance FromJSONKey CurrencySymbol
deriving instance Hashable CurrencySymbol
deriving instance Serialise CurrencySymbol
deriving instance ToJSONKey CurrencySymbol

instance ToJSON CurrencySymbol where
  toJSON c =
    JSON.object
      [ ( "unCurrencySymbol"
        , JSON.String .
          JSON.encodeByteString .
          PlutusTx.fromBuiltin .
          unCurrencySymbol $
          c)
      ]

instance FromJSON CurrencySymbol where
  parseJSON =
    JSON.withObject "CurrencySymbol" $ \object -> do
      raw <- object .: "unCurrencySymbol"
      bytes <- JSON.decodeByteString raw
      Haskell.pure $ CurrencySymbol $ PlutusTx.toBuiltin bytes

deriving instance Hashable TokenName
deriving instance Serialise TokenName

{- note [Roundtripping token names]

How to properly roundtrip a token name that is not valid UTF-8 through PureScript
without a big rewrite of the API?
We prefix it with a zero byte so we can recognize it when we get a bytestring value back,
and we serialize it base16 encoded, with 0x in front so it will look as a hex string.
(Browsers don't render the zero byte.)
-}

instance ToJSON TokenName where
    toJSON = JSON.object . Haskell.pure . (,) "unTokenName" . JSON.toJSON .
        fromTokenName
            (\bs -> Text.cons '\NUL' (asBase16 bs))
            (\t -> case Text.take 1 t of "\NUL" -> Text.concat ["\NUL\NUL", t]; _ -> t)

instance FromJSON TokenName where
    parseJSON =
        JSON.withObject "TokenName" $ \object -> do
        raw <- object .: "unTokenName"
        fromJSONText raw
        where
            fromJSONText t = case Text.take 3 t of
                "\NUL0x"       -> either Haskell.fail (Haskell.pure . tokenName) . JSON.tryDecode . Text.drop 3 $ t
                "\NUL\NUL\NUL" -> Haskell.pure . fromText . Text.drop 2 $ t
                _              -> Haskell.pure . fromText $ t

deriving instance FromJSON AssetClass
deriving instance Hashable AssetClass
deriving instance Serialise AssetClass
deriving instance ToJSON AssetClass

deriving instance FromJSON Value
deriving instance Hashable Value
deriving instance Serialise Value
deriving instance ToJSON Value

-- Orphan instances for 'Map' to make this work
instance (ToJSON v, ToJSON k) => ToJSON (Map.Map k v) where
    toJSON = JSON.toJSON . Map.toList

instance (FromJSON v, FromJSON k) => FromJSON (Map.Map k v) where
    parseJSON v = Map.fromList Haskell.<$> JSON.parseJSON v

deriving anyclass instance (Hashable k, Hashable v) => Hashable (Map.Map k v)
deriving anyclass instance (Serialise k, Serialise v) => Serialise (Map.Map k v)

-- | HttpApiData

instance ToHttpApiData PrivateKey where
    toUrlPiece = toUrlPiece . getPrivateKey

instance FromHttpApiData PrivateKey where
    parseUrlPiece a = PrivateKey <$> parseUrlPiece a

instance ToHttpApiData LedgerBytes where
    toUrlPiece = JSON.encodeByteString . bytes
instance FromHttpApiData LedgerBytes where
    parseUrlPiece = bimap Text.pack fromBytes . JSON.tryDecode

-- | ByteArrayAccess instance for signing support
instance BA.ByteArrayAccess TxId where
  length (TxId bis) = BA.length bis
  withByteArray (TxId bis) = BA.withByteArray bis

-- | OpenApi instances for swagger support

instance OpenApi.ToSchema C.ScriptHash where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "ScriptHash") mempty
instance OpenApi.ToSchema (C.AddressInEra C.AlonzoEra) where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "AddressInAlonzoEra") mempty
deriving instance Generic C.ScriptData
instance OpenApi.ToSchema C.ScriptData where
    declareNamedSchema _ =
        pure $ OpenApi.NamedSchema (Just "ScriptData") OpenApi.byteSchema
instance OpenApi.ToSchema (C.Hash C.ScriptData) where
    declareNamedSchema _ =
        pure $ OpenApi.NamedSchema (Just "HashScriptData") OpenApi.byteSchema
deriving instance Generic C.TxId
deriving anyclass instance OpenApi.ToSchema C.TxId
deriving instance Generic C.TxIx
deriving anyclass instance OpenApi.ToSchema C.TxIx
deriving instance Generic C.Lovelace
deriving anyclass instance OpenApi.ToSchema C.Lovelace
deriving instance Generic C.PolicyId
deriving anyclass instance OpenApi.ToSchema C.PolicyId
instance OpenApi.ToSchema C.AssetName where
    declareNamedSchema _ =
        pure $ OpenApi.NamedSchema (Just "AssetName") OpenApi.byteSchema
deriving instance Generic C.Quantity
deriving anyclass instance OpenApi.ToSchema C.Quantity

deriving anyclass instance (OpenApi.ToSchema k, OpenApi.ToSchema v) => OpenApi.ToSchema (AssocMap.Map k v)
instance OpenApi.ToSchema BuiltinByteString where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "Bytes") mempty
instance OpenApi.ToSchema Crypto.XPub where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "PubKey") mempty
instance OpenApi.ToSchema Crypto.XPrv where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "PrvKey") mempty
instance OpenApi.ToSchema (Crypto.Digest Crypto.Blake2b_160) where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "Digest") mempty
instance OpenApi.ToSchema (Hash.Hash Hash.Blake2b_256 Hashes.EraIndependentTxBody) where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "Hash") mempty
instance OpenApi.ToSchema (C.SafeHash C.StandardCrypto Hashes.EraIndependentData) where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "Hash") mempty
deriving instance OpenApi.ToSchema (LogMessage JSON.Value)
deriving instance OpenApi.ToSchema LogLevel
instance OpenApi.ToSchema JSON.Value where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "JSON") mempty
instance OpenApi.ToSchema Data where
  declareNamedSchema _ = do
    integerSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Integer)
    constrArgsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy (Integer, [Data]))
    mapArgsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [(Data, Data)])
    listArgsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [Data])
    bytestringSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy String)
    return $ OpenApi.NamedSchema (Just "Data") $ mempty
      & OpenApi.type_ ?~ OpenApi.OpenApiObject
      & OpenApi.properties .~
          fromList
          [ ("Constr", constrArgsSchema)
          , ("Map", mapArgsSchema)
          , ("List", listArgsSchema)
          , ("I", integerSchema)
          , ("B", bytestringSchema)
          ]
deriving instance OpenApi.ToSchema ann => OpenApi.ToSchema (Kind ann)
deriving newtype instance OpenApi.ToSchema Ada
deriving instance OpenApi.ToSchema Tx
deriving instance OpenApi.ToSchema ScriptTag
deriving instance OpenApi.ToSchema RedeemerPtr
deriving instance OpenApi.ToSchema TxOutRef
deriving instance OpenApi.ToSchema TxInType
deriving instance OpenApi.ToSchema TxIn
deriving instance OpenApi.ToSchema TxOut
deriving newtype instance OpenApi.ToSchema Validator
deriving newtype instance OpenApi.ToSchema TxId
deriving newtype instance OpenApi.ToSchema Slot
deriving instance OpenApi.ToSchema a => OpenApi.ToSchema (Interval a)
deriving instance OpenApi.ToSchema a => OpenApi.ToSchema (LowerBound a)
deriving instance OpenApi.ToSchema a => OpenApi.ToSchema (UpperBound a)
deriving newtype instance OpenApi.ToSchema Redeemer
deriving newtype instance OpenApi.ToSchema RedeemerHash
deriving newtype instance OpenApi.ToSchema Datum
deriving newtype instance OpenApi.ToSchema Value
deriving instance OpenApi.ToSchema Address
deriving newtype instance OpenApi.ToSchema MintingPolicy
deriving newtype instance OpenApi.ToSchema MintingPolicyHash
deriving newtype instance OpenApi.ToSchema DatumHash
deriving newtype instance OpenApi.ToSchema CurrencySymbol
deriving instance OpenApi.ToSchema Credential
deriving newtype instance OpenApi.ToSchema PubKey
deriving newtype instance OpenApi.ToSchema TokenName
deriving instance OpenApi.ToSchema StakingCredential
deriving newtype instance OpenApi.ToSchema StakeValidator
deriving newtype instance OpenApi.ToSchema StakeValidatorHash
deriving newtype instance OpenApi.ToSchema PubKeyHash
deriving newtype instance OpenApi.ToSchema LedgerBytes
deriving newtype instance OpenApi.ToSchema ValidatorHash
deriving newtype instance OpenApi.ToSchema Signature
deriving newtype instance OpenApi.ToSchema POSIXTime
deriving newtype instance OpenApi.ToSchema BuiltinData
deriving newtype instance OpenApi.ToSchema AssetClass
deriving instance OpenApi.ToSchema a => OpenApi.ToSchema (Extended a)
deriving instance
    ( OpenApi.ToSchema tyname
    , OpenApi.ToSchema name
    , OpenApi.ToSchema (uni ann)
    , OpenApi.ToSchema fun
    , OpenApi.ToSchema ann
    , OpenApi.ToSchema (Type tyname uni ann)
    , OpenApi.ToSchema (Some (ValueOf uni))
    , Typeable uni
    ) => OpenApi.ToSchema (Term tyname name uni fun ann)
deriving instance OpenApi.ToSchema ann => OpenApi.ToSchema (Version ann)
instance OpenApi.ToSchema Script where
    declareNamedSchema _ =
        pure $ OpenApi.NamedSchema (Just "Script") (OpenApi.toSchema (Proxy :: Proxy String))
deriving newtype instance OpenApi.ToSchema ScriptHash
