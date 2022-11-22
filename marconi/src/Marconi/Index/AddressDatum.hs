{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marconi.Index.AddressDatum
  ( -- * AddressDatumIndex
    AddressDatumIndex
  , AddressDatumHandle
  , StorableEvent(..)
  , StorableQuery(..)
  , StorableResult(..)
  , toAddressDatumIndexEvent
  , Query
  , Result
  , Depth(..)
  , open
  ) where

import Cardano.Ledger.Alonzo.TxWitness qualified as Ledger
import Codec.Serialise (Serialise (encode), deserialiseOrFail, serialise)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (Foldable (foldl'), toList)
import Data.Maybe (catMaybes, mapMaybe)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary (fromCBOR, toCBOR)
import Codec.Serialise.Class (Serialise (decode))
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Marconi.Types (CardanoAddress)
import RewindableIndex.Storable (Buffered (persistToStorage), HasPoint (getPoint),
                                 QueryInterval (QEverything, QInterval), Queryable (queryStorage), Resumable,
                                 Rewindable (rewindStorage), StorableEvent, StorableMonad, StorablePoint, StorableQuery,
                                 StorableResult, emptyState, filterWithQueryInterval)
import RewindableIndex.Storable qualified as Storable

{-|
The first thing that we need to define for a new indexer is the `handler` data type, meant as a
wrapper for the connection type (in this case the SQLite connection).
However this is a very good place to add some more configurations that the indexer may require (for
example performance tuning settings).
In our case we add the number of events that we want to return from the on-disk buffer.
-}
data AddressDatumHandle = AddressDatumHandle
    { addressDatumHandleConnection :: SQL.Connection
    , _addressDatumHandleDiskStore :: Int -- ^ TODO: Use when resuming is supported
    }

type instance StorableMonad AddressDatumHandle = IO

{-|
'AddressDatumEvent' is the type of events. Events are the data atoms that the indexer
consumes. They depend on the `handle` because they need to eventually be persisted in the database,
so the database has to be able to accomodate them.
-}
data instance StorableEvent AddressDatumHandle =
    AddressDatumIndexEvent
        (Map C.AddressAny (Set (C.Hash C.ScriptData)))
        (Map (C.Hash C.ScriptData) C.ScriptData)
        !C.SlotNo
    deriving (Eq, Show)

{- The resume and query functionality requires a way to specify points on the chain
   from which we want to resume, or points up to which we want to query. Next we
   define the types of these points. -}

type instance StorablePoint AddressDatumHandle = C.SlotNo

-- We also need to know at which slot number an event was produced.
instance HasPoint (StorableEvent AddressDatumHandle) C.SlotNo where
  getPoint (AddressDatumIndexEvent _ _ s) = s

{- Next we begin to defined the types required for running queries. Both request and
   response types will depend naturally on the structure of the database, which is
   identified by our `handle`. First, lets define the type for queries (or requests). -}
data instance StorableQuery AddressDatumHandle =
    AllAddressesQuery
    | AddressDatumQuery C.AddressAny

-- Now, we need one more type for the query results.
data instance StorableResult AddressDatumHandle =
    AllAddressesResult (Set C.AddressAny)
  | AddressDatumResult (Set C.ScriptData)

type Query        = StorableQuery AddressDatumHandle
type Result       = StorableResult AddressDatumHandle

type AddressDatumIndex = Storable.State AddressDatumHandle

newtype Depth = Depth Int

-- * SQLite

data AddressDatumHashRow = AddressDatumHashRow
  { addressDatumRowAddress   :: !C.AddressAny
  , addressDatumRowDatumHash :: !(C.Hash C.ScriptData)
  , addressDatumRowSlot      :: !C.SlotNo
  } deriving (Generic)

instance SQL.ToRow AddressDatumHashRow where
  toRow (AddressDatumHashRow addr d slotNo) = [SQL.toField addr, SQL.toField d, SQL.toField slotNo]

deriving anyclass instance SQL.FromRow AddressDatumHashRow

data DatumRow = DatumRow
    { datumRowDatumHash :: C.Hash C.ScriptData
    , datumRowDatum     :: C.ScriptData
    } deriving (Generic)

instance SQL.ToRow DatumRow where
  toRow (DatumRow dh d) = [SQL.toField dh, SQL.toField d]

deriving anyclass instance SQL.FromRow DatumRow

toAddressDatumIndexEvent :: Maybe (CardanoAddress -> Bool)  -> [C.Tx era] -> C.SlotNo -> StorableEvent AddressDatumHandle
toAddressDatumIndexEvent addressFilter txs slotNo = do
    let datumsPerAddr = getDatumsPerAddressFromTxs
        filterFun =
            case addressFilter of
              Nothing -> id
              Just f -> Map.filterWithKey $ \k _ ->
                  case k of
                      -- Target addresses filter are only shelley addresses. Therefore, is we
                      -- encounter Byron addresses with datum, we don't filter them. However, that
                      -- is highly improbable as Byron addresses are almost never used anymore.
                      C.AddressByron _      -> True
                      C.AddressShelley addr -> f addr
        filteredDatumsPerAddr = filterFun datumsPerAddr
        datumMap = Map.fromList
                 $ mapMaybe (\(dh, d) -> fmap (dh,) d)
                 $ concatMap (\datums -> Map.toList datums)
                 $ Map.elems filteredDatumsPerAddr
     in AddressDatumIndexEvent
            (fmap Map.keysSet filteredDatumsPerAddr)
            (Map.union datumMap getPlutusWitDatumsFromTxs)
            slotNo
 where
    getDatumsPerAddressFromTxs :: Map C.AddressAny (Map (C.Hash C.ScriptData) (Maybe C.ScriptData))
    getDatumsPerAddressFromTxs =
         Map.filter (not . Map.null)
            $ Map.fromListWith (Map.unionWith (<|>))
            $ concatMap getDatumsPerAddressFromTx txs

    getDatumsPerAddressFromTx :: C.Tx era -> [(C.AddressAny, Map (C.Hash C.ScriptData) (Maybe C.ScriptData))]
    getDatumsPerAddressFromTx (C.Tx (C.TxBody C.TxBodyContent { C.txOuts }) _) =
         fmap
            (\(C.TxOut (C.AddressInEra _ addr) _ dat _) ->
                ( C.toAddressAny addr
                , maybe Map.empty (uncurry Map.singleton) $ getScriptDataFromTxOutDatum dat
                ))
            txOuts

    getScriptDataFromTxOutDatum :: C.TxOutDatum C.CtxTx era -> Maybe (C.Hash C.ScriptData, Maybe C.ScriptData)
    getScriptDataFromTxOutDatum (C.TxOutDatumHash _ dh)  = Just (dh, Nothing)
    getScriptDataFromTxOutDatum (C.TxOutDatumInTx _ d)   = Just (C.hashScriptData d, Just d)
    getScriptDataFromTxOutDatum (C.TxOutDatumInline _ d) = Just (C.hashScriptData d, Just d)
    getScriptDataFromTxOutDatum _                        = Nothing

    getPlutusWitDatumsFromTxs :: Map (C.Hash C.ScriptData) C.ScriptData
    getPlutusWitDatumsFromTxs =
        foldr (\acc x -> Map.union acc x) Map.empty
        $ fmap (\(C.Tx txBody _) -> getPlutusWitDatumsFromTxBody txBody) txs

    getPlutusWitDatumsFromTxBody :: C.TxBody era -> Map (C.Hash C.ScriptData) C.ScriptData
    getPlutusWitDatumsFromTxBody (C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ (Ledger.TxDats' datum) _) _ _) =
        -- TODO I'm recomputing the ScriptHash hash, because cardano-api doesn't provide the correct
        -- functions to convert 'Ledger.DataHash' to 'C.Hash C.ScriptData'.
        Map.fromList
            $ fmap (\(_, alonzoDat) -> let d = C.fromAlonzoData alonzoDat in (C.hashScriptData d, d))
            $ Map.toList datum
    getPlutusWitDatumsFromTxBody (C.TxBody _) = Map.empty

instance SQL.FromField C.AddressAny where
  fromField f = SQL.fromField f >>= maybe (SQL.returnError SQL.ConversionFailed f "Cannot deserialise datumhash.")
           pure
    . C.deserialiseFromRawBytes C.AsAddressAny

instance SQL.ToField C.AddressAny where
  toField = SQL.SQLBlob . C.serialiseToRawBytes

instance Serialise C.ScriptData where
  encode = toCBOR
  decode = fromCBOR

instance SQL.FromField (C.Hash C.ScriptData) where
  fromField f = SQL.fromField f >>=
    maybe (SQL.returnError SQL.ConversionFailed f "Cannot deserialise datumhash.") pure
    . C.deserialiseFromRawBytes (C.AsHash C.AsScriptData)

instance SQL.ToField (C.Hash C.ScriptData) where
  toField = SQL.SQLBlob . C.serialiseToRawBytes

instance SQL.FromField C.ScriptData where
  fromField f = SQL.fromField f >>=
    either (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise datumhash.") pure
    . deserialiseOrFail

instance SQL.ToField C.ScriptData where
  toField = SQL.SQLBlob . toStrict . serialise

instance SQL.FromField C.SlotNo where
  fromField f = C.SlotNo <$> SQL.fromField f

instance SQL.ToField C.SlotNo where
  toField (C.SlotNo s) = SQL.SQLInteger $ fromIntegral s

instance Buffered AddressDatumHandle where
  {- The data is buffered in memory. When the memory buffer is filled, we need to store
     it on disk. -}
  persistToStorage
    :: Foldable f
    => f (StorableEvent AddressDatumHandle)
    -> AddressDatumHandle
    -> IO AddressDatumHandle
  persistToStorage es h = do
    let addressDatumHashRows = foldl' (\ea e -> ea ++ toAddressDatumHashRow e) [] es
        datumRows = foldl' (\ea e -> ea ++ toDatumRow e) [] es
        c    = addressDatumHandleConnection h
    SQL.execute_ c "BEGIN"
    forM_ addressDatumHashRows $
      SQL.execute c "INSERT INTO address_datums (address, datum_hash, slot_no) VALUES (?, ?, ?)"
    forM_ datumRows $
      -- We ignore inserts that introduce duplicate datum hashes in the table
      SQL.execute c "INSERT OR IGNORE INTO datumhash_datum (datum_hash, datum) VALUES (?, ?)"
    SQL.execute_ c "COMMIT"
    pure h
    where
      toAddressDatumHashRow :: StorableEvent AddressDatumHandle -> [AddressDatumHashRow]
      toAddressDatumHashRow (AddressDatumIndexEvent addressDatumHashMap _ sl) = do
          (addr, dhs) <- Map.toList addressDatumHashMap
          dh <- Set.toList dhs
          pure $ AddressDatumHashRow addr dh sl
      toDatumRow :: StorableEvent AddressDatumHandle -> [DatumRow]
      toDatumRow (AddressDatumIndexEvent _ datumMap _) =
          fmap (uncurry DatumRow) $ Map.toList datumMap

  getStoredEvents
    :: AddressDatumHandle
    -> IO [StorableEvent AddressDatumHandle]
  getStoredEvents (AddressDatumHandle c n) = do
      sns :: [[Integer]] <- SQL.query c "SELECT slot_no FROM address_datums GROUP BY slot_no ORDER BY slot_no DESC LIMIT ?" (SQL.Only n)
      -- Take the slot number of the sz'th slot
      let sn = if null sns
                  then 0
                  else head . last $ take n sns
      r <- SQL.query c
          "SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no FROM address_datums LEFT JOIN datumhash_datum ON datumhash_datum.datum_hash = address_datums.datum_hash WHERE slot_no >= ? ORDER BY slot_no DESC, address, datumhash_datum.datum_hash" (SQL.Only (sn :: Integer))
      pure $ asEvents r

-- This function recomposes the in-memory format from the database records.
asEvents
  :: [(C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo)]
  -- ^ Should be sorted by C.SlotNo in ascending order.
  -> [StorableEvent AddressDatumHandle]
asEvents events =
    fmap toEvent $ NonEmpty.groupWith (\(_, _, _, s) -> s) events
 where
     toEvent :: NonEmpty (C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo) -> StorableEvent AddressDatumHandle
     toEvent es =
         let (_, _, _, slot) = NonEmpty.head es
          in AddressDatumIndexEvent (toAddressDatums es) (toDatumMap es) slot

     toAddressDatums
         :: NonEmpty (C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo)
         -> Map C.AddressAny (Set (C.Hash C.ScriptData))
     toAddressDatums es =
         Map.fromListWith (<>)
            $ NonEmpty.toList
            $ fmap (\(addr, dh, _, _) -> (addr, Set.singleton dh)) es

     toDatumMap
         :: NonEmpty (C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo)
         -> Map (C.Hash C.ScriptData) C.ScriptData
     toDatumMap es =
         Map.fromList
            $ catMaybes
            $ NonEmpty.toList
            $ fmap (\(_, dh, d, _) -> fmap (dh,) d) es

instance Queryable AddressDatumHandle where
  queryStorage
    :: Foldable f
    => QueryInterval C.SlotNo
    -> f (StorableEvent AddressDatumHandle)
    -> AddressDatumHandle
    -> StorableQuery AddressDatumHandle
    -> IO (StorableResult AddressDatumHandle)
  queryStorage qi es (AddressDatumHandle c _) AllAddressesQuery = do
    persistedData :: [(C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo)] <-
      case qi of
        QEverything -> SQL.query c
          "SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no FROM address_datums LEFT JOIN datumhash_datum ON datumhash_datum.datum_hash = address_datums.datum_hash ORDER BY slot_no ASC, address, datumhash_datum.datum_hash" ()
        QInterval b e -> SQL.query c
          "SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no FROM address_datums LEFT JOIN datumhash_datum ON datumhash_datum.datum_hash = address_datums.datum_hash WHERE slot_no >= ? AND slot_no < ? ORDER BY slot_no ASC, address, datumhash_datum.datum_hash" (b, e)
    pure $
        AllAddressesResult $ Set.fromList
            (  fmap (\(addr, _, _, _) -> addr) persistedData
            ++ concatMap (\(AddressDatumIndexEvent addrMap _ _) -> Map.keys addrMap) (toList es)
            )

  queryStorage qi es (AddressDatumHandle c _) (AddressDatumQuery q) = do
    persistedData :: [(C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo)] <-
      case qi of
        QEverything -> SQL.query c
          "SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no FROM address_datums, datumhash_datum WHERE datumhash_datum.datum_hash = address_datums.datum_hash AND address = ? ORDER BY slot_no ASC, address, datumhash_datum.datum_hash" (SQL.Only q)
        QInterval b e -> SQL.query c
          "SELECT address, datum_hash, slot_no FROM address_datums ORDER BY slot_no ASC, address, datum_hash WHERE slot_no >= ? AND slot_no <= ? AND address = ?" (b, e, q)
    -- Note that ordering is quite important here, as the `filterWithQueryInterval`
    -- function assumes events are ordered from oldest (the head) to most recent.
    let addressDatumIndexEvents = filterWithQueryInterval qi (asEvents persistedData ++ toList es)
    pure . AddressDatumResult $ filterByAddress q addressDatumIndexEvents

    where
      filterByAddress
          :: C.AddressAny
          -> [StorableEvent AddressDatumHandle]
          -> Set C.ScriptData
      filterByAddress addr addressDatumIndexEvents = do
          foldMap (indexEventToResult addr) addressDatumIndexEvents

      indexEventToResult
          :: C.AddressAny
          -> StorableEvent AddressDatumHandle
          -> Set C.ScriptData
      indexEventToResult addr (AddressDatumIndexEvent addressDatumMap datumMap _slotNo) =
            foldMap (\(_, datumHashes) -> resolveDatumHashes datumHashes datumMap)
                $ filter ((==) addr . fst)
                $ Map.toList addressDatumMap

      resolveDatumHashes
          :: Set (C.Hash C.ScriptData)
          -> Map (C.Hash C.ScriptData) C.ScriptData
          -> Set C.ScriptData
      resolveDatumHashes datumHashes datumMap =
          -- TODO Not efficient to convert back n forth between Set
          Set.fromList $ mapMaybe (\h -> Map.lookup h datumMap) $ Set.toList datumHashes

instance Rewindable AddressDatumHandle where
  rewindStorage
    :: C.SlotNo
    -> AddressDatumHandle
    -> IO (Maybe AddressDatumHandle )
  rewindStorage sn h@(AddressDatumHandle c _) = do
     SQL.execute c "DELETE FROM address_datums WHERE slot_no > ?" (SQL.Only sn)
     pure $ Just h

instance Resumable AddressDatumHandle where
    resumeFromStorage
        :: AddressDatumHandle
        -> IO [C.SlotNo]
    resumeFromStorage h = do
        es <- Storable.getStoredEvents h
        pure $ fmap (\(AddressDatumIndexEvent _ _ slotNo) -> slotNo) es

open
  :: FilePath
  -> Depth
  -> IO AddressDatumIndex
open dbPath (Depth k) = do
  c <- SQL.open dbPath
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS address_datums (address TEXT NOT NULL, datum_hash BLOB NOT NULL, slot_no INT NOT NULL)"
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS datumhash_datum (datum_hash BLOB PRIMARY KEY, datum BLOB)"
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS address_datums_index ON address_datums (address)"
  emptyState k (AddressDatumHandle c k)
