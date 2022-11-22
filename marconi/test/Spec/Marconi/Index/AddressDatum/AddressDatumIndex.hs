{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Index.AddressDatum.AddressDatumIndex
    ( tests
    ) where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Control.Monad (foldM, forM)
import Control.Monad.IO.Class (liftIO)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.Index.AddressDatum qualified as AddressDatum
import RewindableIndex.Storable qualified as Storable
import Spec.Marconi.Index.AddressDatum.Generators (genAddressInEra, genSimpleScriptData)
import Spec.Marconi.Index.AddressDatum.Utils (addressInEraToAddressAny)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit), testPropertyNamed)

tests :: TestTree
tests = localOption (HedgehogTestLimit $ Just 200) $
    testGroup "Spec.Marconi.Index.AddressDatum.AddressDatumIndex"
    [ testPropertyNamed
          "..."
          "propAllAddressesAreQueryable"
          propAllAddressesAreQueryable
    , testPropertyNamed
          "..."
          "propAllAddressesAreQueryableInGeneratedRange "
          propAllAddressesAreQueryableInGeneratedRange
    , testPropertyNamed
          "..."
          "propNoAddressQueryableOutsideOfQueryRange "
          propNoAddressQueryableOutsideOfQueryRange
    , testPropertyNamed
          "..."
          "propRewindingWithNewSlotShouldKeepIndexState "
          propRewindingWithNewSlotShouldKeepIndexState
    , testPropertyNamed
          "..."
          "propRewindingWithOldSlotShouldBringIndexInPreviousState "
          propRewindingWithOldSlotShouldBringIndexInPreviousState
    , testPropertyNamed
          "..."
          "propResumingShouldReturnAtLeastOnePoint"
          propResumingShouldReturnAtLeastOnePoint
    ]

propAllAddressesAreQueryable :: Property
propAllAddressesAreQueryable = property $ do
    n <- forAll $ Gen.word64 (Range.linear 1 5)
    events <- forAll $ forM [0..n] $ \s -> genAddressDatumStorableEvent (C.SlotNo s)
    depth <- forAll $ Gen.word64 (Range.linear 1 n)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatum.Depth $ fromIntegral depth)
    finalIndex <- liftIO $ foldM (\index e -> Storable.insert e index) initialIndex events
    let addrs = Set.fromList
              $ concatMap (\(AddressDatum.AddressDatumIndexEvent addressDatumMap _ _) ->
                  Map.keys addressDatumMap) events
    (AddressDatum.AllAddressesResult actualAddrs) <- liftIO $ do
        Storable.query Storable.QEverything finalIndex AddressDatum.AllAddressesQuery
    actualAddrs === addrs

propAllAddressesAreQueryableInGeneratedRange :: Property
propAllAddressesAreQueryableInGeneratedRange = property $ do
    n <- forAll $ Gen.word64 (Range.linear 1 5)
    events <- forAll $ forM [0..n] $ \s -> genAddressDatumStorableEvent (C.SlotNo s)
    depth <- forAll $ Gen.word64 (Range.linear 1 n)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatum.Depth $ fromIntegral depth)
    finalIndex <- liftIO $ foldM (\index e -> Storable.insert e index) initialIndex events
    let addrs = Set.fromList
              $ concatMap (\(AddressDatum.AddressDatumIndexEvent addressDatumMap _ _) ->
                  Map.keys addressDatumMap) events
    (AddressDatum.AllAddressesResult actualAddrs) <- liftIO $ do
        Storable.query (Storable.QInterval (C.SlotNo 0) (C.SlotNo n)) finalIndex AddressDatum.AllAddressesQuery
    actualAddrs === addrs

propNoAddressQueryableOutsideOfQueryRange :: Property
propNoAddressQueryableOutsideOfQueryRange = property $ do
    n <- forAll $ Gen.word64 (Range.linear 1 5)
    events <- forAll $ forM [0..n] $ \s -> genAddressDatumStorableEvent (C.SlotNo s)
    depth <- forAll $ Gen.word64 (Range.linear 1 n)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatum.Depth $ fromIntegral depth)
    finalIndex <- liftIO $ foldM (\index e -> Storable.insert e index) initialIndex events
    (AddressDatum.AllAddressesResult actualAddrs) <- liftIO $ do
        Storable.query (Storable.QInterval (C.SlotNo $ n + 1) (C.SlotNo $ n + 2)) finalIndex AddressDatum.AllAddressesQuery
    Hedgehog.assert $ Set.null actualAddrs
    (AddressDatum.AllAddressesResult actualAddrs') <- liftIO $ do
        Storable.query (Storable.QInterval (C.SlotNo 0) (C.SlotNo 0)) finalIndex AddressDatum.AllAddressesQuery
    Hedgehog.assert $ Set.null actualAddrs'

propRewindingWithNewSlotShouldKeepIndexState :: Property
propRewindingWithNewSlotShouldKeepIndexState = property $ do
    n <- forAll $ Gen.word64 (Range.linear 1 5)
    events <- forAll $ forM [0..n] $ \s -> genAddressDatumStorableEvent (C.SlotNo s)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatum.Depth 1)
    finalIndex <-
        liftIO
        $ foldM (\index e@(AddressDatum.AddressDatumIndexEvent _ _ slotNo) ->
            Storable.insert e index >>= (\i -> fmap (fromMaybe i) (Storable.rewind slotNo i))) initialIndex events
        >>= Storable.insert (AddressDatum.toAddressDatumIndexEvent Nothing [] 0)
    let addrs = Set.fromList
              $ concatMap (\(AddressDatum.AddressDatumIndexEvent addressDatumMap _ _) ->
                  Map.keys addressDatumMap) events
    (AddressDatum.AllAddressesResult actualAddrs) <- liftIO $ do
        Storable.query Storable.QEverything finalIndex AddressDatum.AllAddressesQuery
    actualAddrs === addrs

propRewindingWithOldSlotShouldBringIndexInPreviousState :: Property
propRewindingWithOldSlotShouldBringIndexInPreviousState = property $ do
    n <- forAll $ Gen.word64 (Range.linear 1 5)
    events <- forAll $ forM [0..n] $ \s -> genAddressDatumStorableEvent (C.SlotNo s)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatum.Depth 1)
    finalIndex <-
        liftIO
        $ foldM (\index e@(AddressDatum.AddressDatumIndexEvent _ _ slotNo) ->
            Storable.insert e index >>= (\i -> fmap (fromMaybe i) (Storable.rewind (slotNo - 1) i))) initialIndex events
        >>= Storable.insert (AddressDatum.toAddressDatumIndexEvent Nothing [] 0)
    (AddressDatum.AllAddressesResult actualAddrs) <- liftIO $ do
        Storable.query Storable.QEverything finalIndex AddressDatum.AllAddressesQuery
    Hedgehog.assert $ List.null actualAddrs

propResumingShouldReturnAtLeastOnePoint :: Property
propResumingShouldReturnAtLeastOnePoint = property $ do
    n <- forAll $ Gen.word64 (Range.linear 1 5)
    events <- forAll $ forM [0..n] $ \s -> genAddressDatumStorableEvent (C.SlotNo s)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatum.Depth 1)
    finalIndex <-
        liftIO
        $ foldM (\index e -> Storable.insert e index) initialIndex events
        >>= Storable.insert (AddressDatum.toAddressDatumIndexEvent Nothing [] 0)

    resumablePoints <- liftIO $ Storable.resumeFromStorage $ finalIndex ^. Storable.handle
    Hedgehog.assert $ not $ null resumablePoints

genAddressDatumStorableEvent :: C.SlotNo -> Gen (Storable.StorableEvent AddressDatum.AddressDatumHandle)
genAddressDatumStorableEvent slotNo = do
    addresses <- fmap addressInEraToAddressAny
             <$> Gen.list (Range.linear 1 5) (genAddressInEra C.BabbageEra)
    addressDatums <- forM addresses $ \address -> do
        scriptDats <- fmap (\dats -> fmap (\dat -> (C.hashScriptData dat, dat)) dats)
                    $ Gen.list (Range.linear 1 5) genSimpleScriptData
        datumMap <- Map.fromList <$> Gen.subsequence scriptDats
        pure (address, Set.fromList $ fmap fst scriptDats, datumMap)
    let addressDatumsMap = Map.fromList $ fmap (\(addr, datums, _) -> (addr, datums)) addressDatums
    let datumMap = foldMap (\(_, _, dm) -> dm) addressDatums
    pure $
        AddressDatum.AddressDatumIndexEvent
            addressDatumsMap
            datumMap
            slotNo

