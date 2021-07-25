{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RobinHoodHashMap where

import Prelude hiding (lookup, foldl, null, elem)

import Control.Monad hiding (foldM)
import Control.Monad.Extra (whenJust)
import Control.Monad qualified as CM (foldM)
import Control.Monad.ST
import Data.Bits
import Data.Hashable hiding (hashed)
import Data.Maybe
import Data.Primitive.Array
import Data.STRef
import Data.Word

-- a Hash is an unsigned number, and mostly the result of hashing some key, with a few special exceptions:
-- if a Hash is 0, that means it's not used by an elem of the hashmap (we initialize all hashes to 0)
-- the most significant bit is a deletion flag, marking a tombstone for some elem
type Hash = Word32

-- if a Hash is a Word32 as in [bit31..bit0], its most significant bit is bit 31
mostSignificantBit :: Int
mostSignificantBit = 31

newtype HashMap s k v = HashMap (STRef s (HashMapData s k v))

data HashMapData s k v = HashMapData
  { hashMapSize       :: {-# UNPACK #-} !Int    -- number of elements
  , hashMapLoadFactor :: {-# UNPACK #-} !Double -- maximum load factor (between 0 and 1)
  , hashMapCapacity   :: {-# UNPACK #-} !Int    -- available slots
  , hashMapHashes     :: {-# UNPACK #-} !(MutableArray s Hash)
  , hashMapElems      :: {-# UNPACK #-} !(MutableArray s (Elem k v))
  }

-- could remove this indirection for performance
data Elem k v = Elem
  { elemKey   :: !k
  , elemValue :: !v
  }

-- Basic public interface functions

empty :: ST s (HashMap s k v)
empty = do
  hashArr <- newArray initialCapacity zeroBits
  elemArr <- newArray initialCapacity undefined
  liftM HashMap . newSTRef $ HashMapData 0 loadFactor initialCapacity hashArr elemArr
  where
    initialCapacity = 256
    loadFactor = 0.9

size :: HashMap s k v -> ST s Int
size (HashMap hashMapDataRef) = do
  hashMapSize <$> readSTRef hashMapDataRef

lookup :: (Eq k, Hashable k) => k -> HashMap s k v -> ST s (Maybe v)
lookup key hm@(HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  mbIndex <- lookupIndex key hm
  case mbIndex of
    Nothing -> pure Nothing
    Just index -> Just . elemValue <$> readArray hashMapElems (fromIntegral index)

insert :: (Eq k, Hashable k) => k -> v -> HashMap s k v -> ST s ()
insert !key !value hm@(HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  when (fromIntegral hashMapSize >= fromIntegral hashMapCapacity * hashMapLoadFactor) $ do
    grow hm
  HashMapData{..} <- readSTRef hashMapDataRef
  !didAddNewKey <- insertWithoutGrowing key value (HashMap hashMapDataRef)
  when (didAddNewKey == AddedNewKey) $
    writeSTRef hashMapDataRef HashMapData{..} { hashMapSize = hashMapSize + 1 }

data DidAddNewKey = AddedNewKey | DidNotAddNewKey
  deriving Eq

insertWithoutGrowing :: (Eq k, Hashable k) => k -> v -> HashMap s k v -> ST s DidAddNewKey
insertWithoutGrowing key value hm@(HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  let !hashed = hashKey key
  !position <- desiredPosition hashed hm
  let
    loopStep dist pos elem = do
      !elemHash <- readArray hashMapHashes $ fromIntegral pos
      newElem <- readArray hashMapElems $ fromIntegral pos
      !existingElemProbeDist <- probeDistance elemHash pos hm

      if | elemHash == 0 ->
             construct pos hashed key value hm >> pure AddedNewKey

         | elemKey newElem == elemKey elem ->
             construct pos hashed key value hm >> pure DidNotAddNewKey

         | existingElemProbeDist < dist && isDeleted elemHash ->
             construct pos hashed key value hm >> pure AddedNewKey

         | otherwise -> do
            when (existingElemProbeDist < dist) $ do
              writeArray hashMapHashes (fromIntegral pos) hashed
              writeArray hashMapElems (fromIntegral pos) elem

            let newPos = (pos + 1) .&. (fromIntegral hashMapCapacity - 1)
            loopStep (dist + 1) newPos newElem

  loopStep 0 position (Elem key value)

construct :: Hash -> Hash -> k -> v -> HashMap s k v -> ST s ()
construct pos hashed key value (HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  writeArray hashMapHashes (fromIntegral pos) hashed
  writeArray hashMapElems (fromIntegral pos) (Elem key value)

delete :: (Hashable k, Eq k) => k -> HashMap s k v -> ST s ()
delete key hm@(HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  mbIndex <- lookupIndex key hm
  whenJust mbIndex $ \index -> do
    hashed <- readArray hashMapHashes (fromIntegral index)
    let newHash = setBit hashed mostSignificantBit
    writeArray hashMapHashes (fromIntegral index) newHash
    writeSTRef hashMapDataRef HashMapData{..} { hashMapSize = hashMapSize - 1}

-- see Data.HashTable.Class.foldM
foldM :: (a -> (k, v) -> ST s a) -> a -> HashMap s k v -> ST s a
foldM func fzero (HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  let go !i !z
        | i >= hashMapCapacity = pure z
        | otherwise = do
          hashed <- readArray hashMapHashes $ fromIntegral i
          if hashed == 0 || isDeleted hashed
            then go (i + 1) z
            else do
              Elem k v <- readArray hashMapElems $ fromIntegral i
              !newZ <- func z (k, v)
              go (i + 1) newZ
  go 0 fzero

-- Derived functions
--
-- Basic interface functions give enough building blocks to write these,
-- so they're convenience functions

null :: HashMap s k v -> ST s Bool
null h = do
  (0 ==) <$> size h

member :: (Eq k, Hashable k) => k -> HashMap s k a -> ST s Bool
member k h = isJust <$> lookup k h

(!?) :: (Eq k, Hashable k) => HashMap s k v -> k -> ST s (Maybe v)
(!?) = flip lookup

findWithDefault :: (Eq k, Hashable k) => v -> k -> HashMap s k v -> ST s v
findWithDefault v k h = fromMaybe v <$> lookup k h

fromList :: (Eq k, Hashable k) => [(k, v)] -> ST s (HashMap s k v)
fromList list = do
  newHashMap <- empty
  void $ CM.foldM (\hashMap (k, v) -> insert k v hashMap >> pure newHashMap) newHashMap list
  pure newHashMap

toList :: HashMap s k v -> ST s [(k, v)]
toList = foldM (\list el -> pure $ el : list) []

-- Useful internals

hashKey :: Hashable k => k -> Hash
hashKey key =
  let !hashed = fromIntegral $ hash key :: Hash
      !withClearedFlag = clearBit hashed mostSignificantBit
      !ensureNotZero = if withClearedFlag == 0 then 1 else withClearedFlag
  in ensureNotZero

isDeleted :: Hash -> Bool
isDeleted = flip testBit mostSignificantBit

desiredPosition :: Hash -> HashMap s k v -> ST s Hash
desiredPosition hashed (HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  pure $ hashed .&. fromIntegral (hashMapCapacity - 1)

probeDistance :: Hash -> Hash -> HashMap s k v -> ST s Hash
probeDistance hashed slotIndex hm@(HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  desiredPos <- desiredPosition hashed hm
  let capacity = fromIntegral hashMapCapacity
  pure $ (slotIndex + capacity - desiredPos) .&. (capacity - 1)

grow :: (Eq k, Hashable k) => HashMap s k v -> ST s ()
grow (HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  let !oldCapacity = hashMapCapacity
      !newCapacity = hashMapCapacity * 2
      !oldElems = hashMapElems
      !oldHashes = hashMapHashes
  !hashArr <- newArray newCapacity (zeroBits :: Hash)
  !elemArr <- newArray newCapacity undefined
  writeSTRef hashMapDataRef $ HashMapData 0 0.9 newCapacity hashArr elemArr
  forM_ [0..oldCapacity - 1] $ \i -> do
    !hashed <- readArray oldHashes i
    when (hashed /= 0 && not (isDeleted hashed)) $ do
      Elem k v <- readArray oldElems i
      void $ insert k v (HashMap hashMapDataRef)

lookupIndex :: (Eq k, Hashable k) => k -> HashMap s k v -> ST s (Maybe Hash)
lookupIndex key hm@(HashMap hashMapDataRef) = do
  HashMapData{..} <- readSTRef hashMapDataRef
  let hashed = hashKey key
  position <- desiredPosition hashed hm

  let loopStep dist pos = do
        elemHash <- readArray hashMapHashes (fromIntegral pos)
        arrAtPos <- readArray hashMapElems (fromIntegral pos)
        probeDist <- probeDistance elemHash pos hm

        if | elemHash == 0 -> pure Nothing
           | dist > probeDist -> pure Nothing
           | elemHash == hashed && elemKey arrAtPos == key -> pure $ Just pos
           | otherwise -> loopStep (dist + 1) ((pos + 1) .&. (fromIntegral hashMapCapacity - 1))

  loopStep 0 position

-- only used for testing
-- i guess if you really wanted a tiny memory footprint....
tinyEmpty :: ST s (HashMap s k v)
tinyEmpty = do
  hashArr <- newArray initialCapacity zeroBits
  valueArr <- newArray initialCapacity undefined
  liftM HashMap . newSTRef $ HashMapData 0 loadFactor initialCapacity hashArr valueArr
  where
    initialCapacity = 1
    loadFactor = 0.9
