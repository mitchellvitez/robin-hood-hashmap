{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Control.Monad.ST (runST)
import Data.List (sort)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude hiding (lookup)

import RobinHoodHashMap

main :: IO ()
main = void tests

tests :: IO Bool
tests =
  checkParallel $$(discover)

genStr :: Gen String
genStr = Gen.list (Range.linear 0 100) Gen.alpha

genKV :: PropertyT IO String
genKV = forAll genStr

notSameAs :: (Monad m, Foldable t) => t String -> PropertyT m String
notSameAs kvs = forAll $ Gen.filter (`notElem` kvs) genStr

prop_doubleInsertSize :: Property
prop_doubleInsertSize = property $ do
  key <- genKV
  value1 <- genKV
  value2 <- genKV
  result <- pure $ runST $ do
    h <- empty
    insert key value1 h
    insert key value2 h
    size h
  result === 1

prop_doubleInsert :: Property
prop_doubleInsert = property $ do
  key <- genKV
  value1 <- genKV
  value2 <- genKV
  result <- pure $ runST $ do
    h <- empty
    insert key value1 h
    insert key value2 h
    lookup key h
  result === Just value2

prop_doubleInsertDifferentKeys :: Property
prop_doubleInsertDifferentKeys = property $ do
  key1 <- genKV
  key2 <- notSameAs [key1]
  value1 <- genKV
  value2 <- genKV
  result <- pure $ runST $ do
    h <- empty
    insert key1 value1 h
    insert key2 value2 h
    size h
  result === 2

prop_lookupMissingKey :: Property
prop_lookupMissingKey = property $ do
  key1 <- genKV
  key2 <- notSameAs [key1]
  value1 <- genKV
  result <- pure $ runST $ do
    h <- empty
    insert key1 value1 h
    lookup key2 h
  result === Nothing

prop_InsertAndDelete :: Property
prop_InsertAndDelete = property $ do
  key1 <- genKV
  key2 <- notSameAs [key1]
  key3 <- notSameAs [key1, key2]
  value1 <- genKV
  value2 <- genKV
  value3 <- genKV
  result <- pure $ runST $ do
    h <- empty
    insert key1 value1 h
    insert key2 value2 h
    insert key3 value3 h
    delete key2 h
    size h
  result === 2

prop_insertAndDeleteOverlap :: Property
prop_insertAndDeleteOverlap = property $ do
  key1 <- genKV
  key2 <- notSameAs [key1]
  key3 <- notSameAs [key1, key2]
  value1 <- genKV
  value2 <- genKV
  value3 <- genKV
  result1 <- pure $ runST $ do
    h <- empty
    insert key1 value1 h
    insert key2 value2 h
    delete key1 h
    insert key3 value3 h
    size h
  result2 <- pure $ runST $ do
    h <- empty
    insert key1 value1 h
    insert key2 value2 h
    insert key3 value3 h
    delete key1 h
    toList h
  result1 === length result2

prop_lookup :: Property
prop_lookup = property $ do
  key1 <- genKV
  key2 <- notSameAs [key1]
  key3 <- notSameAs [key1, key2]
  key4 <- notSameAs [key1, key2, key3]
  value1 <- genKV
  value2 <- genKV
  value3 <- genKV
  result1 <- pure $ runST $ do
    h <- empty
    insert key1 value1 h
    insert key2 value2 h
    delete key1 h
    insert key3 value3 h
    lookup key2 h
  result2 <- pure $ runST $ do
    h <- empty
    insert key1 value1 h
    insert key2 value2 h
    insert key3 value3 h
    insert key4 value3 h
    delete key3 h
    delete key1 h
    lookup key2 h
  result1 === result2

prop_fromList :: Property
prop_fromList = property $ do
  key1 <- genKV
  key2 <- genKV
  key3 <- genKV
  value1 <- genKV
  value2 <- genKV
  value3 <- genKV
  result <- pure $ runST $ do
    h <- empty
    insert key1 value1 h
    insert key2 value2 h
    insert key3 value3 h
    toList h
  let list = [(key1, value1), (key2, value2), (key3, value3)]
  result2 <- pure $ runST $ do
    x <- fromList list
    toList x
  sort result === sort result2

prop_grow :: Property
prop_grow = property $ do
  key1 <- genKV
  key2 <- notSameAs [key1]
  key3 <- notSameAs [key1, key2]
  key4 <- notSameAs [key1, key2, key3]
  key5 <- notSameAs [key1, key2, key3, key4]
  value1 <- genKV
  value2 <- genKV
  value3 <- genKV
  value4 <- genKV
  value5 <- genKV
  result <- pure $ runST $ do
    h <- tinyEmpty
    insert key1 value1 h
    insert key2 value2 h
    insert key3 value3 h
    insert key4 value4 h
    insert key5 value5 h
    toList h
  sort result === sort [(key1, value1), (key2, value2), (key3, value3), (key4, value4), (key5, value5)]

prop_simpleGrow :: Property
prop_simpleGrow = property $ do
  result <- pure $ runST $ do
    h <- tinyEmpty
    insert 'b' 2 h
    insert 'g' 7 h
    insert 'p' 16 h
    insert 'q' 17 h
    insert 'c' 3 h
    insert 'h' 8 h
    insert 'e' 5 h
    insert 'f' 6 h
    insert 'm' 13 h
    insert 'n' 14 h
    insert 'o' 15 h
    insert 'a' 1 h
    insert 'i' 9 h
    insert 'j' 10 h
    insert 'k' 11 h
    insert 'l' 12 h
    insert 'd' 4 h
    toList h
  sort result === zip ['a'..'q'] [1..17]
