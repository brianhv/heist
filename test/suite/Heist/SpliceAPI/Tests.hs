module Heist.SpliceAPI.Tests
  ( tests
  ) where

import           Control.Exception              (ErrorCall(..), try)
import           Control.Monad                  (unless)
import qualified Data.Map                       as M
import           Data.Monoid                    ((<>), mempty)
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit                     as H

------------------------------------------------------------------------------
import           Heist.SpliceAPI
--import           Heist.TestCommon

-- NOTE: We can't test compiled templates on the templates directory as it
-- stands today because that directory contains some error conditions such as
-- infinite bind loops, apply tags with no template attribute, and apply tags
-- with ".." in the tag path (which doesn't currently work).

tests :: [Test]
tests = [ testCase     "spliceapi/keyExistsError"           keyExistsErrorTest
        , testCase     "spliceapi/overwrite"                overwriteTest
        , testCase     "spliceapi/keepExisting"             keepExistingTest
        , testCase     "spliceapi/noDuplicateThrowsNoError" noDuplicateThrowsNoErrorTest
        , testCase     "spliceapi/namespace"                namespaceTest
        , testCase     "spliceapi/namespaceWithNull"        namespaceWithNullTest
        , testCase     "spliceapi/splicesToList"            splicesToListTest
        , testCase     "spliceapi/monoid"                   monoidTest
        , testCase     "spliceapi/applyS"                   applySTest
        , testCase     "spliceapi/insertS"                  insertSTest
        , testCase     "spliceapi/unionWithS"               unionWithSTest
        ]

{- | Asserts that an error is raised by a given action. -}
assertRaisesError :: (Show a) =>
                String -> String -> IO a -> IO ()
assertRaisesError description expectedMessage action = do
    r <- Control.Exception.try action
    case r of
        Left e -> handleException e
        Right _ -> H.assertFailure $ description ++
                   "\nReceived no exception, but was expecting exception: " ++ expectedMessage
    where
        handleException (ErrorCall msg) =
            unless (msg == expectedMessage) $ 
                H.assertFailure $ description ++
                    "\nReceived unexpected error message: " ++ msg ++
                    "\ninstead of exception             : " ++ expectedMessage

-- Makes sure the splices get evaluated fully enough to cause errors to get thrown
forceSpliceEval :: Splices String -> IO ()
forceSpliceEval ss = if length (show $ runSplices ss) > 0 then return () else return ()

keyExistsErrorTest :: IO ()
keyExistsErrorTest =
    assertRaisesError "Adding duplicate key raises error"
                      "Key \"asdf\" already exists in the splice map" $ forceSpliceEval splices
    where
    splices :: Splices String
    splices = do
        "asdf" #! "first"
        "asdf" #! "second"

noDuplicateThrowsNoErrorTest :: IO ()
noDuplicateThrowsNoErrorTest =
    H.assertEqual "Not adding duplicate key throws no error"
                  (Just "asdfvalue")
                  (M.lookup "asdf" (runSplices splices)) where
    splices :: Splices String
    splices = do
        "asdf" #! "asdfvalue"
        "foo" #! "foovalue"
                                        

unionThrowsErrorTest :: IO ()
unionThrowsErrorTest = assertRaisesError "Adding duplicate key with union raises error"
                       "Key \"asdf\" already exists in the splice map" $
                       forceSpliceEval $ splices1 <> splices2 where
    splices1, splices2 :: Splices String
    splices1 = "asdf" #! "first"
    splices2 = "asdf" #! "second"

overwriteTest :: IO ()
overwriteTest = H.assertEqual "Adding duplicate key with ## overwrites old value"
                              (Just "second")
                              (M.lookup "asdf" (runSplices splices)) where
    splices :: Splices String
    splices = do
        "asdf" ## "first"
        "asdf" ## "second"
    
keepExistingTest :: IO ()
keepExistingTest = H.assertEqual "Adding duplicate key with #? keeps old value"
                                 (Just "first")
                                 (M.lookup "asdf" (runSplices splices)) where
    splices :: Splices String
    splices = do
        "asdf" #? "first"
        "asdf" #? "second"

namespaceTest :: IO ()
namespaceTest = H.assertEqual "Namespacing changes keys"
                              (Just "foovalue")
                              (M.lookup "ns:foo" (runSplices (namespaceSplices "ns" splices))) where
    splices :: Splices String
    splices = do
        "foo" ## "foovalue"
        "bar" ## "barvalue"

namespaceWithNullTest :: IO ()
namespaceWithNullTest = H.assertEqual "Namespacing uses only namespace if key is empty"
                                      (Just "emptyvalue")
                                      (M.lookup "ns" (runSplices (namespaceSplices "ns" splices))) where
    splices :: Splices String
    splices = do
        "" ## "emptyvalue"

splicesToListTest :: IO ()
splicesToListTest = H.assertEqual "Splices to list works"
                    [("bar", "barvalue"), ("foo", "foovalue")]
                    (splicesToList splices) where
    splices :: Splices String
    splices = do
        "foo" ## "foovalue"
        "bar" ## "barvalue"

monoidTest :: IO ()
monoidTest = H.assertEqual "Monoid implementation"
             [("foo", "foovalue")]
             (splicesToList (mempty <> splices)) where
    splices :: Splices String
    splices = do
        "foo" ## "foovalue"

applySTest :: IO ()
applySTest = H.assertEqual "applyS"
             [("key", "appliedvalue"::String)]
             (splicesToList $ splices $$ "applied") where
    splices :: Splices (String -> String)
    splices = do
        ("key" ## (++"value"))

insertSTest :: IO ()
insertSTest = H.assertEqual "insertS"
              [("bar", "barvalue"), ("foo", "foovalue")]
              (splicesToList $ insertS "bar" "barvalue" splices) where
    splices :: Splices String
    splices = do
        ("foo" ## "foovalue")

unionWithSTest :: IO ()
unionWithSTest = H.assertEqual "unionWithS"
                [("key1","value1;value3"),("key2","value2"),("key3","value4")]
                (splicesToList $ unionWithS (\s1 s2 -> s1 ++ ";" ++ s2) splices1 splices2) where
                splices1, splices2 :: Splices String
                splices1 = do
                    "key1" #! "value1"
                    "key2" #! "value2"
                splices2 = do
                    "key1" #! "value3"
                    "key3" #! "value4"
