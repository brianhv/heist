{-# LANGUAGE BangPatterns #-}

module Heist.SpliceAPI.Tests
  ( tests
  ) where

import           Control.Exception (ErrorCall(..), try)
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H
import Control.Monad (unless)
import Data.Monoid ((<>))
import qualified Data.Map as M

------------------------------------------------------------------------------
import           Heist.SpliceAPI
--import           Heist.TestCommon

-- NOTE: We can't test compiled templates on the templates directory as it
-- stands today because that directory contains some error conditions such as
-- infinite bind loops, apply tags with no template attribute, and apply tags
-- with ".." in the tag path (which doesn't currently work).

tests :: [Test]
tests = [ testCase     "spliceapi/keyExistsError"       keyExistsErrorTest
        --, testCase     "spliceapi/unionThrowsError"     unionThrowsErrorTest
        ]

{- | Asserts that a specific exception is raised by a given action. -}
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

keyExistsErrorTest :: IO ()
keyExistsErrorTest = do
    let !asdf = runSplices mySplices
    assertRaisesError "Adding duplicate key raises error"
                     "Key \"asdf\" already exists in the splice map" $
                     --seq (M.lookup "asdf" asdf) (return asdf)
                     seq asdf (return ())
    where
    mySplices :: Splices String
    mySplices = do
        "asdf" #! "first"
        "asdf" #! "second"

unionThrowsErrorTest :: IO ()
unionThrowsErrorTest = assertRaisesError "Adding duplicate key with union raises error"
                       "Key \"asdf\" already exists in the splice map" $
                       print $ runSplices $ splices1 <> splices2 where
    splices1, splices2 :: Splices String
    splices1 = "asdf" #! "first"
    splices2 = "asdf" #! "second"
--
--peopleTest :: IO ()
--peopleTest = do
--    res <- personListTest "templates"
--    H.assertEqual "people splice" expected res
--  where
--    expected =
--      "&#10;<p>Doe, John: 42&#32;years old</p>&#10;&#10;<p>Smith, Jane: 21&#32;years old</p>&#10;&#10;"

