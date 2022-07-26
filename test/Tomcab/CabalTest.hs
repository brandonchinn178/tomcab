{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module Tomcab.CabalTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import TOML (DecodeTOML, decode)
import Test.Tasty.HUnit
import UnliftIO.Exception (fromEither)

import Tomcab.Cabal
import Tomcab.Utils.TOML ()

{----- Conditional / DecodeTOML -----}

test =
  testCase "Simple condition can be parsed" $ do
    actual <-
      decodeLines
        [ "condition = \"flag(dev)\""
        , "ghc-options = \"-Werror\""
        ]
    actual
      @?= Conditional
        { condition = "flag(dev)"
        , conditionThen = fields [("ghc-options", "-Werror")]
        , conditionElif = []
        , conditionElse = Nothing
        }

test_todo = "Advanced condition can be parsed with just then"

test_todo = "Advanced condition can be parsed with then and else"

test_todo = "Advanced condition can be parsed with then and elif and else"

{----- Helpers -----}

decodeLines :: DecodeTOML a => [Text] -> IO a
decodeLines = fromEither . decode . Text.unlines

fields :: [(Text, Text)] -> Map Text Text
fields = Map.fromList
