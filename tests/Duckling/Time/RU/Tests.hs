module Duckling.Time.RU.Tests
  ( tests ) where

import Data.String
import Prelude
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Testing.Asserts
import Duckling.Time.RU.Corpus

tests :: TestTree
tests = testGroup "RU Tests"
  [ makeCorpusTest [This Time] corpus
  , makeNegativeCorpusTest [This Time] negativeCorpus
  ]
