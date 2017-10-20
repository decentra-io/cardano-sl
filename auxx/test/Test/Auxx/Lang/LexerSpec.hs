module Test.Auxx.Lang.LexerSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec            (Expectation, Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, property)

import           Lang.Lexer            (BracketSide (..), Token (..), detokenize,
                                        tokenize, tokenize')
import           Lang.Name             (unsafeMkName)

spec :: Spec
spec = describe "Auxx.Lang.Lexer" $ do
    prop "accepts any input" propAcceptsAnyInput
    prop "handles valid input" propHandlesValidInput
    it "handles sample-1" unitLexerSample1

propAcceptsAnyInput :: Property
propAcceptsAnyInput = property $ isJust . tokenize' . fromString

propHandlesValidInput :: Property
propHandlesValidInput = property $ liftA2 (==) (tokenize . detokenize) identity

unitLexerSample1 :: Expectation
unitLexerSample1 = tokenize input `shouldBe` output
  where
    input  = " ( \"Hello\"; [=propose-patak-update ./secret.key /home/a\\ b] \"\\\"\"  ) "
    output =
        [ TokenParenthesis BracketSideOpening
        , TokenString "Hello"
        , TokenSemicolon
        , TokenSquareBracket BracketSideOpening
        , TokenEquals
        , TokenName $ unsafeMkName ["propose", "patak", "update"]
        , TokenFilePath "./secret.key"
        , TokenFilePath "/home/a b"
        , TokenSquareBracket BracketSideClosing
        , TokenString "\""
        , TokenParenthesis BracketSideClosing
        ]
