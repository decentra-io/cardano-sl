module Lang.Lexer
       ( BracketSide(..)
       , _BracketSideOpening
       , _BracketSideClosing
       , UnknownChar(..)
       , FilePath'(..)
       , Token(..)
       , _TokenSquareBracket
       , _TokenParenthesis
       , _TokenString
       , _TokenAddress
       , _TokenFilePath
       , _TokenNumber
       , _TokenName
       , _TokenKey
       , _TokenEquals
       , _TokenSemicolon
       , _TokenUnknown
       , tokenize
       , tokenize'
       , detokenize
       ) where

import           Universum

import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty
import           Control.Lens                             (makePrisms)
import           Data.Char                                (isAlpha, isAlphaNum)
import qualified Data.List                                as List
import qualified Data.List.NonEmpty                       as NonEmpty
import           Data.Scientific                          (Scientific)
import qualified Data.Text                                as Text
import qualified Data.Text.Buildable                      as Buildable
import           Test.QuickCheck.Arbitrary.Generic        (Arbitrary (..),
                                                           genericArbitrary,
                                                           genericShrink)
import qualified Test.QuickCheck.Gen                      as QC
import           Test.QuickCheck.Instances                ()
import           Text.Megaparsec                          (Parsec, between, choice, eof,
                                                           manyTill, parseMaybe, skipMany,
                                                           takeWhile1P, try, (<?>))
import           Text.Megaparsec.Char                     (anyChar, char, satisfy,
                                                           spaceChar, oneOf)
import           Text.Megaparsec.Char.Lexer               (charLiteral, scientific,
                                                           signed)

import           Lang.Name                                (Letter, Name (..),
                                                           unsafeMkLetter)
import           Pos.Arbitrary.Core                       ()
import           Pos.Types                                (Address, decodeTextAddress)

data BracketSide = BracketSideOpening | BracketSideClosing
    deriving (Eq, Ord, Show, Generic)

makePrisms ''BracketSide

withBracketSide :: a -> a -> BracketSide -> a
withBracketSide onOpening onClosing = \case
    BracketSideOpening -> onOpening
    BracketSideClosing -> onClosing

instance Arbitrary BracketSide where
    arbitrary = genericArbitrary
    shrink = genericShrink

newtype UnknownChar = UnknownChar Char
    deriving (Eq, Ord, Show)

instance Arbitrary UnknownChar where
    arbitrary = pure (UnknownChar '\0')

newtype FilePath' = FilePath'
    { getFilePath' :: FilePath
    } deriving (Eq, Ord, Show, Generic, IsString)

instance Arbitrary FilePath' where
    arbitrary = QC.elements
        [ "/a/b/c"
        , "./a/b/c"
        , "/p a t h/h e r e.k"
        ] -- TODO: proper generator

instance Buildable FilePath' where
    build = fromString . concatMap escape . getFilePath'
      where
        escape c | isFilePathChar c = [c]
                 | otherwise = '\\':[c]

isFilePathChar :: Char -> Bool
isFilePathChar c = isAlphaNum c || c `elem` ['.', '/']

data Token
    = TokenSquareBracket BracketSide
    | TokenParenthesis BracketSide
    | TokenString String
    | TokenNumber Scientific
    | TokenAddress Address
    | TokenFilePath FilePath'
    | TokenName Name
    | TokenKey Name
    | TokenEquals
    | TokenSemicolon
    | TokenUnknown UnknownChar
    deriving (Eq, Ord, Show, Generic)

makePrisms ''Token

instance Arbitrary Token where
    arbitrary = genericArbitrary
    shrink = genericShrink

tokenRender :: Token -> Text
tokenRender = \case
    TokenSquareBracket bs -> withBracketSide "[" "]" bs
    TokenParenthesis bs -> withBracketSide "(" ")" bs
    TokenString s -> show s
    TokenNumber n -> show n
    TokenAddress a -> pretty a
    TokenFilePath s -> pretty s
    TokenName ss -> pretty ss
    TokenKey ss -> pretty ss <> ":"
    TokenEquals -> "="
    TokenSemicolon -> ";"
    TokenUnknown (UnknownChar c) -> Text.singleton c

detokenize :: [Token] -> Text
detokenize = Text.unwords . List.map tokenRender

type Lexer a = Parsec Void Text a

tokenize :: Text -> [Token]
tokenize = fromMaybe noTokenErr . tokenize'
  where
    noTokenErr =
        error "tokenize: no token could be consumed. This is a bug"

tokenize' :: Text -> Maybe [Token]
tokenize' = parseMaybe (between pSkip eof (many pToken))

pToken :: Lexer Token
pToken = (try pToken' <|> pUnknown) <* pSkip

pUnknown :: Lexer Token
pUnknown = TokenUnknown . UnknownChar <$> anyChar

pSkip :: Lexer ()
pSkip = skipMany (void spaceChar)

pToken' :: Lexer Token
pToken' = choice
    [ pPunct
    , TokenAddress <$> try pAddress
    , TokenFilePath <$> pFilePath
    , TokenNumber <$> pScientific
    , TokenString <$> pString
    , pNameOrKey
    ] <?> "token"

pPunct :: Lexer Token
pPunct = choice
    [ char '[' $> TokenSquareBracket BracketSideOpening
    , char ']' $> TokenSquareBracket BracketSideClosing
    , char '(' $> TokenParenthesis BracketSideOpening
    , char ')' $> TokenParenthesis BracketSideClosing
    , char '=' $> TokenEquals
    , char ';' $> TokenSemicolon
    ] <?> "punct"

pString :: Lexer String
pString =
    char '\"' *>
    manyTill (charLiteral <|> anyChar) (char '\"')

pSomeAlphaNum :: Lexer Text
pSomeAlphaNum = takeWhile1P (Just "alphanumeric") isAlphaNum

pAddress :: Lexer Address
pAddress = do
    str <- pSomeAlphaNum
    either (fail . toString) return $
        decodeTextAddress str

pFilePath :: Lexer FilePath'
pFilePath = FilePath' <$> do
    c <- oneOf ['.', '/']
    cs <- many pFilePathChar
    return (c : cs)
  where
    pFilePathChar :: Lexer Char
    pFilePathChar =
        char '\\' *> anyChar <|>
        satisfy isFilePathChar

pNameOrKey :: Lexer Token
pNameOrKey = do
    name <- NonEmpty.sepBy1 pNameSection (char '-')
    isKey <- isJust <$> optional (char ':')
    return $ (if isKey then TokenKey else TokenName) (Name name)

pNameSection :: Lexer (NonEmpty Letter)
pNameSection = NonEmpty.some1 pLetter

pLetter :: Lexer Letter
pLetter = unsafeMkLetter <$> satisfy isAlpha

pScientific :: Lexer Scientific
pScientific = signed (return ()) scientific
