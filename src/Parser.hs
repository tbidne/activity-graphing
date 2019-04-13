{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
( entry
, test
) where

import           Control.Applicative (liftA2)
import           Data.Text (Text)
import qualified Data.Text as Txt (append, cons, intercalate, pack, singleton, unpack, isPrefixOf, lines)
import           Data.Time (Day)
import qualified Data.Time as T (defaultTimeLocale, parseTimeOrError)
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, ParseErrorBundle, (<|>), sepBy)
import           Text.Megaparsec.Error as ME (errorBundlePretty)
import qualified Text.Megaparsec as M (between, choice, eof, parse)
import qualified Text.Megaparsec.Char as MC (space1, string, char, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L (space, lexeme, decimal, symbol, skipLineComment, skipBlockComment)

import Activities.Activity
import Activities.Set
import Activities.BenchPress
import Activities.Deadlift

type Parser = Parsec Void Text
type ParseErr = ParseErrorBundle Text Void

data ParseType
  = PBenchPress
  | PDeadlift
  | PRun
  deriving (Show, Eq)

-- not using comments at the moment
sc :: Parser ()
sc = L.space
  MC.space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pList :: Parser a -> Parser [a]
pList p = M.between (L.symbol sc "[") (L.symbol sc "]") $ (p <* sc) `sepBy` (MC.string "," <* sc)

-- 1. Type
pType :: Parser ParseType
pType = M.choice
  [ lexeme $ PBenchPress <$ MC.string "BenchPress"
  , lexeme $  PDeadlift <$ MC.string "Deadlift"
  , lexeme $ PRun <$ MC.string "Run" ]

-- 2. Date
pDateSep :: Parser Text
pDateSep = fmap Txt.singleton $ MC.char '-' <|> MC.char '/'

pNum2 :: Parser Text
pNum2 = liftA2 (\a -> Txt.cons a . Txt.singleton) MC.digitChar MC.digitChar

pNum4 :: Parser Text
pNum4 = liftA2 Txt.append pNum2 pNum2

pDay :: Parser Day
pDay = lexeme $ toDay <$> pNum2 <* pDateSep <*> pNum2 <* pDateSep <*> pNum4
  where toDay d m y = T.parseTimeOrError True T.defaultTimeLocale "%d-%m-%Y" $ toFmtStr d m y
        toFmtStr d m y = Txt.unpack $ Txt.intercalate "-" [d, m, y]

-- 3. Set
pInt :: Parser Integer
pInt = lexeme L.decimal

pSet :: Parser Set
pSet = MkSet <$> pInt <* MC.string "x" <*> pInt

pSets :: Parser [Set]
pSets = pList pSet

-- 4. All together now
pBenchPress :: Parser BenchPress
pBenchPress = MkBenchPress <$> pDay <*> pInt <*> pSets <* M.eof

pDeadlift :: Parser Deadlift
pDeadlift = MkDeadlift <$> pDay <*> pInt <*> pSets <* M.eof

pActivity :: Parser Activity
pActivity = pType >>=
  \case
    PBenchPress -> pActHelper pBenchPress ActBP
    PDeadlift -> pActHelper pDeadlift ActD
    PRun -> undefined -- todo

pActHelper :: (Parser a) -> (a -> Activity) -> Parser Activity
pActHelper p actCon = p >>= return . actCon

entry :: String -> [Either ParseErr Activity]
entry = parsing . Txt.lines . Txt.pack

parsing :: [Text] -> [Either ParseErr Activity]
parsing = fmap (\s -> M.parse pActivity "" s) . filter (not . skip)
  where skip s = s `elem` ["", "\n"] || "#" `Txt.isPrefixOf` s

test :: Either ParseErr Activity -> String
test (Left bundle) = ME.errorBundlePretty bundle
test (Right _) = "Succeeded! "