module ShowParser (parseShow)
  where

import Text.ParserCombinators.Parsec
import qualified Text.ParsecCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

lexer = P.makeTokenParser emptyDef

