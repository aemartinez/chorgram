--
--
-- Author: Emilio Tuosto <emilio@le.ac.uk>

-- A parser for a very basic grammar and parser for the textual editing of global graphs.
-- The grammar is the one used in the ICE16 paper with the addition
-- of the repeat-until construct:
--
--    G ::= P -> P : M | G|G | G+G | G;G | * G @ P | { G }
--
-- the binary operators |, +, and ; are given in ascending order of
-- precedence.  The parser generator is Haskell's 'Happy' and the
-- parser (GGparser.hs) is obtained by typing'make parser'. Note that
-- the empty graph has been removed as not necessary.
--
-- The only syntactic check made (right now) during the parsing are
-- (i) that sender and receiver of interactions have to be different,
-- (2) that the participant controlling a loop is active in the
-- loop. However, the error messages are still not informative.
--
-- Note: strings are made of the following characters
--
--   0123456789<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ()\\^_`abcdefghijklmnopqrstuvwxyz\167/$#&~,.
--
-- and must start with a letter when specifying the identity of a participant.
--
-- Text enclosd by '[' and ']' is treated as comment
--
-- TODO: improve parseError
-- TODO: add line numbers
--


import Text.Parsec.String (Parser)
--
import Text.Parsec.Char (oneOf, char, digit, letter, satisfy, string)
import Text.Parsec.Combinator (many1, chainl1)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)
import CFSM
import Misc
import SyntacticGlobalGraphs

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"


parseArrow :: Parser ()
parseArrow = void $ string "->"

parseMsg :: Parser Message
parseMsg = string

parseInteraction :: Parser Action
parseInteraction = do
  whitespace
  sender <- string
  whitespace
  parseArrow
  whitespace
  receiver <- string
  whitespace
  void $ char ':'
  whitespace
  msg <- parseMsg
  return ((Act (sender, receiver), msg))
  

