{- |
  Module      :  Text.CSV
  Copyright   :  (c) 2013, Stuart A. Kurtz
  License     :  BSD (3-Clause) (see ./LICENSE)
  
  Maintainer  :  stuart@cs.uchicago.edu
  Stability   :  experimental
 
  A library for parsing comma separated values (CSV).
-}

module Text.CSV (
	parseCSV,
	readCSV,
	prune
) where

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

{-
  The 'parse' function used here is a bit non-standard, in that it
  requires the parser to parse the entire string, rather than ignoring
  white-space at the beginning and end of the string, and so we don't
  export it. Unfortunately, ignoring whitespace creates an ambiguity
  in interpreting CSV, so we can't do that here.
-}

parse :: ReadP a -> String -> a
parse p s = case [a | (a,"") <- readP_to_S p s] of
	[x] -> x
	[]  -> error "no parse"
	_   -> error "ambiguous parse"

{- |
  The 'parseCSV' function parses a 'String' containing comma separated
  values (CSV) as a list of records, each of which is a list of fields
  of type 'String'. Note that CSV is ambiguous, in that a newline may be
  intended either as a record terminator, or as a record separator. The
  'parseCSV' function treats newlines as record separators, which allows
  it to successfully parse in either case, albeit with a spurious final
  record consisting of a single, empty field, on a 'String' in which
  newline was intended as a record terminator.
  
  Client code should be aware of the possibility (even likelihood) of
  encountering records that consist of a single, empty field.
-} 
  
parseCSV :: String -> [[String]]	
parseCSV  = parse csv where
	csv = record `sepBy1` newline
	record = field `sepBy1` char ','
	field = complete simpleField <++ complete quotedField
	simpleField = munch (`notElem` ",\"\n\r")
	quotedField = between (char '"') (char '"') (many nextChar)
	nextChar = satisfy (/= '"') <++ fmap (const '"') (string "\"\"")
	complete ma = do
		result <- ma
		peek <- look
		guard $ null peek || head peek `elem` ",\r\n"
		return result
	newline = string "\n" <++ string "\n\r" <++ string "\r"

{- |
  The 'readCSV' function parses a file containing comma separated values
  (CSV) as a list of records, each of which is a list of fields
  of type 'String'. The same caveats regarding CSV that are noted in
  documentation for 'parseCSV' apply to this function as well.
-}

readCSV :: FilePath -> IO [[String]]
readCSV = fmap parseCSV . readFile

{- |
  The 'prune' function is used to filter the values obtained by the application
  of 'parseCSV' or 'readCSV', eliminating any trivial records, i.e., records
  that consist of a single field that consists only of white space.
-}

prune :: [[String]] -> [[String]]
prune = filter nontrivial where
	nontrivial [x] = any (not . isSpace) x
	nontrivial _ = True
