-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSV
-- Copyright   :  (c) 2013, Stuart A. Kurtz
-- License     :  BSD (3-Clause) (see ./LICENSE)
-- 
-- Maintainer  :  stuart@cs.uchicago.edu
-- Stability   :  provisional
--
-- This is a library for parsing comma separated values (CSV).
-----------------------------------------------------------------------------

module Text.CSV (
	parseCSV,
	readCSV
) where

import Data.Char
import Text.ParserCombinators.ReadP

{-
  The 'parse' function used here is a bit non-standard, in that it
  requires the parser to parse the entire string, rather than ignoring
  white-space at the beginning and end of the string, and so we don't
  export it. Unfortunately, ignoring whitespace creates an ambiguity
  in interpreting CSV, so we can't do that here.
-}

parse p s = case [a | (a,"") <- readP_to_S p s] of
	[x] -> x
	[] -> error "no parse"
	_  -> error "ambiguous parse"

{- |
  The 'parseCSV' function parses a 'String' containing comma separated
  values (CSV) as a list of records, each of which is a list of fields
  of type 'String'. Note that CSV is ambiguous, in that a newline may be
  intended either as a record terminator, or as a record separator. The
  'parseCSV' function treats newlines as record separators, which allows
  it to successfully parse in either case, albeit with a spurious final
  record consisting of a single, empty field, for CSV files in which
  newline is a record terminator.
  
  Client code should be aware of the possibility (even likelihood) of
  encountering records that consist of a single, empty field.
-} 
  
parseCSV :: String -> [[String]]	
parseCSV  = parse csv where
	csv = sepBy record newline
	record = sepBy1 field (char ',')
	field = simpleField <++ quotedField
	simpleField = munch (not . flip elem ",\"\n\r")
	quotedField = between (char '"') (char '"') (munch (/= '"'))
	newline = string "\n\r" <++ string "\n" <++ string "\r"

{- |
  The 'readCSV' function parses a file containing comma separated values
  (CSV) as a list of records, each of which is a list of fields
  of type 'String'. The same caveats regarding CSV that are noted in
  documentation for 'parseCSV' apply to this function as well.
-}

readCSV :: FilePath -> IO [[String]]
readCSV = fmap parseCSV . readFile
