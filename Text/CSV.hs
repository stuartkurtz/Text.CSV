{- |
  Module      :  Text.CSV
  Copyright   :  (c) 2013, Stuart A. Kurtz
  License     :  BSD (3-Clause) (see ./LICENSE)
  
  Maintainer  :  stuart@cs.uchicago.edu
  Stability   :  experimental
 
  A module for handling comma-separated values (CSV), either in Strings or files.
-}

module Text.CSV (
    parseCSV,
    readCSV,
    parseRawCSV,
    readRawCSV,
    prune,
    encodeCSVField,
    showCSV,
    writeCSV
) where

import Control.Monad
import Data.Char
import Data.List (intercalate)
import Text.ParserCombinators.ReadP

{-
  The 'parseWith' function used here is a bit non-standard, in that it
  requires the parser to parse the entire string, rather than ignoring
  white-space at the beginning and end of the string, and so we don't
  export it. Unfortunately, ignoring whitespace creates an ambiguity
  in interpreting CSV, so we can't do that here.
-}

parseWith :: ReadP a -> String -> a
parseWith p s = case [a | (a,"") <- readP_to_S p s] of
    [x] -> x
    []  -> error "no parse"
    _   -> error "ambiguous parse"

{- |
  The 'parseCSV' function parses a 'String' containing comma separated
  values (CSV) as a list of records, each of which is a list of fields
  of type 'String'. This function, unlike 'parseRawCSV', filters out 
  trivial records, i.e., records that consist of a single, white-space only
  field.
-} 

parseCSV :: String -> [[String]]
parseCSV = filter (not . trivial) . parseRawCSV where
    trivial :: [String] -> Bool
    trivial [x] = all isSpace x
    trivial _ = False

{- |
  The 'readCSV' function parses a file containing comma separated values
  (CSV) as a list of records, each of which is a list of fields
  of type 'String'. This function uses 'parseCSV' for parsing.
-}

readCSV :: FilePath -> IO [[String]]
readCSV = fmap parseCSV . readFile

{- |
  The 'parseRawCSV' function parses a 'String' containing comma separated
  values (CSV) as a list of records, each of which is a list of fields
  of type 'String'. Note that CSV specification is ambiguous, in that a terminal
  newline may be intended either as a record terminator, or as a record separator. The
  'parseRawCSV' function treats newlines as record separators, which allows
  it to successfully parse in either case, albeit with a spurious final
  record consisting of a single, empty field, given an argument 'String' in which
  newline was intended as a record terminator.
  
  Client code should be aware of the possibility (even likelihood) of
  encountering trivial records.
-} 
  
parseRawCSV :: String -> [[String]] 
parseRawCSV  = parseWith csv where
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
    newline = string "\r\n" <++ string "\n" <++ string "\r"

{- |
  The 'readRawCSV' function parses a file containing comma separated values
  (CSV) as a list of records, each of which is a list of fields
  of type 'String'. This function uses 'parseRawCSV' for parsing, and so
  may contain trivial records.
-}

readRawCSV :: FilePath -> IO [[String]]
readRawCSV = fmap parseRawCSV . readFile

{- |
  The 'prune' function is used to filter the values obtained by the application
  of 'parseCSV' or 'readCSV', eliminating any trivial records, i.e., records
  that consist of a single field that consists only of white space.
-}

prune :: [[String]] -> [[String]]
prune = filter nontrivial where
    nontrivial [x] = any (not . isSpace) x
    nontrivial _ = True

{- |
  The 'encodeCSVField' function will quote a 'String' that contains carriage
  returns, linefeeds, double-quotes, or commas, properly quoting for CSV any
  double-quoted fields.
-}

encodeCSVField :: String -> String
encodeCSVField s
    | any (`elem` "\r\n\",") s = "\"" ++ csvQuote s ++ "\""
    | otherwise = s
    where
        csvQuote = foldr quoteChar ""
        quoteChar '"' s = "\"\"" ++ s
        quoteChar c s = c:s

{- |
  The 'showCSV' function converts a [['String']] to a 'String', following the
  conventions of CSV, i.e., the result string will be valid CSV. The intent is
  that

>       readCSV . showCSV = id

  This may not be precisely so if the original CSV contains trivial records, as
  they will be elided by the process.

-}

showCSV :: [[String]] -> String
showCSV = unlines . map (intercalate "," . map encodeCSVField)

{- |
  The 'writeCSV' function takes a [['String']], converting it to what is intended to
  be equivalent CSV (note the caveat in 'showCSV'), and writing the results to the
  indicated file.
-}

writeCSV :: FilePath -> [[String]] -> IO ()
writeCSV fname = writeFile fname . showCSV
