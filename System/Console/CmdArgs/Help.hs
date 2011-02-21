module  System.Console.CmdArgs.Help where

import Text.ParserCombinators.Parsec
import Data.Char (toUpper)

data ParsedFlag = ParsedFlag {
   parsedFlagShort       :: Maybe Char, 
   parsedFlagLong        :: Maybe String,
   parsedFlagDescription :: Maybe String
 } deriving Show

type ParsedMode = (String, -- ^ program name
                   String, -- ^ mode name
                  [ParsedFlag])
type ParsedCommonMode = (String, [ParsedFlag]) -- no mode name

parseHelp :: String -> Either ParseError (Maybe String,           -- ^ program description
                                          Maybe ParsedCommonMode,
                                          [ParsedMode])
parseHelp input = parse commandArgs "(unknown)" input
  where
    commandArgs = do
      description <- maybeManyTill anyChar (try blankLines)
      blankLines
      common <- optionMaybe (try commonInvocation)
      modes <- many modeInvocation
      return (description, common, modes)
      where
        commonInvocation = do
          -- sample [OPTIONS]
          name <- progName
          blankLines
          -- Common Flags
          optionMaybe $ (many1 space) >> try (many letter >> skipMany1 space) >> optionallyCapitalized "flags"
          blankLines
          opts <- flags
          return (name, opts)

        modeInvocation = do
          name <- progName
          mode <- many1 notSpace
          blankLines
          opts <- flags
          return (name, mode, opts)

        maybeManyTill p stop = do
          r <- manyTill p stop
          return $ if r == [] then Nothing else Just r

        blankLines = eol >> many1 blankLine
          where
            blankLine = many space >> eol

        eol = try (string "\r\n")
          <|> string "\n"
          <|> string "\r"
          <?> "expected end of line"

        optionallyCapitalized (s:str) = ciChar s >> string str
          where
            ciChar c = char c <|> char (toUpper c)

        notSpace = noneOf " \v\f\t\r\n"

        progName = do
          t <- many1 notSpace
          skipMany1 space
          _ <- between (char '[') (char ']') (noneOf "]")
          return t

        flags = endBy flag eol
          where
            flag = do
              skipMany1 spaces
              s <- optionMaybe shortFlag
              skipMany1 spaces
              l <- optionMaybe longFlag
              spaces
              case (s,l) of
                (Nothing, Nothing) -> error "parse error: no flags found"
                _ -> return ()
              d <- maybeManyTill anyChar eol
              blankLines
              return $ ParsedFlag s l d
              where
                shortFlag = do
                  _ <- char '-'
                  short <- alphaNum
                  return short

                longFlag = do
                  _ <- string "--"
                  long <- many1 (notSpace <|> char '=')
                  _ <- optionMaybe $ char '=' >> notSpace
                  return long

prepareParsedHelp :: (Maybe String, Maybe ParsedCommonMode, [ParsedMode]) -> (String, Maybe String, [ParsedFlag], [(String, [ParsedFlag])])
prepareParsedHelp (description, mCommon, parsedModes) =
  let (name, common) = case mCommon of
                        Nothing                  -> (fst3 $ head parsedModes, defaultCommonFlags)
                        Just (commonName, flags) -> (commonName, flags)
  in let modes = map (checkMode name) parsedModes
     in (name, description, common, modes)
  where
    fst3 (x,y,z) = x
    checkMode :: String -> (String, String, [ParsedFlag]) -> (String, [ParsedFlag])
    checkMode commonProgName (progName, mode, flags) =
      if progName /= commonProgName then error $ "inconsistent program name: expected "++commonProgName++", got "++progName
        else (mode, flags)

    defaultCommonFlags = [
       ParsedFlag (Just '?') (Just "help") (Just "Display help message")
     , ParsedFlag (Just 'V') (Just "version") (Just "Print version information")
     ]

main = do
  case parseHelp "help" of
    Left e -> print e
    Right pHelp -> putStrLn . show $ prepareParsedHelp pHelp
  -- makeCmdArgs $ prepareParsedHelp parseHelp


-- check indent level?
{-parseIndent :: Parser Int -}
{-parseIndent =-}
    {-sum `fmap` many ((char ' ' >> return 1) <|> (char '\t' >> return 4)) -}

