{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module  System.Console.CmdArgs.Help where

import Data.Attoparsec.Text
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Data.Char (toUpper)
import Control.Applicative ((<|>))
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T

data ParsedFlag = ParsedFlag {
   parsedFlagShort       :: Maybe Char, 
   parsedFlagLong        :: Maybe Text,
   parsedFlagDescription :: Maybe Text
 } deriving Show

type ParsedMode = (Text, -- ^ program name
                   Text, -- ^ mode name
                  [ParsedFlag])
type ParsedCommonMode = (Text, [ParsedFlag]) -- no mode name

cmdArgsHelp :: QuasiQuoter
cmdArgsHelp = QuasiQuoter { quoteExp = converter }

capitalize t | T.null t = t
             | otherwise = T.cons (toUpper $ T.head t) (T.tail t)
{-
("sample",Just "\nThe sample program",[ParsedFlag {parsedFlagShort = Just '?', parsedFlagLong = Just "help", parsedFlagDescription = Just "       Display help message"},ParsedFlag {parsedFlagShort = Just 'V', parsedFlagLong = Just "version", parsedFlagDescription = Just "    Print version information"}],[])
-}
converter s =
  case parseHelp $ pack s of
    Left e -> error $ show e
    Right pHelp ->
      let (recordName, Just summary, common_flags, mode_flags) = prepareParsedHelp pHelp
      in  error $ unpack (capitalize recordName) ++ "{}[" ++ "] += summary " ++ unpack summary

parseHelp :: Text -> Either String (Maybe Text,           -- ^ program description
                                    Maybe ParsedCommonMode,
                                    [ParsedMode])
parseHelp input = parseOnly commandArgs input
  where
    commandArgs = do
      description <- maybeManyTill anyChar blankLine
      optional blankLines
      common <- optionMaybe commonInvocation
      optional blankLines
      -- modes <- many modeInvocation
      return (description, common, [])
      -- return (description, common, modes)
      where
        spaces = skipSpace
        commonInvocation = do
          -- sample [OPTIONS]
          (name,Nothing) <- progName
          optional blankLines
          spaces
          -- Common Flags
          optional $ takeWhile1 (notInClass "-\r\n") >> eol'
          spaces
          optional blankLines
          opts <- flags
          return (name, opts)

        modeInvocation = do
          (name, mmode) <- progName
          case mmode of
            Nothing -> fail $ "no mode found, just: " ++ unpack name
            Just mode -> do
              optional blankLines
              opts <- flags
              return (name, mode, opts)

        betweenChars start end = do
          char start
          r <- many1 $ notChar end
          char end
          return r

        progName = do
          t <- takeWhile1 notSpace
          spaces
          m <- optionMaybe $ takeWhile1 (notInClass $ '[':whiteSpaceChars)
          spaces
          _ <- betweenChars '[' ']'
          return (t,m)

        whiteSpaceChars = " \v\f\t\r\n"
        notSpace = notInClass whiteSpaceChars

        maybeManyTill p stop = do
          r <- manyTill p stop
          return $ if r == [] then Nothing else Just $ pack r

        blankLines = many1 blankLine >> return ()
        blankLine = eol >> eol

        eol' = eol >> return ()

        eol = endOfLine

        optional = option ()

{-
        optionallyCapitalized (s:str) = ciChar s >> string str
          where
            ciChar c = char c <|> char (toUpper c)
            -}


        restOfLine = manyTill anyChar eol
        showRest = do
          rest <- restOfLine
          error $ "rest: " ++ (show rest)

        optionMaybe p = option Nothing $ (fmap Just) p

        flags = many1 flag
          where
            flag = do
              spaces
              ms <- optionMaybe shortFlag
              spaces
              ml <- optionMaybe longFlag
              case (ms, ml) of
                (Nothing, Nothing) -> fail "expected flags"
                _ -> do
                  d <- optionMaybe (takeTill $ inClass "\r\n")
                  return $ ParsedFlag ms (fmap fst ml) d
              where
                shortFlag = do
                  _ <- char '-'
                  satisfy notSpace

                longFlag = do
                  _ <- string "--"
                  long <- takeWhile1 (notInClass $ '=':whiteSpaceChars)
                  def <- optionMaybe $ char '=' >> takeWhile1 notSpace
                  return (long, def)

prepareParsedHelp :: (Maybe Text, Maybe ParsedCommonMode, [ParsedMode])
                  -> (Text, Maybe Text, [ParsedFlag], [(Text, [ParsedFlag])])
prepareParsedHelp (_, Nothing, []) = error "No command flags found"
prepareParsedHelp (description, mCommon, parsedModes) =
  let (name, common) = case mCommon of
                        Nothing                  -> (fst3 $ head parsedModes, defaultCommonFlags)
                        Just (commonName, flags) -> (commonName, flags)
  in let modes = map (checkMode name) parsedModes
     in (name, description, common, modes)
  where
    fst3 (x,y,z) = x
    checkMode :: Text -> (Text, Text, [ParsedFlag]) -> (Text, [ParsedFlag])
    checkMode commonProgName (progName, mode, flags) =
      if progName /= commonProgName then error $ "inconsistent program name: expected "++unpack commonProgName++", got "++unpack progName
        else (mode, flags)

    defaultCommonFlags = [
       ParsedFlag (Just '?') (Just "help") (Just "Display help message")
     , ParsedFlag (Just 'V') (Just "version") (Just "Print version information")
     ]

  -- makeCmdArgs $ prepareParsedHelp parseHelp


-- check indent level?
{-parseIndent :: Parser Int -}
{-parseIndent =-}
    {-sum `fmap` many ((char ' ' >> return 1) <|> (char '\t' >> return 4)) -}

