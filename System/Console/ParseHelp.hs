{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module  System.Console.ParseHelp (
  parseHelp, ParsedHelp(..), ParsedFlag(..)
) where

import Data.Attoparsec.Text
import Control.Applicative ((<|>), many)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Language.Haskell.TH.Lift
import Prelude hiding (takeWhile)

instance Lift Text where lift = lift . unpack
data ParsedFlag = ParsedFlag {
   parsedFlagShort       :: Maybe Char, 
   parsedFlagLong        :: Maybe Text,
   parsedFlagType        :: Maybe Text,
   parsedFlagDescription :: Maybe Text
 } deriving (Show)
deriveLift ''ParsedFlag

type ParsedMode = (Text, -- ^ program name
                   Text, -- ^ mode name
                  [ParsedFlag])
type ParsedCommonMode = (Text, [ParsedFlag]) -- no mode name


parseHelp :: String -> Either String ParsedHelp
parseHelp s =
  case attoParser $ pack s of
    Left e -> Left e
    Right pHelp -> Right $ prepareParsedHelp pHelp

attoParser :: Text -> Either String (Maybe Text,           -- ^ program description
                                    Maybe ParsedCommonMode,
                                    [ParsedMode])
attoParser input = parseOnly commandArgs input
  where
    commandArgs = do
      description <- maybeManyTill anyChar blankLine
      optional blankLines
      common <- optionMaybe commonInvocation
      optional blankLines
      modes <- many modeInvocation
      return (description, common, modes)
      where
        spaces = skipSpace
        commonInvocation = do
          -- sample [OPTIONS]
          name <- commonProgName
          optional blankLines
          spaces
          -- optional blankLines
          -- optional $ takeWhile (notInClass "\r\n") >> eol'
          optional blankLines
          optional $ string "Common flags" >> takeWhile (notInClass "-\r\n") >> eol'
          spaces
          optional blankLines
          opts <- flags
          return (name, opts)

        modeInvocation = do
          (name, mode) <- progName
          optional blankLines
          opts <- flags
          return (name, mode, opts)

        betweenChars start end = do
          char start
          r <- takeWhile1 (/= end)
          char end
          return r

        bracketed = betweenChars '[' ']'

        commonProgName = do
          spaces
          t <- takeWhile1 notSpace
          spaces
          optional $ (bracketed <|> takeWhile1 notBracketed) >> return ()
          spaces
          optional $ string "..." >> return ()
          spaces
          _ <- bracketed
          return t

        notBracketed = notInClass $ '[':whiteSpaceChars
        progName = do
          spaces
          t <- takeWhile1 notSpace
          spaces
          m <- takeWhile1 notBracketed
          spaces
          _ <- bracketed
          return (t,m)

        maybeText = fmap (\t -> if T.null t then Nothing else Just t)
        flags = many1 flag
          where
            flag = do
              spaces
              ms <- optionMaybe shortFlag
              spaces
              ml <- optionMaybe longFlag
              d <- maybeText (takeTill $ inClass "\r\n")
              case (ms, ml) of
                (Nothing, Nothing) -> fail "expected flags"
                (_, Just (long,typ)) -> return $ ParsedFlag ms (Just long) typ d
                (_, Nothing) ->         return $ ParsedFlag ms Nothing Nothing d
              where
                shortFlag = do
                  _ <- char '-'
                  satisfy notSpace

                longFlag = do
                  _ <- string "--"
                  long <- takeWhile1 (notInClass $ '=':whiteSpaceChars)
                  def <- optionMaybe $ char '=' >> takeWhile1 notSpace
                  return (long, def)

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

        showRest = do
          rest <- restOfLine
          error $ "rest: " ++ (show rest)
        restOfLine = manyTill anyChar eol

        optionMaybe p = option Nothing $ (fmap Just) p

type ParsedHelp = (Text, Maybe Text, [ParsedFlag], [(Text, [ParsedFlag])])
prepareParsedHelp :: (Maybe Text, Maybe ParsedCommonMode, [ParsedMode])
                  -> ParsedHelp
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
       ParsedFlag (Just '?') (Just $ pack "help") Nothing (Just $ pack "Display help message")
     , ParsedFlag (Just 'V') (Just $ pack "version") Nothing (Just $ pack "Print version information")
     ]

  -- makeCmdArgs $ prepareParsedHelp parseHelp


-- check indent level?
{-parseIndent :: Parser Int -}
{-parseIndent =-}
    {-sum `fmap` many ((char ' ' >> return 1) <|> (char '\t' >> return 4)) -}

