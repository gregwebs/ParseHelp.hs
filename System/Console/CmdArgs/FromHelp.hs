{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}
module  System.Console.CmdArgs.FromHelp (
    fromHelp
  , FromHelpArgs(..)
  , mkCmdArgs
) where

import System.Console.ParseHelp (parseHelp, ParsedHelp, ParsedFlag(..) )
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Data.Char (toUpper)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Default (Default(..), def)
import FileLocation (debug)
import Data.List (nubBy)

fromHelp :: QuasiQuoter
fromHelp = QuasiQuoter { quoteExp = lift . parseHelp }

data FromHelpArgs = FromHelpArgs {hello::String} deriving (Show, Data, Typeable)
      -- [| cmdArgs_ $ record (FromHelpArgs def) [hello := def += help "help"]  :: IO FromHelpArgs|]

instance Default Text where def = T.empty

mkCmdArgs :: Either String ParsedHelp -> Q [Dec]
mkCmdArgs s =
  case s of
    Left e -> error $ show e
    Right (progName, mSummary, common_flags, mode_flags) -> do
      f <- [| def |]
      return $ mkDataDec
        (cycle [f]) (progName, common_flags) mode_flags
  where
    mkDataDec :: [Exp] -> (Text, [ParsedFlag]) -> [(Text, [ParsedFlag])] -> [Dec]
    mkDataDec defaults commonMode modes =
      [ DataD [] (mkName recordUpper) []
          (map mkRecord modes')
          [mkName "Show",mkName "Data",mkName "Typeable"]
      ] ++ map mkDefaultRecord modes'

      where
        modes' = map (\(n,fs) -> (n, mergeFlags fs)) modes
          where
            mergeFlags flags = nubBy (\f1 f2 -> parsedFlagLong f1 == parsedFlagLong f2) (flags ++ commonFlags)
            commonFlags = filter notDefault (snd commonMode)

        notDefault = noDef . parsedFlagLong
          where noDef Nothing  = True
                noDef (Just f) = f `notElem` ["help","version"]
        mkDefaultRecord (name, flags) = FunD (mkName $ "default" ++ upperName name) [ Clause [] (NormalB (
          foldl AppE (ConE $ mkName $ upperName name) $ take (length flags) defaults
          )) [] ]
        upperName = capitalize . unpack
        recordUpper = upperName $ fst commonMode
        mkField name = (mkName name, NotStrict, ConT ''String)
        mkRecord (name, flags) =
          RecC (mkName $ capitalize $ unpack name) $
          map (mkField . maybe def unpack . parsedFlagLong) flags

    capitalize [] = []
    capitalize (c:cs) = toUpper c : cs
