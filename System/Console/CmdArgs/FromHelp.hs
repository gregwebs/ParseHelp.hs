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
import System.Console.CmdArgs.Implicit hiding (cmdArgsHelp)
import Data.Char (toUpper)

fromHelp :: QuasiQuoter
fromHelp = QuasiQuoter { quoteExp = lift . parseHelp }

data FromHelpArgs = FromHelpArgs {hello::String} deriving (Show, Data, Typeable)
      -- [| cmdArgs_ $ record (FromHelpArgs def) [hello := def += help "help"]  :: IO FromHelpArgs|]

instance Default Text where def = T.empty

mkCmdArgs :: Either String ParsedHelp -> Q [Dec]
mkCmdArgs s = do
  qRunIO $ print s
  case s of
    Left e -> error $ show e
    Right (progName, mSummary, common_flags, mode_flags) -> do
      f <- [| def |]
      return $
        mkDataDec (capitalize $ unpack progName)
          (cycle [f]) $ (progName, common_flags):mode_flags
  where
    mkDataDec :: String -> [Exp] -> [(Text, [ParsedFlag])] -> [Dec]
    mkDataDec recordType defaults modes =
      [ DataD [] (mkName recordType) []
          (map mkRecord modes)
          [mkName "Show",mkName "Data",mkName "Typeable"]
      ] ++ map mkDefaultRecord modes

      where
        mkDefaultRecord (name, flags) = FunD (mkName $ "default" ++ upperName name) [ Clause [] (NormalB (
          foldl AppE (ConE $ mkName $ upperName name) $ take (length flags) defaults
          )) [] ]
        upperName = capitalize . unpack
        recordUpper = upperName . fst . head $ modes
        mkField name = (mkName name, NotStrict, ConT ''Text)
        mkRecord (name, flags) =
          RecC (mkName $ capitalize $ unpack name) $
          map (mkField . maybe def unpack . parsedFlagLong) flags

    capitalize [] = []
    capitalize (c:cs) = toUpper c : cs
