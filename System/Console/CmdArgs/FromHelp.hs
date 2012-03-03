{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module  System.Console.CmdArgs.FromHelp (
    cmdArgsHelp
) where

import System.Console.ParseHelp (parseHelp)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import System.Console.CmdArgs.Explicit

cmdArgsHelp :: QuasiQuoter
cmdArgsHelp = QuasiQuoter { quoteExp = makeCmdArgs }

upd a b = Right b

makeCmdArgs s =
  case parseHelp s of
    Left e -> error $ show e
    Right (recordName, Just summary, common_flags, mode_flags) -> do
      [| mode "explicit" [] "Explicit Sample" (flagArg upd "file") [] :: Mode [(String,String)]|]
      -- error $ unpack (capitalize recordName) ++ "{}[" ++ "] += summary " ++ unpack summary
{-
("sample",Just "\nThe sample program",[ParsedFlag {parsedFlagShort = Just '?', parsedFlagLong = Just "help", parsedFlagDescription = Just "       Display help message"},ParsedFlag {parsedFlagShort = Just 'V', parsedFlagLong = Just "version", parsedFlagDescription = Just "    Print version information"}],[])
-}

