{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable #-}
import System.Console.CmdArgs.FromHelp
import System.Console.CmdArgs hiding (cmdArgsHelp)
import System.Environment (getArgs)

mkCmdArgs [fromHelp|
  Yesod generator

yesod-generate [COMMAND] [OPTIONS] [ARGS]

Common flags
  -? --help       Display help message
  -V --version    Print version information

yesod-generate view [Model]

yesod-generate model [OPTIONS] [Fields]

  -b --bootstrap
|]

main :: IO ()
main = do
  print =<< cmdArgs (modes [defaultView, defaultModel])
