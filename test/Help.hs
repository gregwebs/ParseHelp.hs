{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable #-}
import System.Console.CmdArgs.FromHelp
import System.Console.CmdArgs hiding (cmdArgsHelp)

{-printHelp :: Annotate Ann -> IO ()-}
{-printHelp a = print =<< (cmdArgs_ a :: IO FromHelpArgs)-}

{-
main :: IO ()
main = print =<< processArgs [cmdArgsHelp|
-}

mkCmdArgs [fromHelp|
  The sample program

sample [COMMAND] ... [OPTIONS]

Common flags
  -h --help       Display help message
  -V --version    Print version information
  -w --whom=GUY

sample hello [OPTIONS]

  -t --top  Top of the Morning to you!

sample goodbye [OPTIONS]

  -w --whom=ITEM
  -s --sianara
  -c --ciao
|]

main :: IO ()
main = do
  print defaultHello
  print defaultGoodbye
  print =<< cmdArgs (modes [defaultHello, defaultGoodbye])
{-
   dist/build/test/test goodbye -s sucker

  Hello {top = "", whom = ""}
  Goodbye {whom = "", sianara = "", ciao = ""}
  Goodbye {whom = "", sianara = "sucker", ciao = ""}
-}
