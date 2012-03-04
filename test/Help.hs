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

sample [OPTIONS]

 Common flags
  -? --help       Display help message
  -V --version    Print version information

sample hello [OPTIONS]

  -w --whom=ITEM


sample goodbye [OPTIONS]
|]

main :: IO ()
main = do
  print defaultSample
  print defaultHello
  print =<< cmdArgs (modes [defaultSample, defaultHello])

{-
  hello = Hello{whom = def}
  goodbye = Goodbye
  print =<< cmdArgs (modes [hello, goodbye])
  -}
