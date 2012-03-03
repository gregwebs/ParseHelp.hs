{-# LANGUAGE QuasiQuotes #-}
import System.Console.CmdArgs.FromHelp
import System.Console.CmdArgs.Explicit

main :: IO ()
main = print =<< processArgs [cmdArgsHelp|
The sample program

sample [OPTIONS]

 Common flags
  -? --help       Display help message
  -V --version    Print version information

sample hello [OPTIONS]

  -w --whom=ITEM

sample goodbye [OPTIONS]
|]

{-
  hello = Hello{whom = def}
  goodbye = Goodbye
  print =<< cmdArgs (modes [hello, goodbye])
  -}
