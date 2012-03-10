A friendlier interface to command line parsing.

# Synopsis

See [this example](https://github.com/gregwebs/ParseHelp.hs/blob/master/test/Help.hs)

Here is a condensed version:

```
mkCmdArgs [fromHelp|
  The sample program

sample [COMMAND] ... [OPTIONS]

sample hello [OPTIONS]

  -t --top  Top of the Morning to you!

sample goodbye [OPTIONS]

  -w --whom=GUY
  -s --sianara
  -c --ciao
|]

main = print =<< cmdArgs (modes [defaultHello, defaultGoodbye])
```
The end result is a data structure where each field represents a long flag.
Running with `goodbye -s sucker` results in:

    Goodbye {whom = "", sianara = "sucker", ciao = ""}


# Explanation

You write out the help contents for your command line, and this generates Haskell code for your command line parsing for you.
Basically we are going about command line parsing backwards: normally you use Haskell code first and have that generate a help message.

This library has the downside of requiring TemplateHaskell and being a more magical.
The upside is a DRY and pure (as opposed to the default CmdArgs interface) way to interface with the command line.
It is also a WYSIWYG was of writing help messages, rather than generating them from code.

I like the CmdArgs technique of using a Record to model the command line.
So currently there is a CmdArgs backend.
However CmdArgs has redundant functionality and is limiting as a backend, so there may be a different backend in the future.
Note that there is no package dependency on cmdargs. There is only a data-default dependency. You call cmdargs functions in your own code.

[Spitting out help](https://github.com/gregwebs/ParseHelp.hs/blob/master/test/Help.hs#L41)
will be improved in the future.


# Status

Alpha quality. This project needs some more effort to take it to the next level.
The parser is more fragile than you would like and features are missing.
