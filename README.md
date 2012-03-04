A friendlier interface to command line parsing.

You write out the help contents for your command line, and this generates Haskell code for your command line parsing for you.
Basically we are going about command line parsing backwards: normally you use Haskell code first and have that generate a help message.

This library has the downside of requiring TemplateHaskell and being a more magical.
The upside is a DRY and pure (as opposed to the default CmdArgs interface) way to interface with the command line.
It will also be a WYSIWYG was of writing help messages, rather than generating them from code.

I like the CmdArgs technique of using a Record to model the command line.
So currently there is a CmdArgs backend.
However CmdArgs is redundant and limiting as a backend, so there may be a different backend in the future.

See this example: https://github.com/gregwebs/ParseHelp.hs/blob/master/test/Help.hs

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


# Status

It works, as can be seen from the tests. I wouldn't be suprised if the help parser were too fragile for your use case. Let me know.
