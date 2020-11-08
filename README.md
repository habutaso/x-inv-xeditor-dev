# x-inv-xeditor-dev
This project aim to expand conflict resolution for X/Inv/XEditor.

## Development environment
* Debian 10 (buster) on WSL
* haskell stack 2.3.3
* GHC 8.8.4 (lts-16.16)

This program is not compatible with `wx`

## Run
### Bidirectional Transformation
You can run X by using the following commands.

```
stack ghci --no-load

Prelude> :load app/X.hs
```
If you want to execute InXmlTest.hs, load `app/InXmlTest.hs` in the REPL.

## Operational Transformation

```
stack ghci --no-load

Prelude> :load ignore/OtTest.hs
```
