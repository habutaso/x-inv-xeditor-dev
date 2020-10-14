# x-inv-xeditor-dev
This project aim to expand conflict resolution for X/Inv/XEditor.

## Development environment
* Debian 10 (buster) on WSL
* haskell stack 2.3.3
* GHC 8.8.4 (lts-16.16)
* HaXml 1.25.5 (insufficient compatibility)

Also, This program is not compatible with `wx`

## run
Excecute the following command in this project root directory.

```
stack ghci --no-load --package containers

Plelude> :load app/X.hs
```
If you want to execute InXmlTest.hs, read `app/InXmlTest.hs` in the REPL.
