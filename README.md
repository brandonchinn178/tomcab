# tomcab

Generate a Cabal file from a TOML file. Alternative to `hpack`.

## Install

The recommended approach is to download `stack-tomcab` or `cabal-tomcab` from the releases page. These are drop-in replacements for `stack` or `cabal` that will run `tomcab` automatically before calling the command as usual.

You may also install `tomcab` yourself, in which case you'll need to manually call it to generate the Cabal file.

* Download `tomcab` from the releases page
* Run `stack install tomcab` or `cabal install tomcab`
* `git clone https://github.com/brandonchinn178/tomcab.git` and build from source

## Usage

TODO

## Design

* All fields in Cabal should be configurable _under the same name_
* Helpers may be added to simplify common operations (e.g. the `github` field)
* `tomcab` will do as little validation as possible, instead generating the cabal file and letting Cabal parse and validate it as usual
