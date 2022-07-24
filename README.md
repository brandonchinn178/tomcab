# tomcab

Generate a Cabal file from a TOML file. Alternative to `hpack`.

## Install

The recommended approach is to download `stack-tomcab` or `cabal-tomcab` from the releases page. These are drop-in replacements for `stack` or `cabal` that will run `tomcab` automatically before calling the command as usual.

You may also install `tomcab` yourself, in which case you'll need to manually call it to generate the Cabal file.

* Download `tomcab` from the releases page
* Run `stack install tomcab` or `cabal install tomcab`
* `git clone https://github.com/brandonchinn178/tomcab.git` and build from source

## Usage

Here's an example `package.toml` file:

```toml
# TODO
```

All of the normal Cabal fields are represented by the exact same names, and are propagated through transparently. In the TOML format, all indentation is optional, but the above format is recommended to optimize readability.

Additionally, there are a few extra fields that are supported by `tomcab`:

| Field         | Description |
|---------------|-------------|
| `extends`     | TODO        |
| `auto-import` | TODO        |
| `github`      | TODO        |

Most fields are simply represented as a String, but fields that take in a list of Strings must be a list; e.g. you can't simply do `hs-source-dirs = "src"`, you have to do `hs-source-dirs = ["src"]`.

Here is a list of some notable fields with different functionality or syntax:

* `cabal-version`: will be set to `1.12` unless explicitly specified

* `exposed-modules`/`other-modules`: These fields support glob patterns, which automatically search through `hs-source-dirs` and include any found module in the closest matching pattern.
    * `*` must ONLY be at the end; e.g. `Foo.*`
    * If both `Foo.*` and `Foo.Bar` are specified, `Foo` will match `Foo.*`, `Foo.Bar` will match `Foo.Bar`, and `Foo.Bar.Baz` will match `Foo.*`.

* `build-depends`: TODO

* `if`: TODO

## Design

* All fields in Cabal should be configurable _under the same name_
* Helpers may be added to simplify common operations (e.g. the `github` field)
* `tomcab` will do as little validation as possible, instead generating the cabal file and letting Cabal parse and validate it as usual
