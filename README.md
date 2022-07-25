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

In the TOML format, all indentation is optional, but the format that will be used in this guide is recommended to optimize readability.

With some exceptions, all fields should be either a String or a list of String, which will be set in the Cabal field verbatim (with lists concatenated with commas). Some fields are handled specially, with different functionality or syntax:

* `cabal-version`: will be set to `1.12` unless explicitly specified

* `build-type`: will be set to `Simple` unless explicitly specified

* `exposed-modules`/`other-modules`: These fields support glob patterns, which automatically search through `hs-source-dirs` and include any found module in the closest matching pattern.
    * `*` must ONLY be at the end; e.g. `Foo.*`
    * If both `Foo.*` and `Foo.Bar` are specified, `Foo` will match `Foo.*`, `Foo.Bar` will match `Foo.Bar`, and `Foo.Bar.Baz` will match `Foo.*`.

* `build-depends`: Can either be a list of strings as normal, or a table mapping library to version constraints. For example, the following are all equivalent:

    ```toml
    [[library]]
        build-depends = [
            "base > 4",
            "text"
        ]

    [[library]]
        [library.build-depends]
        base = "> 4"
        text = ""

    # inline table instead of table section
    [[library]]
        build-depends = { base = "> 4", text = "" }

    # nested keys instead of table
    [[library]]
        build-depends.base = "> 4"
        build-depends.text = ""
    ```

* `if`: If-statements can be added to the top-level or any of the `library`/`executable`/`test-suite` sections. Since a section can have multiple if-statements, it's represented as a list.

    * For simple if-statements, you can just specify `condition` and the fields as normal:

    ```toml
    [[library]]
        [[library.if]]
            condition = "impl(ghc > 8.0)"
            build-depends = [...]
            ghc-options = [...]
    ```

    * You can always use this more formal syntax, which supports both `elif` and `else`:

    ```toml
    [[library]]
        [[library.if]]
            condition = "impl(ghc > 8.0)"
            then.build-depends = [...]
            then.ghc-options = [...]

            # optional
            else.build-depends = [...]
            else.ghc-options = [...]

        [[library.if]]
            condition = "impl(ghc > 8.0)"

            [library.if.then]
            build-depends = [...]
            ghc-options = [...]

            # optional
            [[library.if.elif]]
            condition = "impl(ghc > 8.4)"
            build-depends = [...]
            ghc-options = [...]

            # optional
            [[library.if.elif]]
            condition = "impl(ghc > 8.6)"
            build-depends = [...]
            ghc-options = [...]

            # optional
            [library.if.else]
            build-depends = [...]
            ghc-options = [...]
    ```

* `common` stanzas are explicitly inlined into the Cabal file, to support older Cabal versions, as well as making the semantics of some of these special fields sensical. For example, setting `other-modules` to glob patterns in a common stanza will resolve `other-modules` in the context of each stanza the common stanza is imported in.

Additionally, there are a few extra fields that are supported by `tomcab`:

| Field         | Description |
|---------------|-------------|
| `extends`     | TODO        |
| `auto-import` | TODO        |
| `github`      | TODO        |

## Design

* All fields in Cabal should be configurable _under the same name_
* Helpers may be added to simplify common operations (e.g. the `github` field)
* `tomcab` will do as little validation as possible, instead generating the cabal file and letting Cabal parse and validate it as usual
