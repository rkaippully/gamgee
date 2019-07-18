let Step = < Imports : { imports : { align : Text } }
           | Whitespace : { trailing_whitespace : {} } >

in

{ steps =
  [
    -- There are different ways we can align names and lists.
    --
    -- - global: Align the import names and import list throughout the entire
    --   file.
    --
    -- - file: Like global, but don't add padding when there are no qualified
    --   imports in the file.
    --
    -- - group: Only align the imports per group (a group is formed by adjacent
    --   import lines).
    --
    -- - none: Do not perform any alignment.
    --
    -- Default: global.
    Step.Imports { imports = { align = "file" } }

    -- Remove trailing whitespace
    , Step.Whitespace { trailing_whitespace = {=} }
  ]

    -- A common setting is the number of columns (parts of) code will be wrapped
    -- to. Different steps take this into account. Default: 80.
  , columns = 100

    -- By default, line endings are converted according to the OS. You can override
    -- preferred format here.
    --
    -- - native: Native newline format. CRLF on Windows, LF on other OSes.
    --
    -- - lf: Convert to LF ("\n").
    --
    -- - crlf: Convert to CRLF ("\r\n").
    --
    -- Default: native.
  , newline = "lf"

    -- Sometimes, language extensions are specified in a cabal file or from the
    -- command line instead of using language pragmas in the file. stylish-haskell
    -- needs to be aware of these, so it can parse the file correctly.
    --
    -- No language extensions are enabled by default.
  , language_extensions = ./default-extensions.dhall
}
