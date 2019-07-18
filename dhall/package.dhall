----------------------------------------------------------------------------------------------------
-- Generate an hpack package.yaml file from dhall
----------------------------------------------------------------------------------------------------

let package-info =
    { name               = "gamgee"
    , version            = "1.0.0"
    , synopsis           = "Tool for generating TOTP MFA tokens."
    , description        = "Tool for generating TOTP MFA tokens. Please see the README on GitHub at <https://github.com/rkaippully/gamgee#readme>"
    , category           = "Authentication, Command Line"
    , homepage           = "https://github.com/rkaippully/gamgee#readme"
    , bug-reports        = "https://github.com/rkaippully/gamgee/issues"
    , author             = "Raghu Kaippully"
    , maintainer         = "rkaippully@gmail.com"
    , copyright          = "2018 Raghu Kaippully"
    , license            = "MPL-2.0"
    , license-file       = "LICENSE"
    , build-type         = "Simple"
    , extra-source-files = [ "ChangeLog.md"
                           , "README.md"
                           , "test/data/golden/getOTPTest.txt"
                           ]
    , github = "rkaippully/gamgee"
    }

let common-fields =
    { ghc-options       = [ "-Wall"
                          , "-Wcompat"
                          , "-Wredundant-constraints"
                          , "-Wincomplete-record-updates"
                          , "-Wincomplete-uni-patterns"
                          ]
    , default-extensions = ./default-extensions.dhall
    , dependencies       = [ "aeson"
                           , "base >=4.12.0.0 && <4.13"
                           , "relude"
                           , "text"
                           , "time"
                           , "polysemy"
                           ]
    }

let library =
    { library =
      { source-dirs     = "src"
      , dependencies    = [ "base64-bytestring"
                          , "bytestring"
                          , "cryptonite"
                          , "memory"
                          , "safe-exceptions"
                          ]
      , exposed-modules = [ "Gamgee.Operation"
                          , "Gamgee.Token"
                          , "Gamgee.Effects"
                          , "Gamgee.Effects.Error"
                          , "Gamgee.Effects.Crypto"
                          , "Gamgee.Effects.CryptoRandom"
                          , "Gamgee.Effects.SecretInput"
                          , "Gamgee.Effects.TOTP"
                          , "Gamgee.Effects.JSONStore"
                          , "Gamgee.Effects.ByteStore"
                          ]
      }
    }

let executable =
    { executable =
      { main         = "Main.hs"
      , source-dirs  = "app"
      , dependencies = [ "gamgee"
                       , "optparse-applicative"
                       , "safe-exceptions"
                       , "directory"
                       , "filepath"
                       , "unix"
                       , "Hclip"
                       ]
      , ghc-options  = [ "-threaded"
                       , "-rtsopts"
                       , "-with-rtsopts=-N"
                       ]
      }
    }

let tests =
    { tests =
      { gamgee-test =
        { main         = "Main.hs"
        , source-dirs  = "test"
        , dependencies = [ "gamgee"
                         , "bytestring"
                         , "memory"
                         , "tasty"
                         , "tasty-golden"
                         , "tasty-quickcheck"
                         , "QuickCheck"
                         , "quickcheck-instances"
                         , "filepath"
                         , "cryptonite"
                         ]
        , ghc-options  = [ "-threaded"
                         , "-rtsopts"
                         , "-with-rtsopts=-N"
                         ]
        }
      }
    }

in package-info // common-fields // library // executable // tests
