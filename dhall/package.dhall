----------------------------------------------------------------------------------------------------
-- Generate an hpack package.yaml file from dhall
----------------------------------------------------------------------------------------------------

let package-info =
      { name =
          "gamgee"
      , version =
          "1.2.1"
      , synopsis =
          "Tool for generating TOTP MFA tokens."
      , description =
          "Tool for generating TOTP MFA tokens. Please see the README on GitHub at <https://github.com/rkaippully/gamgee#readme>"
      , category =
          "Authentication, Command Line"
      , homepage =
          "https://github.com/rkaippully/gamgee#readme"
      , bug-reports =
          "https://github.com/rkaippully/gamgee/issues"
      , author =
          "Raghu Kaippully"
      , maintainer =
          "rkaippully@gmail.com"
      , copyright =
          "2018 Raghu Kaippully"
      , license =
          "MPL-2.0"
      , license-file =
          "LICENSE"
      , build-type =
          "Simple"
      , extra-source-files =
          [ "ChangeLog.md", "README.md", "test/data/golden/getOTPTest.txt" ]
      , github =
          "rkaippully/gamgee"
      }

let common-fields =
      { ghc-options =
          [ "-Wall"
          , "-Wcompat"
          , "-Wredundant-constraints"
          , "-Wincomplete-record-updates"
          , "-Wincomplete-uni-patterns"
          ]
      , default-extensions =
          ./default-extensions.dhall
      , dependencies =
          [ "aeson >=1.4.4.0 && <1.5"
          , "base >=4.13.0.0 && <4.14"
          , "relude >=0.6.0.0 && <0.7"
          , "text >=1.2.3.1 && <1.3"
          , "time >=1.9.3 && <1.10"
          , "polysemy >=1.2.3.0 && <1.3"
          ]
      }

let library =
      { library =
          { source-dirs =
              "src"
          , dependencies =
              [ "base64-bytestring >=1.0.0.2 && <1.1"
              , "bytestring >=0.10.8.2 && <0.11"
              , "cryptonite >=0.26 && <0.27"
              , "memory >=0.15.0 && <0.16"
              , "safe-exceptions >=0.1.7.0 && <0.2"
              ]
          , exposed-modules =
              [ "Gamgee.Operation"
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
          { main =
              "Main.hs"
          , source-dirs =
              "app"
          , dependencies =
              [ "gamgee"
              , "optparse-applicative >=0.15.1.0 && <0.16"
              , "safe-exceptions >=0.1.7.0 && <0.2"
              , "directory >=1.3.3.0 && <1.4"
              , "filepath >=1.4.2.1 && <1.5"
              , "unix >=2.7.2.2 && <2.8"
              , "Hclip ==3.0.0.4"
              ]
          , ghc-options =
              [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
          }
      }

let tests =
      { tests =
          { gamgee-test =
              { main =
                  "Main.hs"
              , source-dirs =
                  "test"
              , dependencies =
                  [ "gamgee"
                  , "bytestring >=0.10.8.2 && <0.11"
                  , "memory >=0.15.0 && <0.16"
                  , "tasty >=1.2.3 && <1.3"
                  , "tasty-golden >=2.3.2 && <2.4"
                  , "tasty-quickcheck >=0.10.1 && <0.11"
                  , "QuickCheck >=2.13.2 && <2.14"
                  , "quickcheck-instances >=0.3.22 && <0.4"
                  , "filepath >=1.4.2.1 && <1.5"
                  , "cryptonite >=0.26 && <0.27"
                  ]
              , ghc-options =
                  [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
              }
          }
      }

in  package-info // common-fields // library // executable // tests
