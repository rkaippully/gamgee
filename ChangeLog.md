# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).

## [Unreleased]
- Always performing upcase of a token secret #12 (maksar)
- Simplified build #13
- Upgraded to latest LTS

## [1.2.1] - 2020-05-31
- Upgraded to latest stackage LTS

## [1.2.0] - 2019-09-11
- Added `change-password` command

## [1.1.0] - 2019-07-25
- Added an info command to get information about Gamgee installation (https://github.com/rkaippully/gamgee/issues/5)
- Generate build/configuration files via dhall
- Remove polysemy-plugin so that haddock will run :(
- Updated to latest stackage nightly
- Upgraded to latest version of polysemy

## [1.0.0] - 2019-07-16
- Major reimplementation based on polysemy

## [0.2.0] - 2018-06-07

### Added
- Support for secrets in non-canonical format (https://github.com/rkaippully/gamgee/issues/2)

## [0.1.0] - 2018-06-05

### Added
- First version.
- Supports TOTP based tokens
- Secure storage of tokens via AES256 encryption
- Added installation and usage instructions to README
