[![Build Status](https://travis-ci.org/rkaippully/gamgee.svg?branch=master)](https://travis-ci.org/rkaippully/gamgee)
[![Release](https://img.shields.io/github/release/rkaippully/gamgee.svg)](https://github.com/rkaippully/gamgee/releases)
[![Hackage](https://img.shields.io/hackage/v/gamgee.svg)](https://hackage.haskell.org/package/gamgee)
[![Stackage LTS](http://stackage.org/package/gamgee/badge/lts)](http://stackage.org/lts/package/gamgee)

# About
Gamgee is your sidekick for managing multi-factor authentication tokens. It is a command-line tool
that can be used as a drop-in replacement for Google Authenticator app.

Gamgee implements the TOTP algorithm specified in [RFC6238](https://tools.ietf.org/html/rfc6238)

# License
Gamgee is distributed under Mozilla Public License 2.0. See the file LICENSE for details.

This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla
Public License, v. 2.0.

# Installation
On MacOS, install using homebrew:

```
brew install rkaippully/tools/gamgee
```

# Usage
Gamgee is a replacement for Google Authenticator app or any other similar app providing multi-factor
authentication. Typically, such tools scan a QR code to create an account. Gamgee cannot scan a QR
code, but you can instead get a secret token from your service and add that to Gamgee.

Once you get the secret, create a token with this command:

```
gamgee add -l <token-label> -s <secret>
```

`token-label` is a convenient label that you choose for this token.

You can find a list of all your tokens with:

```
gamgee list
```

You can also remove a token if it is no longer needed. Please remember that this cannot be undone:

```
gamgee delete -l <token-label>
```

Finally, you can generate an OTP for authenticating with your token:

```
gamgee <token-label>
```

This will generate an OTP and copy it to your clipboard. You can paste it into your authentication
service. Alternatively, you can write the OTP to stdout with:

```
gamgee <token-label> --stdout
```
