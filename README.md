[![Build Status](https://travis-ci.org/rkaippully/gamgee.svg?branch=master)](https://travis-ci.org/rkaippully/gamgee)
[![Release](https://img.shields.io/github/release/rkaippully/gamgee.svg)](https://github.com/rkaippully/gamgee/releases)
[![Hackage](https://img.shields.io/hackage/v/gamgee.svg)](https://hackage.haskell.org/package/gamgee)

# About
Gamgee is your sidekick for managing multi-factor authentication
tokens. It is a command-line tool that can be used as a drop-in
replacement for Google Authenticator app or any other similar app
providing multi-factor authentication using Time-based One-Time
Password (TOTP) algorithm.

Gamgee implements the TOTP algorithm specified in
[RFC6238](https://tools.ietf.org/html/rfc6238)

# Installation
On MacOS, install using homebrew:

```
brew install rkaippully/tools/gamgee
```

See other releases here:
https://github.com/rkaippully/gamgee/releases/latest

# Usage
Gamgee is a replacement for apps like Google Authenticator or Authy
that are used to generate TOTP tokens. Typically, such tools scan a QR
code to create an account. Gamgee cannot scan a QR code (yet), but you
can instead get a secret token from your service and add that to
Gamgee.

1. Go to the multi-factor authentication setup page of your service
   (such as Github, Gmail, etc).  You will see a QR code in that page
   and instructions to scan it with an app.
2. Look for a link in the page that gives alternative installation
   instructions. For example, many services include a "Can't scan
   barcode?" or "Enter this text code" link next to the QR code.
   Follow that link.
3. Note down the secret code mentioned in the page. Once you get the
   secret, create a token with this command:

```
gamgee add -l "<token-label>" -s "<secret-code>"
```

`token-label` is a convenient label that you choose for this token.

Now you are ready to generate an OTP with your token:

```
gamgee <token-label>
```

This will generate an OTP and copy it to your clipboard. You can paste
it into your authentication service. Alternatively, you can write the
OTP to standard output with:

```
gamgee <token-label> --stdout
```

You can find a list of all your tokens with:

```
gamgee list
```

You can also remove a token if it is no longer needed. Please remember
that this cannot be undone:

```
gamgee delete -l "<token-label>"
```

Polysemy stores the tokens in a configuration file under your home
directory. You can find the location of this file (along with some
other details) by running:

```
gamgee info
```

# License
Gamgee is distributed under Mozilla Public License 2.0. See the file
LICENSE for details.

This Source Code Form is "Incompatible With Secondary Licenses", as
defined by the Mozilla Public License, v. 2.0.
