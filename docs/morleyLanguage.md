<!--

© 2019 Tocqueville Group

SPDX-License-Identifier: AGPL-3.0-or-later

-->


# Morley Language

The Morley Language is a low-level syntactic sugar over the core Michelson
instructions (core Michelson being the instructions which are actually executed
in the Tezos blockchain, as opposed to the various syntactic conveniences
provided by the OCaml reference client)

The general principle is that any syntactically valid core Michelson expression
will also be a valid Morley expression, i.e. Morley is a superset of
Michelson. Any language extensions that break this principle must be explicitly
enabled

## Syntax Sugar

[See `morleySyntaxSugar.md`](./morleySyntaxSugar.md)

## Extended NOP Instructions

[See `morleyInstructions.md`](./morleyInstructions.md)

## Let-block Definitions

[See `morleyLet.md`](./morleyLet.md)
