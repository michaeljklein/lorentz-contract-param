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

[See `morleySyntaxSugar.md`](./morleySyntaxSugar.md).

## Instruction and type names

In Michelson, all letters in all instructions must be CAPITAL and all letters in types and type constructors must be lowercase.
In Morley, one can also use lowercase letters in instructions.
Types and type constructors can start with a capital letter (like in Haskell).

## Macros and Type synonyms

[See `morleyMacros.md`](./morleyMacros.md).

## Extended NOP Instructions

[See `morleyInstructions.md`](./morleyInstructions.md).

## Let-block Definitions

[See `morleyLet.md`](./morleyLet.md).
