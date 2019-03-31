# Morley: Developer tools for the Michelson Language

Morley is a library to make writing smart contracts in Michelson pleasant and
effective.

## I: A reimplementation of the Michelson Language in Haskell

It consists of the following parts:

- `Tezos.*` hierarchy is designed to implement cryptographic primitives, string and byte formats, and any other functionality specific to the Tezos protocol which is required for testing/execution of Michelson contracts, but is used not only by Michelson.
- `Michelson.Untyped` and `Michelson.Typed` hierarchies define Haskell data types that assemble a Michelson contract. See [michelsonTypes.md](/docs/michelsonTypes.md).
- `Michelson.TypeCheck`: A typechecker that validates Michelson contracts according to Michelson's typing rules. Essentially it performs conversion from untyped representation to the typed one. See [morleyTypechecker.md](/docs/morleyTypechecker.md).
- `Michelson.Intepreter`: An intepreter for Michelson contracts which doesn't perform any side effects. See [morleyInterpreter.md](/docs/morleyInterpreter.md).
- `Morley.Types`: Types for macros, syntactic sugar and other extensions described in the next chapter.
- `Morley.Parser` A parser to turn a `.tz` or `.mtz` file (`.mtz` is a Michelson contract with Morley extensions) into a Haskell ADT.
- `Morley.Runtime`: A high-level interface to Morley functionality, see [morleyRuntime.md](/docs/morleyRuntime.md).

## II: Morley extensions

The Morley Language is a superset of the Michelson language, which means that each Michelson contract is also a valid Morley contract but not vice versa.
There are several extensions which make it more convenient to write Michelson contracts and test them.
See [the document](/docs/morleyLanguage.md) about these extensions.
Also there is a transpiler from Morley to Michelson.

## III: Morley-to-Michelson transpiler

Morley-to-Michelson transpiler can be used to produce a Michelson contract from Morley contract.
You should use it if you want to develop contracts in Morley and submit them to Tezos network.
Workflow is the following:

1. If your contract is called `foo.mtz`, use `morley print --contract foo.mtz > foo.tz`. Note that normally you should not use `morley` directly, you should use `morley.sh` or `stack exec -- morley`. See usage instructions below.
2. After that you can use existing Tezos tools to deploy your contract. You can also typecheck or interpreter it using reference implementation. If you are not familiar with Tezos tooling, please read [Tezos documentation](http://tezos.gitlab.io/zeronet/index.html) or [Michelson tutoriral](https://gitlab.com/camlcase-dev/michelson-tutorial).

## IV: Testing EDSL

Coming soon, see TM-77.

## Issue Tracker

We use [YouTrack](https://issues.serokell.io/issues/TM) as our issue
tracker. You can login using your GitHub account to leave a comment or
create a new issue.

## For Contributors

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.
