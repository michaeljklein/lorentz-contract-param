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

Coming soon, see TM-58.

## IV: Testing EDSL

Coming soon, see TM-77.

## Running and building

Morley executable provides following functionality:
- `parse` contract and return its representation in haskell types.
- `typecheck` contract.
- `run` contract. Given contract is being originated first and then transaction is being sent to it
- `originate` contract.
- `transfer` tokens to given address.
- `print` produce `.tz` contract that can be parsed by the OCaml referenced client from `.mtz` or `.tz` contract.

You can get more info about this command by running `morley <command> --help`

There are two ways to get morley executable:
- [Docker](https://docs.docker.com/) based (preferable). Get [script](/scripts/morley.sh)
 (e. g. using `curl https://gitlab.com/camlcase-dev/morley/raw/master/scripts/morley.sh > morley.sh`)
  and run it `./morley.sh <args>`. This script will pull docker image that contains latest version of morley executable from master branch and run it with given arguments.
  Usage example:
  `./morley.sh` to see help message
  `./morley.sh run --contract add1.tz --storage 1 --parameter 1 --amount 1`
- [Stack](https://docs.haskellstack.org/en/stable/README/) based. Clone this git repository and run `stack build` command,
  after that you can do `stack exec -- morley <args>` to run morley executable built from source code.
  Usage example:
  `stack exec -- morley --help` to see help message
  `stack exec -- morley originate --contract contracts/add1.tz --storage 1 --verbose`

For more information about morley commands check out following docs:
- [interpreter doc](/docs/morleyInterpreter.md)
- [typechecker doc](/docs/morleyTypechecker.md)

## III: Running and building
Morley executable provides following functionality:
- `parse` contract and return its representation in haskell types.
- `typecheck` contract.
- `run` contract. Given contract is being originated first and then transaction is being sent to it
- `originate` contract.
- `transfer` tokens to given address.

You can get more info about this command by running `morley <command> --help`

There are two ways to get morley executable:
- [Docker](https://docs.docker.com/) based (preferable). Get [script](/scripts/morley.sh)
 (e. g. using `curl https://gitlab.com/camlcase-dev/morley/raw/master/scripts/morley.sh > morley.sh`)
  and run it `./morley.sh <args>`. This script will pull docker image that contains latest version of morley executable from master branch and run it with given arguments.
  Usage example:
  `./morley.sh` to see help message
  `./morley.sh run --contract add1.tz --storage 1 --parameter 1 --amount 1`
- [Stack](https://docs.haskellstack.org/en/stable/README/) based. Clone this git repository and run `stack build` command,
  after that you can do `stack exec -- morley <args>` to run morley executable built from source code.
  Usage example:
  `stack exec -- morley --help` to see help message
  `stack exec -- morley originate --contract contracts/add1.tz --storage 1 --verbose`

For more information about morley commands check out following docs:
- [interpreter doc](/docs/morleyInterpreter.md)
- [typechecker doc](/docs/morleyTypechecker.md)

## Issue Tracker

We use [YouTrack](https://issues.serokell.io/issues/TM) as our issue
tracker. You can login using your GitHub account to leave a comment or
create a new issue.

## For Contributors

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.
