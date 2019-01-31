# Testing a Michelson contract

In this document we aim to come up with strategies to test Michelson contracts.
First, we outline the important details of Michelson contract design and execution environment.
Second, we show what kind of static analysis is possible for an arbitrary contract and (or for some restricted versions of Michelson language).
Third, we present various strategies for testing and necessary tooling should be given to a user.

In the context of this document we define *unit testing* as testing a single execution of contract (not considering execution of oprations and their effect).
We define *integrational testing* as testing an execution of contract along with all effects caused by execution of operations.
We define *lifecycle testing* as testing assumptions about contract's state within the whole lifecycle of a contract.

## Design of Michelson contracts and their properties

Michelson language is a stack-based language with nested constructions (hence no **goto**s).
It allows do define contract as a single program, no instrument for implementing subprogramms is given.

### Control flow expressiveness

Michelson has support for potentially infinite loops.
Loops are restricted to preserve the stack type.

This is an important restriction which seems to disable general kind of recursion
and leaves only tail recursion possible.

(**TODO**: provide a more formal argument)

**TODO** experiment whether it's possible to store lambda in storage.

Lambdas are rather limited and do not allow partial application.
So we can not call lambda from current stack in the newly constructed lambda.

### Environment

This is a comprehensive list of data which contract may access during its run:

* Parameter (passed to contract in transaction launching it)
* Contract's storage
  * Private to the contract (may not be accessed by other contracts)
* Current amount of mutez of the current contract (via `BALANCE`)
* Mapping from existing contracts' addresses to their executable representation
  * This is implicit and not described in spec, but is necessary as address is only a hash of contract
  * This mapping is accessed via `CONTRACT p` instruction
* Contract that initiated the current transaction
  * I.e. the contract that paid the fees and storage cost, and whose manager signed the operation that was sent on the blockchain
  * Accessed via `SOURCE` instruction
* Contract that initiated the current internal transaction (via `SENDER`)
* Contract's self representation as data (via `SELF`)
* Amount of current transaction (via `AMOUNT`)
* Time of block creation (for block which included transaction, via `NOW`)
* Remaining steps (via `STEPS_TO_QUOTA`)

All of the data (except for remaining steps) is constant within contract execution.

By calling convention, contract returns a pair `(storage', operations)` of new storage value and operations to be executed.
This is the only way contract may influence the outer world, no side-effects in between.

### Operations

Contract returns a list of operations, which are side effects executed after the contract returned.
Operations may trigger execution of other contracts and these contracts in turn produce even more operations.

Execution starts with origin transaction.
A queue of operations is initiated and a single operation `TRANSFER_TOKENS` is put into it.
Execution processes operations in the queue one by one and when an operations executes a contract,
operations returned by the contract execution are added to queue.
We stress that this is a *queue* and not *stack*, meaning that elements are added and taken from queue in FIFO order.

(**TODO** run an experiment to validate this claim).

### Communication between contracts

A contract *C₁* can not anyhow influence other contracts except by calling them.
I.e. other contracts do not depend on C₁'s storage, balance or any other data that is being accessed by C₁ during its execution.

The only way *C₁* is able to share any of the data with other contracts is by calling them and passing some data as parameter.

Any contract can be called given its address. Code of contract can not be manipulated or generated at runtime.

Address is a contract's hash and thus for calling other contract, *C₁* has to be provided with other contract's address either
hardcoded in contract definition or passed as parameter.

Ability to execute the contract which address is determined from runtime parameters means that in general case
we can not deduce all the code which will be executed during transaction processing.

## Unit testing

First, we need a convenient way to split contract's code to smaller modular units which later can be severely tested.
This can be done by extending a language with ability to define named sections (similar to the format used by most ASM representations)
and use these named sections from the code.

Example:

```

calculateValue:
  # top of stack: `x`
  # second element on stack: `y`
  # calculates `x²-y³+12` and pushes it to stack

  DUP; MUL; SWAP; DUP; DUP; MUL; MUL; SWAP; SUB;
  PUSH int 12;
  ADD;


storage int;
parameter int;

code {
   DUP;
   CAR;
   SWAP;
   CDR;
   $calculateValue
   NIL operation; PAIR;
};

```

Second, we need to encode Michelson's data types in Haskell.
Haskell representations shall be fully equivalent to data types (so there is a bijection between Haskell and Michelson types).
This would allow us to use QuickCheck generators for these types;

The strategy for unit testing is as follows: each test is targeted at a certain named section.
Generators are used to make up an input stack value, then contract is executed and the output value is checked to have desired properties.

As a simplification, we may come up with a special format for test vector testing, something like following:

```
BALANCE=100
AMOUNT=10
(...)

calculateValue:
  (12 : 13 : S) -> (-2041 : S)
  (12 : 3 : S) -> (129 : S)

```

As can be noted from example above, along with stack values we also need to define values of all variables contract execution depends on (which is not too many though).

Note, that developing both named section extension and test vector format is an easy task as soon as we have parser for Michelson grammar.

