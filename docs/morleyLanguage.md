# Morley Language

The Morley Language is a low-level syntactic sugar over the core Michelson
instructions (core Michelson being the instructions which are actually executed
in the Tezos blockchain, as opposed to the various syntactic conveniences
provided by the OCaml reference client)

The general principle is that any syntactically valid core Michelson expression
will also be a valid Morley expression, i.e. Morley is a superset of
Michelson. Any language extensions that break this principle must be explicitly
enabled. 

The filename extension of a Morley Language file is `.mtz`.

## Syntax sugar

See `docs/morleySyntaxSugar.md`

## Instruction syntax:

Instructions and macros may be written in lower case:

```
DROP ~ drop
```

## Stack Signature

A stack signature can be either an empty stack, a list of types, or a pattern
match on the head of the stack:

```
Empty stack: '[]
stack of three int: '[int, int, int]
A pattern match on a stack with two int at the top: '[int, int, ...]
```

More formally, a stack signature is like a `cons` list with two distinct
`nil`-like terminators:

```
<stack-sig> := "'[" <empty-stack> | <rest-of-stack> | <stack-cons> 
<empty-stack> := "]"
<rest-of-stack> := "...]"
<stack-cons> := <type> (("," (<stack-cons> | <stack-rest)) | <empty-stack>)
```

Stack signatures are used in custom macro definitions and inline assertions

## Macros/Expressions

In addition to the built-in macros defined in the Michelson specification,
Morley allows the programmer to define their own custom macros.

A macros may be defined by including the following syntax outside of the `code`,
`parameter` and `storage` blocks:

```
<macro> := <macro-name> "::" <stack-sig> "->" <stack-sig> \n
           <macro-name> "=" <instructions>
```

As a concrete example:

```
add3 :: '[int, ...] -> '[int, ...]
add3 = {push int 3; add;}
```

The first line of the macro declaration is the type signature, which denotes the
stack transformation the macro performs.

Crucially, if both input and output stack types contain a `<rest-of-stack>`
pattern match (syntactically `...]`), then the stack type captured by both
pattern matches must be identical.

For example, the type signature of 

```
add3 :: '[int, ...] -> [int, ...]
add3 = {push int 3; add;}
```

would be written using the type notation from the Michelson specification as:

```
add3 :: int : 'S -> int : 'S
```

meaning that the pattern match must be universally quantified over the same
stack-type `'S`.

Furthermore, type signatures can also have universally quantified type
parameters, which must be declared in a `forall`:

For instance, the type of the primitive `SWAP` instruction could be notated as:

```
swap :: forall a b. [a, b, ...] -> [b, a, ...]
```

## Directives

Morley supports the following interpreter directives:

```
<directive> := <check> | <import> | <pragma>
<import> := "#import" <filepath>
<check> := "#check" <property>
<pragma> := "#pragma" <pragma>
```

Directives must appear at the beginning of a line.

### Definition importing

```
<import> := "#import" <filepath>
```

`#import` allows for macro definitions in other files to be brought into scope.

### Property Testing

`#check <test-name> <property>`:

```
#check {push int 2; add';} ?S == {push int 2; push int 3; add';} ?S
add3 :: [int, ...] -> [int, ...]
add3 = push int 3; add;
```

### Property syntax:

The syntax for `<property>` is as follows, where `<op>` is an instruction, macro
or sequence, `<value>` is a Michelson value, `<type>` is a Michelson type. One 
addition that any `<op>`, `<value>` or `<type>` may be replaced by a `?<string>`
hole which instructs the property checker to use an arbitrary generator instead
of a concrete value or type at that point. A `<stack>` can also be replaced with
a hole.

The string in `?<string>` is a variable for an implicit universal
quantification. That is, in any property `str1 == str2 => ?str1 == ?str2`.

```
<stack> = '[ <value> :: <type>, ..., <value> <type>]
<cmp> = < | > | == | <= | >=

<property> = <op> <stack> == <op> <stack> # check equality
           | gas(<op> <stack>) <cmp> nat # check gas consumption
           | pure <op>  # <op> doesn't read from chain or make `operation`s
```

Some examples:

Checking instructions against a random stack:
```
#check {push int 2; add 2;} ?S == {push int 2; push int 2; add;} ?S
```

Checking arbitrary values in instructions against a known stack:
```
#check {push int ?A; add 2;} '[2] == {push int 2; push int ?A; add;} '[2]
```

Checking arbitrary types in instructions against known values

```
#check {push ?type 2; add 2;} [2] == {push int 2; push int ?A; add;} [2]
```

Properties can also be split across multiple lines:
```
#check "Test-name" 
#    {push ?type 2; add 2;}          [2]
# == {push int 2; push int ?A; add;} [2]
```

### Assertions/Predicates

An inline assertion is a labeled sequence of instructions that runs in parallel
to the main sequence for testing purposes. That is, an assertion has no actual
effect on the program, but can run tests on intermediate stack states.

For example, suppose we want to verify that the sum of two numbers is greater
than 10:

```
sumIsGreaterThan10 :: '[int, int] -> '[bool]
sumIsGreaterThan10 = {add; push int 10; compare}

parameter unit;
storage unit;
code { DROP;
       PUSH int 2; 
       PUSH int 10;
       #- Test1 "%[0] + %[1] > 10" {sumIsGreaterThan10;} -#;
       DROP; UNIT; NIL operation; PAIR; };

```

The syntax:

```
#- Test1 "%[0] + %[1] > 10" {sumIsGreaterThan10;} -#;
```

is identical in effect to  a `NOOP`, but runs the macro `sumIsGreaterThan10` on
the stack state at its location.

In the above, `Test1` is the assertion name, `"%[0] + %[1] > 10"` is a
comment to be printed during execution. The syntax `%[0]` is a reference into
the stack and prints the `n`-th stack element from the head.

### Pragmas
TBD

## Breaking language extensions
By enabling `#pragma -XContractMain`, the `code`, `parameter` and `storage` blocks
can be replaced with:

```
main :: ('parameter, 'storage):'[] -> ('[operation], 'storage)
main = .. #code goes here
```

## Inline Testing

TBD

