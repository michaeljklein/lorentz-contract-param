# Morley Language

The Morley Language is a low-level syntactic sugar over the core Michelson
instructions (core Michelson being the instructions which are actually executed
in the Tezos blockchain, as opposed to the various syntactic conveniences
provided by the OCaml reference client)

The general principle is that any syntactically valid core Michelson expression
will also be a valid Morley expression, i.e. Morley is a superset of
Michelson. Any language extensions that break this principle must be explicitly
enabled

## Pairs

### Type Syntax
`pair` types may be written using Haskell-style tuples:

```
(a, b) ~ (pair a b)
(a, b) :t %f ~ (pair :t %f a b)
```

When tuples are nested, parenthesis may be omitted:

```
(a,b,c) ~ (a,(b,c))
```

But if so, only the outer pair may be annotated:

```
(a, b, c) :t %f ~ (a, (b, c)) :t %f
```

Inner types may be annotated as usual:

```
(a :ta %fa, b :tb %fb, c :tc %fc) ~ (a :ta %fa, (b :tb %fb, c :tc %fc))
```

### Value Syntax

`Pair` values may also be written with tuples:

```
(a, b) ~ (Pair a b)
(a, b, c) ~ (a,(b,c))
```

## Unions

### Type Syntax

`or` types may be written using the `|` character: 

```
(or a b) ~ (a | b)
(or :t %f a b) ~ (a | b) :t %f
```

When bars are nested, parenthesis may be omitted:

```
(a | b | c) ~ (a | (b | c))
```

Annotations follow the same pattern as Tuples:
```
(a | b | c) :t %f ~ (a | (b | c)) :t %f
(a :ta %fa | b :tb %fb | c :tc %fc) ~ (a :ta %fa | (b :tb %fb | c :tc %fc))
```

## Unit

The `unit` type may be written as a `0`-tuple

```
unit ~ ()
```

The `Unit` value may also be written this way:

```
Unit ~ ()
```

## Lambda: 

They `lambda` type may be written:

```
(lambda a b) ~ (\ a -> b)
```

## Containers

The `list` and `set` types may be written:

```
(list a) ~ [a]
(set a) ~ {a}
```


## Instruction syntax:

Instructions and macros may be written in lower case:

```
DROP ~ drop
```


## Stack Signature

```
Empty stack: '[]
stack of three int: '[int, int, int]
stack function: 'S -> 'X
```

Capital letter variables in stack signature. Equal letters are equal types

```
(lambda a b) ~ (\ a -> b)
(lambda a b) ~ ('[a] -> '[b])
```

## Macros

**Pair subtree**:
```
tree 0 ~ NOP;
tree 1 ~ NOP;
tree 2 ~ CAR;
tree 3 ~ CDR;
tree n ~ if n % 2 == 0 then get (n/2); CAR else get (n/2); CDR;
```

### Dependent macros

```
empty :: 'S -> '[]
const :: 'a -> 'S -> 'a:'[]
const t n = empty; push t n
```

## Records

A record type is syntactic sugar for a tuple with field annotations
```
{ f0 :: 't0, ..., fn :: 'tn} ~ ('t0 %f0, ..., 'tn %fn)
```

Record fields can be accessed with `field`:

```
field tk %fk :: {'f0 :: t0, ..., 'fn :: tn}:'S -> option tk:'S
```

Union branches can also be accessed with `field`

```
field tk %fk :: (t0 %f0 | ... | tn %fn):'S -> option %k tk:'S
```

## Custom Macros/Expressions

User defined macros/instruction sequences can be defined in a `.mtz` file
outside of the `code`, `parameter` and `storage` blocks

```
add3 :: int:'S -> int:'S
add3 = push int 3; add;
````

Custom macros can also be parameterized on concrete types with 
`#check <test-name> <property>`:

```
#check {push int 2; add 2;} 'S == {push int 2; push int 2; add;} 'S
add :: int -> int:'S -> int:'S
add n = push int 3; add;
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
#check {push ?type 2; add 2;} '[2] == {push int 2; push int ?A; add;} '[2]
```

Properties can also be split across multiple lines:
```
#check "Test-name" 
#    {push ?type 2; add 2;}          '[2]
# == {push int 2; push int ?A; add;} '[2]
```

### Property testing

```
add :: int -> int:'S -> int:'S
add n = push int 3; add;
```

## Breaking language extensions
By enabling `#pragma -XMainMethod`, the `code`, `parameter` and `storage` blocks
can be replaced with:

```
main :: ('parameter, 'storage):'[] -> ('[operation], 'storage)
main = .. #code goes here
```


## Inline Testing

TBD
