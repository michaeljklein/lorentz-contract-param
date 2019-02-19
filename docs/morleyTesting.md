# Morley Property Testing Syntax

Property tests can be defined in a `check` block, which must come after any
`let` block:

```
check {<property-name> ":" <property>}
```

The syntax for `<property>` is as follows, where `<op>` is an instruction, macro
or sequence, `<value>` is a Michelson value, `<type>` is a Michelson type. One
addition is that any `<op>`, `<value>` or `<type>` may be replaced by a `?<string>`
hole which instructs the property checker to use an arbitrary generator instead
of a concrete value or type at that point. A `<stack>` can also be replaced with
a hole.

```
<stack> = '[ <value> :: <type>, ..., <value> :: <type>] | <stack-hole>
<cmp> = "<" | ">" | "==" | "<=" | ">="
<ops'> = {<op> | <op-With-Hole>} | <ops-hole>

<property> = Equals <ops'> <stack> <ops'> <stack> # check equality
           | Gas nat <cmp> <ops'> <stack> # check gas consumption
           | Pure <ops'>  # <ops'> doesn't read from chain or make `operation`s
```

An example:

```
let {
  add3 :: [int, ...] -> [int, ...]
  add3 = push int 3; add;
}

check {
  "Test stack":
    Equals {push int 2; add3;} ?S {push int 2; push int 3; add3;} ?S

  "Test purity":
    Pure {push int 2; add3;}

  "Test gas":
    Gas 200 > add3 '[2]
}
```

The string in `?<string>` is a variable for an implicit universal
quantification. That is, in any property `str1 == str2 => ?str1 == ?str2`.

Some examples:

Checking instructions against a random stack:
```
Equals {push int 2; add 2;} ?S {push int 2; push int 2; add;} ?S
```

Checking arbitrary values in instructions against a known stack:
```
Equals {push int ?A; add 2;} '[2] {push int 2; push int ?A; add;} '[2]
```

Checking arbitrary types in instructions against known values

```
Equals {push ?type 2; add 2;} [2] {push int 2; push int ?A; add;} [2]
```

Properties can also be split across multiple lines:
```
"Test-name": Equals
    {push ?type 2; add 2;} [2]
    {push int 2; push int ?A; add;} [2]
```
