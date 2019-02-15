# Morley Language


# Syntax Sugar for types

| type | Sugar                                   | Desugar                                  |
|------|-----------------------------------------|------------------------------------------|
| pair | `(a ,b)`                                | `(pair a b)`                             |
| pair | `(a, b) :t %f`                          | `(pair :t %f a b)`                       |
| pair | `(a,b,c)`                               | `(a,(b,c))`                              |
| pair | `(a, b, c) :t %f`                       | `(a, (b, c)) :t %f`                      |
| pair | `(a :ta %fa, b :tb %fb, c :tc %fc)`     | `(a :ta %fa, (b :tb %fb, c :tc %fc))`    |
| or   | `(or a b)`                              | `(a \| b)`                               |
| or   | `(or :t %f a b)`                        | `(a \| b) :t %f`                         |
| or   | `(a \| b \| c)`                         | `(a \| (b \| c))`                        |
| or   | `(a \| b \| c) :t %f`                   | `(a \| (b \| c)) :t %f`                  |
| or   | `(a :ta %fa \| b :tb %fb \| c :tc %fc)` | `(a :ta %fa \| (b :tb %fb \| c :tc %fc))` |

