# powerpants

### Polynomials

Polynomials (in one variable) are represented as a list of monomials, given as a degree-coefficient pairs.

```
type Polynomial a = [(Integer, a)]
```

For example, the polynomial <i> 5x<sup>3</sup> + 2x + 7 </i> is implemented in list form as:

```
[(3, 5), (1, 2), (0, 7)]
```

No assumption should be made about the order of the terms in the list.

The zero polynomial is represented by the empty list.

```
pxzero = []
```

### Expressions in one variable

### Simplifying expressions

##### Flatten nested nodes

A multiplication or addition node that appears inside a node of its own type can be merged with its parent, since these operators satisfy the associative law. For example, the identity <i> a + b + (c + d + e) = a + b + c + d + e </i> is captured by to the following simplification:

```
   (+)                    (+)
    |                      |
[ a b (+) ]   ===>   [ a b c d e ]
       | 
   [ c d e ]
```

###### Recursively

Flattening can be done recursively.

```
   (+)                      (+)
    |                        |
[ a b (+) ]   ===>   [ a b c d e f g ]
       | 
  [ c (+) g ]
       |
   [ d e f ]
```

##### Identities

```
   (+)
    |   ===>   0
   [ ]
```

```
   (×)
    |   ===>   1
   [ ]
```

```
   (+)
    |   ===>   a
  [ a ]
```

```
   (×)
    |   ===>   a
  [ a ]
```
