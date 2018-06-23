# powerpants

### Polynomials

Polynomials (in one variable) are represented as a list of monomials, where each term is given as a degree-coefficient pair.

```haskell
type Polynomial a = [(Integer, a)]
```

For example, the polynomial <i> 5x<sup>3</sup> + 2x + 7 </i> is implemented in list form as:

```haskell
[(3, 5), (1, 2), (0, 7)]
```

We implement some basic building blocks.

```haskell
px1x = [(1, 1)]            
constant n = Px [(0, n)]   -- A constant is a zero-degree monomial.
```

<!-- No assumption should be made about the order of the terms in the list (or?). -->

The zero polynomial is represented by the empty list.

```haskell
pxzero = []
```

### Expressions in one variable

### Symbolic manipulation

### Simplifying expressions

##### Flattening nested nodes

A multiplication or addition node that appears inside a node of the same type can be merged with its parent, since these operators satisfy the associative law. For example, the identity <i> a + b + (c + d + e) = a + b + c + d + e </i> is captured by to the following simplification:

```
   (+)                    (+)
    |                      |
[ a b (+) ]   ===>   [ a b c d e ]
       | 
   [ c d e ]
```

Naturally, flattening can be done recursively.

```
   (+)                      (+)
    |                        |
[ a b (+) ]   ===>   [ a b c d e f g ]
       | 
  [ c (+) g ]
       |
   [ d e f ]
```

```haskell
data Op = MulOp | AddOp deriving (Eq)

flat :: Op -> [Expr a] -> [Expr a]
flat op = rec where
    rec [] = []
    rec (expr:exprs) =
        case (expr, op) of
          (Mul xs, MulOp) -> rec exprs ++ rec xs
          (Add xs, AddOp) -> rec exprs ++ rec xs
          _               -> expr : rec exprs

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
