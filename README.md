# powerpants

### Polynomials

A polynomial (in one variable) is represented by a list of monomials, in which each term is given as a degree-coefficient pair.

```haskell
type Polynomial a = [(Integer, a)]
```

For example, the polynomial <i> 5x<sup>3</sup> + 2x + 7 </i> is implemented in list form as:

```haskell
[(3, 5), (1, 2), (0, 7)]
```

It is convenient to work with these lists in sorted order, with the leading term first.

```haskell
pxsorted = sortBy (flip compare `on` fst)
```

We can implement some basic building blocks.

```haskell
px1x = [(1, 1)]            
pxconst n = [(0, n)]    -- A constant is a zero-degree monomial.
```

The zero polynomial is represented by the empty list.  

```haskell
pxzero = []
```
This is consistent with the idea that the degree of a polynomial is eqaul to the degree of its highest order monomial. In our implementation, this is the first element's first component in the sorted list of terms. Since the empty list doesn't have any terms, its degree is undefined.

```haskell
pxdeg [] = -1
pxdeg terms = fst (head (pxsorted terms))
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

##### Misc.

```
     (+)              (+)
      |      ===>      |
  [ a 0 b ]         [ a b ]
```

```
     (×)              (×)
      |      ===>      |
  [ a 1 b ]         [ a b ]
```

##### Zero product

```
     (×)         
      |      ===>   0
  [ a 0 b ]     
```
