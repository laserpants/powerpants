# powerpants

### Polynomials

Polynomials are implemented as a map from degree keys to coefficient values.

```haskell
newtype Polynomial a = Px (Map Integer a)
```

There are two invariants that need to be enforced. Firstly, there can't be any duplicate keys. This is already taken care of by the data structure. And secondly, there shouldn't be any terms with coefficients equal to zero (e.g., 0x<sup>3</sup>). To eliminate zero values in the map, we export the following function:

```haskell
polynomial = Px . Map.filter (/= 0) . fromListWith (+)
```

The `Polynomial` type itself is opaque. That is to say, we do not export the `Px` constructor itself. Instead we use the `polynomial` function as a constructor. This will ensure that `Polynomial` values always are in this canonical form.

For example, the polynomial 5x<sup>3</sup> + 2x + 7 is created with `polynomial [(3, 5), (1, 2), (0, 7)]`. The order in which these terms appear in the list is irrelevant. Zero terms are ignored. Comparison of two polynomials now agree with our intuitive understanding of what it means for two polynomials to be equal:

```haskell
λ> polynomial [(3, 5), (1, 2), (0, 7)] == polynomial [(0, 7), (1, 2), (2, 0), (3, 5)]
True
```

Keys that appear more than once in the list are simply added together. For example, 5x<sup>3</sup> + x<sup>3</sup> + 2x<sup>3</sup> = 8x<sup>3</sup> 

```haskell
λ> polynomial [(3, 5), (3, 1), (3, 2)] == polynomial [(3, 8)]
True
```



<!--

A polynomial (in one variable) is represented by a list of monomials, in which each term is given as a degree-coefficient pair.

```haskell
newtype Polynomial a = Px [(Integer, a)] 
  deriving (Show, Eq, Ord)
```
For example, the polynomial 5x<sup>3</sup> + 2x + 7 is implemented in list form as:

```haskell
polynomial [(3, 5), (1, 2), (0, 7)]
```

```haskell
λ> polynomial [(3, 5), (1, 2), (0, 7)] == polynomial [(0, 7), (1, 2), (2, 0), (3, 5)]
True
```



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

This is consistent with the idea that the degree of a polynomial is equal to the degree of its highest order monomial. In our implementation, this is the first element's first component in the sorted list of terms. Since the empty list doesn't have any terms, its degree is undefined. 

```haskell
pxdeg [] = -1
pxdeg terms = fst (head (pxsorted terms))
```

The additive inverse of a polynomial is constructed by negating the second component of each element in the list.

```
pxneg = fmap (fmap negate)
```

### Expressions in one variable

### Symbolic algebra

### Simplifying expressions

##### Flattening nested nodes

A multiplication or addition node that appears inside a node of the same type can safely be merged with its parent, since these operators satisfy the associative law. For example, the identity <i> a + b + (c + d + e) = a + b + c + d + e </i> is captured by to the following node tree simplification step:

```
   (+)                    (+)
    |                      |
[ a b (+) ]   ===>   [ a b c d e ]
       | 
   [ c d e ]
```

Flattening can be performed recursively.

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
flat :: Op -> [Expr a] -> [Expr a]
flat op = rec where
    rec [] = []
    rec (expr:exprs) = 
        case expr of
          (Op op' xs) | op == op' -> rec exprs ++ rec xs
          _ -> expr : rec exprs
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

-->
