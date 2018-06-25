# powerpants

### Polynomials

Polynomials are implemented as a map from degree keys to coefficient values.

```haskell
newtype Polynomial a = Px (Map Integer a)
  deriving (Show, Eq, Ord)
```

There are two invariants that need to be enforced. Firstly, there can't be any duplicate keys. This is already taken care of by the data structure. And secondly, there shouldn't be any terms with coefficients equal to zero (e.g., 0x<sup>3</sup>). To eliminate zero values in the map, we export the following function:

```haskell
polynomial = Px . Map.filter (/= 0) . fromListWith (+)
```

The `Polynomial` type itself is opaque. That is to say, the `Px` constructor is not exported. Instead we use the `polynomial` function as a, sort of, proxy. This will ensure that `Polynomial` values always are in this canonical form.

For example, the polynomial 5x<sup>3</sup> + 2x + 7 is created using `polynomial [(3, 5), (1, 2), (0, 7)]`. The order in which these terms appear in the list is irrelevant. Zero terms are ignored. Comparison of two polynomials now agree with our intuitive understanding of what it means for two polynomials to be equal:

```haskell
λ> polynomial [(3, 5), (1, 2), (0, 7)] == polynomial [(0, 7), (1, 2), (2, 0), (3, 5)]
True
```

Keys that appear more than once in the list are simply added together (due to `fromListWith (+)`). For example, consider the equation 5x<sup>3</sup> + x<sup>3</sup> + 2x<sup>3</sup> = 8x<sup>3</sup>. This equality translates to the following code:

```haskell
λ> polynomial [(3, 5), (3, 1), (3, 2)] == polynomial [(3, 8)]
True
```

We can implement some basic building blocks.

```haskell
x = Px (fromList [(1, 1)])            

constant 0 = pzero
constant n = Px (fromList [(0, n)])
```

The zero polynomial is represented by the `empty` map.  

```haskell
pzero = Px empty
```

This is consistent with the idea that the degree of a polynomial is equal to the degree of its highest order monomial (and either undefined or -1 for the zero polynomial). In our implementation, this corresponds to the value of the maximal key in the map. For the `empty` map, this is `undefined`. To be nice to implementations, we use -1 to denote this.

```haskell
degree (Px px) 
  | null px   = -1
  | otherwise = fst (findMax px)
```

The additive inverse of a polynomial is constructed by negating the second component of each element in the list.

```haskell
pneg (Px px) = Px (Map.map negate px)
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
