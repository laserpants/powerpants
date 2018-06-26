# powerpants

### Polynomials

Polynomials are implemented as a map (`Data.Map.Strict.Map`) from degree keys to coefficient values. The `Polynomial a` type is parameterized by the type of the coefficients. The implementation requires `a` to be an instance of the `Eq` and `Algebra.Ring.C` type classes. This latter constraint is especially useful, since the set of polynomials with coefficients from some ring **R** itself forms a ring, usually denoted **R**[x]. The degree of a term is a non-negative integer, so we also introduce a new `Nat` type for this purpose, using an alias for `Number.NonNegative.Integer` (defined in [Numeric Prelude](http://hackage.haskell.org/package/numeric-prelude)).

```haskell
import Data.Map.Strict ( Map )

type Nat = Number.NonNegative.Integer

newtype Polynomial a = Px { terms :: Map Nat a }
  deriving (Show, Eq, Ord)
```

Our API should allow for `Polynomial` values to be created from a sparse list of degree-coefficient pairs. To make things run smoothly, there are two invariants that need to be enforced. Firstly, there mustn't be any duplicate keys. This is already taken care of by the data structure. By using `fromListWith (+)` to create the map, values of those keys that appear more than once in the list are added together. Secondly, there shouldn't be any terms with coefficients equal to zero (i.e., things like 0x<sup>3</sup>). To eliminate zero coefficients, we can use the `filter` function in `Data.Map.Strict`. Here is the function we end up with:

```haskell
polynomial :: (Ord a, Algebra.Ring.C a) => [(Nat, a)] -> Polynomial a
polynomial = Px . Map.filter (/= 0) . fromListWith (+)
```

The `Polynomial` type itself is opaque. That is to say, the `Px` constructor is not exported. Instead we use the `polynomial` function as a &ndash; sort of &ndash; proxy for `Px`. This will ensure that `Polynomial` values always appear in this canonical form.

For example, the polynomial 5x<sup>3</sup> + 2x + 7 is created using `polynomial [(3, 5), (1, 2), (0, 7)]`. The order in which these terms appear in the list is irrelevant. Zero terms are ignored, and duplicate keys are added together. Comparison of two polynomials now agrees with our intuitive understanding of what it means for polynomials to be equal:

```haskell
λ> polynomial [(3, 5), (1, 2), (0, 7)] == polynomial [(0, 7), (1, 2), (2, 0), (3, 5)]
True
```

The equality 5x<sup>3</sup> + x<sup>3</sup> + 2x<sup>3</sup> = 8x<sup>3</sup> translates to the following code:

```haskell
λ> polynomial [(3, 5), (3, 1), (3, 2)] == polynomial [(3, 8)]
True
```

We can now implement some basic building blocks.

```haskell
mono d c = Px (singleton d c) -- Create a monomial

constant 0 = zero
constant n = mono 0 n

x = mono 1 1
```

The zero polynomial is represented by the `empty` map.  

```haskell
zero = Px empty
```

This is consistent with the idea that the degree of a polynomial is equal to the degree of its highest order monomial (and  undefined for the zero polynomial). In our implementation, this corresponds to the value of the maximal key in the map. For the `empty` map, this is `undefined`. To be nice, we can wrap the result in a `Maybe` type and return `Nothing` for the zero polynomial.

```haskell
degree (Px px) 
  | null px   = Nothing
  | otherwise = Just (fst (findMax px))
```

The additive inverse of a polynomial is constructed by negating all values in the map.

```haskell
neg = Px . Map.map negate . terms
```

### Expressions in one variable

### Symbolic algebra

### Simplifying expressions

##### Flattening nested nodes

A multiplication or addition node that appears inside a node of the same type can safely be merged with its parent, since these operators satisfy the associative law. For example, the identity a + b + (c + d + e) = a + b + c + d + e is captured by to the following node tree simplification step:

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
