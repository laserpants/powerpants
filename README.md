# powerpants


### Simplifying expressions

##### Flatten nested nodes

A multiplication or addition node that appear inside a node of the same type can be merged with its parent, since these operators satisfy the associative law. For example, the identity <i> a + b + (c + d + e) = a + b + c + d + e </i> is captured by to the following simplification:

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

