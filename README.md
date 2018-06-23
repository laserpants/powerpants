# powerpants


### Simplifying expressions

##### Flatten nested nodes

Multiplication and addition nodes that appear inside a node of its own type can be flattnend, since both of these operators satisfy the associative law.

```
   (+)                    (+)
    |                      |
[ a b (+) ]   ===>   [ a b c d e ]
       | 
   [ c d e ]
```

<i> a + b + (c + d + e) = a + b + c + d + e </i>

###### Recursively

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
