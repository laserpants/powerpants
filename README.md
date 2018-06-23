# powerpants


### Simplifying expressions

##### Flatten nested nodes

A multiplication or addition node that appear inside a node of its own type can be merged with the parent node, since these operators satisfy the associative law. For example, the identity <i> a + b + (c + d + e) = a + b + c + d + e </i> is captured by to the following simplifaction:

```
   (+)                    (+)
    |                      |
[ a b (+) ]   ===>   [ a b c d e ]
       | 
   [ c d e ]
```



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
