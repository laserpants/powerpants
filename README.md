# powerpants


### Simplifying expressions

##### Flatten nested addition nodes

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
   [ c d (+) ]
          |
      [ e f g ]
```

##### Flatten nested multiplication nodes

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
