## Beta reductions

```
(lambda x . x) 2
2

(lambda x . x) (lambda y .y)
[ x := (lambda y . y) ]  ## means lambda y . y will be sub'd for all x
lambda y . y

(lambda x . x) (lambda y . y) z
[ x := lambda y .y ]
(lambda y . y) z
[ y := z ]
z

(lambda x . xy) z
[ x := z ]
zy
```

Combinator -> a lambda expression with no free variables

Divergence -> omega example

(lambda x . xx) (lambda x . xx)
[ x := lambda x . xx ]
(lambda x . xx) (lambda x . xx)
... and so on ...
