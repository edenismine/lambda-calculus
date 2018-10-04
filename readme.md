Introducción
------------

A continuación se definen

Ejecución
---------

Utilizando cabal de @cabal es sencillo cargar los ejemplos usando el
siguiente comando:

```
$ cabal repl
```

Y una vez en la REPL, puede correr cada uno de los ejemplos o todos
usando el `main`:

```
*Examples> main
```

A continuación se muestran los ejemplos disponibles.

```haskell
------------------------------
---------- EJEMPLOS ----------
------------------------------

```

Implementación
--------------

Se incluye una implementación de la semántica del cálculo lambda
sin tipos (**$\lambda$**) cuyo tipo de Haskell puede verse a
continuación.

```haskell
data Expr = Var Identifier | Lam Identifier Expr | App Expr Expr deriving Eq
```


Bibliografía
------------
