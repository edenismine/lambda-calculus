## Introducción

### Descripción del programa

El programa es la implementación del cálculo lambda. El cálculo lambda
$\textbf(\lambda)$ consiste de tres términos y todas las combinaciones
recursivas válidas de estos mismos.

- Var - Una variable.

- Lam - Una abstracción.

- App - Una aplicación.

### Sintáxis $\textbf(\lambda)$

Se definen las expresiones del cálculo lambda del siguiente modo:

```haskell
type Identifier = String

data Exp = Var Identifier
           | Lam Identifier Exp
           | App Exp Exp
```

Nosotros denotaremos la lambda por la diagonal (\$, el cuerpo con
(-\>), y la aplicación con espacio encerrando las expresiones en un
paréntesis ((e1 e2)). Las variables serán nombradas únicamente con
caracteres alfanuméricos (la parte que es un número deberá ser precedida
de la parte alfabética).

Se creó una instancia de la clase `Show` para las expresiones lambda.

```haskell
instance Show Exp where
  show e = case e of
    Var x   -> x
    Lam x e -> "\\"++ x ++ " -> " ++ show e
    App x y -> "("++show x ++ " " ++ show y ++ ")"
```

## Ejecución

Utilizando cabal de @cabal es sencillo cargar los ejemplos usando el
siguiente comando:

    $ cabal repl

Y una vez en la REPL, puede correr cada una de las funciones en el
paquete.

## Implementación

### Sustitución y $\alpha$-equivalencia

#### Sustitución

La evaluación de un término lambda $(\lambda x\text{.}\epsilon)a$ consiste en
sustituir todas las ocurrencias libres de $x$ en $\epsilon$ por el
argumento $a$. A este paso de la sustitución se le llama reducción. La
sustitución se denota como $[x:=a]$ y se define del siguiente
modo:

$$x [x : = a] = a$$

$$y [x:=a] = y \succ x \neq y$$

$$e e'[x:=a]=(e[x:=a])(e'[x:=a])$$

$$\lambda x\text{.} e[x:=a]=\lambda x\text{.} e$$

$$\lambda y\text{.} e [x:=a] = \lambda y\text{.} (e[x:=a])\text{ }si\text{ } x \neq y \land y \notin frVars(a)$$

Definimos el tipo de sustitución:

```haskell
type Substitution = (Identifier, Exp)
```

#### $\alpha$-equivalencia

La alfa equivalencia es la propiedad de cambiar la variable ligada,
junto con todas sus ocurrencias libres dentro del cuerpo sin cambiar el
significado de la expresión.

$$\lambda x\text{.} e \equiv^\alpha \lambda y\text{.} (e[x:=y])$$

En esta parte se implementaron las siguientes funciones:

```haskell
-- | frVars
-- | Función que obtiene el conjunto de variables libres de una expresión.
frVars :: Exp -> [Identifier]

-- | lkVars
-- | Función que obtiene el conjunto de variables ligadas de una expresión.
lkVars :: Exp -> [Identifier]

-- | incrVar
-- | Función que dado un identificador,
-- | si este no termina en número le agrega el sufijo 1,
-- | en caso contrario toma el valor del número y lo incrementa en 1.
incrVar :: Identifier -> Identifier

-- |alphaExpr.
-- Función que toma una expresión lambda y devuelve una alfa-equivalencia hasta
-- encontrar un nombre que no aparezca en el cuerpo.
alphaExpr :: Exp -> Exp

-- |subst.
-- Función que aplica la sustitución a la expresión dada.
subst :: Exp -> Substitution -> Exp
```

### $\beta$-reducción

La beta reducción es simplemente un paso de sustitución remplazando la
variable ligada por una expresión lambda por el argumento de la
aplicación.

$$(\lambda x\text{.} a)y \rightarrow^\beta a[x:=y]$$

### Evaluación

La estrategia de evaluación de una expresión consistirá en aplicar la
beta reducción hasta que ya no sea posible, usando las siguientes
reglas:

$$\infer[(Lam)]{\lambda x\text{.} t \rightarrow \lambda x\text{.} t'}{t \rightarrow t'}$$

$$\infer[(App1)]{t_1 t_2 \rightarrow t'_1 t_2}{t_1 \rightarrow t'_1}$$

$$\infer[(App2)]{(\lambda x\text{.} t) t_1 \rightarrow (\lambda x\text{.} t)t'_1}{t_1 \rightarrow t'_1}$$

$$\infer[(Beta)]{(\lambda x\text{.} t)y \rightarrow^\beta t[x:=y]}{}$$

Y se implementaron las siguientes funciones:

```haskell
--------------------------------------------------
--------------   beta-reducción  -----------------
--------------------------------------------------

-- | beta.
-- | Función que aplica un paso de la beta reducción.
beta :: Exp -> Exp

-- | locked.
-- | Función que determina si una expresión esta bloqueada,
-- | es decir, no se pueden hacer mas beta reducciones.
locked :: Exp -> Bool

-- | eval.
-- | Función que evalúa una expresión lambda
-- | aplicando beta reducciones hasta quedar bloqueada.
eval :: Exp -> Exp
```

## Bibliografía
