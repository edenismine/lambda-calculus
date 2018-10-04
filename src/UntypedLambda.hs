{-
- Lenguajes de Programación 2019-1
- Práctica 3
- Autores:
-   Luis Daniel Aragón Bermúdez
-   Joshua Jair Pedrero Gómez
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo Gerardo González López
-}

module UntypedLambda (
  Identifier,
  Expr,
  frVars,
  lkVars,
  incrVar,
  alphaExpr,
  subst,
  beta,
  locked,
  eval
) where

import Data.List

type Identifier = String

data Expr = Var Identifier | Lam Identifier Expr | App Expr Expr deriving Eq

--Creando una instancia de la clase show.
instance Show Expr where
  show e = case e of
    Var x   -> x
    Lam x e -> "\\"++ x ++ " -> " ++ show e
    App x y -> "("++show x ++ " " ++ show y ++ ")"

--Definiendo el tipo de sustitución.
type Substitution = (Identifier , Expr)

-- | frVars
-- | Función que obtiene el conjunto de variables libres de una expresión.
frVars :: Expr -> [Identifier]
frVars e = error "implementar"

-- | lkVars
-- | Función que obtiene el conjunto de variables de una expresión.
lkVars :: Expr -> [Identifier]
lkVars e = error "Implementar"

-- | incrVar
-- | Función que dado un identificador,
-- | si este no termina en número le agrega el sufijo 1,
-- | en caso contrario toma el valor del número y lo incrementa en 1.
incrVar :: Identifier -> Identifier
incrVar e = error "implementar"

-- | alphaExpr
-- | Función que toma una expresión lambda
-- | y devuelve una α-equivalencia utilizando la función incrVar
-- | hasta encontrar un nombre que no aparezca en el cuerpo.
alphaExpr :: Expr -> Expr
alphaExpr e = error "Implementar"

-- | subst
-- | Función que aplica la sustitución a la expresión dada.
subst :: Expr -> Substitution -> Expr
subst e s = error "Implementar"

--------------------------------------------------
--------------   β-reducción  --------------------
--------------------------------------------------

-- | beta.
-- | Función que aplica un paso de la beta reducción.
beta :: Expr -> Expr
beta e = error "Implementar"

-- | locked.
-- | Función que determina si una expresión está bloqueada,
-- | es decir, no se pueden hacer más beta reducciones.
locked :: Expr -> Bool
locked e = error "Implementar"

-- | eval.
-- | Función que evalúa una expresión lambda
-- | aplicando beta reducciones hasta quedar bloqueada.
eval :: Expr -> Expr
eval e = error "Implementar"

{-  FUNCIONES AUXILIARES  -}
dedup :: Ord a => [a] -> [a]
dedup = map head . group . sort
