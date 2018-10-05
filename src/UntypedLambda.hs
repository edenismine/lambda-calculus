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
  Exp,
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
import Text.Read

type Identifier = String

data Exp = Var Identifier | Lam Identifier Exp | App Exp Exp deriving Eq

--Creando una instancia de la clase show.
instance Show Exp where
  show e = case e of
    Var x   -> x
    Lam x e -> "\\"++ x ++ " -> " ++ show e
    App x y -> "("++ show x ++ " " ++ show y ++ ")"

--Definiendo el tipo de sustitución.
type Substitution = (Identifier , Exp)

-- | frVars
-- | Función que obtiene el conjunto de variables libres de una expresión.
frVars :: Exp -> [Identifier]
frVars = dedup . fvAux [] []

-- | lkVars
-- | Función que obtiene el conjunto de variables ligadas de una expresión.
lkVars :: Exp -> [Identifier]
lkVars = dedup . bvAux []

-- | incrVar
-- | Función que dado un identificador,
-- | si este no termina en número le agrega el sufijo 1,
-- | en caso contrario toma el valor del número y lo incrementa en 1.
incrVar :: Identifier -> Identifier
incrVar var = case ivAux var of
  Left message -> error message
  Right newVar -> newVar

-- | alphaExpr
-- | Función que toma una expresión lambda
-- | y devuelve una α-equivalencia utilizando la función incrVar
-- | hasta encontrar un nombre que no aparezca en el cuerpo.
alphaExpr :: Exp -> Exp
alphaExpr e = error "Implementar"

-- | subst
-- | Función que aplica la sustitución a la expresión dada.
subst :: Exp -> Substitution -> Exp
subst e s = error "Implementar"

--------------------------------------------------
--------------   β-reducción  --------------------
--------------------------------------------------

-- | beta.
-- | Función que aplica un paso de la beta reducción.
beta :: Exp -> Exp
beta e = error "Implementar"

-- | locked.
-- | Función que determina si una expresión está bloqueada,
-- | es decir, no se pueden hacer más beta reducciones.
locked :: Exp -> Bool
locked e = error "Implementar"

-- | eval.
-- | Función que evalúa una expresión lambda
-- | aplicando beta reducciones hasta quedar bloqueada.
eval :: Exp -> Exp
eval e = error "Implementar"

-------------------------------------------------
---------   Funciones Auxiliares  ---------------
-------------------------------------------------

-- |dedup.
-- Esta función remueve duplicados de una lista.
dedup :: Ord a => [a] -> [a]
dedup = union []

-- |fvAux.
fvAux :: [Identifier] -> [Identifier] -> Exp -> [Identifier]
fvAux free bounded exp = case exp of
  Var x   -> if x `elem` bounded then free else x : free
  Lam x e -> fvAux free (x:bounded) e
  App x y -> fvAux free bounded x `union` fvAux free bounded y

-- |bvAux.
bvAux :: [Identifier] -> Exp -> [Identifier]
bvAux bounded exp = case exp of
  Var x   -> bounded
  Lam x e -> bvAux (x:bounded) e
  App x y -> bvAux bounded x `union` bvAux bounded y

-- |ivAux.
ivAux :: Identifier -> Either Identifier String
ivAux var = case break isNum var of
  ([], nums) -> Left "Invalid variable name, it's a digit."
  (letters, []) -> Right (letters ++ "1")
  (letters, nums) -> case readMaybe nums of
    Nothing -> Left "Invalid variable name, found letters after digits"
    Just n -> Right (letters ++ show (n + 1))
  where isNum char = char `elem`  "0123456789"
