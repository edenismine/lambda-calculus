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

import           Data.List
import           Text.Read

type Identifier = String

data Exp = Var Identifier | Lam Identifier Exp | App Exp Exp deriving Eq

--Creando una instancia de la clase show.
instance Show Exp where
  show e = case e of
    Var x   -> x
    Lam x exp -> "\\"++ x ++ " -> " ++ show exp
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

-- |alphaExpr.
-- Función que toma una expresión lambda y devuelve una α-equivalencia hasta
-- encontrar un nombre que no aparezca en el cuerpo.
alphaExpr :: Exp -> Exp
alphaExpr exp = case exp of
  Var x -> exp
  App e e' -> App (alphaExpr e) (alphaExpr e')
  Lam x e -> let x' = renameWithContex x (allVars exp) in
    Lam x' (subst e (x, Var x'))

-- |subst.
-- Función que aplica la sustitución a la expresión dada.
subst :: Exp -> Substitution -> Exp
subst exp s@(x, exp') = case exp of
  Var x'
    | x == x' -> exp'
    | otherwise -> exp
  App e e' -> App (subst e s) (subst e' s)
  Lam x' e
    | x' == x -> exp
    | x' `elem` frVars exp' ->
      let newVar = renameWithContex x' (allVars exp ++ allVars exp') in
        subst (Lam newVar (subst e (x', Var newVar))) s
    | otherwise -> Lam x' (subst e s)

--------------------------------------------------
--------------   β-reducción  --------------------
--------------------------------------------------

-- | beta.
-- | Función que aplica un paso de la beta reducción.
beta :: Exp -> Exp
beta = fromMaybe (error "Cannot beta reduce an expression in beta normal form") . safeBeta

-- | locked.
-- | Función que determina si una expresión está bloqueada,
-- | es decir, no se pueden hacer más beta reducciones.
locked :: Exp -> Bool
locked e = case safeBeta e of
  Just _ -> False
  Nothing -> True

-- | eval.
-- | Función que evalúa una expresión lambda
-- | aplicando beta reducciones hasta quedar bloqueada.
eval :: Exp -> Exp
eval= fromMaybe undefined . safeEval

-------------------------------------------------
---------   Funciones Auxiliares  ---------------
-------------------------------------------------

-- |dedup.
-- Esta función remueve duplicados de una lista.
dedup :: Ord a => [a] -> [a]
dedup = union []

-- |fvAux.
-- Función recursiva con acumulador que acumula las variables libres de una
-- expresión dada.
fvAux :: [Identifier] -> [Identifier] -> Exp -> [Identifier]
fvAux free bounded exp = case exp of
  Var x   -> if x `elem` bounded then free else x : free
  Lam x e -> fvAux free (x:bounded) e
  App x y -> fvAux free bounded x `union` fvAux free bounded y

-- |bvAux.
-- Función recursiva con acumulador que acumula las variables ligadas de una
-- expresión dada.
bvAux :: [Identifier] -> Exp -> [Identifier]
bvAux bounded exp = case exp of
  Var x   -> bounded
  Lam x e -> bvAux (x:bounded) e
  App x y -> bvAux bounded x `union` bvAux bounded y

-- |ivAux.
-- Given an identifier, this function attempts to retrieve a new name and if it
-- fails a descriptive error message is returned instead. Right values are
-- valid new identifiers and Left values are error messages.
ivAux :: Identifier -> Either Identifier String
ivAux var = case break isNum var of
  ([], nums) -> Left "Invalid variable name, it's a digit."
  (letters, []) -> Right (letters ++ "1")
  (letters, nums) -> case readMaybe nums of
    Nothing -> Left "Invalid variable name, found letters after digits"
    Just n  -> Right (letters ++ show (n + 1))
  where isNum char = char `elem`  "0123456789"

-- |renameWithContex.
-- Given an identifier and a list of used identifiers (henceforth called
-- context), this function retrieves a new identifier using incrVar until it
-- cannot be found inside the context.
renameWithContex :: Identifier -> [Identifier] -> Identifier
renameWithContex used context
  | newVar `elem` context = renameWithContex newVar (used:context)
  | otherwise = newVar
  where newVar = incrVar used

-- |allVars.
-- Given a lambda expression, this function retrieves all its variables.
allVars :: Exp -> [Identifier]
allVars = dedup . allVarsAux []
  where allVarsAux vars exp = case exp of
          Var x    -> x:vars
          App e e' -> allVarsAux vars e ++ allVarsAux vars e'
          Lam x e  -> allVarsAux (x:vars) e

-- |safeBeta.
-- Given a lambda expression, attempts to beta reduce it. If the expression
-- is not in its beta normal form it's beta reduced and wrapped in Just, else
-- this function yields Nothing.
safeBeta :: Exp -> Maybe Exp
safeBeta exp = case exp of
  Var _ -> Nothing
  Lam x e -> case safeBeta e of
    Just e' -> Just (Lam x e')
    Nothing -> Nothing
  App e1 e2 -> case safeBeta e1 of
    Just e1' -> Just (App e1' e2)
    Nothing -> case safeBeta e2 of
      Just e2' -> Just (App e1 e2')
      Nothing  -> case e1 of
        Lam x e -> Just (subst e (x, e2))
        _       -> Nothing

-- | safeEval.
-- Dada una expresión e, devuelve Just e' si existe e' tal que e ->^{beta}* e'
-- y e' está bloqueado.
safeEval :: Exp -> Maybe Exp
safeEval e = if locked e then Just e else safeEval (beta e)

-- |fromMaybe.
-- Given a default value and a value wrapped inside a maybe type, it returns
-- the wrapped value if its present or the default value if no value is
-- wrapped inside.
fromMaybe :: a -> Maybe a -> a
fromMaybe def maybeVal = case maybeVal of
  Just x -> x
  Nothing -> def
