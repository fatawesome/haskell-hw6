{-# LANGUAGE DeriveFunctor #-}

module Lib where

import Data.Maybe (fromMaybe)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Expr a
  = Lit Integer             -- ^ Integer literal
  | Var a                   -- ^ Variable
  | Add (Expr a) (Expr a)   -- ^ Addition
  | Mul (Expr a) (Expr a)   -- ^ Multiplication
  deriving (Show, Functor)

-- / Num instance for nice syntax
instance Num (Expr a) where
  e1 + e2 = Add e1 e2
  e1 * e2 = Mul e1 e2
  fromInteger = Lit

x = Var "x"
y = Var "y"
z = Var "z"

eval :: Expr Integer -> Integer
eval (Lit n) = n
eval (Var n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

display :: Expr String -> String
display (Lit n) = show n
display (Var s) = s
display (Add e1 e2) = display e1 <> " + " <> display e2
display (Mul e1 e2) = "(" <> display e1 <> ") * (" <> display e2 <> ")"

-- / Evaluate an expression using an associative list
-- to lookup variable values and a default value
-- (in case there is no value in associative list)
--
-- >>> let vars = [("x", 2), ("y", 3)]
-- >>> evalWith 0 vars (Add (Var "x") (Var "y"))
-- 5
--
evalWith :: Eq var => Int -> [(var, Int)] -> Expr var -> Int
evalWith fallback [] _ = fallback
evalWith fallback vars expr = fromInteger $ eval (fmap getValue expr)
  where
    getValue key = fromIntegral $ fromMaybe fallback (lookup key vars)

-- / Display an expression using a given
-- display function for variables
displayWith :: (var -> String) -> Expr var -> String
displayWith f expr = display $ fmap f expr

unknown = Var "<unknown>"

getVal 
  :: Eq a 
  => [(a, Expr a)] 
  -> Expr a 
  -> a 
  -> Expr a
getVal list fallback key 
  = fromMaybe fallback (lookup key list) 

expandVars 
  :: Expr String 
  -> [(String, Expr String)] 
  -> Expr String 
  -> Expr String
expandVars fallback vars 
  = fmap (display . getVal vars fallback)
  
-- ^
-- ^ Everything below is not completed. I was tired, sorry :(  
-- ^
  
-- / GExpr :: (* -> *) -> * -> *
data GExpr f a
  = GVar a
  | GOp (f (GExpr f a))
  
data IExpr expr
  = ILit Integer
  | IAdd expr expr
  | IMul expr expr
  deriving (Show, Functor)
  
-- / Convert from simple to generalised expression
fromExpr :: Expr a -> GExpr IExpr a
fromExpr (Lit n)     = GOp (ILit n)
fromExpr (Var n)     = GVar n
fromExpr (Add e1 e2) = GOp (IAdd e1 e2)
fromExpr (Mul e1 e2) = GOp (IMul e1 e2)

-- / Convert from generalised to simple expression
toExpr :: GExpr IExpr a -> Expr a
toExpr (GOp (ILit n))     = Lit n
toExpr (GVar n)           = Var n
toExpr (GOp (IAdd e1 e2)) = Add e1 e2
toExpr (GOp (IMul e1 e2)) = Mul e1 e2
  