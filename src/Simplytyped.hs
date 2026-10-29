module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------


{-
data Type = EmptyT | FunT Type Type
data LamTerm = LVar String | LAbs String Type LamTerm | LApp LamTerm LamTerm

data Term = Bound Int
          | Free Name
          | Term :@: Term
          | Lam Type Term
-}

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion term = conversionaux term [] 

conversionaux :: LamTerm -> [String] -> Term
conversionaux (LVar s) xs = case elemIndex s xs of 
                              Just i -> Bound i 
                              Nothing -> Free (Global s) -- Name = Global string 
conversionaux (LAbs s t l) xs = Lam t (conversionaux l (s:xs)) 
conversionaux (LApp l r) xs =  conversionaux l xs :@: conversionaux r xs
conversionaux (LLet x t1 t2) xs = Let (conversionaux t1 xs) (conversionaux t2 (x:xs))



----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let t1 t2) = Let (sub i t t1) (sub (i + 1) t t2)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f

-- evalúa un término en un entorno dado
eval :: NameEnv Value Type -> Term -> Value
eval nvs (Free name)          = case lookup name nvs of 
                                    Just (v, _) -> v  
eval nvs (Lam t term)         = VLam t term 
eval nvs (t1 :@: t2 )         = case eval nvs t1 of
                                    VLam _ term ->
                                      let v2 = eval nvs t2
                                      in  eval nvs (sub 0 (quote v2) term)
eval nvs (Let t1 t2)          = let v1 = eval nvs t1
                                in  eval nvs (sub 0 (quote v1) t2) 
--[(name, (v, t))]
----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t u) = infer' c e t >>= \tt -> infer' (tt : c) e u


