module PrettyPrinter
  ( printTerm  ,     -- pretty printer para terminos
    printType        -- pretty printer para tipos
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c) = sep
  [ parensIf (not (isAtom i || isApp i)) (pp ii vs i) -- <-- Si fuera \x. t :@: y, necesita poner (\x.t) y ya que sino \x.t y no se sabe si es aplicacion o la misma abs 
  , nest 1 (parensIf (not (isAtom c)) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let t u) =
  text "let "
    <> text (vs !! ii)
    <> text " = "
    <> pp ii vs t
    <> text " in "
    <> pp (ii + 1) vs u
pp _  _  Zero = text "0"
pp ii vs (Suc t) = text "suc " <> parensIf (not (isAtom t)) (pp ii vs t)
pp ii vs (Rec t1 t2 t3) =
  sep [ text "R"
      , nest 1 (parensIf (not (isAtom t1)) (pp ii vs t1))
      , nest 1 (parensIf (not (isAtom t2)) (pp ii vs t2))
      , nest 1 (parensIf (not (isAtom t3)) (pp ii vs t3)) ]

pp ii vs (RL t1 t2 t3) =
  sep [ text "RL"
      , nest 1 (parensIf (not (isAtom t1)) (pp ii vs t1))
      , nest 1 (parensIf (not (isAtom t2)) (pp ii vs t2))
      , nest 1 (parensIf (not (isAtom t3)) (pp ii vs t3)) ]
pp ii vs (Cons t1 xs) =
  sep [ text "cons"
      , nest 1 (parensIf(not (isAtom t1)) (pp ii vs t1))
      , nest 1 (parensIf(not (isAtom xs)) (pp ii vs xs))]
pp _ _ Nil = text "nil"


isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType NatT = text "Nat"
printType ListT = text "List Nat"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]

isAtom :: Term -> Bool
isAtom (Bound _) = True
isAtom (Free _)  = True
isAtom Zero      = True
isAtom Nil       = True
isAtom _         = False

isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let t   u       ) = fv t ++ fv u
fv (Suc t)            = fv t
fv (Rec t1 t2 t3    ) = fv t1 ++ fv t2 ++ fv t3
fv (Zero)             = []
fv (RL t1 t2 t3)      = fv t1 ++ fv t2 ++ fv t3
fv (Cons t xs)        = fv t ++ fv xs
fv (Nil)              = []


---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

