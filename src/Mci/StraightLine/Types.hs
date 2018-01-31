{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Mci.StraightLine.Types where
import           Mci.Prelude
import qualified Data.Map.Strict as Map


newtype Table = Table { unwrap :: Map.Map Id Int }

type Id = Text

data Binop = Plus | Minus | Times | Div

data Stm = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp = IdExp Id
  | NumExp Int
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp

class Eval p where
  eval :: p -> Int -> Int -> Int

instance Eval Binop where
  eval Plus  = (+)
  eval Minus = (-)
  eval Times = (*)
  eval Div   = div

class HasPrintStatment s where
  printStatmens :: s -> Int

instance HasPrintStatment Stm where
  printStatmens (PrintStm xs) = length xs
  printStatmens (CompoundStm s1 s2) = printStatmens s1 + printStatmens s2
  printStatmens (AssignStm _ e) = printStatmens e

instance HasPrintStatment Exp where
  printStatmens (IdExp _) =  0
  printStatmens (NumExp _) =  0
  printStatmens (OpExp e1 _ e2) = printStatmens e1 + printStatmens e2
  printStatmens (EseqExp s e) = printStatmens s + printStatmens e


-------------------------------------------------------------------------------
--     Next step try to model all this stuff using F-Algebra, Initial Algebra
--  and Fix point of Functor
-------------------------------------------------------------------------------

type  FAlgebra f a = f a -> a

newtype Fix f a = In { unFix :: f (Fix f a)} deriving (Functor, Foldable, Traversable)

cata :: Functor f => (FAlgebra f a) -> Fix f a -> a
cata g = g . fmap (cata g) . unFix

data ExpF a = Ident Id
            | Val a
            | Add a a
            | Sub a a
            | Mult a a
            | Div' a a
            deriving (Functor, Foldable, Traversable)

data StmF a = Compound  a a
  | Assign Id Exp
  | Print [Exp]
  deriving (Functor, Foldable, Traversable)

type Expr = Fix ExpF
type Statment = Fix StmF

