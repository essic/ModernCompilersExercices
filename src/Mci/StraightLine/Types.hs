{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Mci.StraightLine.Types where
import  Mci.Prelude
import qualified Data.Map.Strict as Map


newtype Table = Table { unwrap :: Map.Map Id Int }

type Id = Text

data Binop = Plus | Minus | Times | Div'

data Stm = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp = IdExp Id
  | NumExp Int
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp


type  FAlgebra f a = f a -> a

newtype Fix f a = In { unFix :: f (Fix f a)} deriving (Functor, Foldable, Traversable)

cata :: Functor f => (FAlgebra f a) -> Fix f a -> a
cata g = g . fmap (cata g) . unFix

data ExpF a = Ident Id
            | Val a
            | Add a a
            | Sub a a
            | Mult a a
            | Div a a
            deriving (Functor, Foldable, Traversable)

data StmF a = Compound  a a
  | Assign Id Exp
  | Print [Exp]
  deriving (Functor, Foldable, Traversable)

type Expr = Fix ExpF
type Statment = Fix StmF
