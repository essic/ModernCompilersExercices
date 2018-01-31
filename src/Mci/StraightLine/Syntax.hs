module Mci.StraightLine.Syntax where

import           Mci.Prelude

type Id = Text

data Binop = Plus | Minus | Times | Div

data Stm = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp = IdExp Id
  | NumExp Int
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp
