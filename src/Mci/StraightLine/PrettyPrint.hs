module Mci.StraightLine.PrettyPrint where

import           Data.Foldable           (foldl1)
import qualified Formatting              as F
import           Mci.Prelude
import           Mci.StraightLine.Syntax

prettyStmSL :: Stm -> LText
prettyStmSL (AssignStm (id,expr)) =
  F.format (F.text F.% " := " F.% F.text) (toS id) (prettyExpSL expr)

prettyStmSL (PrintStm expList) = F.format ( "print (" F.% F.text F.% ")") (printParams expList)
    where
      printParams :: [Exp] -> LText
      printParams [e] = prettyExpSL e
      printParams x = foldl1 (F.format ( F.text F.% "," F.% F.text)) $ map prettyExpSL x

prettyStmSL (CompoundStm (s1,s2)) =
  F.format (F.text F.% " ; " F.% F.text ) (prettyStmSL s1) (prettyStmSL s2)


prettyExpSL :: Exp -> LText
prettyExpSL (IdExp val) = toSL val
prettyExpSL (NumExp val) = show val
prettyExpSL (OpExp (expr1,op,expr2)) =
  F.format ( "" F.% F.text F.% F.text F.% F.text ) (prettyExpSL expr1) (prettyBinOpSL op) (prettyExpSL expr2)
prettyExpSL (EseqExp (stm,expr)) =
  F.format ( "( " F.% F.text F.% " , " F.% F.text F.% " ) ") (prettyStmSL stm) (prettyExpSL expr)

prettyBinOpSL :: Binop -> LText
prettyBinOpSL op =
  case op of
    Plus  -> "+"
    Minus -> "-"
    Times -> "*"
    Div   -> "/"

prettyStm :: Stm -> Text
prettyStm = toS . prettyStmSL

prettyBinOp :: Binop -> Text
prettyBinOp = toS . prettyBinOpSL

prettyExp :: Exp -> Text
prettyExp = toS . prettyExpSL
