{-# LANGUAGE LambdaCase #-}
module Mci.StraightLine.PrettyPrint where

import           Data.Foldable           (foldl1)
import qualified Formatting              as F
import           Mci.Prelude
import           Mci.StraightLine.Syntax

prettyStmSL :: Stm -> LText
prettyStmSL (AssignStm lbl expr) =
  F.format (F.text F.% " := " F.% F.text) (toLText lbl) (prettyExpSL expr)

prettyStmSL (PrintStm expList) = F.format ( "print (" F.% F.text F.% ")") (printParams expList)
    where
      printParams :: [Exp] -> LText
      printParams [e] = prettyExpSL e
      printParams x = foldl1 (F.format ( F.text F.% "," F.% F.text)) $ map prettyExpSL x

prettyStmSL (CompoundStm s1 s2) =
  F.format (F.text F.% " ; " F.% F.text ) (prettyStmSL s1) (prettyStmSL s2)


prettyExpSL :: Exp -> LText
prettyExpSL (IdExp val) = toLText val
prettyExpSL (NumExp val) = show val
prettyExpSL (OpExp expr1 op expr2) =
  F.format ( "" F.% F.text F.% F.text F.% F.text ) (prettyExpSL expr1) (prettyBinOpSL op) (prettyExpSL expr2)
prettyExpSL (EseqExp stm expr) =
  F.format ( "( " F.% F.text F.% " , " F.% F.text F.% " ) ") (prettyStmSL stm) (prettyExpSL expr)

prettyBinOpSL :: Binop -> LText
prettyBinOpSL =
  \case
    Plus  -> "+"
    Minus -> "-"
    Times -> "*"
    Div   -> "/"

prettyStm :: Stm -> Text
prettyStm = toText . prettyStmSL

prettyBinOp :: Binop -> Text
prettyBinOp = toText . prettyBinOpSL

prettyExp :: Exp -> Text
prettyExp = toText . prettyExpSL
