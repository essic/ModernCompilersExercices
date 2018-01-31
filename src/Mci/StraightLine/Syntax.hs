{-# LANGUAGE LambdaCase    #-}
module Mci.StraightLine.Syntax where

import           Data.Foldable
import           Mci.Prelude

type Id = Text

data Binop = Plus | Minus | Times | Div

data Stm = CompoundStm (Stm,Stm)
  | AssignStm (Id,Exp)
  | PrintStm [Exp]

data Exp = IdExp Id
  | NumExp Int
  | OpExp (Exp, Binop, Exp)
  | EseqExp (Stm, Exp)

maxargs :: Stm -> Int
maxargs statement =
  foldl max 0 $ getListOfAllPrintStmExp statement
  where
    getListOfAllPrintStmExp :: Stm -> [Int]
    getListOfAllPrintStmExp =
      \case
        AssignStm (_,e') -> getListOfAllPrintStmExpFromExp e'
        PrintStm xs' -> [length xs']
        CompoundStm (s1',s2') -> getListOfAllPrintStmExp s1' ++ getListOfAllPrintStmExp s2'
    getListOfAllPrintStmExpFromExp :: Exp -> [Int]
    getListOfAllPrintStmExpFromExp =
      \case
        OpExp (e1', _, e2') -> getListOfAllPrintStmExpFromExp e1' ++ getListOfAllPrintStmExpFromExp e2'
        EseqExp (s1', e1') -> getListOfAllPrintStmExp s1' ++ getListOfAllPrintStmExpFromExp e1'
        _ -> [0]

 

