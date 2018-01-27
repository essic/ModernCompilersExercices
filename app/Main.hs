module Main where

import Protolude
import Ch01.Introduction

prog :: Stm
prog =
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

main :: IO ()
main = do
  putText "Evaluate max occurence of print statement in :"
  putText . show $ prog
  putText "is ..."
  putText . show $ maxargs prog
