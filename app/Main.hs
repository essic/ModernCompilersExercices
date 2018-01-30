module Main where

import           Mci.StraightLine.PrettyPrint
import           Mci.StraightLine.Syntax
import           Protolude

prog :: Stm
prog =
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

prog2 :: Stm
prog2 =
  PrintStm [OpExp(NumExp 10,Div, NumExp 2)]

smallTest :: IO ()
smallTest = do
  putText "Evaluate max occurence of print statement in :"
  putText . prettyStm $ prog
  putText "is ..."
  putText . show $ maxargs prog

main :: IO ()
main = interp prog >> interp prog2 >> smallTest
