module Main where

import qualified Formatting                   as F
import           Mci.StraightLine.Interpretor
import           Mci.StraightLine.PrettyPrint
import           Mci.StraightLine.Syntax
import           Protolude

evaluateWithInfo :: Stm -> (Stm -> IO () ) -> IO ()
evaluateWithInfo s interpretProgram = do
  putText "**** Start ****"
  putText . toSL $ F.format ( "-> " F.% F.text) (prettyStmSL s)
  interpretProgram s
  putText "**** End ****"
  pure ()

main :: IO ()
main = do
  evaluateWithInfo prog interp
  evaluateWithInfo prog2 interp
  evaluateWithInfo prog3 interp
  pure ()
  where
    prog =
      CompoundStm
      (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
      (
        CompoundStm
        (AssignStm
         "b"
         (
           EseqExp
           (PrintStm [IdExp"a",OpExp (IdExp "a") Minus (NumExp 1)])
           (OpExp (NumExp 10) Times (IdExp"a"))
         )
        )
        (PrintStm[IdExp "b"])
      )
    prog2 =
      PrintStm [OpExp (NumExp 10) Div (NumExp 2)]
    prog3 =
      CompoundStm
      (
        AssignStm
        "superValue"
        (
          EseqExp
            (PrintStm [IdExp "superValue"])
            (OpExp (NumExp 10) Times (NumExp 10))
        )
      )
      (
        PrintStm [IdExp "superValue"]
      )
