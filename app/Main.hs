module Main where

import qualified Formatting                   as F
import           Mci.StraightLine.PrettyPrint
import           Mci.StraightLine.Syntax
import           Protolude
import           Mci.StraightLine.Types
import qualified Data.Map.Strict as Map
import qualified Control.Monad.State.Strict as State

evaluateWithInfo :: Stm -> (Stm -> State.StateT Table IO () ) -> IO ()
evaluateWithInfo s interpretProgram = do
  putText "**** Start ****"
  putText . toSL $ F.format ( "-> " F.% F.text) (prettyStmSL s)
  State.evalStateT (interpretProgram s) $! Table Map.empty
  putText $! "Max Args = " <> (show $! maxargs s)
  putText "**** End ****"

main :: IO ()
main = do
  evaluateWithInfo prog  interpStm
  evaluateWithInfo prog2 interpStm
  evaluateWithInfo prog3 interpStm
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
