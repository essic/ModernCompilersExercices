module Ch01.Introduction (
  Id
  ,Binop (..)
  ,Stm (..)
  ,Exp (..)
  ,maxargs
)
  where

import           Lib.Prelude
import qualified GHC.Show as S
import qualified Formatting as F
import Data.Foldable

type Id = Text

data Binop = Plus | Minus | Times | Div

instance Show Binop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"

data Stm = CompoundStm (Stm,Stm)
  | AssignStm (Id,Exp)
  | PrintStm [Exp]

instance S.Show Stm where
  show (AssignStm (v, e)) = toS $ F.format (F.text F.% " := " F.% F.text) (toS v) (show e)
  show (PrintStm xe) = toS $ F.format ( "print (" F.% F.text F.% ")") (showArgs xe)
    where
      showArgs :: [Exp] -> LText
      showArgs [e] = show e
      showArgs x = foldl1 (F.format ( F.text F.% "," F.% F.text)) $ map (toSL . S.show) x
  show (CompoundStm (s1,s2)) =
    toS $ F.format (F.text F.% " ; " F.% F.text ) (show s1) (show s2)

data Exp = IdExp Id
  | NumExp Int
  | OpExp (Exp, Binop, Exp)
  | EseqExp (Stm, Exp)

instance S.Show Exp where
  show (IdExp v) = toS v
  show (NumExp v) = show v
  show (OpExp (v1,op,v2)) = toS $ F.format ( "" F.% F.text F.% F.text F.% F.text ) (toSL $ S.show v1) (toSL $ S.show op) (toSL $ S.show v2)
  show (EseqExp (s,e)) = toS $ F.format ( "( " F.% F.text F.% " , " F.% F.text F.% " ) ") (toSL $ S.show s) (toSL $ S.show e)

maxargs :: Stm -> Int
maxargs statement =
  foldl max 0 $ getListOfAllPrintStmExp statement
  where
    getListOfAllPrintStmExp :: Stm -> [Int]
    getListOfAllPrintStmExp s =
      case s of
        AssignStm (_,e') -> getListOfAllPrintStmExpFromExp e'
        PrintStm xs' -> [length xs']
        CompoundStm (s1',s2') -> count
          [getListOfAllPrintStmExp s1', getListOfAllPrintStmExp s2']
    getListOfAllPrintStmExpFromExp :: Exp -> [Int]
    getListOfAllPrintStmExpFromExp e =
      case e of
        OpExp (e1', _, e2') -> count
          [getListOfAllPrintStmExpFromExp e1', getListOfAllPrintStmExpFromExp e2']
        EseqExp (s1', e1') -> count
          [getListOfAllPrintStmExp s1', getListOfAllPrintStmExpFromExp e1']
        _ -> [0]
    count :: [[a]] -> [Int]
    count = map length
