module Mci.StraightLine.Interpretor
  (
    interp
  ) where

import           Mci.Prelude
import           Mci.StraightLine.Syntax

newtype Table = Table { unwrap :: [(Id,Int)] }

interp :: Stm -> IO ()
interp =
  compute
  where
    compute :: Stm -> IO ()
    compute (CompoundStm s1 s2) =
      interpStm(s1,Table []) >>= interpStm . (,) s2 >> pure ()
    compute s =
      interpStm(s, Table[]) >> pure ()

interpStm :: (Stm, Table) -> IO Table

interpStm ((CompoundStm s1 s2),t) = do
  t1 <- interpStm (s1,t)
  t2 <- interpStm (s2,t1)
  pure t2

interpStm ((AssignStm lbl expr),t) = do
  (val,t1) <- interpExp (expr,t)
  pure $ update t1 (lbl, val)
  where
    update :: Table -> (Id,Int) -> Table
    update t' e =
      Table $ e : unwrap t'

interpStm (PrintStm xs, t) =
  foldl' f (pure t) xs
  where
    f :: IO Table -> Exp -> IO Table
    f t' e = do
      t1 <- t'
      (val,t1') <- interpExp (e,t1)
      putText (show val) >> pure t1'

interpExp :: (Exp, Table) -> IO (Int, Table)

interpExp (NumExp v, t) =
  pure (v, t)

interpExp (IdExp lbl, t) =
  pure (lookup (unwrap t,lbl), t)
  where
    lookup :: ([(Id,Int)],Id) -> Int
    lookup ([],_) = 0
    lookup (x:xt,id')
      | fst x == id' = snd x
      | otherwise = lookup (xt,lbl)

interpExp ( (OpExp exp1 op exp2),t) = do
  (v1,t1) <- interpExp (exp1, t)
  (v2,t2) <- interpExp (exp2, t1)
  pure (operate v1 v2, t2)
  where
    operate :: Int -> Int -> Int
    operate v1' v2' =
      case op of
        Plus  -> v1' + v2'
        Minus -> v1' - v2'
        Times -> v1' * v2'
        Div   -> v1'`div` v2'

interpExp (EseqExp s e, t) = do
  t1 <- interpStm (s,t)
  interpExp (e,t1)

