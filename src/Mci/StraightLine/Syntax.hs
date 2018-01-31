{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mci.StraightLine.Syntax where

import           Data.Foldable
import  Mci.Prelude
import Mci.StraightLine.Types
import qualified Data.Map.Strict as Map

maxargs :: Stm -> Int
maxargs statement =
  foldl max 0 $ getListOfAllPrintStmExp statement
  where
    getListOfAllPrintStmExp :: Stm -> [Int]
    getListOfAllPrintStmExp =
      \case
        AssignStm _ e' -> getListOfAllPrintStmExpFromExp e'
        PrintStm xs' -> [length xs']
        CompoundStm s1' s2' -> getListOfAllPrintStmExp s1' ++ getListOfAllPrintStmExp s2'
    getListOfAllPrintStmExpFromExp :: Exp -> [Int]
    getListOfAllPrintStmExpFromExp =
      \case
        OpExp e1' _ e2' -> getListOfAllPrintStmExpFromExp e1' ++ getListOfAllPrintStmExpFromExp e2'
        EseqExp s1' e1' -> getListOfAllPrintStmExp s1' ++ getListOfAllPrintStmExpFromExp e1'
        _ -> [0]

-------------------------------------------------------------------------------
--                                Intrepret a statment                       --
-------------------------------------------------------------------------------
interpStm :: (MonadIO m, MonadState Table m ) => Stm -> m ()
interpStm (CompoundStm s1 s2) = interpStm s1 >> interpStm s2
interpStm (PrintStm xs) =  traverse_ (alg >=> putText . show) xs
interpStm (AssignStm id expr) = do
  val <- alg expr
  modify (Table . (id `Map.insert` val) . unwrap)


-------------------------------------------------------------------------------
--                      Interpret an expression                              --
-------------------------------------------------------------------------------
alg :: (MonadIO m, MonadState Table m) => Exp -> m Int
alg (NumExp v) = return v
alg (IdExp k) = maybe 0 identity . Map.lookup k . unwrap <$> get
alg (EseqExp s e) = interpStm s >> alg e
alg (OpExp exp1 op exp2) = do
  v1 <- alg exp1
  v2 <- alg exp2
  return (operate v1 v2)
  where
    operate :: Int -> Int -> Int
    operate v1' v2' =
      case op of
        Plus  -> v1' + v2'
        Minus -> v1' - v2'
        Times -> v1' * v2'
        Div'   -> v1'`div` v2'

-------------------------------------------------------------------------------
--  Define a simple F-Agebra with type `Int` as carrier                      --
-------------------------------------------------------------------------------
algebra :: (MonadIO m, MonadState Table m) => ExpF Int -> m Int
algebra (Val v) = return v
algebra (Ident k) = maybe 0 identity . Map.lookup k . unwrap <$> get
algebra (Add val1 val2) = return $! val1 + val2
algebra (Mult val1 val2) = return $! val1 * val2
algebra (Sub val1 val2) = return $! val1 - val2
algebra (Div val1 val2) = return $! val1 `div` val2
-- algebra (ESeq s e) = algebra s >> return e
-- eval :: Fix ExpF Int -> m Int
--eval :: (MonadState Table m, MonadIO m) => Expr Int -> m (Fix ExpF Int)
--eval = traverse algebra
