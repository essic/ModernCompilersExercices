{-# LANGUAGE FlexibleContexts #-}
module Mci.StraightLine.Syntax where

import           Data.Foldable
import           Mci.Prelude
import           Mci.StraightLine.Types
import qualified Data.Map.Strict as Map


-------------------------------------------------------------------------------
--                           MaxArgs                                         --
-------------------------------------------------------------------------------
maxargs ::(HasPrintStatment s)=> s -> Int
maxargs = printStatmens

-------------------------------------------------------------------------------
--                                Intrepret a statment                       --
-------------------------------------------------------------------------------
interpStm :: (MonadIO m, MonadState Table m ) => Stm -> m ()
interpStm (CompoundStm s1 s2) = interpStm s1 >> interpStm s2
interpStm (PrintStm xs) =  traverse_ (interpretExp >=> putText . show) xs
interpStm (AssignStm id expr) = do
  val <- interpretExp expr
  modify (Table . (id `Map.insert` val) . unwrap)


-------------------------------------------------------------------------------
--                      Interpret an expression                              --
-------------------------------------------------------------------------------
interpretExp :: (MonadIO m, MonadState Table m) => Exp -> m Int
interpretExp (NumExp v) = return v
interpretExp (IdExp k) = maybe 0 identity . Map.lookup k . unwrap <$> get
interpretExp (EseqExp s e) = interpStm s >> interpretExp e
interpretExp (OpExp exp1 op exp2) = liftM2 (eval op) (interpretExp exp1) (interpretExp exp2)
