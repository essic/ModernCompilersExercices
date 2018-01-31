{-# LANGUAGE LambdaCase#-}
module Mci.StraightLine.Utils
  (
    maxargs
  ) where

import Mci.Prelude
import Mci.StraightLine.Syntax

data ExpOrStm = E Exp | S Stm

maxargs :: Stm -> Int
maxargs statement =
  foldl' max 0 $ getAllPrintStm ( S statement )
  where
   getAllPrintStm :: ExpOrStm -> [Int]

   getAllPrintStm =
     \case
       (E (OpExp e1' _ e2')) -> (++) (getAllPrintStm $ E e1') (getAllPrintStm $ E e2')
       (E (EseqExp s1' e1')) -> (++) (getAllPrintStm $ S s1') (getAllPrintStm $ E e1')
       (E _) -> [0]
       (S (AssignStm _ e')) -> getAllPrintStm $ E e'
       (S (PrintStm xs')) -> [length xs']
       (S (CompoundStm s1' s2')) -> (++) (getAllPrintStm $ S s1') (getAllPrintStm $ S s2')

