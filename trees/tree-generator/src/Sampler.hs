-- | Compiler:     Boltzmann Brain v1.6 (11-12-2019 11:23:46)
-- | Generated at: 16-12-2019 08:41:25
-- | Singularity:  0.560000000000001
-- | System:       (Types: 1, Constr: 10)
-- | System type:  algebraic
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell #-}
module Sampler (Tree(), genRandomTree, sample, sampleTreeIO) where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Buffon.Machine
       (BuffonMachine, DecisionTree(..), decisionTree, choice, runRIO)
import qualified Language.Haskell.TH.Syntax as TH
import System.Random (RandomGen(..))

data Tree = Node0
          | Node1 Tree
          | Node2 Tree Tree
          | Node3 Tree Tree Tree
          | Node4 Tree Tree Tree Tree
          | Node5 Tree Tree Tree Tree Tree
          | Node6 Tree Tree Tree Tree Tree Tree
          | Node7 Tree Tree Tree Tree Tree Tree Tree
          | Node8 Tree Tree Tree Tree Tree Tree Tree Tree
          | Node9 Tree Tree Tree Tree Tree Tree Tree Tree Tree

decisionTreeTree :: DecisionTree Int
decisionTreeTree
  = $(
      TH.lift
        (decisionTree
           [0.35999999999999965, 0.560000000000001, 9.999999999999967e-3,
            9.999999999999988e-3, 1.0e-2, 1.0000000000000023e-2,
            9.999999999999992e-3, 1.0000000000000056e-2,
            1.0000000000000031e-2])
      )

genRandomTree ::
                (RandomGen g) => Int -> MaybeT (BuffonMachine g) (Tree, Int)
genRandomTree ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeTree)
       case n of
           0 -> return (Node0, 1)
           1 -> do (x0, w0) <- genRandomTree (ub - 1)
                   return (Node1 x0, 1 + w0)
           2 -> do (x0, w0) <- genRandomTree (ub - 1)
                   (x1, w1) <- genRandomTree (ub - 1 - w0)
                   return (Node2 x0 x1, 1 + w1 + w0)
           3 -> do (x0, w0) <- genRandomTree (ub - 1)
                   (x1, w1) <- genRandomTree (ub - 1 - w0)
                   (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                   return (Node3 x0 x1 x2, 1 + w2 + w1 + w0)
           4 -> do (x0, w0) <- genRandomTree (ub - 1)
                   (x1, w1) <- genRandomTree (ub - 1 - w0)
                   (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                   (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                   return (Node4 x0 x1 x2 x3, 1 + w3 + w2 + w1 + w0)
           5 -> do (x0, w0) <- genRandomTree (ub - 1)
                   (x1, w1) <- genRandomTree (ub - 1 - w0)
                   (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                   (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                   (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                   return (Node5 x0 x1 x2 x3 x4, 1 + w4 + w3 + w2 + w1 + w0)
           6 -> do (x0, w0) <- genRandomTree (ub - 1)
                   (x1, w1) <- genRandomTree (ub - 1 - w0)
                   (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                   (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                   (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                   (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                   return (Node6 x0 x1 x2 x3 x4 x5, 1 + w5 + w4 + w3 + w2 + w1 + w0)
           7 -> do (x0, w0) <- genRandomTree (ub - 1)
                   (x1, w1) <- genRandomTree (ub - 1 - w0)
                   (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                   (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                   (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                   (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                   (x6, w6) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                   return
                     (Node7 x0 x1 x2 x3 x4 x5 x6, 1 + w6 + w5 + w4 + w3 + w2 + w1 + w0)
           8 -> do (x0, w0) <- genRandomTree (ub - 1)
                   (x1, w1) <- genRandomTree (ub - 1 - w0)
                   (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                   (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                   (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                   (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                   (x6, w6) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                   (x7, w7) <- genRandomTree
                                 (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                   return
                     (Node8 x0 x1 x2 x3 x4 x5 x6 x7,
                      1 + w7 + w6 + w5 + w4 + w3 + w2 + w1 + w0)
           _ -> do (x0, w0) <- genRandomTree (ub - 1)
                   (x1, w1) <- genRandomTree (ub - 1 - w0)
                   (x2, w2) <- genRandomTree (ub - 1 - w0 - w1)
                   (x3, w3) <- genRandomTree (ub - 1 - w0 - w1 - w2)
                   (x4, w4) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3)
                   (x5, w5) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4)
                   (x6, w6) <- genRandomTree (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5)
                   (x7, w7) <- genRandomTree
                                 (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6)
                   (x8, w8) <- genRandomTree
                                 (ub - 1 - w0 - w1 - w2 - w3 - w4 - w5 - w6 - w7)
                   return
                     (Node9 x0 x1 x2 x3 x4 x5 x6 x7 x8,
                      1 + w8 + w7 + w6 + w5 + w4 + w3 + w2 + w1 + w0)

sample ::
         (RandomGen g) =>
         (Int -> MaybeT (BuffonMachine g) (a, Int)) ->
           Int -> Int -> BuffonMachine g a
sample gen lb ub
  = do str <- runMaybeT (gen ub)
       case str of
           Nothing -> sample gen lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sample gen lb ub

sampleTreeIO :: Int -> Int -> IO Tree
sampleTreeIO lb ub = runRIO (sample genRandomTree lb ub)

instance Show Tree where
   show Node0 = "()"
   show (Node1 x0) = "(" ++ show x0 ++ ")"
   show (Node2 x0 x1) = "(" ++ show x0 ++ show x1 ++ ")"
   show (Node3 x0 x1 x2) = "(" ++ show x0 ++ show x1 ++ show x2 ++ ")"
   show (Node4 x0 x1 x2 x3) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ ")"
   show (Node5 x0 x1 x2 x3 x4) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ ")"
   show (Node6 x0 x1 x2 x3 x4 x5) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ ")"
   show (Node7 x0 x1 x2 x3 x4 x5 x6) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ ")"
   show (Node8 x0 x1 x2 x3 x4 x5 x6 x7) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ ")"
   show (Node9 x0 x1 x2 x3 x4 x5 x6 x7 x8) = "(" ++ show x0 ++ show x1 ++ show x2 ++ show x3 ++ show x4 ++ show x5 ++ show x6 ++ show x7 ++ show x8 ++ ")"
