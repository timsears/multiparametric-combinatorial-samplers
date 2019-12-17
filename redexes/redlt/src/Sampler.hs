-- | Compiler:     Boltzmann Brain v1.6 (17-12-2019 10:16:31)
-- | Generated at: 17-12-2019 10:33:55
-- | Singularity:  0.259621541813701
-- | System:       (Types: 3, Constr: 8)
-- | System type:  algebraic
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell #-}
module Sampler
       (A(..), DB(..), L(..), genRandomA, genRandomDB, genRandomL, sample,
        sampleAIO, sampleDBIO, sampleLIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Buffon.Machine
       (BuffonMachine, DecisionTree(..), decisionTree, choice, runRIO)
import qualified Language.Haskell.TH.Syntax as TH
import System.Random (RandomGen(..))

data A = AppA A L
       | AppIndex DB L
       | Redex L L
           deriving Show

data DB = S DB
        | Z deriving Show

data L = Abs L
       | App A
       | Index DB
           deriving Show

decisionTreeA :: DecisionTree Int
decisionTreeA
  = $(
      TH.lift (decisionTree [0.24592568898801825, 0.24592568898801895]) )

decisionTreeDB :: DecisionTree Int
decisionTreeDB = $( TH.lift (decisionTree [0.259621541813701]) )

decisionTreeL :: DecisionTree Int
decisionTreeL
  = $(
      TH.lift (decisionTree [0.259621541813701, 0.37018922909314916]) )

genRandomA ::
             (RandomGen g) => Int -> MaybeT (BuffonMachine g) (A, Int)
genRandomA ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeA)
       case n of
           0 -> do (x0, w0) <- genRandomA (ub - 1)
                   (x1, w1) <- genRandomL (ub - 1 - w0)
                   return (AppA x0 x1, 1 + w1 + w0)
           1 -> do (x0, w0) <- genRandomDB (ub - 1)
                   (x1, w1) <- genRandomL (ub - 1 - w0)
                   return (AppIndex x0 x1, 1 + w1 + w0)
           _ -> do (x0, w0) <- genRandomL (ub - 2)
                   (x1, w1) <- genRandomL (ub - 2 - w0)
                   return (Redex x0 x1, 2 + w1 + w0)

genRandomDB ::
              (RandomGen g) => Int -> MaybeT (BuffonMachine g) (DB, Int)
genRandomDB ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeDB)
       case n of
           0 -> do (x0, w0) <- genRandomDB (ub - 1)
                   return (S x0, 1 + w0)
           _ -> return (Z, 1)

genRandomL ::
             (RandomGen g) => Int -> MaybeT (BuffonMachine g) (L, Int)
genRandomL ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeL)
       case n of
           0 -> do (x0, w0) <- genRandomL (ub - 1)
                   return (Abs x0, 1 + w0)
           1 -> do (x0, w0) <- genRandomA ub
                   return (App x0, w0)
           _ -> do (x0, w0) <- genRandomDB ub
                   return (Index x0, w0)

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

sampleAIO :: Int -> Int -> IO A
sampleAIO lb ub = runRIO (sample genRandomA lb ub)

sampleDBIO :: Int -> Int -> IO DB
sampleDBIO lb ub = runRIO (sample genRandomDB lb ub)

sampleLIO :: Int -> Int -> IO L
sampleLIO lb ub = runRIO (sample genRandomL lb ub)
