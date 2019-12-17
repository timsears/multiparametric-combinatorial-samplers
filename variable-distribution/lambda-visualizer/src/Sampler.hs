-- | Compiler:     Boltzmann Brain v1.6 (17-12-2019 09:22:35)
-- | Generated at: 17-12-2019 09:23:37
-- | Singularity:  0.169538242368874
-- | System:       (Types: 3, Constr: 27)
-- | System type:  algebraic
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell #-}
module Sampler
       (DeBruijn(..), DeBruijnR(..), Lambda(..), genRandomDeBruijn,
        genRandomDeBruijnR, genRandomLambda, sample, sampleDeBruijnIO,
        sampleDeBruijnRIO, sampleLambdaIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Buffon.Machine
       (BuffonMachine, DecisionTree(..), decisionTree, choice, runRIO)
import qualified Language.Haskell.TH.Syntax as TH
import System.Random (RandomGen(..))

data DeBruijn = Var0
              | Var1
              | Var2
              | Var3
              | Var4
              | Var5
              | Var6
              | Var7
              | Var8
              | Var9
              | Var10
              | Var11
              | Var12
              | Var13
              | Var14
              | Var15
              | Var16
              | Var17
              | Var18
              | Var19
              | Var20
              | IndexR DeBruijnR
                  deriving Show

data DeBruijnR = S DeBruijnR
               | Z deriving Show

data Lambda = Abs Lambda
            | App Lambda Lambda
            | Index DeBruijn
                deriving Show

decisionTreeDeBruijn :: DecisionTree Int
decisionTreeDeBruijn
  = $(
      TH.lift
        (decisionTree
           [0.16670785801027596, 0.15750075257743648, 0.1050005026600443,
            7.875037773905007e-2, 6.300030290807944e-2, 5.2500253042293214e-2,
            4.500021743137572e-2, 3.937519071767119e-2, 3.5000169971701973e-2,
            3.1500153363166465e-2, 2.863650343582939e-2, 2.62501284841362e-2,
            2.4230888151521707e-2, 2.2500110720844912e-2, 2.100010361701767e-2,
            1.9687597407817035e-2, 1.8529503686475917e-2, 1.75000870518526e-2,
            1.657903006531559e-2, 1.5750078776036396e-2,
            1.5000075229528717e-2])
      )

decisionTreeDeBruijnR :: DecisionTree Int
decisionTreeDeBruijnR
  = $( TH.lift (decisionTree [0.169538242368874]) )

decisionTreeLambda :: DecisionTree Int
decisionTreeLambda
  = $( TH.lift (decisionTree [0.169538242368874, 0.4152308784456171])
      )

genRandomDeBruijn ::
                    (RandomGen g) => Int -> MaybeT (BuffonMachine g) (DeBruijn, Int)
genRandomDeBruijn ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeDeBruijn)
       case n of
           0 -> return (Var0, 1)
           1 -> return (Var1, 2)
           2 -> return (Var2, 3)
           3 -> return (Var3, 4)
           4 -> return (Var4, 5)
           5 -> return (Var5, 6)
           6 -> return (Var6, 7)
           7 -> return (Var7, 8)
           8 -> return (Var8, 9)
           9 -> return (Var9, 10)
           10 -> return (Var10, 11)
           11 -> return (Var11, 12)
           12 -> return (Var12, 13)
           13 -> return (Var13, 14)
           14 -> return (Var14, 15)
           15 -> return (Var15, 16)
           16 -> return (Var16, 17)
           17 -> return (Var17, 18)
           18 -> return (Var18, 19)
           19 -> return (Var19, 20)
           20 -> return (Var20, 21)
           _ -> do (x0, w0) <- genRandomDeBruijnR ub
                   return (IndexR x0, w0)

genRandomDeBruijnR ::
                     (RandomGen g) => Int -> MaybeT (BuffonMachine g) (DeBruijnR, Int)
genRandomDeBruijnR ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeDeBruijnR)
       case n of
           0 -> do (x0, w0) <- genRandomDeBruijnR (ub - 1)
                   return (S x0, 1 + w0)
           _ -> return (Z, 22)

genRandomLambda ::
                  (RandomGen g) => Int -> MaybeT (BuffonMachine g) (Lambda, Int)
genRandomLambda ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeLambda)
       case n of
           0 -> do (x0, w0) <- genRandomLambda (ub - 1)
                   return (Abs x0, 1 + w0)
           1 -> do (x0, w0) <- genRandomLambda (ub - 1)
                   (x1, w1) <- genRandomLambda (ub - 1 - w0)
                   return (App x0 x1, 1 + w1 + w0)
           _ -> do (x0, w0) <- genRandomDeBruijn ub
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

sampleDeBruijnIO :: Int -> Int -> IO DeBruijn
sampleDeBruijnIO lb ub = runRIO (sample genRandomDeBruijn lb ub)

sampleDeBruijnRIO :: Int -> Int -> IO DeBruijnR
sampleDeBruijnRIO lb ub = runRIO (sample genRandomDeBruijnR lb ub)

sampleLambdaIO :: Int -> Int -> IO Lambda
sampleLambdaIO lb ub = runRIO (sample genRandomLambda lb ub)
