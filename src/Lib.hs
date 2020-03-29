module Lib where

import Control.Monad.Random
import Control.Monad.Random.Class

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate

import Data.List (iterate')

import Debug.Trace (trace)

data Individual = Individual
           { xPos :: {-# UNPACK #-} !Double
           , yPos :: {-# UNPACK #-} !Double
           , xVel :: {-# UNPACK #-} !Double
           , yVel :: {-# UNPACK #-} !Double
           }
           deriving Show

data Population = Population
                  { susceptible :: [Individual]
                  , infected :: [(Individual,Double)]
                  , removed :: [Individual]
                  }
                  deriving Show

moveInd :: Double -> Individual -> Individual
moveInd step (Individual x y xv yv)
    | inBounds x' && inBounds y' = Individual x' y' xv yv
    | inBounds x' && not (inBounds y') = Individual x' y'' xv (-yv)
    | not (inBounds x') && inBounds y' = Individual x'' y' (-xv) yv
    | otherwise = Individual x'' y'' (-xv) (-yv)
  where
    x'  = x + step*xv
    y'  = y + step*yv
    y'' = if y' < 0
          then -y'
          else 2 - y'
    x'' = if x' < 0
          then -x'
          else 2 - x'
    
    inBounds a = a > 0 && a < 1

randInd :: MonadRandom m => m Individual
randInd = do
  x <- getRandomR (0,1)
  y <- getRandomR (0,1)
  xv <- getRandomR (-0.2,0.2)
  yv <- getRandomR (-0.2,0.2)
  return $ Individual x y xv yv

randPop :: MonadRandom m => Int -> Int -> m Population
randPop nSus nInf = do
  sus <- sequence (replicate nSus randInd)
  inf <- sequence (replicate nInf randInd)
  return $ Population sus (zip inf (repeat 0)) []

move :: Double -> Population -> Population
move step (Population sus inf rem) = Population sus' inf' rem'
  where
    sus' = fmap (moveInd step) sus
    inf' = fmap (\(i,t) -> (moveInd step i,t)) inf
    rem' = fmap (moveInd step) rem

infects :: MonadRandom m => Double -> Double -> Double -> Individual -> Individual -> m Bool
infects attackRate radius step (Individual xi yi _ _) (Individual xs ys _ _)
    | d2 > r2 = pure False
    | otherwise = do
                   p' <- getRandomR (0,1)
                   if p' < p
                   then pure True
                   else pure False
  where
    d2 = (xi-xs)^2 + (yi-ys)^2
    r2 = radius^2
    p = 1 - (1 - attackRate)**step

infectSingle :: MonadRandom m =>
                Double -> Double -> Double -> Individual -> [Individual] -> m ([Individual], [(Individual,Double)])
infectSingle attackRate radius step i sus =
  foldM (\(sus', inf') s -> do
                             b <- infects attackRate radius step i s
                             case b of
                               True -> pure (sus', (s,0):inf')
                               False -> pure (s:sus', inf')) ([],[]) sus

infect :: MonadRandom m =>
          Double -> Double -> Double -> Population -> m Population
infect ar r step (Population sus inf rem) = do
  (sus',inf') <- foldM (\(susAcc, infAcc) i ->
                          do
                           (s',i') <- infectSingle ar r step (fst i) susAcc
                           return (s', i' ++ infAcc)) (sus,[]) inf
  return $ Population sus' (inf' ++ inf) rem

recover :: Double -> Double -> Population -> Population
recover recov step (Population sus inf rem) = Population sus inf' rem'
  where
    (inf', rem') = foldr (\(i,c) (infAcc, remAcc) ->
                           let c' = c + step
                           in if c' > recov
                              then (infAcc, i:remAcc)
                              else ((i,c'):infAcc, remAcc)) ([],rem) inf

simulate' :: MonadRandom m =>
             Double -> Double -> Double -> Double -> Population -> m Population
simulate' ar r recov step p = do
  let p' = (recover recov step) p
  p'' <- infect ar r step p'
  return $ move step p''

drawInd :: Individual -> Picture
drawInd (Individual x y _ _) = translate x' y' $ circleSolid 3
  where
    x' = realToFrac (200*x)
    y' = realToFrac (200*y)

drawPop :: Population -> Picture
drawPop (Population sus inf rem) = translate (-100) (-100) $ pictures [sus', inf', rem']
  where
    sus' = color blue $ pictures $ fmap drawInd sus
    inf' = color red $ pictures $ fmap (drawInd . fst) inf
    rem' = color (greyN 0.5) $ pictures $ fmap drawInd rem

someFunc :: IO ()
someFunc = do
  pop <- evalRandIO (randPop 50 1)
  let attack = 0.2
  let radius = 0.05
  let recov  = 20
  simulateIO (InWindow "epidemic" (640, 480) (0,0)) white 20 pop (pure . drawPop) (\_ t m -> simulate' attack radius recov (realToFrac t) m)
