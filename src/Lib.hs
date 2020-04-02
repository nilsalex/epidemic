module Lib where

import Control.Monad.Random
import Control.Monad.Random.Class

{-
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
-}

import Data.List (iterate')

import Debug.Trace (trace)

import Reanimate

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

data Model = Model
             { history :: [(Int,Int,Int)]
             , population :: Population
             } deriving Show

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

randModel :: MonadRandom m => Int -> Int -> m Model
randModel nSus nInf = do
  p@(Population s i r) <- randPop nSus nInf
  return $ Model [(length s,length i, length r)] p

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
             Double -> Double -> Double -> Double -> Model -> m Model
simulate' ar r recov step (Model h p) = do
  let p' = (recover recov step) p
  p''@(Population sus inf rec) <- infect ar r step p'
  let h' = h ++ [(length sus,length inf,length rec)]
  return $ Model h' (move step p'')

someFunc :: IO ()
someFunc = reanimate $ playThenReverseA $ mkAnimation duration $ \t ->
  partialSvg t $ pathify $ mkCircle radius
  where duration = 2; radius = screenHeight/3

{-
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
    rem' = color (greyN 0.7) $ pictures $ fmap drawInd rem

drawHist :: [(Int,Int,Int)] -> Picture
drawHist xs@((s0,i0,r0):_) =
    pictures
    [ color red $ pictures $ fmap (\(a,b,c) -> polygon [a,b,c]) tsI
    , color blue $ pictures $ fmap (\(a,b,c) -> polygon [a,b,c]) tsS
    , color (greyN 0.7) $ pictures $ fmap (\(a,b,c) -> polygon [a,b,c]) tsR
    ]
  where
    (sl,il,rl) = last xs
    n = s0+i0+r0
    n' = fromIntegral n :: Float
    l = length xs
    l' = min 200 l
    coords = if l <= 200
             then zipWith (\(s,i,r) x -> (fromIntegral x, scale s, scale i, scale r)) xs [0..]
             else let fac = (fromIntegral l) / 200
                  in fmap (\ix -> let ix' = (round ((fromIntegral ix :: Double)*fac) :: Int)
                                      (s,i,r) = xs !! ix'
                                  in (fromIntegral ix, scale s, scale i, scale r)) [0..199]
    bottom = fmap (\(x,_,_,_) -> (x-100, -200)) coords
    top    = fmap (\(x,_,_,_) -> (x-100, -130)) coords
    pathI = fmap (\(x,s,i,r) -> (x-100, i-200)) coords
    pathS = fmap (\(x,s,i,r) -> (x-100, i+s-200)) coords

    tsI = triangulate bottom pathI
    tsS = triangulate pathI pathS
    tsR = triangulate pathS top

    scale x = 70 * fromIntegral x / n'

triangulate :: [a] -> [a] -> [(a,a,a)]
triangulate upper lower = go upper lower
  where
    go (u1:u2:us) (l1:l2:ls) = (u1,l1,l2) : (u2,u1,l1) : go (u2:us) (l2:ls)
    go _ _ = []

drawModel :: Model -> Picture
drawModel (Model h p) = pictures [drawPop p,drawHist h]

someFunc' :: IO ()
someFunc' = do
  let attack = 0.2
  let radius = 0.05
  let recov  = 20
  let s0 = 100
  let i0 = 2
  model <- evalRandIO (randModel s0 i0)
  simulateIO (InWindow "epidemic" (640, 480) (0,0)) white 20 model (pure . drawModel) (\_ t m -> simulate' attack radius recov (realToFrac t) m)
-}
