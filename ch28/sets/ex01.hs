-- ch28/ex01.hs
-- compare the performance of insertion and lookup in the set

module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0,0)

m' :: M.Map Int Int
m' = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (5000, 5000)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

s' :: S.Set Int
s' = S.fromList $ take 10000 stream
  where stream = iterate (+1) 5000

membersMap :: Int -> Bool
membersMap i = M.member i m

insertMap :: (Int, Int) -> M.Map Int Int
insertMap (i1, i2) = M.insert i1 i2 m

unionMap :: Ord k => (M.Map k a, M.Map k a) -> M.Map k a
unionMap (m1, m2) = M.union m1 m2

membersSet :: Int -> Bool
membersSet i = S.member i s

insertSet :: Int -> S.Set Int
insertSet i = S.insert i s

unionSet :: Ord a => (S.Set a, S.Set a) -> S.Set a
unionSet (s1, s2) = S.union s1 s2

main :: IO ()
main = defaultMain
  [ bench "map insert" $
      whnf insertMap (10001, 10001)
  , bench "set insert" $
      whnf insertSet 10001
  , bench "map union" $
      whnf unionMap (m, m')
  , bench "set union" $
      whnf unionSet (s, s')
  ]
