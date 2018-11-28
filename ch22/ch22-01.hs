-- ch22-01.hs

module Practice where

import Control.Applicative

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Num a => a -> a
bloop = fmap boop doop

bloop' :: Num a => a -> a
bloop' x = (*2) ((+10) x)

bbop :: Num a => a -> a
bbop = (+) <$> boop <*> doop

duwop :: Num a => a -> a
duwop = liftA2 (+) boop doop

boopDoop :: Num a => a -> a
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)
