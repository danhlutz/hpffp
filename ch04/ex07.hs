--ex07.hs 
module MyBackZip where

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((b, d), (a, c))
  where a = fst x
        b = snd x
        c = fst y
        d = snd y
