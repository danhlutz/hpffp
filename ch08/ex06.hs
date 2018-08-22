-- ex06.hs
module McCarthy where

mc91 :: Integral a => a -> a
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 (mc91 (x + 11))

mc91' :: Integral a => a -> a
mc91' x
    | x < 101   = mc91 (mc91 (x + 11))
    | otherwise = x - 10

-- mc91 95 =
-- mc91 (mc91 106)
-- mc91 96
-- mc91 (mc91 107)
-- mc91 97
-- mc91 (mc91 108)
-- mc91 98
-- mc91 (mc91 109)
-- mc91 99
-- mc91 (mc91 110)
-- mc91 100
-- mc91 (mc91 111)
-- mc91 101
-- 91

-- mc91 87
-- mc91 (mc91 98)
-- mc91 (mca91 (mc91 109))
-- mc91 (mc91 99)
-- mc91 (mc91 (mc91 110))
-- mc91 (mc91 100)
-- mc91 (mc91 (mc91 111))
-- mc91 (mc91 101)
-- mc91 91
-- mc91 (mc91 102)
-- mc91 92
-- mc91 (mc91 103)
-- ...
