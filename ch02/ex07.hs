-- this should work in GHCi
-- let x = 5; y = 6 in x * y

mult1     = x * y
  where x = 5
        y = 6

-- let x = 3; y = 1000 in x * 3 + y

combo1 = x * 3 + y
  where x = 3
        y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5

combo2 = x * 5
  where y = 10
        x = 10 * 5 + y

-- let x = 7
--     y = negate x
--     z = y * 10
--   in z / x + y

combo3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

