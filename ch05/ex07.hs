--ex07.hs
--Does it compile?

-- 1) 
-- bigNum = (^) 5 $ 10
-- wahoo = bigNum $ 10

bigNum x = (^) 5 $ x
wahoo = bigNum $ 10

-- bigNum takes no arguments, so the application in wahoo doesn't work

-- 2) 
-- x = print
-- y = print "woohoo!"
-- z = x "hello world"

x = print
y = print "woohoo!"
z = x "hello world"

-- compiles!

-- 3)
-- a = (+)
-- b = 5
-- c = b 10
-- d = c 200

a = (+)
b x = a 5 x
c = b 10
d = a c 200

-- 4) 
-- a = 12 + b
-- b = 10000 * c

a2 = 12 + b2
b2 = 10000 * c
