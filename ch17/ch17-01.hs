-- ch17/ch17-01.hs

module Practice where

import Control.Applicative

f :: Integral a => a -> Maybe String
f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")]

g :: Integral a => a -> Maybe String
g y = lookup y [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")]

h :: Integral a => a -> Maybe Integer
h z = lookup z [(2, 3), (5, 6), (7, 8)]

m :: Integral a => a -> Maybe Integer
m x = lookup x [(4, 10), (8, 13), (1, 9001)]
