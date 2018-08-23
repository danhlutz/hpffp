-- ex04.hs
module Symmetry2 where

skipDrop :: Char -> String -> String
skipDrop c s
    | s == ""   = []
    | otherwise =
        dropWhile (==c) (dropWhile (/=c) s)

myWords :: String -> [String]
myWords x
    | x == ""    = []
    | otherwise =
        (takeWhile (/=' ') x) : (myWords (go x))
    where go n = skipDrop ' ' n


firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
          \ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines x
   | x == ""   = []
   | otherwise = 
       (takeWhile (/='\n') x) : (myLines (go x))
   where go n = skipDrop '\n' n

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
          == shouldEqual)
