-- ch27-11.hs

{-# LANGUAGE Strict #-}

module Practice where

blah x = 1

main = print (blah undefined)

willForce x = 1

willNotForce ~x = 1
