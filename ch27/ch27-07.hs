-- ch27-07.hs

module Practice where

refutable :: Bool -> Bool
refutable True = False
refutable False = True

irrefutable :: Bool -> Bool
irrefutable x = not x

oneOfEach :: Bool -> Bool
oneOfEach True = False
oneOfEach _    = True

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
