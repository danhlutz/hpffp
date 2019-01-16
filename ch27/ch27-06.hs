-- ch27-06.hs

module Practice where

import Debug.Trace (trace)

inc = (+1)

twice = inc . inc

howManyTimes =
  inc (trace "I got eval'd" (1 + 1))
    + twice
      (trace "I also got eval'd" (1 + 1))

howManyTimes' =
  let onePlusOne =
        trace "i got eval'd" (1 + 1)
  in inc onePlusOne + twice onePlusOne
