-- ch11-08.hs
module Practice11 where

newtype Name   = Name String deriving Show
newtype Acres  = Acres Int deriving Show

-- FarmerType is a sum
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving (Eq, Show)

-- Farmer is a product of Name, Acres, and FarmerType
data Farmer =
  Farmer Name Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
  FarmerRec { name       :: Name
            , acres      :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
-- isDairyFarmerRec farmer =
--  case farmerType farmer of
--    DairyFarmer -> True
--    _           -> False
isDairyFarmerRec farmer
  | farmerType farmer == DairyFarmer = True
  | otherwise                        = False
