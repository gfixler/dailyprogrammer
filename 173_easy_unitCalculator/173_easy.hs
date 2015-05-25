{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Fractional v => Unit u v where
    ratio :: u -> v
    convert :: v -> u -> u -> (v, u)
    convert n u1 u2 = (n * (ratio u2 / ratio u1), u2)

data Length = Meters | Inches | Miles | Attoparsecs deriving (Show)
data Mass = Kilograms | Pounds | Ounces | HogsheadsOfBeryllium deriving (Show)
data School = ZoolanderSchoolForKidsWhoCantReadGood | SchoolForAnts deriving (Show)

instance Fractional a => Unit Length a where
    ratio Meters      = 1.0
    ratio Inches      = 39.3701
    ratio Miles       = 0.000621371
    ratio Attoparsecs = 32.4077929

instance Fractional a => Unit Mass a where
    ratio Kilograms            = 1.0
    ratio Pounds               = 2.20462
    ratio Ounces               = 35.274
    ratio HogsheadsOfBeryllium = 440.7

instance Fractional a => Unit School a where
    ratio ZoolanderSchoolForKidsWhoCantReadGood = 1.0
    ratio SchoolForAnts                         = 3.0

