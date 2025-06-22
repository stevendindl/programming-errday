{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

tricks :: [String]
tricks = ["kickflip", "switch tre flip", "inward heel flip", "ollie", 
        "nose manual", "fakie bigspin", "nollie shuv"]

-- String = list of Char
-- "tre" == ['t', 'r', 'e']    is    True

-- map 
-- makeSwitch :: [String] -> [String]
-- makeSwitch = map ("switch " ++)

-- switchStance nollie ["kickflip, "tre flip", "inward heel flip"] 
-- ["nollie kickflip","nollie tre flip","nollie inward heel flip"]
switchStance' :: String -> [String] -> [String]
switchStance' s ts =  map (\t -> s ++ " " ++ t) ts

-- same as above but only nollie, fakie, switch are valid stances
-- switchStance balls ["kickflip, "tre flip", "inward heel flip"] should return same list
switchStance'' :: String -> [String] -> [String]
switchStance'' s ts = map (\t -> if valid then s ++ " " ++ t else t) ts
    where
        valid = s == "nollie" || s == "fakie" || s == "switch"

{-
1) We need to account for if we already have a stance in the list

switchStance fakie ["kickflip, "tre flip", "fakie inward heel flip"]  (should not result in "fakie fakie")
             ^ valid input    -> ["fakie kickflip, "fakie tre flip", "fakie inward heel flip"]
switchStance regular ["kickflip, "tre flip", "fakie inward heel flip"] 
             ^ valid input    results in ["kickflip, "tre flip", "inward heel flip"] 
"
2) We need to account for special trick stance conversions s="nollie" ("ollie" -> "nollie") s="switch" ("nollie" -> "switch ollie")
-}

switchStance :: String -> [String] -> [String]
switchStance s ts = 
    if stanceValid then 
        map (\t -> convertTrick s (revertTrick t)) ts
    else 
        ts
    where
        stanceValid = s == "nollie" || s == "fakie" 
                    || s == "switch" || s == "regular"

-- Drops a stances t = fakie ollie & s = fakie -> ollie
stanceDrop :: String -> String
stanceDrop t
    | length t < 8              = t
    | take 6 t == "nollie"      = drop 7 t
    | take 6 t == "switch"      = drop 7 t
    | take 5 t == "fakie"       = drop 6 t
    | otherwise                 = t

-- Reverts the trick back to its base
revertTrick :: String -> String
revertTrick t
    | t == "nollie"         = "ollie"
    | t == "nollie body varial"     = "ollie body varial"
    | t == "nollie sex change"      = "ollie sex change" -- should prob handle tricks with equal names somehow
    | t == "nollie north"   = "ollie north"
    | t == "nose manual"    = "manual"
    | t == "half cab"       = "bs 180"
    | t == "full cab"       = "bs 360"
    | otherwise             = stanceDrop t



convertTrick :: String -> String -> String
convertTrick "nollie" t 
    | t == "ollie"          = "nollie"
    | t == "ollie body varial"  = "nollie body varial"
    | t == "ollie sex change"   = "nollie sex change"
    | t == "ollie north"    = "nollie north"
    | t == "manual"         = "nose manual"
    | length t >= 8 && take 8 t == "boneless"                       = t
    | t == "caveman" || t == "sex change" || t == "body varial"     = t
    | t == "firecracker" || t == "fs powerslide" || t == "bs powerslide"    = t
    | otherwise             = "nollie " ++ t

convertTrick "fakie" t 
    | t == "bs 180"         = "half cab"
    | t == "bs 360"         = "full cab"
    | length t >= 8 && take 8 t == "boneless"                       = "switch " ++ t
    | t == "caveman" || t == "sex change" || t == "body varial"     = "switch " ++ t
    | t == "firecracker" || t == "fs powerslide" || t == "bs powerslide"    = "switch " ++ t
    | otherwise             = "fakie " ++ t

convertTrick "regular" t = t


convertTrick s t = s ++ " " ++ t


makeFakie :: [String] -> [String]
makeFakie = switchStance "fakie"

makeNollie :: [String] -> [String]
makeNollie = switchStance "nollie"

makeSwitch :: [String] -> [String]
makeSwitch = switchStance "switch"

makeRegular :: [String] -> [String]
makeRegular = switchStance "regular"


-- alternateNames
alternateNames :: String -> [String]
alternateNames t
    | t == "tre flip" || t == "tre"    = ["tre flip", "tre", "360 flip"]
    | t == "shuv"       = [t, "shuv-it"]
    | t == "pop shuv"       = [t, "pop shuv-it"]
    | t == "bs powerslide"  = [t, "powerslide"]
    | otherwise         = [t] 

-- alternateNames "tre flip"       = ["tre flip", "tre", "360 flip"]
-- alternateNames "shuv"           = ["shuv", "shuv-it"]
-- alternateNames "pop shuv"       = ["pop shuv", "pop shuv-it"]
-- alternateNames "bs powerslide"  = ["bs powerslide", "powerslide"]
-- alternateNames t        = [t] 


-- filter



-- foldr (/foldl)





-- ~100 trick list

tricks100 :: [String]
tricks100 =
  [ "kickflip"
  , "heelflip"
  , "varial kickflip"
  , "varial heelflip"
  , "tre flip"
  , "inward heelflip"
  , "hardflip"
  , "double kickflip"
  , "double heelflip"
  , "laser flip"
  , "bigspin flip"
  , "bigspin"
  , "bigflip"
  , "impossible"
  , "hospital flip"
  , "pressure flip"
  , "ollie"
  , "switch ollie"
  , "nollie"
  , "fakie ollie"
  , "fakie bigspin"
  , "fakie kickflip"
  , "fakie heelflip"
  , "fakie tre flip"
  , "fakie varial flip"
  , "fakie inward heel"
  , "fakie bigflip"
  , "switch flip"
  , "switch heel"
  , "switch tre flip"
  , "switch bigspin"
  , "switch varial flip"
  , "switch bigflip"
  , "nollie flip"
  , "nollie heel"
  , "nollie bigspin"
  , "nollie tre flip"
  , "nollie inward heel"
  , "nollie bigflip"
  , "shuvit"
  , "pop shuvit"
  , "fs shuvit"
  , "fs pop shuvit"
  , "fakie shuvit"
  , "fakie pop shuvit"
  , "switch shuvit"
  , "switch pop shuvit"
  , "nollie shuvit"
  , "nollie pop shuvit"
  , "manual"
  , "nose manual"
  , "switch manual"
  , "fakie manual"
  , "nollie manual"
  , "manual to flip out"
  , "manual shuvit out"
  , "ollie to manual"
  , "flip to manual"
  , "fs 180"
  , "bs 180"
  , "half cab"
  , "full cab"
  , "nollie 180"
  , "switch 180"
  , "fakie fs 180"
  , "fakie bs 180"
  , "fs flip"
  , "bs flip"
  , "nollie fs flip"
  , "nollie bs flip"
  , "switch fs flip"
  , "switch bs flip"
  , "fakie fs flip"
  , "fakie bs flip"
  , "bs revert"
  , "fs revert"
  , "bs powerslide"
  , "fs powerslide"
  , "kickflip underflip"
  , "heelflip body varial"
  , "kickflip body varial"
  , "no comply"
  , "boneless"
  , "boneless 180"
  , "firecracker"
  , "caveman"
  , "darkslide"
  , "casper flip"
  , "sex change"
  , "ollie north"
  , "nollie big heel"
  , "switch inward heel"
  , "switch hardflip"
  , "bs bigspin"
  , "fs bigspin"
  , "fs 360"
  , "bs 360"
  , "fs 360 shuvit"
  , "bs 360 shuvit"
  ]
