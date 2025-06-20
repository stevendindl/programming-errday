{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

tricks :: [String]
tricks = ["kickflip", "switch tre flip", "inward heel flip", "ollie", "manual", 
        "nose manual", "fakie bigspin", "nollie shuv"]

-- String = list of Char
-- "tre" == ['t', 'r', 'e']    is    True

-- map 
makeSwitch :: [String] -> [String]
makeSwitch = map ("switch " ++)

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
    | t == "nollie" || t == "folly" || t == "swally ollie"   = "ollie"
    | t == "nose manual"    = "manual"
    | otherwise             = stanceDrop t

convertTrick :: String -> String -> String
convertTrick "nollie" t 
    | t == "ollie"          = "nollie"
    | t == "manual"         = "nose manual"
    | otherwise             = "nollie " ++ t

convertTrick "switch" t 
    | t == "ollie"          = "swally ollie"
    | otherwise             = "switch " ++ t

convertTrick "fakie" t 
    | t == "ollie"          = "folly"
    | otherwise             = "fakie " ++ t

convertTrick "regular" t = t

-- convertTrick s t = s ++ " " ++ t


-- makeFakie
-- makeNollie


-- filter


-- foldr (/foldl)

