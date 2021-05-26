--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- up957029
--

import Data.List
import Data.List (intercalate)
import Data.Map
import Data.Ord
import Data.String
import Text.Printf
import Text.PrettyPrint

--
-- Types
--
data City = City {city:: String, northDeg:: Int, southDeg:: Int, populationPerYear:: [Int]}
    deriving (Show, Read)

testData :: [City]
testData = [
    City "Amsterdam" 52 5 [1158, 1149, 1140, 1132],
    City "Athens" 38  23    [3153, 3153, 3154, 3156],
    City "Berlin" 53  13    [3567, 3562, 3557, 3552],
    City "Brussels" 51   4    [2096, 2081, 2065, 2050],
    City "Bucharest" 44  26    [1794, 1803, 1812, 1821],
    City "London"       52   0    [9426, 9304, 9177, 9046],
    City "Madrid"       40   4    [6669, 6618, 6559, 6497],
    City "Paris"        49   2    [11079, 11017, 10958, 10901],
    City  "Rome"         42  13    [4278, 4257, 4234, 4210],
    City "Sofia"        43  23    [1284, 1281, 1277, 1272],
    City  "Vienna"       48  16    [1945, 1930, 1915, 1901],
    City "Warsaw"       52  21    [1790, 1783, 1776, 1768]
    ]

-- gets the city name
getCity :: City -> String
getCity (City name _ _ _ ) = name

population :: City -> [Int]
population (City _ _ _ pop ) = pop

north :: City -> Int
north (City _ n _ _ ) = n


east :: City -> Int
east (City _ _ e _) = e






showCities :: [City] -> String
showCities [] = ""
showCities [x] = showCity x
showCities (x:xs) = showCity x ++ "\n" ++ showCities xs

citiesToString :: [City] -> String
citiesToString [] = ""
citiesToString [x] = getCity  x
citiesToString (x:xs) = getCity x ++ "\n" ++ citiesToString xs

pop :: [City] -> String -> Int -> String
pop cities cityName year =
    intoString (findCity >>= getPopulation)
    where
    intoString = maybe "no data" formatPopulation
    formatPopulation pInThousand = show (pInThousand `div` 1000) ++ "." ++ show(pInThousand `mod` 1000) ++ "m"
    findCity = find (\c -> getCity c == cityName) cities
    getPopulation city =
        let
            ps = population city
            l = length ps
        in
            if year >= 0 && year < l
                then Just  (ps !! year)
                else Nothing

showPopulationList:: PrintfArg int => [int] -> String
showPopulationList popList = intercalate "," $
                            Data.List.map (printf "%3d") popList


formatList :: City -> String
formatList (City a b c cs ) = a ++ "|" ++ show b ++ "|" ++ show c ++"|" ++ intercalate ", " (Data.List.map show cs)


showCity :: City -> String
showCity city = 
    getCity city
        ++" | "
        ++ show (north city)
        ++ " | "
        ++ show (east city)
        ++ " | "
        ++ show (population city)

updatePopulations :: [City] -> [Int] -> [City]
updatePopulations ps ms = zipWith f ps ms
                            where
                            f p m = p {populationPerYear = m :  (populationPerYear p)}












demo :: (Eq a, Num a) => a -> IO ()
demo 1  = putStrLn (citiesToString testData)
demo 2 = putStrLn (pop testData "Madrid" 2)
demo 3 = putStrLn (showCities testData)
demo 4 = putStrLn (showCities(updatePopulations testData [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000]))

--




--
-- Screen Utilities (use these to do the population map)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text


--
-- Your population map code goes here
--



--
-- Your user interface (and loading/saving) code goes here
--
