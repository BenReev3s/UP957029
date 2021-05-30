--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- up957029
--

import Data.List



import Data.Map
import Data.Ord

import Data.String
import GHC.Base ((<|>))
import Text.PrettyPrint
import Text.PrettyPrint
import Text.Printf
import Text.Read (readMaybe) 
--
-- Types
data City = City {city:: String, northDeg:: Int, southDeg:: Int, populationPerYear:: [Int]}
--
    deriving (Show, Read, Eq, Ord)

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
-- gets population list
population :: City -> [Int]
population (City _ _ _ pop ) =  pop
-- gets the north degree 
north :: City -> Int
north (City _ n _ _ ) = n
-- gets the east degree
east :: City -> Int
east (City _ _ e _) = e

-- iii)
showCities :: [City] -> String
showCities [] = ""
showCities [x] = showCity [x] x 
showCities (x:xs) = showCity (x:xs) x ++ "\n" ++ showCities xs

-- i)
citiesToString :: [City] -> String
citiesToString [] = ""
citiesToString [x] = getCity  x
citiesToString (x:xs) = getCity x ++ "\n" ++ citiesToString xs

-- ii)
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

-- helper function
showPopulationList:: PrintfArg int => [int] -> String
showPopulationList popList = intercalate "," $
                            Data.List.map (printf "%3d") popList

formatList :: City -> String
formatList (City a b c cs ) = a ++ "|" ++ show b ++ "|" ++ show c ++"|" ++ intercalate ", " (Data.List.map show cs)

twoPop :: [City] -> String -> String
twoPop (City name n e popul:rest) ctName
    | ctName == name =  printf ( zeroP  ++ " "  ++  oneP) 
    | ctName /= name = twoPop rest ctName
    where zeroP = pop (City name n e popul:rest) name 0
          oneP  = pop (City name n e popul:rest) name 1


twoPopData :: [City] -> String -> String
twoPopData ((City name degN degE popList):rest) ctName
    |  ctName == name = twoPop ((City name degN degE popList):rest) ctName
    |  ctName /= name = twoPopData rest ctName



-- helper function for question (iii) 
showCity :: [City] -> City -> String
showCity listOfCities city =
    getCity city
        ++" | "
        ++ show (north city)
        ++ " | "
        ++ show (east city)
        ++ " | "
        ++  (twoPopData listOfCities (getCity city))


-- iv)
updatePopulations :: [City] -> [Int] -> [City]
updatePopulations ps ms = zipWith f ps ms
    where
    f p m = p {populationPerYear = m :  (populationPerYear p)}




-- v)

addCity :: [City] -> String -> Int -> Int ->[Int] -> [City]
addCity listOfCities name n e popList =
   sort (City name n e popList :  listOfCities)


-- For a given city name, return a list of annual percentage population growth figures for
--that city (i.e., the result list should begin with the percentage increase from last year’s
--figure to this year’s; the second value should give the increase from two years ago to
--last year, etc.). The list will include negative values for shrinking populations.


-- vi)
yearlyIncrease :: [City] -> String -> [Double] 
yearlyIncrease ((City name degN degE (x:xs)):rest) cityName
    | cityName == name =  populationList xs x 
    | otherwise = yearlyIncrease rest cityName 
    


populationList :: [Int] -> Int -> [Double]
populationList [] year = []
populationList (x:xs) year = ((fromIntegral (year - x)) / fromIntegral (x)) : populationList xs x

-- vii)

--Given a location and a number, return the name of the closest city with a population
--bigger than the number, or “no city” if there are no such cities; use Pythagoras’ theorem
--to calculate the distance between locations (i.e. assume the world is flat!)

closestDistance :: [City] -> Int -> Int -> Int -> (Double,String) 
closestDistance ((City name degN degE (x:xs)):rest) n e minPop
    | x < minPop =  (closestDistance rest n e minPop)
    | x >= minPop =
    if distance n e degN degE < d
    then (distance n e degN degE,name)
    else (d, cityName)
        where (d, cityName)  = (closestDistance rest n e minPop)
closestDistance [] n e minPop = (100000000, "No city") 

distance :: Int -> Int -> Int -> Int -> Double
distance n1 e1 n2 e2 = sqrt(fromIntegral ((n1 - n2)^2 + (e1 - e2)^2))

closestCity :: [City] -> Int -> Int -> Int -> String
closestCity listOfCities n e minPop = cityName
    where (distance,cityName) = closestDistance listOfCities n e minPop 

    




demo :: (Eq a, Num a) => a -> IO ()
demo 1  = putStrLn (citiesToString testData)
demo 2 = putStrLn (pop testData "Madrid" 2)
demo 3 = putStrLn (showCities testData)
demo 4 = putStrLn (showCities(updatePopulations testData [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000]))
demo 5 = putStrLn (showCities(addCity testData "LA" 45 23 [3214, 3781, 4000]))
demo 6 = putStrLn (show((yearlyIncrease testData "London")))
demo 7 = putStr (closestCity testData 34 42 3567)


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
--data City = City {city:: String, northDeg:: Int, southDeg:: Int, populationPerYear:: [Int]}

parseCities :: String -> [City]
parseCities fileContents = do
    [city', northDeg', southDeg' ] <- words <$> lines fileContents
    Just northDeg'' <- return (readMaybe northDeg')
    Just southDeg'' <- return (readMaybe southDeg')
    return (City { city = city', northDeg = northDeg'',southDeg  = southDeg'', populationPerYear = [] })

loadCities :: IO [City]
loadCities = do
    cities <- parseCities <$> readFile "cities.txt"
    --contents <- readFile "cities.txt" 
    return cities

startMenu :: [City] -> IO ()
startMenu citiesList   = do
    putStrLn ""  
    putStrLn "Enter a number that corresponds to the desired choice: "
    putStrLn "1: Return a list of names of all cities."
    putStrLn "2: Return the population of a city with the given year."
    putStrLn "3: Return all data as a single string in 5 columns."
    putStrLn "4: Update the data with a list of new populations."
    putStrLn "5: Add a new city."
    putStrLn "6: return a list of annual percentage population growth figures."
    putStrLn "7: Return the closest city"
    putStrLn "0: Close"
    option <- getLine
    executeOption option citiesList

executeOption :: String -> [City] -> IO ()
executeOption "1" citiesList = do
  putStrLn (citiesToString citiesList)
  startMenu citiesList

executeOption "2" citiesList = do
    putStrLn "What city?"
    city <- getLine
    let cityName = read city :: String
    putStrLn "What year do you want to see the population?"
    populationInt <- getLine
    let popFigure = read populationInt :: Int
    putStrLn (pop citiesList cityName popFigure)
    startMenu citiesList

executeOption "3" citiesList = do
    putStrLn (showCities citiesList)
    startMenu citiesList

executeOption "4" citiesList = do
    putStrLn "Please enter 10 population figures in a list to update the population of all cities"
    popLists <- getLine
    let listOfPops = read popLists :: [Int]
    putStrLn (showCities(updatePopulations citiesList listOfPops))
    startMenu citiesList

executeOption "5" citiesList = do
    putStrLn "Please enter a city name"
    nameStr <- getLine
    let nameOfCity = read nameStr :: String
    putStrLn "Enter north cords"
    northCords <- getLine
    let n = read northCords :: Int
    putStrLn "Please enter east cords"
    eastCords <- getLine
    let e = read eastCords :: Int
    putStrLn "Please enter a population list"
    populationL <- getLine
    let listPops = read populationL :: [Int]
    putStrLn (showCities(addCity citiesList nameOfCity n e listPops))
    startMenu citiesList

executeOption "6" citiesList = do
    putStrLn "Please enter the city you want to check the yearly increase on."
    cityChoice <- getLine
    let cityName = read cityChoice :: String
    putStrLn (show((yearlyIncrease testData cityName)))
    startMenu citiesList

main :: IO ()
main = do
    citiesList <- loadCities
    putStrLn "City data:\n"
    putStrLn (citiesToString citiesList)
    startMenu citiesList


--
