--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- up957029
--

import Data.List




import Text.Printf


--
-- Types
data City = City {city:: String, northDeg:: Int, eastDeg:: Int, populationPerYear:: [Int]}


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

-- helper functions for fomratting populations in iii)
formatPop :: [City] -> String -> String
formatPop (City name n e popul:rest) cityName
    | cityName == name =  printf ( zeroP  ++ " "  ++  oneP)
    | cityName /= name = formatPop rest cityName
    where zeroP = pop (City name n e popul:rest) name 0
          oneP  = pop (City name n e popul:rest) name 1


twoYears :: [City] -> String -> String
twoYears ((City name degN degE popList):rest) cityName
    |  cityName == name = formatPop (City name degN degE popList:rest) cityName
    |  cityName /= name = twoYears rest cityName

-- helper function for creating columns
cityToString :: [City] -> City -> String
cityToString listOfCities (City name n e populations) =
    printf "%-9s" name ++ " "                    ++
    printf "%9d " e ++ printf "%9.0d " n  ++ 
    twoYears listOfCities (getCity (City name n e populations))

-- iii)
showCities :: [City] -> String
showCities [] = ""
showCities [x] = cityToString [x] x
showCities (x:xs) = cityToString (x:xs) x ++ "\n" ++ showCities xs

-- iv)
updatePopulations :: [City] -> [Int] -> [City]
updatePopulations = zipWith f
    where
    f p m = p {populationPerYear = m :  populationPerYear p}

-- v)
addCity :: [City] -> String -> Int -> Int ->[Int] -> [City]
addCity listOfCities name n e popList =
    sort (City name n e popList :  listOfCities)

-- helper function for vi)
populationList :: [Int] -> Int -> [Double]
populationList [] year = []
populationList (x:xs) year = (fromIntegral (year - x) / fromIntegral x) : populationList xs x

-- vi)
yearlyIncrease :: [City] -> String -> [Double]
yearlyIncrease ((City name degN degE (x:xs)):rest) cityName
    | cityName == name =  populationList xs x
    | otherwise = yearlyIncrease rest cityName

-- Helper functions for vii)
closestDistance :: [City] -> Int -> Int -> Int -> (Double,String)
closestDistance ((City name degN degE (x:xs)):rest) n e minPop
    | x < minPop =  closestDistance rest n e minPop
    | x >= minPop =
    if distance n e degN degE < d
    then (distance n e degN degE,name)
    else (d, cityName)
        where (d, cityName)  = closestDistance rest n e minPop
closestDistance [] n e minPop = (100000000, "No city")

distance :: Int -> Int -> Int -> Int -> Double
distance n1 e1 n2 e2 = sqrt(fromIntegral ((n1 - n2)^2 + (e1 - e2)^2))

-- vii)
closestCity :: [City] -> Int -> Int -> Int -> String
closestCity listOfCities n e minPop = cityName
    where (distance,cityName) = closestDistance listOfCities n e minPop

-- demos
demo :: (Eq a, Num a) => a -> IO ()
demo 1  = putStrLn (citiesToString testData)
demo 2 = putStrLn (pop testData "Madrid" 2)
demo 3 = putStrLn (showCities testData)
demo 4 = putStrLn (showCities(updatePopulations testData [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000]))
demo 5 = putStrLn (showCities(addCity testData "LA" 45 23 [3214, 3781, 4000]))
demo 6 = print (yearlyIncrease testData "London")
demo 7 = putStr (closestCity testData 67 42 3567)
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
saveCities :: [City] -> IO()
saveCities citiesTestData =
    writeFile "cities.txt" (show citiesTestData)

loadCities :: IO [City]
loadCities = do
  contents <- readFile "cities.txt"
  return (read contents :: [City])

choices :: [City] -> IO ()
choices citiesList = do
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
    choice <- getLine
    completeChoice choice testData



completeChoice :: String -> [City] -> IO ()
completeChoice "1" citiesList = do
  putStrLn (citiesToString citiesList)
  choices citiesList


completeChoice "2" citiesList = do
    putStrLn "What city?"
    city <- getLine
    let cityName = read city :: String
    putStrLn "What year do you want to see the population?"
    populationInt <- getLine
    let popFigure = read populationInt :: Int
    putStrLn (pop citiesList cityName popFigure)
    choices citiesList

completeChoice "3" citiesList = do
    putStrLn (showCities citiesList)
    choices citiesList

completeChoice"4" citiesList = do
    putStrLn "Please enter 10 population figures in a list to update the population of all cities"
    popLists <- getLine
    let listOfPops = read popLists :: [Int]
    putStrLn (showCities(updatePopulations citiesList listOfPops))
    choices citiesList

completeChoice "5" citiesList = do
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
    choices citiesList

completeChoice "6" citiesList = do
    putStrLn "Please enter the city you want to check the yearly increase on."
    cityChoice <- getLine
    let cityName = read cityChoice :: String
    print (yearlyIncrease testData cityName)
    choices citiesList

completeChoice "7" citiesList = do
    putStrLn "Please enter the north coordinates. "
    n <- getLine
    let north = read n :: Int
    putStrLn "Please enter the south Coordinates"
    s <- getLine
    let south = read s :: Int
    putStrLn "Please enter the mininum population. "
    minPop <- getLine
    let minPopulation = read minPop :: Int
    putStrLn (closestCity citiesList north south minPopulation)
    choices citiesList

completeChoice "0" citiesList = do
    saveCities citiesList

completeChoice _ citiesList = do
    putStrLn "\n"
    putStrLn "Please enter a choice from 1 - 7"
    choices citiesList

main :: IO ()
main = do
    citiesList <- loadCities
    choices citiesList


--
