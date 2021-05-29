import Data.List
import System.IO

-- Double
-- Char
-- Tuples

--Basic math functions--
sumOfNums :: Integer
sumOfNums = sum [1..1000]
addEx :: Integer
addEx = 5 + 5
subEx :: Integer
subEx = 5 - 4
multEx :: Integer
multEx = 5 * 4
divEx :: Double
divEx = 5 / 4
modEx :: Integer
modEx = mod 5 4
modEx2 :: Integer
modEx2 = 5 `mod` 4
negNumEx :: Integer
negNumEx = 5 + (-4)
--------------------

---Advanced math functions---

-- fromIntegral converts Int to Float
num9 = 9 :: Int
sqrtOf9 :: Double
sqrtOf9 = sqrt(fromIntegral num9)
-----------------------------

---Logical Operators-----

trueAndFalse :: Bool
trueAndFalse = True && False

trueOrFalse :: Bool
trueOrFalse = True || False

notTrue :: Bool
notTrue = False
-----------------------

---List---

primeNumbers :: [Integer]
primeNumbers = [3,5,7,11]


morePrimes = primeNumbers ++ [13, 17, 19, 23, 29]

-- combine numbers to a list
favNums :: [Integer]
favNums = 2 : 7 : 21 : 66 :[]

multList :: [[Integer]]
multList = [[3,5,7], [11, 13, 17]]

multLists :: [[Integer]]
multLists = [3,5,7]:[4,5,7]:[]
--- adds numbers to front of list
morePrimes2 :: [Integer]
morePrimes2 = 2 : morePrimes

lenPrime :: Int
lenPrime = length morePrimes2
---
--- reverseList
revPrime :: [Integer]
revPrime = reverse morePrimes2

isListEmpty :: Bool
isListEmpty = null morePrimes

-- returns 2nd number in list
secondPrime :: Integer
secondPrime = morePrimes2 !! 1

firstPrime :: Integer
firstPrime = head morePrimes2

lastPrime :: Integer
lastPrime = last morePrimes2

--returns everything but the last number
primeInit :: [Integer]
primeInit = init morePrimes2

first3Primes :: [Integer]
first3Primes = take 3 morePrimes2

removedPrimes :: [Integer]
removedPrimes = drop 3 morePrimes2

is7InList :: Bool
is7InList = 7 `elem` morePrimes2

maxPrime :: Integer
maxPrime = maximum morePrimes2

minPrime :: Integer
minPrime = minimum morePrimes

sumPrimes :: Integer
sumPrimes = sum morePrimes2

newList :: [Integer]
newList = [2,3,5]

prodPrimes :: Integer
prodPrimes = product newList

--makes list from 0 to 10
zeroToTen :: [Integer]
zeroToTen = [0..10]
-- makes even list
evenList :: [Integer]
evenList = [2, 4..20]
-- makes alphabet list
letterList :: [Char]
letterList = ['A', 'B'..'Z']

--infinite list
infinPow10 :: [Integer]
infinPow10 = [10,10..]

many2s :: [Integer]
many2s = take 10 (repeat 2)

many3s :: [Integer]
many3s = replicate 10 3

cycleList :: [Integer]
cycleList = take 10 (cycle [1,2,3,4,5])

--define x times 2 pull value out of list and temp store in x and multipley by 2 and create new list called listTimes2 
listTimes2 :: [Integer]
listTimes2 = [x * 2 | x <- [1..10]]

listTimes3 :: [Integer]
listTimes3 = [x * 3 | x <- [1..50], x * 3 <= 50]

divisBy9N13 :: [Integer]
divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0]

sortedList :: [Integer]
sortedList = sort [9, 1, 8, 3, 4, 7, 6]

sumOfList :: [Integer]
sumOfList = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

listBiggerThen5 :: [Integer]
listBiggerThen5 = filter (>5) morePrimes

evenUpTo20 :: [Integer]
evenUpTo20 = takeWhile (<= 20) [2, 4..]

multOfList :: Integer
multOfList = foldr (*) 1 [2,3,4,5]

--- List comprehension----

pow3List :: [Integer]
pow3List = [3^n | n<-[1..10]]

---genrate multiplication table
multTable :: [[Integer]]
multTable = [[x * y | y <- [1..10 ]] | x <- [1..10]]
---------------------------------

---Tuples----------------
randTuple :: (Integer, [Char])
randTuple = (1, "Random Tuple")

bobSmith = ("Bob Smith", 52)

bobsName :: [Char]
bobsName = fst bobSmith

bobsAge :: Integer
bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]

namesNAdress = zip names addresses

---functions------------------
sumMe :: Num a => a -> a -> a
sumMe x y = x + y