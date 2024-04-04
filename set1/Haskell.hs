-----------------------------------------------------------------------------------------

-- ASKHSH 1


luckyDigits :: Integer->Integer->Int                         
luckyDigits a b = p
    -- Get the amount of luckyDigits that the player has found
    -- and then return the price that he is going to win
    where p = price (luckyDigitsHlp a b 0)


-- This helper function just counts how many lucky digits
-- the player has found
luckyDigitsHlp :: Integer->Integer->Int->Int
luckyDigitsHlp a b c
   | a == 0 && b == 0
        = c
   | a `mod` 10 == b `mod` 10 
        = luckyDigitsHlp (a `div` 10) (b `div` 10) (c+1)
   | otherwise = luckyDigitsHlp (a `div` 10) (b `div` 10) c

-- This function is going to return the price 
-- that the player is going to win based on the 
-- number of lucky digits that he has found
price :: Int->Int
price a 
   | a == 0 = 0
   | a == 1 = 0
   | a == 2 = 3
   | a == 3 = 10
   | a == 4 = 45
   | a == 5 = 333
   | a == 6 = 1250
   | a == 7 = 10000
   | a == 8 = 75000
   | a == 9 = 1000000
   | a == 10 = 10000000
   | otherwise = 0



-----------------------------------------------------------------------------------------
-- Γράψτε μία συνάρτηση ab σε Haskell η οποία θα δέχεται ως παράμετρο έναν θετικό ακέραιο αριθ-
-- μό n και θα επιστρέφει ένα ζεύγος ακεραίων (a, b), τέτοια ώστε 1 ≤ a ≤ b, a · b = n και το b − a
-- να είναι το ελάχιστο δυνατό. Ο τύπος της συνάρτησης ab θα πρέπει να είναι Int->(Int,Int).
-- Μπορείτε να υποθέσετε ότι n είναι μη αρνητικός ακέραιος αριθμός. Δεν επιτρέπεται να χρησι-
-- μοποιήσετε λίστες.
-- ASKHSH 2

-- Idea: We are going to get the sqrt of the number n
-- and set it as b. The increment b until we find a number
-- that divides n. Then we return the pair (n/b,b)
-- !We start from sqrt(n) cause there is no reason to start earlier
-- than that cause it not going to be a valid pair a<=b and a*b=n
-- so smaller b is b*b=n
ab :: Int->(Int,Int) 
ab n = abHlp (1, m ) n
    where m = ceiling (sqrt (fromIntegral n))

-- pass it to a helper function that is going to do the job
-- and return the pair
abHlp :: (Int,Int)->Int->(Int,Int)
abHlp (a,b) n
    -- | a > b = abHlp (1,b+1) n
    | n `mod` b == 0 
        = (n `div` b, b)
    | otherwise = abHlp (1,b+1) n



-----------------------------------------------------------------------------------------
     
-- ASKHSH 3

seconds :: (Int,Int)->(Int,Int,Int)->Int

seconds (d,mm) (h,m,s)
    -- check if the time is valid, else return -777
    | h < 0 || h > 23 || m < 0|| m > 59 || s < 0 || s > 59 = -777
    -- check if the month is valid, else return -777
    | mm < 1 || mm > 12 = -777
    -- check if the day is valid, else return -777
    | hasValidDays (d,mm) == False = -777
    -- calculate the seconds
    -- ! have d-1 cause today should not be counted!
    | otherwise = daysTillToday (d-1,mm)*24*60*60 + h*60*60 + m*60 + s

-- Calculate the days that have passed till today
daysTillToday :: (Int,Int)-> Int
daysTillToday (d,mm)
    | mm == 0 = d
    | daysInMonth mm > 0 = (daysInMonth (mm-1)) + daysTillToday (d,mm-1)
    | otherwise = 0

-- Check if the day is valid depending on the month
hasValidDays :: (Int,Int)->Bool
hasValidDays (d,mm)
    | d > 0 && d <= daysInMonth mm = True
    | otherwise = False

-- Gets the month of the year and returns the amount of days that it has
daysInMonth :: Int->Int
daysInMonth mm
    | mm == 1 = 31
    | mm == 2 = 28
    | mm == 3 = 31
    | mm == 4 = 30
    | mm == 5 = 31
    | mm == 6 = 30
    | mm == 7 = 31
    | mm == 8 = 31
    | mm == 9 = 30
    | mm == 10 = 31
    | mm == 11 = 30
    | mm == 12 = 31
    | otherwise = 0






-----------------------------------------------------------------------------------------
     
-- ASKHSH 4

deleteIntI :: [Int]->[Int]->[Int]

deleteIntI s r = [-2024] 






-----------------------------------------------------------------------------------------
     
-- ASKHSH 5

smooth :: [Int]->Int->[Int]

smooth s k =  [-2025.. -2000]





-----------------------------------------------------------------------------------------
     
-- ASKHSH 6

partition :: String->[[String]]

partition w = [["-2024"]] 






-----------------------------------------------------------------------------------------

-- ASKHSH 7

move :: Eq u => [u]->u->Int->[u]

move s x n = []





-----------------------------------------------------------------------------------------

-- ASKHSH 8

inverse :: (Int->Int)->(Int->Int)

inverse f = \n -> -2024





-----------------------------------------------------------------------------------------
     
-- ASKHSH 9

sumfab :: (Int->Int->Int->Int)->Int->Int->Int

sumfab f a b = -2024






-----------------------------------------------------------------------------------------
     
-- ASKHSH 10

hof :: [Integer->Integer]->(Integer->Integer)

hof s = \n -> -2024










   









