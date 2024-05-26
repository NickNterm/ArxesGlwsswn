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
deleteIntI s r = deleteIntIHlp s r 1

-- Helper function that keeps the index of 
-- the elements that we want to delete
deleteIntIHlp :: [Int]->[Int]->Int->[Int]
-- if the r list is empty then we return the s list
deleteIntIHlp s [] i = s
-- if the s list is empty then we again return thes list
deleteIntIHlp [] r i = []
-- Else we check if the i element is on the r list
deleteIntIHlp (h1:t1) (h2:t2) i
    -- if it is we delete it 
    | i == h2 = deleteIntIHlp t1 t2 (i+1)
    -- else skip this index
    | otherwise = h1:deleteIntIHlp t1 (h2:t2) (i+1)


-----------------------------------------------------------------------------------------
     
-- ASKHSH 5

smooth :: [Int]->Int->[Int]
smooth s@(h1:t1) k
    -- in case the length of the list is smaller than k
    -- return an empty list, ( no reason to smooth it )
    | length s < k = []
    -- otherwise we use the smoothHlp to get the averange of
    -- the first k elements. Then we keep it as the h1
    -- of the list that we return. afterwards we call 
    -- smooth to the rest of the list
    | otherwise = avrg:smooth t1 k
        where avrg = smoothHlp s k 1 0

-- this function takes the list and the k, i and it gets
-- the averange for those items on the list
smoothHlp :: [Int]->Int->Int->Int->Int
smoothHlp [] k i c = c `div` k 
smoothHlp (h1:t1) k i c
    | i == k = c `div` k
    | i < k = smoothHlp t1 k (i+1) (c+h1)





-----------------------------------------------------------------------------------------
     
-- ASKHSH 6
-- Basic Info: This function is going to return all the possible partitions
-- of a String. To do that we use the partitionHlp function that is going to
-- get the Word (w) and then Current result (r) and return all the possible partiitons
partition :: String->[[String]]
partition w = tail(partitionHlp w [])

partitionHlp :: String->[String]->[[String]]
partitionHlp w r 
    -- if the length of the word is 0 then return the result
    -- r is going to have the splitted version of the word
    | length w == 0 = [r]
    -- otherwise we call the partitionHlpHlp to get all the possible
    -- partitions of the word
    | otherwise = partitionHlpHlp w r 1

-- this function gets the Word (w) and the Current result (r)
-- also the index (i) that is going to increment for the loop
-- We init the i in 1
partitionHlpHlp :: String->[String]->Int->[[String]]
partitionHlpHlp w r i
    -- cause i starts in 1 then the max is len w + 1 that the loop breaks
    | i == length w + 1 = []
    -- otherwise we call the partitionHlpHlp to continue the loop and we add
    -- the call to the partitionHlp to get the partitions of the word which is dropped by i
    -- also here we save the (r++[take i w]) which is the current partition
    | otherwise = partitionHlpHlp w r (i+1) ++ partitionHlp (drop i w) (r++[take i w]) 

    

-----------------------------------------------------------------------------------------

-- ASKHSH 7

move :: Eq u => [u]->u->Int->[u]

move s x n 
    -- if the element x is not in the list s then return the list s
    | findInList s x == False = s
    -- if n == 0 then just remove the first element x
    | n == 0 = removeFirstxItem s x
    -- Fact: if the n is negative, then the index of x is MINUS the |n|
    --       if the n is positive, then the index of x is PLUS the  |n|
    -- so either way we move the element x by |n| and the sign is the direction we move
    -- that way we don't need 2 different lines to move to right/left and we only need one
    | otherwise =  insertAt (removeFirstxItem s x) x (index s x + n)

-- This returns the index of the element x in the list s
index :: Eq u => [u]->u->Int
index s x = indexHlp s x 0

-- We use a helper to count the index of the element x
indexHlp :: Eq u => [u]->u->Int->Int
indexHlp [] x i = -1
indexHlp (h1:t1) x i
    | h1 == x = i
    | otherwise = indexHlp t1 x (i+1)

-- This Inserts the element x at the index n and returns the list
insertAt :: Eq u => [u]->u->Int->[u]
insertAt [] x n = [x]
insertAt (h1:t1) x n
    | n == 0 = x:h1:t1
    | otherwise = h1:insertAt t1 x (n-1)

-- This removes the first element x from the list s
removeFirstxItem :: Eq u => [u]->u->[u]
removeFirstxItem [] x = []
removeFirstxItem (h1:t1) x
    | h1 == x = t1
    | otherwise = h1:removeFirstxItem t1 x

-- This function checks if the element x is in the list s
findInList :: Eq u => [u]->u->Bool
findInList [] x = False
findInList (h1:t1) x
    | h1 == x = True
    | otherwise = findInList t1 x




-----------------------------------------------------------------------------------------

-- ASKHSH 8

inverse :: (Int->Int)->(Int->Int)
-- here just return the helper
inverse f = \n -> inverseHlp f n 0

inverseHlp :: (Int->Int)->Int->Int->Int
inverseHlp f n i
    -- if the f(i) == n then return i
    | f i == n = i
    -- else increment i and call the function again
    -- that way you are going to find the smallest i
    -- that is positive and f(i) == n
    | otherwise = inverseHlp f n (i+1)





-----------------------------------------------------------------------------------------
     
-- ASKHSH 9

sumfab :: (Int->Int->Int->Int)->Int->Int->Int
-- just user the helper to use the k as the a as first
sumfab f a b = sumfabHlp f a b a 


sumfabHlp :: (Int->Int->Int->Int)->Int->Int->Int->Int
sumfabHlp f a b i
    -- if a == b then return 0
    | a == b = 0
    -- if the i is equal to b then we are done
    | i == b = f a i b
    -- if the i is smaller than b then we increment i
    | i < b = (f a i b) + sumfabHlp f a (i+1) b
    -- if the i is bigger than b then we decrement i
    | i > b = (f a i b) + sumfabHlp f a (i-1) b


-----------------------------------------------------------------------------------------
     
-- ASKHSH 10

hof :: [Integer->Integer]->(Integer->Integer)

-- just use the helper to start from 1
hof fk = \n -> hofHlp fk n 1

hofHlp :: [Integer->Integer]->Integer->Integer->Integer
-- if the list has only one function then use the general calc
hofHlp [f] n i = calc f n i
-- just use the calc to calculate the currect and move to the next
hofHlp fk@(f1:ft) n i = calc f1 n i + hofHlp ft n (i+1)

calc :: (Integer->Integer)->Integer->Integer->Integer
calc f n i = f ((n-i)+1) `div` (2^(i-1))










   









