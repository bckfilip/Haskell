module F1 where
-- Filip Bäck 2019-01-19
-- F1

import Data.Char

{- 1. fibonacci
Prints the sum of the first n numbers in the fibonacci sequence recursively.
input: integer
output: integer
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)



{- 2,1. rovarsprak
prints the 'rovarsprak' version of a word. That is, every consonant n is concatenated with 'on', recursively.
Input: Char
Output: Char
-}
vowels = "aeoiyu" --variable used in both 2.x functions.

rovarsprak :: [Char] -> [Char]
rovarsprak [] = []
rovarsprak (x:xs) 
    |vokal = x:rovarsprak xs 
    |otherwise = x:'o':x:rovarsprak xs
    where vokal = x `elem` vowels

{- 2,2. karpsravor
Reverses words written in 'rovarsprak' to it's original form. It locates every consonant and drops the two following elements, recursively.
Input: Char
Output: Char
-}
karpsravor :: [Char] -> [Char]
karpsravor [] = []
karpsravor (x:xs)
    |vokal = x:karpsravor xs
    |otherwise = x:karpsravor(drop 2 xs)
    where vokal = x `elem` vowels

{- 3. medellangd
Counts the average length of the words in a string recursively. isAlpha is imported and used in helper function for letter-in-alphabet check.
Variables: a=lettercount, b=wordcount. Guards in helper function only increment wordcount if a letter is followed by a worddivider.
Input: String
Output: average length of words in inputstring as a Double.
-}
medellangd :: [Char] -> Double
medellangd [] = 0
medellangd s 
            |isAlpha(last s) = a/(b+1)
            |otherwise = a/b
            where (a,b) = mlhelper s (0, 0) False

mlhelper :: [Char] -> (Double, Double) -> Bool -> (Double, Double)
mlhelper [] (a,b) f = (a,b)
mlhelper (x:xs) (a,b) f
    |isAlpha x =  mlhelper xs ((a+1), b) True 
    |not(isAlpha x) && f =  mlhelper xs (a,(b+1)) False
    |otherwise = mlhelper xs (a,b) False



{- 4. skyffla
Takes a list and shuffles them in a specific order. Basically picking every other element of the original list over and over until it's empty.
Done recursively with a helper function that takes four different cases.
Input: a list
Output: shuffled inputlist
-}
skyffla :: [a] -> [a]
skyffla [] = []
skyffla s = skyfflahelp s [] []

skyfflahelp :: [a] -> [a] -> [a] -> [a]
skyfflahelp [] [] c =  reverse c
skyfflahelp [] b c = skyfflahelp (reverse b) [] c
skyfflahelp (a1:a2:as) bs cs = skyfflahelp as (a2:bs) (a1:cs)
skyfflahelp (a1:as) bs cs = skyfflahelp as bs (a1:cs)   
    
