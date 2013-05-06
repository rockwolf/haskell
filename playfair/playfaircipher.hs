-- Author: Andy Nagels
-- Date: 2013-05-03
-- Playfair cipher encryptor/decryptor
-- For another solution, please see
-- http://bonsaicode.wordpress.com/2009/07/03/programming-praxis-the-playfair-cipher/

import Data.Char (isLetter, isSpace, toUpper, toLower)


data encryptedText = []
data decryptedText = []
data keyword = "tesuto"
data abc = ['a'..'z']

encryptText :: String -> String
encryptText undefined

decryptText :: String -> String
decryptText undefined

removeWhiteSpace :: String -> String
removeWhiteSpace = filter (not . isSpace) 

removeNonLetters :: String -> String
removeNonLetters = filter isLetter

xPadding :: String -> String
xPadding [] = []
xPadding (x:y:xs) = do
    if (x == y && x /= "x")
    then xPadding x:"x":y:xs
    else xPadding xs

xEnding :: String -> String
xEnding [] = []
xEnding xs = do
    if even $ length xs
    then xs = xs ++ "x" 
    else xs = xs

upperCase, lowerCase :: String -> String
upperCase = map toUpper
lowerCase = map toLower

preparedString :: String -> String
preparedString = removeWhiteSpace . removeNonLetters $ toLower

-- create the ciphertable
-- e.g.:
-- K E Y W O
-- R D A B C
-- F H I J L
-- M P Q S T
-- U V X Y Z
cipherTable :: String -> [Char]
cipherTable keyword = do
    keyword ++ (getMissingLetters keyword)

-- needs to be a 5x5 grid
makeTable5x5 :: [Char] -> [Char]
makeTable5x5 [] = []
makeTable5x5 xs = if length xs == 25 
                   then xs
                   else removeQ xs

-- or replace J with I in the input
iToJ :: [Char] -> [Char]
iToJ [] = []
iToJ (x:xs) = if x == 'j'
              then iToJ 'i':xs
              else iToJ xs

removeQ :: [Char] -> [Char]
removeQ [] = []
removeQ xs = [x | x <- xs, not x == q]

getMissingLetters :: [Char] -> [Char]
getMissingLetters [] = abc -- no keyword = return the abc
getMissingLetters = [x | x <- ['a'..'z'], not . x `elem` keyword] -- x not in keyword

-- [a, b, c, d] becomes [ab, cd]
groupByTwo :: [Char] -> [String]
groupByTwo [] = []
groupByTwo (x:y:xs) = x:[y] ++ (groupByTwo xs)

main = do
    let var_prepared = preparedString $ lowerCase "   aBC def ghi .123,'/?=+-_ "
    putStrLn var_prepared
    let encryptedText = var_prepared
    putStrLn encryptedText
    let var_padded = xPadding var_prepared 
    putStrLn var_padded
    let var_ended = xEnding var_padded
    putStrLn var_ended
    let var_grouped = groupByTwo var_ended
    putStrLn $ first var_grouped
    putStrLn $ last var_grouped
