module Main exposing (..)

esPrimo : Int -> Bool
esPrimo n = contador 2 n 
contador n y = if y == 2
    then  True
    else if modBy n y == 0  then False
    else if n == y - 1 then True else contador (n + 1) y

fibonacci : Int -> Int
fibonacci n = if n == 0 then 0 else if n == 1 then 1 else if n > 1 
    then fibonacci (n - 1) + fibonacci (n - 2) else 0


primos : Int -> List Int
primos n = if n < 2 then [] else if esPrimo n == True then n :: primos (n - 1) else primos (n - 1)

nPrimos : Int -> List Int 
nPrimos n = rango (2,n)
rango (y,n) = if n == 0 then [] else if esPrimo y == True then y :: rango (y + 1,n - 1) else rango (y + 1,n)