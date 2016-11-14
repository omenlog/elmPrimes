module Main exposing (..)

import Html exposing (text)


checkPrimalityOf : Int -> Int -> Int -> Bool
checkPrimalityOf x y root =
    if (x <= 1) then
        False
    else if (x == 2) then
        True
    else if (rem x 2 == 0) then
        False
    else if (y > root) then
        True
    else if (rem x y == 0) then
        False
    else
        checkPrimalityOf x (y + 2) root


squareRoot : Int -> Int
squareRoot x =
    toFloat x |> sqrt |> floor


createSieve : number -> List number
createSieve x =
    [2..(x - 1)]


isPrime : Int -> Bool
isPrime x =
    checkPrimalityOf x 3 (squareRoot x)


removeMultiplesOf : Int -> List Int -> List Int
removeMultiplesOf x sieve =
    let
        begin =
            List.filter (\n -> n > x && n < x * x) sieve

        end =
            List.filter (\n -> n > x * x) sieve
    in
        begin ++ (removeMultiples x end)


removeMultiples : Int -> List Int -> List Int
removeMultiples prime sieveEnd =
    case sieveEnd of
        [] ->
            []

        x :: xs ->
            if (rem x prime /= 0) then
                x :: removeMultiples prime xs
            else
                removeMultiples prime xs


findPrimesIn : List Int -> List Int
findPrimesIn sieve =
    case sieve of
        [] ->
            []

        x :: _ ->
            if ((isPrime x)) then
                let
                    newSieve =
                        removeMultiplesOf x sieve
                in
                    [ x ] ++ (findPrimesIn newSieve)
            else
                findPrimesIn (List.drop 1 sieve)


primesLessThan : Int -> Maybe (List Int)
primesLessThan x =
    if (x <= 2) then
        Nothing
    else
        let
            sieve =
                createSieve x
        in
            Just (findPrimesIn sieve)


main : Html.Html a
main =
    text (primesLessThan 1000 |> toString)
