import Data.Array

-- Define Intcodes as an Array of Integers indexed by Int
type Intcodes = Array Int Integer

split :: Char -> String -> [Integer]
split _ "" = []
split c s = read firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s
main = do
    content <- readFile "input"
    let items = split ',' content
    let amount = length items
    let array = (listArray (1, amount) items)  // [(2, 12), (3, 2)]
    putStrLn (show (runProgram array))

computeIntCodes :: Intcodes -> Int -> Intcodes
computeIntCodes input index
    | indexValue == 99 = input
    | indexValue == 1 = computeIntCodes (doAddition input (index + 1) (index + 2) (index + 3)) (index + 4)
    | indexValue == 2 = computeIntCodes (doMultiplication input (index + 1) (index + 2) (index + 3)) (index + 4)
    where indexValue = input ! index
computeIntCodes a _ = a

doAddition :: Intcodes -> Int -> Int -> Int -> Intcodes
doAddition input firstIndex secondIndex resultIndex = input // [(position, result)]
    where result = input ! (fromIntegral((input ! (firstIndex)+1))) + input ! (fromIntegral(((input ! (secondIndex)+1))))
          position = fromIntegral(input ! resultIndex)+1

doMultiplication :: Intcodes -> Int -> Int -> Int -> Intcodes
doMultiplication input firstIndex secondIndex resultIndex = input // [(position, result)]
    where result = (input ! (fromIntegral(input ! (firstIndex)+1))) * (input ! (fromIntegral(input ! (secondIndex)+1)))
          position = fromIntegral(input ! resultIndex) +1

runProgram :: Intcodes -> Int
runProgram input = fromIntegral ((computeIntCodes input (fromIntegral 1)) ! 1)