main = do
    content <- readFile "input"
    let contentAsLines = lines content
    putStr (show (calculateTotal (map read contentAsLines)))

calculateTotal :: [Integer] -> Integer 
calculateTotal input = foldl (+) 0 (calculateFuels input)

calculateFuels :: [Integer] -> [Integer]
calculateFuels = map calculateFuel

calculateFuel :: Integer -> Integer
calculateFuel mass = div mass 3 - 2