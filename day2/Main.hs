split :: Char -> String -> [Integer]
split _ "" = []
split c s = read firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

data Operation = Add | Multiply | Halt deriving (Show)

main = do
    content <- readFile "input"
    let items = split ',' content
    -- putStr (show items)
    putStrLn (show (computeIntCodes [1,0,0,3,99]))

computeIntCodes :: [Integer] -> Integer
computeIntCodes input = computeSingleResult operation (input !! 1) (input !! 2)
    where operation = determineOperation (head input)

determineOperation :: Integer -> Operation
determineOperation 1 = Add
determineOperation 2 = Multiply
determineOperation 99 = Halt

computeSingleResult :: Operation -> Integer -> Integer -> Integer
computeSingleResult Add a b = a + b
computeSingleResult Multiply a b = a * b
computeSingleResult Halt _ _ = 0
