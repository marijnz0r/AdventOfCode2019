split :: Char -> String -> [Integer]
split _ "" = []
split c s = read firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

main = do
    content <- readFile "input"
    let items = split ',' content
    putStr (show items)

