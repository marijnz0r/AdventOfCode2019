import Data.List.Split

data Direction = Up | Right | Down | Left deriving Show

type Instruction = (Direction, Int) 

main :: IO ()
main = do
    content <- readFile "input"
    let wires = splitOn "\n" content
    let firstWire = splitOn "," (wires !! 0)
    let secondWire = splitOn "," (wires !! 1)
    putStrLn $ show $ parseInstruction $ head $ firstWire
    putStrLn $ show firstWire

parseInstruction :: String -> Instruction
parseInstruction a = ((parseDirection $ head a) , read(tail a))

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Main.Left
parseDirection 'R' = Main.Right

