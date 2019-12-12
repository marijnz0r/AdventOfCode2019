import Data.List.Split

data Direction = Up | Right | Down | Left deriving Show

type Instruction = (Direction, Int) 
type Point = (Int, Int)

main :: IO ()
main = do
    content <- readFile "input"
    let wires = splitOn "\n" content
    let firstWire = splitOn "," (wires !! 0)
    let secondWire = splitOn "," (wires !! 1)
    let firstWireInstructions = map parseInstruction firstWire
    let secondWireInstructions = map parseInstruction secondWire
    let firstWireMap = Map.fromList [(0,0)]
    putStrLn $ show firstWireInstructions 
    -- putStrLn $ show $ map parseInstruction firstWire

parseInstruction :: String -> Instruction
parseInstruction a = ((parseDirection $ head a) , read(tail a))

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Main.Left
parseDirection 'R' = Main.Right

addPoint :: [Point] -> Point -> [Point]
addPoint points point = points : point