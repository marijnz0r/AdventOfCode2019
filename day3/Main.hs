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
    let firstWirePoints = executeInstruction [(0,0)] Up
    putStrLn $ show firstWirePoints
    -- putStrLn $ show $ map parseInstruction firstWire

parseInstruction :: String -> Instruction
parseInstruction a = ((parseDirection $ head a) , read(tail a))

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Main.Left
parseDirection 'R' = Main.Right

addPoint :: [Point] -> Point -> [Point]
addPoint points point = points ++ [point]

incrementPoint :: Point -> Direction -> Point
incrementPoint point Up = (fst point, (snd point) + 1)  
incrementPoint point Down = (fst point, (snd point) - 1)  
incrementPoint point Main.Left = ((fst point) - 1, snd point)  
incrementPoint point Main.Right = ((fst point) + 1, snd point)  

executeInstruction :: [Point] -> Direction -> [Point]
executeInstruction points direction = points ++ [incrementPoint (last points) direction]

