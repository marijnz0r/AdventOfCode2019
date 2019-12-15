import Data.List.Split
import Data.List
import qualified Data.HashMap.Strict as HM

data Direction = Up | Right | Down | Left deriving Show

type Instruction = (Direction, Int) 
type Point = (Int, Int)
type PointWithDistance = (Point, Int)

main :: IO ()
main = do
    content <- readFile "input"
    let wires = splitOn "\n" content

    let firstWire = splitOn "," (wires !! 0)
    let secondWire = splitOn "," (wires !! 1)

    let firstWireInstructions = map parseInstruction firstWire
    let secondWireInstructions = map parseInstruction secondWire

    let firstWireDirections = concat $ map getDirectionsFromInstruction firstWireInstructions
    let secondWireDirections = concat $ map getDirectionsFromInstruction secondWireInstructions

    let firstWirePoints = executeDirections ((0,0), 0) firstWireDirections
    let secondWirePoints = executeDirections ((0,0), 0) secondWireDirections

    let firstWirePointsMap = HM.fromList firstWirePoints
    let secondWirePointsMap = HM.fromList secondWirePoints
    print $ minimum (findIntersectionsWithDistance firstWirePointsMap secondWirePointsMap)

parseInstruction :: String -> Instruction
parseInstruction a = ((parseDirection $ head a) , read(tail a))

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Main.Left
parseDirection 'R' = Main.Right

incrementPoint :: PointWithDistance -> Direction -> PointWithDistance
incrementPoint pointWithDistance Up = ((fst (fst pointWithDistance), snd (fst pointWithDistance) + 1),  (snd pointWithDistance)+1)
incrementPoint pointWithDistance Down = ((fst (fst pointWithDistance), snd (fst pointWithDistance) - 1),  (snd pointWithDistance)+1) 
incrementPoint pointWithDistance Main.Left = ((fst (fst pointWithDistance) - 1, snd (fst pointWithDistance)),  (snd pointWithDistance)+1)  
incrementPoint pointWithDistance Main.Right = ((fst (fst pointWithDistance) + 1, snd (fst pointWithDistance)),  (snd pointWithDistance)+1)

executeDirections :: PointWithDistance -> [Direction] -> [PointWithDistance]
executeDirections point directions = scanl incrementPoint point directions

getDirectionsFromInstruction :: Instruction -> [Direction]
getDirectionsFromInstruction instruction = replicate (snd instruction) (fst instruction)

manhattanDistance :: Point -> Int
manhattanDistance point = abs (fst point) + abs (snd point)

findIntersectionsWithDistance :: HM.HashMap Point Int -> HM.HashMap Point Int -> HM.HashMap Point Int
findIntersectionsWithDistance map1 map2 = HM.intersectionWith (+) map1 map2