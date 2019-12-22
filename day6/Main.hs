import Data.List.Split

main :: IO ()
main = do
    content <- readFile "input"
    let contentAsLines = lines content
    let orbits = map parseOrbit contentAsLines
    let constellation = Node "COM" []
    let constellation2 = insertBody "COM" (head $ findBodies orbits "COM") constellation
    -- let bodiesToBeInserted = 
    print constellation2

type Body = String

type Orbit = (Body, Body)

data Tree a = EmptyTree | Node a [Tree a] deriving Show

parseOrbit :: String -> Orbit
parseOrbit input = (head bodies, last bodies)
    where bodies = splitOn ")" input

findBodies :: [Orbit] -> Body -> [Body]
findBodies allOrbits primary = map snd $ filter (\x -> fst x == primary) allOrbits

insertBody :: Body -> Body -> Tree Body -> Tree Body
insertBody primary satellite (Node a children)
    -- insert in children
    | a == primary = Node primary (children ++ [Node satellite []])
    -- don't insert
    | otherwise = Node a children

