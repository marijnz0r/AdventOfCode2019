import Data.List 

main :: IO ()
main = do
    let allPasswords = [x | x <- [158126..624574]]
    let numberOfOptionalPasswords = length $ filter compliesToRules allPasswords
    print numberOfOptionalPasswords

compliesToRules :: Integer -> Bool
compliesToRules a = (adjacent b) && (neverDecrease b)
    where b = digits a

adjacent :: [Int] -> Bool
adjacent a = any (\x -> length x ==2) (Data.List.group a)

neverDecrease :: [Int] -> Bool
neverDecrease a = all (\x -> (fst x <= snd x)) (zip a (tail a))

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

