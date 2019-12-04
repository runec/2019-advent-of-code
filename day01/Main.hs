module Main where

main :: IO ()
main = do
  masses <- readMassesFromFile "puzzle.input"
  let fuelsPartOne = map calculateFuelPartOne masses
  let totalFuelPartOne = sum fuelsPartOne
  putStrLn $ "Total fuel part one: " ++ show totalFuelPartOne 

  let fuelsPartTwo = map calculateFuelPartTwo masses
  let totalFuelPartTwo = sum fuelsPartTwo
  putStrLn $ "Total fuel part two: " ++ show totalFuelPartTwo 

readMassesFromFile :: FilePath -> IO [Int]
readMassesFromFile filePath = do
  fileContent <- readFile filePath
  let stringMasses = lines fileContent
  return (map read stringMasses :: [Int])

calculateFuelPartOne :: Int -> Int
calculateFuelPartOne mass = mass `div` 3 - 2

calculateFuelPartTwo :: Int -> Int
calculateFuelPartTwo mass = 
  let
    fuel = calculateFuelPartOne mass
  in
    if fuel > 0 then fuel + calculateFuelPartTwo fuel else 0 