module Main where

import Data.List.Split
import Data.Vector ((!), (//))
import qualified Data.Vector as V

type IntCodeProgram = V.Vector Int

data IntCodeProgramState = Running IntCodeProgram Int  | Completed IntCodeProgram | Failed

main :: IO ()
main = do
  intCodes <- readIntCodeFromFile "puzzle.input"
  let program = V.fromList intCodes 
  let finalState = runIntCodeProgram program
  case finalState of
    Completed p -> putStrLn $ "Final Value at position 0 was " ++ show (p ! 0)
    Failed -> putStrLn "Program execution failed"
    _ -> putStrLn "Program did neither fail nor complete"
  let part2Solution = searchForPart2Solution program 19690720 (0, 0)
  case part2Solution of
    Just (a, b) -> putStrLn $ "Found solution for part 2: (" ++ show a ++ ", " ++ show b ++ ")"
    Nothing -> putStrLn "Execution failed while searching for part 2 solution" 

readIntCodeFromFile :: FilePath -> IO [Int]
readIntCodeFromFile filePath = do
  fileContent <- readFile filePath
  return $ map read $ splitOn "," fileContent

runIntCodeProgram :: IntCodeProgram -> IntCodeProgramState
runIntCodeProgram program = runIntCodeProgramFromState $ Running program 0

runIntCodeProgramFromState :: IntCodeProgramState -> IntCodeProgramState
runIntCodeProgramFromState state = 
  let 
    nextState = runIntCodeStep state
  in
    case nextState of 
      Running a b -> runIntCodeProgramFromState nextState
      Failed -> Failed
      Completed p -> Completed p

runIntCodeStep :: IntCodeProgramState -> IntCodeProgramState
runIntCodeStep (Running program currentPointer) =
  let
    opcode = program ! currentPointer
  in
    case opcode of
      99 -> Completed program
      x | x `notElem` [1, 2] -> Failed
      _ ->
        let
          input1Pointer = program ! (currentPointer + 1)
          input2Pointer = program ! (currentPointer + 2)
          input1 = program ! input1Pointer
          input2 = program ! input2Pointer
          value = if opcode == 1 then input1 + input2 else input1 * input2
          outputPointer = program ! (currentPointer + 3)
          updatedProgram = program // [(outputPointer, value)]
        in
          Running updatedProgram (currentPointer + 4)
        
runIntCodeStep Failed = Failed
runIntCodeStep (Completed x) = Completed x

searchForPart2Solution :: IntCodeProgram -> Int -> (Int, Int) -> Maybe (Int, Int)
searchForPart2Solution program target (a,b) =
  let
    initializedProgram = program // [(1, a), (2, b)]
    result = runIntCodeProgram initializedProgram
  in
    case result of
      Completed p
        | (p ! 0) == target -> Just (a,b)
        | a == 0 ->  searchForPart2Solution program target (b+1, 0)
        | otherwise -> searchForPart2Solution program target (a-1, b+1)
      _ -> Nothing
