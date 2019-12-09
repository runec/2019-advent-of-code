module Main where

  import Data.List.Split (splitOn)
  import Data.List (intercalate)
  import Data.Vector ((!), (//))
  import qualified Data.Vector as V
  
  type IntCodeProgram = V.Vector Int
  
  data IntCodeProgramState = Running IntCodeProgram Int [Int] [Int] | Completed [Int] | Failed
  
  main :: IO ()
  main = do
    intCodes <- readIntCodeFromFile "puzzle.input"
    let program = V.fromList intCodes 
    let finalState = runIntCodeProgram program [5]
    case finalState of
      Completed outs -> putStrLn $ "Completed with outputs [" ++ intercalate ", " (map show outs) ++ "]"
      Failed -> putStrLn "Program execution failed"
      _ -> putStrLn "Program did neither fail nor complete"
  
  readIntCodeFromFile :: FilePath -> IO [Int]
  readIntCodeFromFile filePath = do
    fileContent <- readFile filePath
    return $ map read $ splitOn "," fileContent
  
  runIntCodeProgram :: IntCodeProgram -> [Int] -> IntCodeProgramState
  runIntCodeProgram program inputs = runIntCodeProgramFromState $ Running program 0 inputs []
  
  runIntCodeProgramFromState :: IntCodeProgramState -> IntCodeProgramState
  runIntCodeProgramFromState state = 
    let 
      nextState = runIntCodeStep state
    in
      case nextState of 
        Running{}-> runIntCodeProgramFromState nextState
        Failed -> Failed
        Completed out -> Completed $ reverse out
  
  runIntCodeStep :: IntCodeProgramState -> IntCodeProgramState
  runIntCodeStep (Running program currentPointer inputs outputs) =
    let
      instructionCode = program ! currentPointer
      opcode = instructionCode `mod` 100
      inputMode1 = (instructionCode `div` 100) `mod` 10
      inputMode2 = (instructionCode `div` 1000) `mod` 10
    in
      case opcode of
        99 -> Completed outputs
        3 -> 
          case inputs of
            [] -> Failed
            i:ins -> 
              let
                input1 = program ! (currentPointer + 1)
                updatedProgram = program // [(input1, i)]
              in
                Running updatedProgram (currentPointer + 2) ins outputs
        4 ->
          let
            out = program ! (currentPointer + 1)
            outVal = if inputMode1 == 1 then out else program ! out
          in
            Running program (currentPointer + 2) inputs (outVal:outputs) 
        x | x `elem` [5, 6] ->
          let
            input1 = program ! (currentPointer + 1)
            input2 = program ! (currentPointer + 2)
            input1Value = if inputMode1 == 1 then input1 else program ! input1
            input2Value = if inputMode2 == 1 then input2 else program ! input2
            shouldJump = (x == 5 && input1Value /= 0) || (x == 6 && input1Value == 0)
          in
            Running program (if shouldJump then input2Value else currentPointer + 3) inputs outputs
        x | x `elem ` [7, 8] ->
          let
            input1 = program ! (currentPointer + 1)
            input2 = program ! (currentPointer + 2)
            input1Value = if inputMode1 == 1 then input1 else program ! input1
            input2Value = if inputMode2 == 1 then input2 else program ! input2
            isSatisfied = (x == 7 && input1Value < input2Value) || (x == 8 && input1Value == input2Value)
            outputPointer = program ! (currentPointer + 3)
            updatedProgram = program // [(outputPointer, if isSatisfied then 1 else 0)]
          in
            Running updatedProgram (currentPointer + 4) inputs outputs
        x | x `elem` [1, 2] ->
          let
            input1 = program ! (currentPointer + 1)
            input2 = program ! (currentPointer + 2)
            input1Value = if inputMode1 == 1 then input1 else program ! input1
            input2Value = if inputMode2 == 1 then input2 else program ! input2
            value = if opcode == 1 then input1Value + input2Value else input1Value * input2Value
            outputPointer = program ! (currentPointer + 3)
            updatedProgram = program // [(outputPointer, value)]
          in
            Running updatedProgram (currentPointer + 4) inputs outputs
        _ -> Failed
  runIntCodeStep Failed = Failed
  runIntCodeStep (Completed x) = Completed x