module Main where
import Data.Char (digitToInt)
import Data.List (foldl')
main :: IO ()
main = do
  let range = (183564, 657474) :: (Int, Int)
  let codesPart1 = validCodes checkCodePart1 range 
  print $ length codesPart1
  let codesPart2 = validCodes checkCodePart2 range
  print $ length codesPart2

validCodes :: (Int -> Bool) -> (Int, Int) -> [Int]
validCodes validator (start, end)
  | start > end = []
  | otherwise = if validator start then start:validCodes validator (start+1, end) else validCodes validator (start+1, end)

checkCodePart1 :: Int -> Bool
checkCodePart1 code = checkEqualAdjacentDigits code && checkDigitsNonDecreasing code

checkCodePart2 :: Int -> Bool
checkCodePart2 code = checkHasDigitGroupWithLength 2 code && checkDigitsNonDecreasing code

checkEqualAdjacentDigits :: Int -> Bool
checkEqualAdjacentDigits code =
  let 
    digits = map digitToInt $ show code
  in
    fst $ foldl' (\(matchFound, prevDigit) digit -> if digit == prevDigit then (True, digit) else (matchFound, digit)) (False, -1) digits

checkHasDigitGroupWithLength :: Int -> Int -> Bool
checkHasDigitGroupWithLength len code =
  let 
    digits = map digitToInt $ show code
    (found, lastGroupLength, _) = foldl' (\(groupFound, curGroupLength, prevDigit) digit -> 
      if digit == prevDigit 
        then (groupFound, curGroupLength +1, digit) 
        else (groupFound || curGroupLength == len, 1, digit)
      ) (False, 0, -1) digits
  in
    found || lastGroupLength == len


checkDigitsNonDecreasing :: Int -> Bool
checkDigitsNonDecreasing code =
  let
    digits = map digitToInt $ show code
  in
    fst $ foldl' (\(result, prevDigit) digit -> if digit >= prevDigit then (result, digit) else (False, digit)) (True, -1) digits
  