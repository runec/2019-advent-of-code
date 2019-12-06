module Main where
import Data.List.Split
import Text.Read
import Data.Maybe
import Data.List

data Direction = R | U | L | D
type InputWirePart = (Direction, Int)
type InputWire = [InputWirePart]

data SimpleDir = Horizontal | Vertical
type Point = (Int, Int)
type WirePart = (Point, (SimpleDir, Int, Direction))
type Wire = [WirePart]

main :: IO ()
main = do
  inputWires <- readWiresFromFile "./puzzle.input"
  let wires = map getWireFromInputWire inputWires
  let closest = searchForClosestCrossing (head wires) (head $ tail wires)
  let fewestSteps = searchForFewestStepsCrossing (head wires) (head $ tail wires)
  case closest of 
    Just (a,b) -> putStrLn $ "Found closest crossing " ++ show a ++ ", " ++ show b
    Nothing -> putStrLn "Found no crossings"
  case fewestSteps of 
    (Just n, Just (a,b)) -> putStrLn $ "Found fewest steps crossing " ++ show a ++ ", " ++ show b ++ ". Steps: " ++ show n
    _ -> putStrLn "Found no crossings"


readWiresFromFile :: FilePath -> IO [InputWire]
readWiresFromFile filePath = do
  fileContent <- readFile filePath
  let wireParts = map (map readWirePart . splitOn ",") $ lines fileContent
  let wires = mapM sequence wireParts
  return $ fromMaybe [] wires

readWirePart :: String -> Maybe InputWirePart
readWirePart [] = Nothing
readWirePart (d:num) = 
  let
    parsedNum = readMaybe num :: Maybe Int
  in
    case parsedNum of
      Nothing -> Nothing
      Just a -> case d of
        'R' -> Just (R, a)
        'U' -> Just (U, a)
        'L' -> Just (L, a)
        'D' -> Just (D, a)
        _ -> Nothing

getWireFromInputWire :: InputWire -> Wire
getWireFromInputWire inputWire = 
  snd $ mapAccumL (\point inputWirePart -> 
    let 
      newEndPoint = getEndPointOfWirePart point inputWirePart
    in
      (newEndPoint, inputWirePartToWirePart point inputWirePart)
    ) (0,0) inputWire

getEndPointOfWirePart :: Point -> InputWirePart -> Point
getEndPointOfWirePart (x, y) inputWire =
  case inputWire of 
    (R, a) -> (x + a, y)
    (U, a) -> (x, y + a)
    (L, a) -> (x - a, y)
    (D, a) -> (x, y - a)

inputWirePartToWirePart :: Point -> InputWirePart -> WirePart
inputWirePartToWirePart point (d, num) =
  let
    endPoint = getEndPointOfWirePart point (d, num) 
  in
    case d of 
      R -> (point, (Horizontal, num, R))
      U -> (point, (Vertical, num, U))
      L -> (endPoint, (Horizontal, num, L))
      D -> (endPoint, (Vertical, num, D))

manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

stepsToPointAlongWire :: Point -> Wire -> Maybe Int
stepsToPointAlongWire _ [] = Nothing
stepsToPointAlongWire (a, b) (((wx, wy), (Horizontal, num, d)):wire) =
  if b == wy && wx <= a && a <= wx + num then (case d of 
    R -> Just $ a - wx 
    _ -> Just $ wx + num - a 
    ) else 
    case stepsToPointAlongWire (a, b) wire of 
      Nothing -> Nothing
      Just x -> Just $ num + x 
stepsToPointAlongWire (a, b) (((wx, wy), (Vertical, num, d)):wire) =
  if a == wx && wy <= b && b <= wy + num then (case d of 
     U -> Just $ b - wy 
     _ -> Just $ wy + num - b 
     ) else 
    case stepsToPointAlongWire (a, b) wire of 
      Nothing -> Nothing
      Just x -> Just $ num + x 

totalStepsToPointAlongWires :: Point -> Wire -> Wire -> Maybe Int
totalStepsToPointAlongWires point wire1 wire2 =
  let 
    steps1 = stepsToPointAlongWire point wire1
    steps2 = stepsToPointAlongWire point wire2
  in
    case (steps1, steps2) of
      (Just a, Just b) -> Just $ a + b
      _ -> Nothing

wirePartsCrossing :: WirePart -> WirePart -> Maybe Point
wirePartsCrossing ((x1, y1), (d1, num1, _)) ((x2, y2), (d2, num2, _)) =  case (d1, d2) of
    (Horizontal, Vertical) -> if x1 <= x2 && x2 <= x1 + num1 && y2 <= y1 && y1 <= y2 + num2 then Just (x2, y1) else Nothing
    (Vertical, Horizontal) -> if x2 <= x1 && x1 <= x2 + num2 && y1 <= y2 && y2 <= y1 + num1 then Just (x1, y2) else Nothing
    (Horizontal, Horizontal) -> if y1 == y2 && x1 <= x2 + num2 && x2 <= x1 + num1 then Just (max x1 x2, y1) else Nothing
    (Vertical, Vertical) -> if x1 == x2 && y1 <= y2 + num2 && y2 <= y1 + num1 then Just (x1, max y1 y2) else Nothing 

searchForClosestCrossing :: Wire -> Wire -> Maybe Point
searchForClosestCrossing wire1 wire2 =
  foldl (\closest2 x -> 
    let 
      p = foldl (\closest y -> case wirePartsCrossing x y of
        Nothing -> closest
        Just (0, 0) -> closest
        Just crossing -> case closest of 
          Nothing -> Just crossing
          Just closestPoint -> if manhattan crossing < manhattan closestPoint then Just crossing else closest
        ) Nothing wire2
    in
      case p of 
        Nothing -> closest2
        Just pPoint -> case closest2 of
          Nothing -> Just pPoint
          Just c2 -> if manhattan pPoint < manhattan c2 then p else closest2
  ) Nothing wire1

searchForFewestStepsCrossing :: Wire -> Wire -> (Maybe Int, Maybe Point)
searchForFewestStepsCrossing wire1 wire2 =
  foldl (\closest2 x -> 
    let 
      p = foldl (\closest y -> case wirePartsCrossing x y of
        Nothing -> closest
        Just (0, 0) -> closest
        Just crossing -> 
          let 
            stepsToCrossing = totalStepsToPointAlongWires crossing wire1 wire2
          in 
            case closest of 
              (_, Nothing) -> (stepsToCrossing, Just crossing)
              (n, Just _) -> if stepsToCrossing < n then (stepsToCrossing, Just crossing) else closest
        ) (Nothing, Nothing) wire2
    in
      case p of 
        (_, Nothing) -> closest2
        (n, Just _) -> 
          case closest2 of
            (_, Nothing) -> p
            (m, Just _) -> if n < m then p else closest2
  ) (Nothing, Nothing) wire1