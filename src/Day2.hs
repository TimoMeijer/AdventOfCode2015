module Day2 where

import Control.Lens
import qualified Text.ParserCombinators.UU as PC (parse)
import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

type Measurement = (Int, Int, Int)
type Sides = ((Int, Int), (Int, Int), (Int, Int))
type Parser a = P (Str Char String LineColPos) a

solve2_1 :: (Monad m) => String -> m Int
solve2_1 = solve2 calcWrappingPaper

solve2_2 :: (Monad m) => String -> m Int
solve2_2 = solve2 calcRibbon

solve2 :: (Monad m) => (Measurement -> Int) -> String -> m Int
solve2 f i = do
    l <- parse i
    let x = map f l
    return $ sum x

calcWrappingPaper :: Measurement -> Int
calcWrappingPaper m = 2 * add c + Day2.min c
    where c = calcSides $ sides m

calcRibbon :: Measurement -> Int
calcRibbon m = uncurry perimeter (smallest2 m) + volume m

sides :: Measurement -> Sides
sides (x, y, z) = ((x, y), (x, z), (y, z))

calcSides :: Sides -> Measurement
calcSides = over each (uncurry (*))

min :: Measurement -> Int
min (x, y, z) = minimum [x, y, z]

add :: Measurement -> Int
add (x, y, z) = x + y + z

volume :: Measurement -> Int
volume (x, y, z) = x * y * z

smallest2 :: Measurement -> (Int, Int)
smallest2 (x, y, z)
    | x <= y && x <= z && y <= z || y <= x && y <= z && x <= z = (x, y)
    | x <= y && x <= z && z <= y || z <= x && z <= y && x <= y = (x, z)
    | y <= x && y <= z && z <= x || z <= x && z <= y && y <= x = (y, z)
    | otherwise = error "Should not be reached"

perimeter :: Int -> Int -> Int
perimeter x y = 2 * (x + y)

parse :: (Monad m) => String -> m [Measurement]
parse = run pMeasurements

run :: (Monad m) => Parser a -> String -> m a
run p s = let (r, e) = PC.parse ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) s) in
    if null e then return r else fail "Parser failure"

pMeasurements :: Parser [Measurement]
pMeasurements = pListSep (pSym ',') pMeasurement

pMeasurement :: Parser Measurement
pMeasurement = (,,) <$> (pInt <* pSym 'x') <*> (pInt <* pSym 'x') <*> pInt

pInt :: Parser Int
pInt = read <$> pSome (pRange ('0', '9'))
