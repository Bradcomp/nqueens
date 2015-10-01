import qualified Data.Set as Set 
data Point = Point Int Int deriving (Show, Eq, Ord)

safeHead :: [a] -> Maybe(a)
safeHead []   = Nothing
safeHead (x:xs) = Just x

offBoard :: Int -> Point -> Bool
offBoard n (Point x y) = x < 1 || y < 1 || x > n || y > n

-- From here to cancel are all for populating the discard set
cancelStep :: (Point -> Point) -> Int -> Point -> Set.Set(Point) -> Set.Set(Point)
cancelStep next n pt xs = 
    if offBoard n pt then xs else cancelStep next n (next pt) $ Set.insert pt xs

diagUp :: Point -> Point
diagUp (Point x y) = Point (x + 1) (y - 1)

diagDown :: Point -> Point
diagDown (Point x y) = Point (x + 1) (y + 1)

across :: Point -> Point
across (Point x y) = Point (x + 1) y

cancel :: Int -> Point -> Set.Set(Point) -> Set.Set(Point)
cancel n pt = (cancelStep across n pt) . (cancelStep diagUp n pt) . (cancelStep diagDown n pt)
   
nextOpen :: Int -> Int -> Set.Set(Point) -> Maybe(Point)
nextOpen n col xs = safeHead $ filter (\x -> not $ Set.member x xs) $ map (Point col) [1..n]

addQueen :: Int -> Int -> Set.Set(Point) -> Maybe([Point]) -> Maybe([Point])
addQueen n col discard queens  
    | col > n            = queens
    | nextPt == Nothing  = Nothing
    | attempt == Nothing = addQueen n col (Set.insert unwrapped discard) queens
    | otherwise          = attempt
    where attempt = addQueen n (col + 1) (cancel n unwrapped discard) $ fmap (unwrapped:) queens
          nextPt = nextOpen n col discard
          unwrapped = (maybe (Point 0 0) id nextPt)
        

solve :: Int -> Maybe([Point])
solve n = fmap reverse $ addQueen n 1 Set.empty $ Just []


