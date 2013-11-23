module ShortestPath
where

data Section = Section { getA :: Integer, getB :: Integer, getC ::Integer } deriving (Show)
type RoadSystem = [Section]

data Road = A | B | C deriving (Show)
data Path = Path [Road] Cost deriving (Show)

type Cost = Integer

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

shortestPath :: RoadSystem -> Path
shortestPath = reversePath . cheapestPath . foldl calculateSection (Path [] 0, Path [] 0)

reversePath :: Path -> Path
reversePath (Path roads cost) = Path (reverse roads) cost

cheapestPath :: (Path, Path) -> Path
cheapestPath tuple@(Path _ cost1, Path _ cost2)
    | cost1 <= cost2 = fst tuple
    | otherwise = snd tuple

calculateSection :: (Path, Path) -> Section -> (Path, Path)
calculateSection (Path roadsA costA, Path roadsB costB) (Section a b 0) = (Path (A:roadsA) (costA + a), Path (B:roadsB) (costB + b))
calculateSection (Path roadsA costA, Path roadsB costB) (Section a b c) =
    let forwardCostToA = costA + a
        crossCostToA = costB + b + c
        forwardCostToB = costB + b
        crossCostToB = costA + a + c
        newPathToA = if forwardCostToA <= crossCostToA
                        then Path (A:roadsA) forwardCostToA
                        else Path (C:B:roadsB) crossCostToA
        newPathToB = if forwardCostToB <= crossCostToB
                        then Path (B:roadsB) forwardCostToB
                        else Path (C:A:roadsA) crossCostToB
    in (newPathToA, newPathToB)

