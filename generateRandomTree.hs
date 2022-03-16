import qualified System.Random as R
import System.Environment(getArgs)
import Text.Read (readMaybe)

type Player = Int
type Id = Int

-- Binary Game Tree
data BGTree = Leaf Id | Branch Player Id BGTree BGTree deriving (Show, Read)

-- Generates a complete binary tree of height h with randomly assigned players at each non-terminal node
generateCompleteTree :: R.StdGen -> Int -> Int -> BGTree
generateCompleteTree _ 0 _ = Leaf 0
generateCompleteTree g h m =
  let (i, g') = R.next g
      (gl, gr) = R.split g'
      p = i `mod` m in
      Branch p 0 (generateCompleteTree gl (h-1) m) (generateCompleteTree gr (h-1) m)

-- Gives nodes in a Game tree unique identifiers
idTree :: BGTree -> BGTree
idTree t =
    let (t', i') = idLeaves t 0
        (t'', _) = idBranches t' i' in
    t''

idLeaves :: BGTree -> Id -> (BGTree, Id)
idLeaves (Leaf _) i = (Leaf i, i+1)
idLeaves (Branch p ci l r) i =
    let (l', i') = idLeaves l i
        (r', i'') = idLeaves r i' in
            (Branch p ci l' r', i'')

idBranches :: BGTree -> Id -> (BGTree, Id)
idBranches (Branch p _ l r) i =
    let (l', i') = idBranches l i
        (r', i'') = idBranches r i' in
            (Branch p i'' l' r', i''+1)
idBranches l i = (l, i)

usageMessageIO = putStrLn "Usage: generateRandomTree <height> <player count> [seed]\nExample:\n\t./generateRandomTree 4 2\n\t./generateRandomTree 20 5 5123"

main :: IO ()
main  = do
    args <- getArgs
    case args of
        height:playerCount:seed:_ ->
            case (readMaybe height, readMaybe playerCount, readMaybe seed) of
                (Just h, Just p, Just s) -> do
                    let g = R.mkStdGen s
                    print $ idTree (generateCompleteTree g h p)
                _ -> usageMessageIO
        height:playerCount:_ ->
            case (readMaybe height, readMaybe playerCount) of
                (Just h, Just p) -> do
                    g <- R.getStdGen
                    print $ idTree (generateCompleteTree g h p)
                _ -> usageMessageIO
        _ -> usageMessageIO
