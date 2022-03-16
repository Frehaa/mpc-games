import System.Environment(getArgs)
import Text.Read (readMaybe)
import Text.Html (input)

type Player = Int
type Id = Int

-- Binary Game Tree
data BGTree = Leaf Id | Branch Player Id BGTree BGTree deriving (Show, Read)

-- Count terminal and total nodes in tree
count :: BGTree -> (Int, Int)
count (Leaf _) = (1, 1)
count (Branch _ _ l r) = 
  let (leaves_l, total_l) = count l in
  let (leaves_r, total_r) = count r in
    (leaves_l + leaves_r, total_l + total_r + 1)

getId :: BGTree -> Id
getId (Branch _ id _ _) = id
getId (Leaf id) = id

generateSetup ::  BGTree -> Int -> [String]
generateSetup tree nPlayers = 
  -- ["program.use_edabit(True)", "",
   ["from Compiler.util import if_else", "",
   "n_inputs = " ++ show inputCount,
   "n_players = " ++ show nPlayers,
   "tree_size = " ++ show treeSize,
   "bio = " ++ show (treeSize - 1), ""] 
    where (inputCount, treeSize) = count tree

generateUtilityMatrixCode :: [String]
generateUtilityMatrixCode = 
   ["u = Matrix(tree_size, n_players, sint)",
   "for n in range(n_inputs):",
   "\tfor i in range(n_players):",
   "\t\tu[n][i] = sint.get_input_from(i)", ""]

generateSplitsCode :: [Int] -> [String]
generateSplitsCode ss = 
  ["total_split = " ++ show (sum ss),
  "splits = cint.Array(n_players)"] ++ 
  zipWith (\s i -> "splits[" ++ show i ++ "] = " ++ show s) ss [0..] ++ [""]

generateBackwardsInductionCode :: BGTree -> [String]
generateBackwardsInductionCode tree = 
  generateBackwardsInductionFunctionCode ++
  generateBackwardsInductionNodeCode tree ++ [""]

generateBackwardsInductionFunctionCode :: [String]
generateBackwardsInductionFunctionCode = 
  ["def backwards_induction(party, left, right, parent):",
   "\tcmp = u[left][party] > u[right][party]",
   "\tfor i in range(n_players):",
   "\t\tu[parent][i] = if_else(cmp, u[left][i], u[right][i])", ""]

generateBackwardsInductionNodeCode :: BGTree -> [String]
generateBackwardsInductionNodeCode (Leaf id) = []
generateBackwardsInductionNodeCode (Branch p id l r) = 
  generateBackwardsInductionNodeCode l ++ 
  generateBackwardsInductionNodeCode r ++ 
  ["backwards_induction(party=" ++ show p ++ ", left=" ++ idl ++ ", right=" ++ idr ++ ", parent=" ++ show id ++ ")"]
  where idl = show $ getId l
        idr = show $ getId r

generateTumoCode :: [String]
generateTumoCode = 
  ["tumo = sint(0)",
   "tumo_sum = sum(u[0])", 
   "for n in range(1, n_inputs):",
   "\ttumo_candidate_value = sum(u[n])",
   "\tis_new_tumo = tumo_candidate_value > tumo_sum",
   "\ttumo = if_else(is_new_tumo, n, tumo)",
   "\ttumo_sum = if_else(is_new_tumo, tumo_candidate_value, tumo_sum)", ""]

generateSplitValuesCode :: [String]
generateSplitValuesCode = 
   ["T = tumo_sum - sum(u[bio])", 
   "S = sint.Array(n_players)",
   "S[n_players-1] = T",
   "for i in range(n_players-1):",
   "\tS[i] = (T * splits[i]).private_division(total_split)",
   "\tS[n_players-1] -= S[i]", ""]

generateMinimumCode :: [String]
generateMinimumCode = 
  ["p_min = sint.Array(n_players)",
   "for i in range(n_players):",
   "\tp_min[i] = u[0][i]",
   "\tfor n in range(1, n_inputs):",
   "\t\tis_new_min = u[n][i] < p_min[i]",
   "\t\tp_min[i] = if_else(is_new_min, u[n][i], p_min[i])", ""]

generatePaymentsCode :: [String]
generatePaymentsCode = 
  ["D = sint.Array(n_players)",
   "F = sint.Array(n_players)",
   "tumo = tumo.reveal()", "",
   "for i in range(n_players):",
   "\tD[i] = u[bio][i] + S[i] - u[tumo][i]",
   "\tF[i] = sum([u[bio][j] + S[j] - p_min[j] for j in range(n_players) if j != i])", ""]

generatePrintOutputCode :: [String]
generatePrintOutputCode =
  ["print_ln('The selected outcome (TUMO) is %s', tumo)",
   "for i in range(n_players):",
   "\tpay = D[i].reveal()",
   "\tprint_ln_if(pay < 0, '%s should pay %s at TUMO', i, -pay)",
   "\tprint_ln_if(pay > 0, '%s should be paid %s at TUMO', i, pay)", "",
   "for i in range(n_players):",
   "\tprint_ln(\"%s's fine for deviating anywhere is %s\", i, F[i].reveal())"]


generateMpcCode :: BGTree -> [Int] -> String
generateMpcCode tree splits = 
  unlines $ generateSetup tree (length splits) ++ 
            generateUtilityMatrixCode ++
            generateSplitsCode splits ++ 
            generateBackwardsInductionCode tree ++
            generateTumoCode ++
            generateSplitValuesCode ++
            generateMinimumCode ++ 
            generatePaymentsCode ++
            generatePrintOutputCode

usageMessageIO = putStrLn "Usage: generateMPC {split} < input tree file\nExample:\n\t./generateMPC \"[3, 5]\" < exampleTree"

main :: IO ()
main  = do 
  args <- getArgs
  if null args
    then usageMessageIO
    else
      case readMaybe (head args) of 
        Just splits -> do
          t <- readLn :: IO BGTree
          putStrLn $ generateMpcCode t splits
        Nothing -> usageMessageIO
