import qualified Data.Map.Strict as M
import qualified Data.List(nub)

data Prop =  Const Bool
           | Var String
           | Not Prop
           | Conj Prop Prop
           | Disj Prop Prop
           | Impl Prop Prop
           | Iff Prop Prop
  deriving (Show,Eq)

type Variables = String

type Assignment = M.Map Variables Bool

allAssignments :: [Variables] -> [Assignment]
allAssignments vrs = case vrs of
  []      -> []
  [v]     -> [M.fromList [(v,False)], M.fromList [(v,True)]]
  (v:vs)  -> [M.insert v b assig | b <- [False,True], assig <-allAssignments vs]
  

vars :: [String]
vars = map (("X"++).show) [1..100]

propVariables' :: Prop -> [Variables]
propVariables' prop = case prop of
  Const bool -> []
  Var var -> [var]
  Not prop -> propVariables prop
  Conj prop1 prop2 -> (propVariables prop1 ++ propVariables prop2)
  Disj prop1 prop2 -> (propVariables prop1 ++ propVariables prop2)
  Impl prop1 prop2 -> (propVariables prop1 ++ propVariables prop2)
  Iff prop1 prop2  -> (propVariables prop1 ++ propVariables prop2)

propVariables :: Prop -> [Variables]
propVariables = (Data.List.nub).propVariables'

fromJust :: Maybe a -> a
fromJust a = case a of
  Just a  -> a
  Nothing -> error "unassigned boolean variable" 

evaluate :: Prop -> Assignment -> Bool
evaluate prop ass = case prop of
  Const bool        -> bool
  Var var           -> fromJust $ M.lookup var ass
  Not prop          -> not (evaluate prop ass)
  Conj prop1 prop2  -> (evaluate prop1 ass) && (evaluate prop2 ass)
  Disj prop1 prop2  -> (evaluate prop1 ass) || (evaluate prop2 ass)
  Impl prop1 prop2  -> (evaluate prop2 ass) || (not (evaluate prop1 ass)) 
  Iff  prop1 prop2  -> (evaluate prop1 ass) == (evaluate prop2 ass)

testProp1 :: Prop
testProp1 = Iff (Conj (Var "A") (Var "B")) (Conj (Var "B") (Var "A"))

tautology :: Prop -> Bool
tautology prop = and $ map (evaluate prop) (allAssignments propVars)
  where propVars = propVariables prop

trueAssignments :: Prop -> [Assignment]
trueAssignments prop = filter (\ass -> (evaluate prop ass)) $ allAssignments (propVariables prop)

convert :: (Variables, Bool) -> Prop
convert (var, bool) = case bool of
  True -> Var var
  False -> Not (Var var)

convertAssignment' :: Assignment -> Maybe Prop
convertAssignment' assig =
  foldr (\a acc -> if (acc==Nothing) then Just (convert a) else Just (Conj (convert a) (fromJust acc))) Nothing listAssign 
  where listAssign = M.toList assig

convertAssignment :: Assignment -> Prop
convertAssignment = fromJust.convertAssignment'

disjunctiveNormalForm :: Prop -> Maybe Prop
disjunctiveNormalForm prop = (foldr(\a acc -> if (acc==Nothing) then conv a else Just (Disj (fromJust $ conv a) (fromJust acc))) Nothing trueAssigns)
  where trueAssigns = trueAssignments prop
        conv = convertAssignment'


        