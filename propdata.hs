module PropData where

data Prop =  Const Bool
           | Var String
           | Not Prop
           | Conj Prop Prop
           | Disj Prop Prop
           | Impl Prop Prop
           | Iff Prop Prop
  deriving (Eq)

showProp :: Prop -> String
showProp (Const bool) = show bool
showProp (Var var) = var
showProp (Not prop) = "~" ++ showProp prop
showProp (Conj prop1 prop2) = (showProp prop1) ++ " /\\ " ++ (showProp prop2)
showProp (Disj prop1 prop2) = (showProp prop1) ++ " \\/ " ++ (showProp prop2)
showProp (Impl prop1 prop2) = (showProp prop1) ++ " => " ++ (showProp prop2)
showProp (Iff prop1 prop2) = (showProp prop1) ++ " <=> " ++ (showProp prop2)

instance Show Prop where
  show = showProp
