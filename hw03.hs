module HW03 where

data Expression =
        Var String                      -- Variable
      | Val Int                         -- Integer literal
      | Op Expression Bop Expression    -- Operation
    deriving (Show, Eq)

-- Binary (2-input) operation
data Bop =
        Plus
      | Minus
      | Times
      | Divide
      | Gt
      | Ge
      | Lt
      | Le
      | Eql
    deriving (Show, Eq)

data Statement =
        Assign      String      Expression
      | Incr        String
      | If          Expression  Statement   Statement
      | While       Expression  Statement
      | For         Statement   Expression  Statement   Statement
      | Sequence    Statement   Statement
      | Skip
    deriving (Show, Eq)


data DietStatement =
      DAssign String Expression
    | DIf Expression DietStatement DietStatement
    | DWhile Expression DietStatement
    | DSequence DietStatement DietStatement
    | DSkip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1   ------------------------------

extend :: State -> String -> Int -> State
extend s x v y
    | x == y = v
    | otherwise = s y

empty :: State
empty _ = 0

-- Exercise 2   ------------------------------

evalE :: State -> Expression -> Int
evalE _ (Val value) = value
evalE st (Var name) = st name
evalE st (Op x bop y) =
    case bop of
        Plus    -> expr1 + expr2
        Minus   -> expr1 - expr2
        Times   -> expr1 * expr2
        Divide  -> expr1 `div` expr2
        Gt      -> toInt $ expr1 > expr2
        Ge      -> toInt $ expr1 >= expr2
        Lt      -> toInt $ expr1 < expr2
        Le      -> toInt $ expr1 <= expr2
        Eql     -> toInt $ expr1 == expr2
    where expr1 = evalE st x
          expr2 = evalE st y
          toInt b = if b then 1 else 0

-- Exercise 3 -----------------------------------------

desugar :: Statement -> DietStatement
desugar st = case st of
    (Assign str expr)       -> DAssign str expr
    (Incr str)              -> DAssign str (Op (Var str) Plus (Val 1))
    (If expr st1 st2)       -> DIf expr (desugar st1) (desugar st2)
    (While expr1 stt)       -> DWhile expr1 (desugar stt)
    (For st1 expr st2 x)    -> desugar (Sequence st1 (While expr (Sequence x st2)))
    (Sequence st1 st2)      -> DSequence (desugar st1) (desugar st2)
    (Skip)                  -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st diet = case diet of
    (DAssign str expr)      -> extend st str (evalE st expr)
    (DIf expr diet1 diet2)  -> let dt
                                    | evalE st expr > 0 = diet1
                                    | otherwise         = diet2
                               in evalSimple st dt
    (DWhile expr diet1)
        | evalE st expr > 0 -> evalSimple (evalSimple st diet1) diet
        | otherwise         -> st
    (DSequence diet1 diet2) -> evalSimple (evalSimple st diet1) diet2
    (DSkip)                 -> st

run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
