

-------------------------
-------- PART A ---------    75% COMPLETE
-------------------------


type Var = String


data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
--  deriving Show


instance Show Term where
  show = pretty


example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))


pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m


------------------------- Assignment 1        5%    DONE, COMPILED AND PASSED      

-- recursively calculates Church numerals
numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeralprime i))
    where 
      numeralprime 0 = Variable "x"
      numeralprime i = Apply (Variable "f") (numeralprime (i-1)) 


-------------------------

-- merge sort
merge :: Ord a => [a] -> [a] -> [a] 
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


------------------------- Assignment 2        10%   DONE, COMPILED AND PASSED 
-- DONE AND WORKS


-- create an infinite list of variables by combining letters and numbers
letters = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]
numbers = map show [1..]


variables :: [Var]
variables = letters ++ [ x ++ y |  y <- numbers , x <- letters]


--DONE AND WORKS
-- takes two lists of variables and returns the first list with all variables from the second removed from it
filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs []     = xs
filterVariables xs (y:ys) = filterVariables ( remove xs y ) ys
  where    
    remove []  _ = []
    remove (x:xs) y
        | x == y    = xs
        | otherwise = x : remove xs y


--DONE AND WORKS
-- given a list of variables, this returns a fresh variable that isn't in the list
fresh :: [Var] -> Var
fresh xs = head (filterVariables variables xs)


--DONE AND WORKS
-- collects all variable names used in a Term and returns them in an ordered list
used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda y z) =  merge [y] (used z)
used (Apply u v)  = merge (used u) (used v)


------------------------- Assignment 3      20% DONE, COMPILED AND PASSED 
-- CAPTURE-AVOIDING SUBSITUTION

--DONE AND WORKS
-- renames all instances of x to y in Term and returns the new Term
rename :: Var -> Var -> Term -> Term
rename x y (Variable z) 
    | z==x      = Variable y
    | otherwise = Variable z
rename x y (Lambda z n)
    | z==x      = Lambda z n
    | otherwise = Lambda z (rename x y n)
rename x y (Apply n m) = Apply(rename x y n) (rename x y m)


-- DONE AND WORKS
-- implements capture-avoiding substitution
substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
    | y==x      = n
    | otherwise = Variable y
substitute x n (Lambda y m)
    | y==x      = Lambda y m 
    | otherwise = Lambda z (substitute x n (rename y z m)) where z = fresh (merge [x] (merge (used m) (used n)))
substitute x n (Apply m1 m2) = Apply ( substitute x n m1) (substitute x n m2)


------------------------- Assignment 4      25%
-- BETA-REDUCTION

--DONE AND WORKS
--SHOULD BE DOING NORMAL ORDER REDUCTION (L.O.)


beta :: Term -> [Term]
beta (Apply (Lambda x n) m) = [substitute x m n] ++ [ Apply (Lambda x n) i |i<-beta m] ++ [ Apply (Lambda x i) m | i<-beta n]
beta (Lambda x n)           = [Lambda x i | i<-beta n] 
beta (Apply n m)            = [Apply n i | i<-beta m] ++ [Apply i m | i<-beta n]
beta (Variable x)           = []


--DONE AND WORKS
normalize :: Term -> IO () 
normalize x = do
    print x
    let b = beta x 
        l = length b 
    if l == 0
    then do 
        return () -- returns nothing
    else do
        normalize (head b)  --meant to return the last element in the list of beta reductions of x


------------------------ 

--DONE AND WORKS
--SHOULD BE DOING APPLICATIVE ORDER REDUCTION (L.I.)

{-
a_beta :: Term -> [Term]
a_beta (Apply n m)                = [Apply n i | i<-(a_beta m)] ++ [Apply i m | i<-(a_beta n)]
a_beta (Lambda x n)               = [Lambda x i | i<-(a_beta n)]
a_beta (Apply (Lambda x n) m)     = [substitute x m n] ++ [ Apply (Lambda x i) m | i<-(a_beta n)] ++ [ Apply (Lambda x n) i |i<-(a_beta m)]
a_beta (Variable x)               = []
-}


--use this one for a_beta
aBeta :: Term -> [Term] 
aBeta (Apply (Lambda x n) m) = [ Apply (Lambda x n) i |i<-aBeta m] ++ [ Apply (Lambda x i) m | i<-aBeta n] ++ [substitute x m n]
aBeta (Lambda x n)           = [Lambda x i | i<-aBeta n] 
aBeta (Apply n m)            = [Apply n i | i<-aBeta m] ++ [Apply i m | i<-aBeta n]
aBeta (Variable x)           = []


--DONE, COMPILES AND PASSED
--think this one actually does LI reduction
aNormalize :: Term -> IO () 
aNormalize x = do
    print x
    let b = beta x 
        l = length b
        y = beta x !! (l-1)
    if l == 0
    then do 
        return () -- returns nothing
    else do
        aNormalize y  --meant to return the last element in the list of beta reductions of x


-------------------------


example1 :: Term --has more steps with normal order
example1 = Apply (Lambda "x" (Apply (Variable "x") (Variable "x"))) (Apply(Lambda "y" (Variable "y"))(Lambda "z" (Variable "z")))


example2 :: Term --has more steps with applicitave order
example2 = Apply (Lambda "x" (Variable "y")) (Apply(Lambda "y" (Variable "y"))(Lambda "z" (Variable "z")))




-------------------------
-------- PART B --------- 
-------------------------


------------------------- Assignment 5 (PAM)      15%      DONE,COMPILED AND PASSED 



--part a    DONE, COMPILES AND PASSED


type PState = (Term ,[Term])

state1 = (Lambda "x" (Lambda "y" (Variable "x")) , [Variable "Yes", Variable "No"])

term1 = Apply (Apply (Lambda "x" (Lambda "y" (Variable "x"))) (Variable "Yes")) (Variable "No")

term2 = Apply (Apply (Lambda "b" (Apply example (Variable "Yes"))) (Lambda "z" (Apply(Variable "z")(Variable "z")))) (Variable "No")


--part b  DONE, COMPILES AND PASSED


pStart :: Term -> PState
pStart n = (n,[])


pStep :: PState -> PState
pStep (Lambda x n, m:ms ) = (substitute x m n , ms )
pStep (Apply n m , ms ) = ( n , m:ms ) --adds m to head of list ie top of stack


pFinal :: PState -> Bool
pFinal (Lambda x n, [])   = True
pFinal (Variable x ,s)    = True
pFinal _         = False



--part c DONE, COMPILES AND PASSED


pRun :: Term -> IO ()    
pRun n = f (pStart n)
  where f m = do
              print m
              if pFinal m 
              then do 
                print (pReadback m)
                return ()
              else do 
                f (pStep m)



--part d DONE, COMPILES AND PASSED


pReadback :: PState -> Term
pReadback (Lambda x n, s)  = Lambda x n 
pReadback (Variable x , s) 
    | null s      = Variable x    
    | otherwise             = Apply (Variable x) (head s)
pReadback (Apply n m, s)   = n



------------------------- Assignment 6 (KAM)    25%  works


-- part a 
data Env = EnvPair Var Closure Env
        | EnvStar
instance Show Env where
         show = prettyenv

--show Env as a list of Pairs
prettyenv :: Env -> String
prettyenv EnvStar = "[]"
prettyenv (EnvPair var closure EnvStar) = "[" ++ "(" ++ show var ++ "," ++ show closure ++ ")" ++ "]"
prettyenv (EnvPair var closure env) = "[" ++ "(" ++ show var ++ "," ++ show closure ++ ")," ++ init ( tail (show env)) ++ "]"

data State = StatePair Closure Stack
            | StateStar
instance Show State where
        show = prettystate

prettystate :: State -> String
prettystate StateStar = "[]"
prettystate (StatePair closure stack) = "(" ++ show closure ++ "," ++ show stack ++ ")"

data Closure = ClosurePair Term Env
            | ClosureStar
instance Show Closure where
            show = prettyclosure

prettyclosure :: Closure -> String
prettyclosure ClosureStar = "[]"
prettyclosure (ClosurePair term env) = show term ++ "," ++ show env

data Stack = StackPair Closure Stack
            | StackStar
instance Show Stack where
        show = prettystack

prettystack :: Stack -> String
prettystack StackStar = "[]"
prettystack (StackPair closure StackStar) = "[(" ++ show closure ++ ")]"
prettystack (StackPair closure stack) = "[(" ++ show closure ++ ")," ++ init (tail (show stack)) ++ "]"

-- state2: ( (λx.x) y, (y,λz.z,⋆)·⋆, ⋆ )
term2a = Apply (Lambda "x" (Variable "x")) (Variable "y")
env2 = EnvPair "y" closure2 EnvStar
closure2 = ClosurePair (Lambda "z" (Variable "z")) EnvStar
state2 = StatePair (ClosurePair term2a env2) StackStar

-- currently have: 
-- ((\x. x) y,[("y",\z. z,[])],[])
-- we want:
-- ((\x. x) y,[("y",\z. z,[])],[])


-- state3: ( xx, (x,λx.xx,⋆)·⋆, ⋆ )
term3 = Apply (Variable "x") (Variable "x")
env3 = EnvPair "x" closure3 EnvStar
closure3 = ClosurePair (Lambda "x" (Apply (Variable "x") (Variable "x"))) EnvStar
state3 = StatePair (ClosurePair term3 env3) StackStar

-- currently have:
-- (x x,[("x",\x. x x,[])],[])
-- we want:
-- (x x,[("x",\x. x x,[])],[])


-- state4: ( λy.x, ⋆, (z, (z,λa.b,(b,c,⋆)·⋆)·⋆)·⋆)
term4a = Lambda "y" (Variable "x")
stack4a = StackPair (ClosurePair term4b env4b) StackStar
term4b = Variable "z"
env4b = EnvPair "z" closure4b EnvStar
closure4b = ClosurePair (Lambda "a" (Variable "b")) env4c
env4c = EnvPair "b" (ClosurePair (Variable "c") EnvStar) EnvStar
state4 = StatePair (ClosurePair term4a EnvStar) stack4a

-- currently have:
-- (\y. x,[],[(z,[("z",\a. b,[("b",c,[])])])])
-- we want:
-- (\y. x,[],[(z,[("z",\a. b,[("b",c,[])])])])

-- part b

start :: Term -> State
start n = StatePair (ClosurePair n EnvStar) StackStar


step :: State -> State
step (StatePair (ClosurePair (Variable var1) (EnvPair (var2) (ClosurePair term env1) env2)) stack)
    | var1 == var2  = StatePair (ClosurePair term env1) stack
    | otherwise     = StatePair (ClosurePair (Variable var1) env2) stack
step (StatePair (ClosurePair (Lambda var term1) env1) (StackPair (ClosurePair term2 env2) stack))   = StatePair (ClosurePair term1 (EnvPair var (ClosurePair term2 env2) env1)) stack
step (StatePair (ClosurePair (Apply term1 term2) env) stack)    = StatePair (ClosurePair term1 env) (StackPair (ClosurePair term2 env) stack)


final :: State -> Bool
final (StatePair (ClosurePair (Lambda var term) env) StackStar) = True
final (StatePair (ClosurePair (Variable x) EnvStar) stack) = True
final _     = False


--part c


run :: Term -> IO ()
run n = f (start n)
  where f m = do
              print m
              if final m 
              then do 
                print (readback m)
                return ()
              else do 
                f (step m)
-- part d 


readback :: State -> Term 
readback (StatePair closure (StackPair (ClosurePair n e) stack)) = Apply (readback (StatePair closure stack)) (readback (StatePair (ClosurePair n e) StackStar)) -- this line is new, trying to add in stack to readback, needs to be checked first so that StackStar is introduced if there is more on the stack
readback (StatePair (ClosurePair (Variable x) EnvStar) stack) = Variable x
readback (StatePair (ClosurePair (Variable x) (EnvPair y (ClosurePair n e) f)) stack)
    | x==y      = readback (StatePair (ClosurePair n e) stack)
    | otherwise = readback (StatePair (ClosurePair (Variable x) f) stack)
readback (StatePair (ClosurePair (Lambda x n) EnvStar) stack)   = Lambda x n
readback (StatePair (ClosurePair (Lambda x n) e) stack)     = Lambda x (readback (StatePair (ClosurePair n (EnvPair x (ClosurePair (Variable x) EnvStar) e)) stack))
readback (StatePair (ClosurePair (Apply n m) EnvStar) stack)    = Apply n m
readback (StatePair (ClosurePair (Apply n m) e) stack)  = Apply (readback (StatePair (ClosurePair n e) stack)) (readback (StatePair (ClosurePair m e) stack))






{-

--part a DONE AND COMPILES

data Env = Triple Var Term Env Env
         | Star
instance Show Env where
         show = prettyenv


--show Env as a list of triples
prettyenv :: Env -> String
prettyenv Star = "[]"
prettyenv (Triple var term env1 Star) = "[" ++ "(" ++ show var ++ "," ++ show term ++ "," ++ show env1 ++ ")" ++ "]"
prettyenv (Triple var term env1 env2) = "[" ++ "(" ++ show var ++ "," ++ show term ++ "," ++ show env1 ++ ")" ++ "]" ++ show env2 


data Closure = Term Env
--data State = State Term Env [Closure]
data State =  Quad Term Env State
           | Star2
instance Show State where
         show = prettystate


--show State as a triple

prettystate :: State -> String
prettystate Star2 = "[]"
prettystate (Quad term env state) ="[" ++ "(" ++ show term ++ "," ++ show env ++ "," ++ show state ++ ")" ++ "]"

--change this for different cases 


state2 = Quad (Apply (Lambda "x" (Variable "x")) (Variable "y")) (Triple "y" (Lambda "z" (Variable "z")) Star Star) Star2
 
state3 = Quad (Apply (Variable "x") (Variable "x")) (Triple "x" (Apply (Lambda "x" (Variable "x")) (Variable "x")) Star Star) Star2


state4 =Quad (Lambda "y" (Variable "x")) Star (Quad (Variable "z") (Triple "z" (Lambda "a" (Variable "b")) (Triple "b" (Variable "c") Star Star) Star ) Star2)


--part b SQUARE BRACKETS WRONG IN TEST -PROBABLY A PROBLEM WITH PRETTY

--pretty much passes test but the square brackets are a bit odd


start :: Term -> State
start n = Quad n Star Star2


step :: State -> State
step ( Quad (Variable var1) ( Triple var2 term env1 env2 ) state )
    | var1==var2   = Quad term env1 state
    | otherwise    = Quad (Variable var1) env2 state 
step ( Quad (Lambda var term1) env1 ( Quad term2 env2 state ) )    =  Quad term1 ( Triple var term2 env2 env1 ) state 
step ( Quad (Apply term1 term2) env state )                       =  Quad term1 env ( Quad term2 env state ) 


final :: State -> Bool
final ( Quad (Lambda var term) env Star2)  = True
final ( Quad (Variable x) Star state)       = True
final _                                 = False


--part c


run :: Term -> IO ()
run n = f (start n)
  where f m = do
              print m
              if final m 
              then do 
                print (readback m)
                return ()
              else do 
                f (step m)


--part d


--does state2 fine but not 3 and 4


readback :: State -> Term 
readback (Quad (Variable x) Star state) = Variable x
readback (Quad (Variable x)(Triple y n e f) state)
    | x==y      = readback (Quad n e state)
    | otherwise = readback (Quad (Variable x) f state)
readback (Quad (Lambda x n) e state)    = Lambda x (readback (Quad n (Triple x (Variable x) Star e ) state)) --THIS LINE WRONG
--readback (Quad (Lambda x n) e state)    = Lambda x (readback (Quad n e state)) --THIS LINE WRONG
readback (Quad (Lambda x n) Star state) = Lambda x n
readback (Quad (Apply n m) e state) = Apply (readback (Quad n e state)) (readback (Quad m e state)) --THIS LINE WRONG
readback (Quad (Apply n m) Star state)  = Apply n m


-}
