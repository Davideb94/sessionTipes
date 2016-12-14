
type Name = String

data Va =  Vi Int | Ch (Polarity, Name) | Na Name deriving (Show, Eq, Read)

data Polarity = Plus | Minus deriving (Show, Eq, Read)

type Label = [Char]

data Process = Zero | Input Va Va Process | Output Va Va Process |	
               Select Va Label Process | Branch Va [(Label, Process)]
               deriving (Show, Read) 

--Flips the Polarity value
opposite :: Polarity -> Polarity
opposite Plus = Minus
opposite Minus = Plus

--Channel equality
eq_channel :: Va -> Va -> Bool
eq_channel (Ch (p, name)) (Ch (p1, name1)) = if (((opposite p) == p1 ) && (name1==name)) then True
                                   else False
eq_channel  _  _ = False

--Substitutes every occurrence in the Process of Va var with Va val 
substitute :: Process -> Va -> Va -> Process
substitute Zero val var = Zero
substitute (Input ch1 var1 pr1 ) val var = Input ch1 var1 (substitute pr1 val var)  
substitute (Select ch1 la1 pr1 ) val var = Select ch1 la1 (substitute pr1 val var)
substitute (Branch ch1 as ) val var = Branch ch1 (zip [ ys  | ys<-(map fst as) ] [(substitute xs val var) | xs<-(map snd as)])
substitute (Output ch1 val1 pr1 ) val var = if val1 == var then Output ch1 val (substitute pr1 val var)
                                             else Output ch1 val1 (substitute pr1 val var)                                          

--Determines wheter two process are dual ( i.e. are able to communicate each other ) or not
dual :: Process -> Process -> Bool
dual (Input ch1 var pr1) (Output ch val pr) = dual (Output ch val pr ) (Input ch1 var pr1 )
dual (Branch ch1 (a:as)) (Select ch la pr ) = dual (Select ch la pr ) (Branch ch1 (a:as))
dual (Output ch val pr ) (Input ch1 var pr1 ) = eq_channel ch ch1
dual (Select ch la pr ) (Branch ch1 []) = False
dual (Select ch la pr ) (Branch ch1 (a:as)) =  if ((fst a) == la) && (eq_channel ch ch1) then True 
	                                           else dual (Select ch la pr ) (Branch ch1 as)
dual _ _  = False

--Determines wheter in the given [Process] exists a dual for pr1 
dualList :: Process -> [Process] -> Bool
dualList pr1 [] = False
dualList pr1 (pr:prs) = if dual pr1 pr then True
                        else dualList pr1 prs 

--given a [Process] applies reduction to each couple of matching Process
pList :: [Process] -> [Process]
pList [] = []
pList [p] = [p]
pList (p:ps) = if dualList p ps then pList (reducepList p ps)
                  else p : (pList ps) 

--Finds the first dual Process of a and applies reduction on both of them
reducepList :: Process -> [Process] -> [Process]
reducepList a (b:bs) = if dual a b then (exec a b) ++ bs
                       else [b] ++ (reducepList a bs)

--Executes two Processes reducing them according to their Computation Rules
exec :: Process -> Process -> [Process]
exec (Input ch1 var pr1 ) (Output ch val pr ) = exec (Output ch val pr ) (Input ch1 var pr1 )
exec (Branch ch1 (a:as)) (Select ch la pr ) = exec (Select ch la pr ) (Branch ch1 (a:as)) 
exec (Output ch val pr ) (Input ch1 var pr1 ) = [(substitute pr1 val var), pr]
exec (Select ch la pr ) (Branch ch1 (a:as)) = if ((fst a) == la) then [pr, (snd a)]
	                                              	else exec (Select ch la pr ) (Branch ch1 as)

{--INPUT PER TESTARE--

--Case IO--

--pList [Output (Ch (Plus, "a")) (Vi 3) Zero, Input (Ch (Plus, "b")) (Na "y") Zero, Input (Ch (Minus, "a")) (Na "x") (Output (Ch (Minus, "b")) (Vi 3) Zero ) ]

--Case Select Branch--

--pList [Branch (Ch (Plus, "a")) [("in",Input (Ch (Plus, "b")) (Na "y") Zero)], Select (Ch (Minus, "a")) "in" Zero, Output (Ch (Minus, "b")) (Vi 3) Zero ] 

--Case non riducibili--

--pList [Branch (Ch (Plus, "a")) [("in",Input (Ch (Plus, "c")) (Na "y") Zero)], Select (Ch (Minus, "a")) "in" Zero, Output (Ch (Minus, "b")) (Vi 3) Zero ]

-}