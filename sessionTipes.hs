
type Name = String

type Var = Name 

data Val = Int | Name deriving (Show, Eq)

data Polarity = Plus | Minus deriving (Show)

opposite Plus = Minus
opposite Minus = Plus

type Channel = (Polarity, Name) 

(-=) Plus Plus = True
(-=) Minus Minus = True
(-=) _ _ = False 

eq_channel (p, name) (p1, name1) = if (((opposite p) -= p1 ) && (name1==name)) then True
                                   else False

data Label = String deriving (Show, Eq)

data Process = Zero | Input Channel Var Process | Output Channel Val Process |	--Input aspetta un input
               Select Channel Label Process | Branch Channel [(Label, Process)]
               deriving (Show) 

getChannel (Output ch val pr ) = ch
getVal (Output ch val pr ) = val
getProcess (Output ch val pr ) = pr

substitute Zero val var = Zero
substitute (Input ch1 var1 pr1 ) val var = substitute pr1 val var
substitute (Select ch1 la1 pr1 ) val var = substitute pr1 val var
substitute (Branch ch1 as ) val var = ( Branch ch1 (zip [ ys  | ys<-(map fst as) ] [(substitute xs val var) | xs<-(map snd as)]) )
substitute (Output ch1 val1 pr1 ) val var = if val1 == var then (Output ch1 val (substitute pr1 val var))
                                            else substitute pr1 val var

dual (Input ch1 var pr1) (Output ch val pr) = dual (Output ch val pr ) (Input ch1 var pr1 )
dual (Branch ch1 (a:as)) (Select ch la pr ) = dual (Select ch la pr ) (Branch ch1 (a:as))
dual (Output ch val pr ) (Input ch1 var pr1 ) = if (eq_channel ch ch1) then True else False
dual (Select ch la pr ) (Branch ch1 []) = False
dual (Select ch la pr ) (Branch ch1 (a:as)) =  if ((fst a) == la) && (eq_channel ch ch1) then True 
	                                           else dual (Select ch la pr ) (Branch ch1 as)
dual _ _  = False

pList [] = []
pList (a:as) = reducePList a as : pList as 

reducePList x [] = []
reducePList x (a:as) = if (dual x a) then exec x a else reducePList x as  

exec (Output ch val pr ) (Input ch1 var pr1 ) = [(substitute pr1 val var), pr]
exec (Select ch la pr ) (Branch ch1 (a:as)) = if ((fst a) == la) then [pr, (snd a)]
	                                              	else exec (Select ch la pr ) (Branch ch1 as)
