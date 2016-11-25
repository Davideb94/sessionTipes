
type Val = Int | Name 

type Var = Name

type Name = String

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

substitute Zero val var = Zero
substitute (Input ch1 var1 pr1 ) val var = substitute pr1 val var
substitute (Output ch1 val1 pr1 ) val var = if val1 == var then do {val1 = val; substitute pr1 val var }  --side effect?? ??
                                            else substitute pr1 val var
substitute (Select ch1 la1 pr1 ) val var = substitute pr1 val var
substitute (Branch ch1 as ) val var = map substitute (map snd as )

dual (Output ch val pr ) (Input ch1 var pr1 ) = if (eq_channel ch ch1) then True else False
dual (Select ch la pr ) (Branch ch1 []) = False
dual (Select ch la pr ) (Branch ch1 (a:as)) =  if ((fst a) == la) && (eq_channel ch ch1) then True 
	                                           else dual (Select ch la pr ) (Branch ch1 as)
dual _ _  = False

reducePList [] = []
reducePList as = map exec [ (x,y) | x<-as ,y<-as, dual x y == True ] 

exec ( (Output ch val pr ) , (Input ch1 var pr1 ) ) = (substitute pr1 val var) pr
exec ( (Select ch la pr ) , (Branch ch1 (a:as)) ) = if ((fst a) == la) then pr (snd a)
	                                              	else dual (Select ch la pr ) (Branch ch1 as)
