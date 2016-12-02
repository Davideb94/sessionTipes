
type Name = String

type Var = Name 

data Val = Int | Name deriving (Show, Eq)

data Polarity = Plus | Minus deriving (Show)

--Flips the Polarity value
opposite :: Polarity -> Polarity
opposite Plus = Minus
opposite Minus = Plus

--Polarity equality
(-=) :: Polarity -> Polarity -> Bool
(-=) Plus Plus = True
(-=) Minus Minus = True
(-=) _ _ = False 

type Channel = (Polarity, Name) 

-- Channel equality
eq_channel :: Eq a => (Polarity, a) -> (Polarity, a) -> Bool
eq_channel (p, name) (p1, name1) = if (((opposite p) -= p1 ) && (name1==name)) then True
                                   else False

data Label = String deriving (Show, Eq)

data Process = Zero | Input Channel Var Process | Output Channel Val Process |	
               Select Channel Label Process | Branch Channel [(Label, Process)]
               deriving (Show) 

--Tests wether a Process is Zero or not
ispZero :: Process -> Bool
ispZero Zero = True
ispZero _ = False

--Destructors for Output
getChannel :: Process -> Channel
getChannel (Output ch val pr ) = ch
getVal :: Process -> Val
getVal (Output ch val pr ) = val
getProcess :: Process -> Process
getProcess (Output ch val pr ) = pr

--Substitutes every occurrence in the Process of Var var with Val val 
--ERRORE DI TIPO, var : Var ma, come mostra l'assegnazione di tipo della funzione var : Val
substitute :: Process -> Val -> Val -> Process
substitute Zero val var = Zero
substitute (Input ch1 var1 pr1 ) val var = substitute pr1 val var
substitute (Select ch1 la1 pr1 ) val var = substitute pr1 val var
substitute (Branch ch1 as ) val var = ( Branch ch1 (zip [ ys  | ys<-(map fst as) ] [(substitute xs val var) | xs<-(map snd as)]) )
substitute (Output ch1 val1 pr1 ) val var = if val1 == var then (Output ch1 val (substitute pr1 val var))
                                            else substitute pr1 val var

--Determines wheter two process are dual ( i.e. are able to communicate each other ) or not
dual :: Process -> Process -> Bool
dual (Input ch1 var pr1) (Output ch val pr) = dual (Output ch val pr ) (Input ch1 var pr1 )
dual (Branch ch1 (a:as)) (Select ch la pr ) = dual (Select ch la pr ) (Branch ch1 (a:as))
dual (Output ch val pr ) (Input ch1 var pr1 ) = if (eq_channel ch ch1) then True else False
dual (Select ch la pr ) (Branch ch1 []) = False
dual (Select ch la pr ) (Branch ch1 (a:as)) =  if ((fst a) == la) && (eq_channel ch ch1) then True 
	                                           else dual (Select ch la pr ) (Branch ch1 as)
dual _ _  = False

--Determines wheter a [Process] is composed only by Zero
isZeroList :: [Process] -> Bool
isZeroList as = and (map ispZero as)

{-----------------------------------------------------------------------------------------------------------------------------

{-
pList
	1- pList richiama reducePlist nella where con "a" e "as" 
	2- Se il risultato è [] allora
		3- se la lista è composta solo da processi inattivi restituisci []
		4- altrimenti result è la lista i cui processi ridotti sono stati aggiornati
		   perciò posso ricominciare da capo
	5- altrimenti rimetto "a" infonfo alla lista e riparto dalla nuova testa
problema: se la lista non è composta solo da zeri, ma dentro ha un processo non riducibile,
possibile soluzione: applicare dual alla lista e vedere se esiste almeno una coppia riducibile 
-}

pList [] = []
pList (a:as) = if ( result /= [] ) then ( if (isZeroList as) then [] else pList result )
               else pList as ++ [a] 
               where result = reducepList a as

{-
reducepList
	1- se a e b sono duali, restituisce il risultato della exec di a e b concatenato con la coda della lista
	2- altrimenti concatena b con il risultato di reduceplist applicata alla coda della lista
Nel momento in cui si applica una riduzione (exec) restituisco la lista i cui processi ridotti 
sono stati aggiornati.
Se restituisco la lista vuota, a non ha al momento un processo con cui comunicare.
-}

reducepList a [] = []
reducepList a (b:bs) = if result then bs ++ (exec a b)
                       else [b] ++ (reducepList a bs)  
                       where result = (dual a b) == True

--Executes two Processes reducing them according to their Computation Rules
--Persiste il problema di tipo causato dalla substitute

exec (Output ch val pr ) (Input ch1 var pr1 ) = [(substitute pr1 val var), pr]
exec (Select ch la pr ) (Branch ch1 (a:as)) = if ((fst a) == la) then [pr, (snd a)]
	                                              	else exec (Select ch la pr ) (Branch ch1 as)

-----------------------------------------------------------------------------------------------------------------------------}