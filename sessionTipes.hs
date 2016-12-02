
type Name = String

data Va =  Vi Int | Ch (Polarity, Name) | Na Name deriving (Show, Eq)

data Polarity = Plus | Minus deriving (Show, Eq)

--Flips the Polarity value
--opposite :: Polarity -> Polarity
opposite Plus = Minus
opposite Minus = Plus

-- Channel equality
eq_channel :: Va -> Va -> Bool
eq_channel (Ch (p, name)) (Ch (p1, name1)) = if (((opposite p) == p1 ) && (name1==name)) then True
                                   else False
eq_channel  _  _ = False

data Label = String deriving (Show, Eq)

data Process = Zero | Input Va Va Process | Output Va Va Process |	
               Select Va Label Process | Branch Va [(Label, Process)]
               deriving (Show) 

--Tests wether a Process is Zero or not
--ispZero :: Process -> Bool
ispZero Zero = True
ispZero _ = False

--Destructors for Output
--getChannel :: Process -> Channel
getChannel (Output ch val pr ) = ch
--getVal :: Process -> Va
getVal (Output ch val pr ) = val
--getProcess :: Process -> Process
getProcess (Output ch val pr ) = pr

--Substitutes every occurrence in the Process of Var var with Val val 
--ERRORE DI TIPO, come mostra l'assegnazione di tipo della funzione, var : Val anzichè var : Var 
--substitute :: Process -> Va -> Va -> Process
substitute Zero val var = Zero
substitute (Input ch1 var1 pr1 ) val var = Input ch1 var1 (substitute pr1 val var)  
substitute (Select ch1 la1 pr1 ) val var = Select ch1 la1 (substitute pr1 val var)
substitute (Branch ch1 as ) val var = Branch ch1 (zip [ ys  | ys<-(map fst as) ] [(substitute xs val var) | xs<-(map snd as)])
substitute (Output ch1 val1 pr1 ) val var = Output ch1 val (substitute pr1 val var)                                          

--Determines wheter two process are dual ( i.e. are able to communicate each other ) or not
--dual :: Process -> Process -> Bool
dual (Input ch1 var pr1) (Output ch val pr) = dual (Output ch val pr ) (Input ch1 var pr1 )
dual (Branch ch1 (a:as)) (Select ch la pr ) = dual (Select ch la pr ) (Branch ch1 (a:as))
dual (Output ch val pr ) (Input ch1 var pr1 ) = eq_channel ch ch1
dual (Select ch la pr ) (Branch ch1 []) = False
dual (Select ch la pr ) (Branch ch1 (a:as)) =  if ((fst a) == la) && (eq_channel ch ch1) then True 
	                                           else dual (Select ch la pr ) (Branch ch1 as)
dual _ _  = False

--dualList :: Process -> [Process] -> Bool
dualList pr1 [] = False
dualList pr1 (pr:prs) = if dual pr1 pr then True
                        else dualList pr1 prs 

--Determines wheter a [Process] is composed only by Zero
--isZeroList :: [Process] -> Bool
isZeroList as = and (map ispZero as)

----------------------------------------------------------------------------------------------------------------------------

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
pList [p] = [p]
pList (p:ps) = if dualList p ps then pList (reducepList p ps)
               else pList (ps ++ [p])

{-
reducepList
	1- se a e b sono duali, restituisce il risultato della exec di a e b concatenato con la coda della lista
	2- altrimenti concatena b con il risultato di reduceplist applicata alla coda della lista
Nel momento in cui si applica una riduzione (exec) restituisco la lista i cui processi ridotti 
sono stati aggiornati.
Se restituisco la lista vuota, a non ha al momento un processo con cui comunicare.
-}

--reducepList a [] = []
reducepList a (b:bs) = if dual a b then (exec a b) ++ bs
                       else [b] ++ (reducepList a bs)

--Executes two Processes reducing them according to their Computation Rules
--Persiste il problema di tipo causato dalla substitute

exec (Output ch val pr ) (Input ch1 var pr1 ) = [(substitute pr1 val var), pr]
exec (Select ch la pr ) (Branch ch1 (a:as)) = if ((fst a) == la) then [pr, (snd a)]
	                                              	else exec (Select ch la pr ) (Branch ch1 as)

-----------------------------------------------------------------------------------------------------------------------------}