
type Val = Int

type Var = Name

type Name = String

data Polarity = Plus | Minus deriving (Show)

opposite Plus = Minus
opposite Minus = Plus

type Channel = (Polarity, Name) 

(-=) Plus Plus = True
(-=) Minus Minus = True
(-=) _ _ = False 

eq_channel (p, name) (p1, name1) = if ((opposite p) -= p1 ) then True
                                   else False

data Label = String deriving (Show, Eq)

data Process = Zero | Input Channel Var Process | Output Channel Val Process |
               Select Channel Label Process | Branch Channel [(Label, Process)]
               deriving (Show) 

exec Zero Zero = Zero 
exec (Output ch var pr ) (Input ch1 val pr1 ) = if (eq_channel ch ch1) then exec pr pr1
                                                else Zero
exec (Select ch la pr ) (Branch ch1 []) = Zero
exec (Select ch la pr ) (Branch ch1 (a:as)) = if ((fst a) == la) && (eq_channel ch ch1) then exec pr (snd a)
                                              else exec (Select ch la pr ) (Branch ch1 as)
