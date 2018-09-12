module Lab5 where
import Prelude 

-- Agustin Verdera 182584


-- Las variables son strings (listas de caracteres que se escriben entre " ")
type Var = String

-- Una Memoria es una lista de variables con su valor asociado
type Memoria = [(Var,Int)]

-- Funciones para crear y acceder a la Memoria
-- Memoria vacia
ini :: Memoria 
ini = []

-- Devuelve el valor de una variable en la Memoria (si la variable no fue inicializada, da error)
(@@) :: Var -> Memoria -> Int 
(@@) = \v -> \m -> case (length v == 0) of {
                      True -> error "Error";
					  False -> case m of {
					             [] -> error "Error";
                                 k:ks -> case (fst(k) == v) of {
								                 True -> snd k;
												 False -> (v @@ ks)
								          }
                              }					  
}

-- Modifica el valor de una variable en la Memoria
upd ::  (Var,Int) -> Memoria -> Memoria
upd = \vi -> \m -> case m of {
					   [] -> [vi];
                       k:ks -> case ((fst k) == (fst vi)) of {
								   True -> vi:ks;
								   False -> k:(upd vi ks)
							   }
}		

-- Las expresiones del lenguaje son expresiones de enteros que pueden contener variables
-- Al contener variables, su valor depende del contenido de la Memoria, por lo que se representan como funciones del tipo Memoria -> Int
-- Por ejemplo, la expresion x*2+z se representa como la función \m -> ("x" @@ m) * 2 + ("z" @@ m) y el valor de esta expresion en la memoria se obtiene aplicando la expresion a la memoria:
--  (\m -> ("x" @@ m) * 2 + ("z" @@ m)) [(x,1),(z,5)] = 7

type Exp = Memoria -> Int


-- Los programas son funciones que reciben una memoria y devuelven otra
type Prog = Memoria -> Memoria

-- Las siguientes funciones definen los distintos tipos de programas de nuestro lenguaje

-- Asignación: se asigna simultáneamente una lista de expresiones a una lista de variables
as :: [(Var, Exp)] -> Prog
as = \ves -> \m -> updates (eval ves m) m

eval :: [(Var,Exp)] -> Memoria -> [(Var,Int)]
eval = \vs -> \m -> case vs of {	
                       [] -> [];
                       k:ks -> ((fst k),(snd k) m):(eval ks m)
					}

updates :: [(Var,Int)]-> Memoria -> Memoria
updates = \vs -> \m -> case vs of {
                          [] -> m;
						  k:ks -> updates ks (upd k m)
}

-- Composición Secuencial: se ejecuta primero un programa y luego otro
(>*>) :: Prog -> Prog -> Prog
(>*>) = \p1 -> \p2 -> \m -> p2 (p1 m)

-- If con Guardas: recibe una lista de expresiones booleanas (Guardas), cada una con su programa asociado, 
-- y ejecuta un programa cuya guarda sea verdadera. Si ninguna guarda es verdadera da error
-- Del mismo modo que las expresiones, las guardas son funciones del tipo Memoria -> Bool
type Guarda = Memoria -> Bool
--data Maybe a = Nothing | Just a 

gif :: [(Guarda,Prog)] -> Prog
gif = \gps -> \m -> case choice gps m of {Nothing -> error "gif: ninguna guarda verdadera";
										  Just p -> p m}

	
choice :: [(Guarda,Prog)] -> Memoria -> Maybe Prog										  
choice = \gps -> \m -> case gps of {
                        [] -> Nothing;
						(g,p):ks -> case (g m) of {
						              True -> Just  p;
									  False -> choice ks m
								}
}

-- Do con guardas: al igual que el If, recibe una lista de expresiones guardas, cada una con su programa asociado, 
--y ejecuta un programa cuya guarda sea verdadera. Este procedimiento se repite hasta que ninguna guarda sea verdadera,
-- en cuyo caso el programa termina devolviendo la memoria que recibió.
gdo :: [(Guarda,Prog)] -> Prog
gdo = \gps -> \m -> case (choice gps m) of {
				          Nothing -> m;
				          Just p -> gdo gps (p m)   --(p >*> (gdo gps)) m
				}
					 				


-- Factorial de n = / 1                      si n=0
--                  \ n(n-1)(n-2)....3.2.1   si n>=1

n :: Var
n = "n"
p :: Var
p = "p"

fact :: Prog
fact = as [(p, \m -> 1)] >*> 			                      -- p:=1
	   gdo [(\m ->(n@@m > 0), 			                      -- guarda n>0
	   as [(p,\m -> (p@@m)*(n@@m)), (n,\m -> (n@@m)-1)])]     -- p=p*n, n=n-1 

	   
		
		
-- Fibonaccci de n =   / 1                     si n=0 || n=1
--                     \ fib(n-1) + fib(n-2)   si n>=2
--
-- Nota: Comenzamos la sucesion a partir de 1	
	
f :: Var		
f = "f"
ant :: Var
ant = "ant"	
	
fib :: Prog
fib = as [(ant, \m -> 1), (f, \m -> 1)] >*>                                     -- ant:=1 , f:=1
	  gdo[(\m ->(n@@m > 1), 							                        -- guarda n>1 
	  as [(f, \m ->(ant@@m)+(f@@m)), (ant, \m -> (f@@m)), (n,\m ->(n@@m)-1)])]  -- f:=ant+f, ant:=f, n:=n-1

		
	  