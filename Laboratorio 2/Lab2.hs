module Lab2 where
import Prelude(Show)
import Lab1 hiding ((==), (/=))   --usamos hiding para que no haya conflictos con el (==) programado en el Lab1

-- 1 - Los naturales
data N = O | S N
     deriving (Show)

-- 1?
cero = O
uno = S O
dos = S ( S O)
tres = S ( S (S O) )
cuatro = S ( S ( S ( S O) ) )
cinco = S ( S ( S ( S ( S O) ) ) )
seis  = S (S ( S ( S ( S ( S O) ) ) ) )
siete = S( S (S ( S ( S ( S ( S O) ) ) ) ) )
ocho = S( S( S (S ( S ( S ( S ( S O) ) ) ) ) ) )

pred :: N -> N
pred = \n -> case n of {
				O -> O;
				S x -> x
			 }
			 
suc :: N -> N
suc = \n -> case n of {
				O -> S O;
				S x -> S ( S x)
		    }
	

-- 2?
par :: N -> Bool
par = \n -> case n of {
                 O -> True;
				 S x -> not ( par x )
			}
			

			
-- 3?

-- Basado en que: 2 + 3 = 3 + 2 = 4 + 1 = 5 + 0 = 5
-- Disminuimos n y aumentamos m
-- (+) :: N -> N -> N
-- (+) = \n -> \m -> case m of {
                     -- O -> n;
					 -- S x -> (S n) + x
				  -- }
			
-- Version a partir de la definicion de suma en axiomas de Peano: 
--   n + (m+1) = (n+m)+1
-- En nuestra notacion:
--   n + (S x) = S (n + x) 
(+) :: N -> N -> N                      
(+) = \n -> \m -> case m of {
                     O -> n;
					 S x -> S (n + x)
				  }
				
				
-- (*) :: N -> N -> N
-- (*) = \n -> \m -> case m of {
                     -- O -> O;     -- Propiedad de absorcion
					 -- S O -> n;
					 -- S x -> (n + n) * x
				-- }

-- Version a partir de la definicion de multiplicacion en axiomas de Peano: 
--   n * (m+1) = n*m + n
-- En nuestra notacion:
--   n * (S x) = (n * x) + n 
(*) :: N -> N -> N                      
(*) = \n -> \m -> case m of {
                     O -> O;     -- Propiedad de absorcion
					 S x -> (n * x) + n
				  }		

				  
-- 4? 
-- Analogo a la suma: 
--   n - (m+1) = (n-m)-1
-- En nuestra notacion:
--   n - (S x) = pred (n - x) 
(-) :: N -> N -> N                      
(-) = \n -> \m -> case m of {
                     O -> n;
					 S x -> pred (n - x)
				  }	
				  
				  
-- 5?
fact :: N -> N
fact = \n -> case n of {
                  O -> S O;
                  S x -> (S x) * (fact x)				  
              }
			  
			  
pot :: N -> N -> N
pot = \b -> \p -> case p of {
                  O -> S O;
                  S x -> b * (pot b x)				  
              }
			  
-- 6?
sumi :: N -> N
sumi = 	\n -> case n of {
                  O -> O;
				  S x -> S x + (sumi x)
              }		
	
	
-- 2 - Estructuras abstractas de dato

class Eq a where {                        -- Relacion de Equivalencia
	(==) :: a -> a -> Bool;
    (/=) :: a -> a -> Bool;
	(/=) = \x -> \y -> not (x == y)       -- Definicion estandar para (/=)
}
			
-- 7?
instance Eq N where {                     -- Instanciar equivalencia para el tipo N de los naturales
   (==) = \n -> \m -> case n of {
                          O -> case m of {
						           O -> True;
								   S k -> False
						       };
                          S x -> case m of {
						           O -> False;
								   S k -> (x == k)
						       };
				      }
}
		
		
class (Eq a) => Ord a where {                     -- Relacion de Orden, extiende de Equivalencia
	(<)  :: a -> a -> Bool;                       -- Podemos escribirlos todos en una sola linea como: (<), (<=), (>), (>=) :: a -> a -> Bool
	(<=) :: a -> a -> Bool;
	(>)  :: a -> a -> Bool;
	(>=) :: a -> a -> Bool;
	
	--(<=) = \n -> \m -> ((n < m) || (n == m));    -- Si damos definiciones estandar en terminos de (<), luego al instanciar solo es necesario programar (<)
	--(>)  = \n -> \m -> (m < n);
	--(>=) = \n -> \m -> ((m < n) || (n == m))
}

-- 8?
instance Ord N where {                           -- Instanciar orden para el tipo N de los naturales
	(<) = \n -> \m -> case n of {
                          O -> case m of {
						           O -> False;
								   S k -> True
						       };
                          S x -> case m of {
						           O -> False;
								   S k -> (x < k)
						       };
					  };
					 
	(<=) = \n -> \m -> case n of {
                          O -> case m of {       -- Esta rama puede simplificarse en:  O -> True;
						           O -> True;
								   S k -> True
						       };
                          S x -> case m of {
						           O -> False;
								   S k -> (x <= k)
						       };
					  };
					  
	(>) = \n -> \m -> case n of {
                          O -> case m of {           -- Esta rama puede simplificarse en:  O -> False;
						           O -> False;
								   S k -> False;
						       };
                          S x -> case m of {
						           O -> True;
								   S k -> (x > k)
						       };
					  };	
					  
	(>=) = \n -> \m -> case n of {
                          O -> case m of {
						           O -> True;
								   S k -> False;
						       };
                          S x -> case m of {
						           O -> True;
								   S k -> (x >= k)
						       };
					  };
}
		 
-- ?10
-- Con funciones auxiliares para comparar
max :: N -> N -> N
max = \n -> \m -> case (n > m) of {
                      True -> n;
					  False -> m
                  }
				  
-- Sin funciones auxiliares
max' :: N -> N -> N
max' = \n -> \m -> case n of {
                          O -> m;
                          S x -> case m of {
						           O -> n;
								   S k -> S( max' x k)
						       };
					  };

					  
-- Con funciones auxiliares para comparar
min :: N -> N -> N
min = \n -> \m -> case (n < m) of {
                      True -> n;
					  False -> m
                  }
				  
-- Sin funciones auxiliares
min' :: N -> N -> N
min' = \n -> \m -> case n of {
                          O -> O;
                          S x -> case m of {
						           O -> O;
								   S k -> S( min' x k)
						       };
					  };				  
					  
-- ?11
max3 :: N -> N -> N -> N
max3 = \n -> \m -> \q -> case (n > m) of {
                            True -> case (n > q) of {
							              True -> n;
										  False -> q
							         };
					        False -> case (m > q) of {
							              True -> m;
										  False -> q
							         }
                         }
						 
min3 :: N -> N -> N -> N
min3 = \n -> \m -> \q -> case (n < m) of {
                            True -> case (n < q) of {
							              True -> n;
										  False -> q
							         };
					        False -> case (m < q) of {
							              True -> m;
										  False -> q
							         }
                         }
						 
						 
-- 3 - Mas funciones sobre naturales

-- ?12
times' :: N -> (a -> a) -> a -> a
times' = \n -> \f -> \x -> case n of {
					    O -> x;
					    S k -> times k f (f x)
                   }
				   
times :: N -> (a -> a) -> a -> a
times = \n -> \f -> case n of {
					    O -> \x-> x;
					    S k -> \x -> f ( (times k f ) x )
                   }
		 
--ej: times tres suc cero   aplica el sucesor a cero 3 veces, el resultado debe ser 3

-- ?13
-- Nueva suma : (dos % tres) = (dos + tres)
-- Sumarle m a n, equivale a calcular el sucesor de n unas m veces
(%) :: N -> N -> N
(%) = \n -> \m -> times m suc n 

-- ?14
-- Nuevo producto : (dos & tres) = (dos * tres)
-- Multiplicar m x n, equivale a sumar n unas m veces
-- La suma (+) o (%) es de tipo (N -> N -> N), pero el segundo parametro de times debe ser una funcion de tipo (N -> N).
-- Entoncs usamos (n+)=(n%) que recibe un solo parametro y se aplica parcialmente quedando de tipo (N -> N), 
-- luego el segundo parametro en el cual se aplicara la suma sera "cero" y es el tercer parametro que le damos a la funcion times
-- Por lo tanto, esto es sumar n desde cero unas m veces
(&) :: N -> N -> N
(&) = \n -> \m -> times m (n+) cero    

-- Nuevo potencia
-- Hacer la potencia de b^p, equivale a multiplicar b p veces
-- El producto (*) o (&) es de tipo (N -> N -> N), pero el segundo parametro de times debe ser una funcion de tipo (N -> N).
-- Esto lo solucionamos de la misma manera que el caso anterior, utilizando (b*) que queda de tipo (N -> N)
-- Por lo tanto, esto es multiplicar b desde uno unas p veces
pot' :: N -> N -> N
pot' = \b -> \p -> times p (b*) uno


-----------------------------------------------------------
