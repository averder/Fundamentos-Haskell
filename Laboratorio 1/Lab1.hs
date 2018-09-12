-- 0 - Preámbulo
module Lab1 where
import Prelude (Show)

-- 1 - Funciones
-- ?1
id :: a -> a
id = \x -> x

-- ?2
kid :: a -> b -> b
kid = \x -> id          --kid = \x -> \y -> y

k :: a -> b -> a
k = \x -> \y -> x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) = \f -> \g -> \x -> f ( g x)

commute :: (a -> b -> c) -> b -> a -> c
commute = \f -> \x -> \y -> f y x 


-- ?3	
f1 :: (a -> b) -> (a -> b)
f1 = \f -> \x -> ( f x )

f2 :: (a -> a) -> a -> a
f2 = \f -> \x -> f (f x)

f3 :: a -> (a -> b) -> (b -> b -> c) -> c 
f3 = \x -> \y -> \z -> z (y x) (y x)

f4 :: a -> (a -> a -> b) -> (b -> a -> c) -> c
f4 = \x -> \y -> \z -> z (y x x) x

f5 :: a -> b -> (b -> b -> c) -> c
f5 = \x -> \y -> \z -> z y y


-- ?4
h1 :: a -> (a -> b) -> b
h1 = \x -> \f -> f x 

h2 :: (a -> b) -> (b -> c) -> a -> c
h2 = \f -> \g -> \x -> g ( f x )

h3 :: (a -> a -> b) -> a -> b
h3 = \f -> \x -> f x x

h4 :: ((c -> b) -> b -> c -> a) -> c -> (c -> b) -> a
h4 = \f -> \z -> \g -> f g (g z) z

h5 :: (a -> b -> a) -> b -> (b -> a) -> a
h5 = \f -> \x -> \g -> f (g x) x


-- 2 - Los Booleanos
data Bool = False | True
     deriving (Show)

not :: Bool -> Bool
not = \x -> case x of {
                 False -> True;
				 True -> False
			}
			
-- ?5
n1 = not False
n2 = not True			
			
(||) :: Bool -> Bool -> Bool
(||) = \x -> \y -> case x of {
                        False -> y;
						True -> True
                   }
				  
-- ?6
or1 = False || False
or2 = False || True
or3 = True || False
or4 = True || True				  

-- ?7			
(&&) :: Bool -> Bool -> Bool
(&&) = \x -> \y -> case x of {
				        False -> False;
						True -> y;
				   }

--(>>) :: Bool -> Bool -> Bool
--(>>) = ...

--ni :: Bool -> Bool -> Bool
--ni = ...
			

-- ?8.1			
(==) :: Bool -> Bool -> Bool
(==) = \x -> \y -> case x of {
                        False -> case y of {
						              False -> True;
									  True -> False
								 };
						True -> case y of {
						              False -> False;
									  True -> True
						        }
                   }			
	
-- ?8.2	
if' :: Bool -> a -> a -> a
if' = \x -> \y -> \z -> case x of {
                             True -> y;
                             False -> z				 
                        }    	

igual :: Bool -> Bool -> Bool
igual True True   = True
igual False False = True
igual _     _     = False


-- ?8.3
(/=) :: Bool -> Bool -> Bool
(/=) = \x -> \y -> case x of {
                         False -> y;
						 True -> not y;
					}