module PracticaListas where
import Prelude hiding ((+), (==), (*), (++), (&&), (||), Bool, True , False)
import Lab1 hiding ((==))
import Lab2 hiding ((%), (<))
import Lab3


length' :: [a] -> N
length' = \ xs -> case xs of {
              [] -> O;
			  k:ks -> S(length' ks) 
}

(++) :: [a]->[a]->[a]
(++) = \xs -> \ ys -> case xs of{
					[] -> ys;
					k:ks->k:(ks ++ ys);
}
reverse :: [a]->[a]
reverse = \xs -> case xs of {
				[] -> [];
				k:ks -> (reverse ks)++[k]
}
concat :: [[a]]->[a]
concat = \xs -> case xs of{
				[]->[];
				k:ks -> k ++ (concat ks)
}
filter :: (a-> Bool) -> [a]->[a]
filter = \p -> \xs -> case xs of {
					[] -> [];
					k:ks -> case (p k) of {
							False -> filter p ks;
							True -> k:(filter p ks) 
							} 
}
takeWhile :: (a-> Bool) -> [a]->[a]
takeWhile = \p -> \xs -> case xs of {
						[]->[];
						k:ks -> case (p k) of {
								False -> [];
								True -> k:(filter p ks)
								}
}
dropWhile :: (a-> Bool) -> [a]->[a]
dropWhile = \p -> \xs -> case xs of {
						[]->[];
						k:ks -> case (p k) of {
								False -> k:(dropWhile p ks);
								True -> xs
								}
}
sum :: [N] -> [N]
sum = \xs -> case xs of {
			[]->[];
			k:ks -> k + (sum ks)
}


map :: (a-> b) -> [a] -> [b]
map = \f -> \xs -> case xs of {
				[] -> [];
				k:ks -> (f k):(map f ks)
}

all :: (a->Bool) -> [a] -> Bool
all = /p -> xs -> case xs of {
				[] -> [];
				k:ks -> 
}
































-- -- 1
-- length' :: [a] -> N
-- length' = \ xs -> case xs of {
              -- [] -> O;
			  -- k:ks -> S(length' ks) 
-- }

-- (++) :: [a] -> [a] -> [a]
-- (++) = \xs -> \ys -> case xs of {
           -- [] -> ys;
		   -- k:ks -> k:(ks ++ ys)  
-- }

-- reverse' :: [a] -> [a] 
-- reverse' = \xs  ->case xs of{
				-- [] -> [];
				-- k:ks -> (reverse' ks) ++ [k]
-- }

-- concat' :: [[a]] -> [a]
-- concat' = \xs -> case xs of {
				-- [] -> [];
				-- k:ks -> k ++ (concat' ks)
				-- }

				
-- filter' :: (a -> Bool) -> [a] -> [a]
-- filter' = \p -> \xs -> case xs of {
						-- [] -> [];
						-- k:ks -> case (p k) of {
								-- True -> k: (filter' p ks);
								-- False -> filter' p ks
								-- }
-- }								

-- takeWhile' :: (a -> Bool) -> [a] -> [a]
-- takeWhile' = \p -> \xs -> case xs of {
						-- [] -> [];
						-- k:ks -> case (p k) of {
								-- True -> k: (takeWhile' p ks);
								-- False -> []
								-- }
-- }

-- dropWhile' :: (a-> Bool) -> [a] -> [a]
-- dropWhile' = \p -> \xs -> case xs of {
						-- [] -> [];
						-- k:ks -> case (p k)  of {
								-- True -> dropWhile' p ks;
								-- False -> xs
								-- }
-- }

-- sum' :: [N] -> N	
-- sum' = \xs -> case xs of {
				-- [] -> O;
				-- k:ks -> k+(sum' ks)
				
-- }	

-- prod' :: [N] -> N	
-- prod' = \xs -> case xs of {
				-- [] -> O;
				-- k:ks -> k * (sum' ks)
-- }				

-- -- 2
-- foldr' :: b -> (a -> b -> b) -> [a] -> b
-- foldr' = \c -> \f -> \xs -> case xs of {
            -- [] -> c;
		    -- k:ks -> f k (foldr' c f ks)
-- }

-- concat'' :: [[a]] -> [a]
-- concat'' = \xs -> (foldr' [] (++) xs )

-- sum'' :: [N] -> N
-- sum'' = \xs -> (foldr' O (+) xs )

-- prod'' :: [N] -> N
-- prod'' = \xs -> (foldr' (S O) (*) xs )

-- and'' :: [Bool] -> Bool
-- and'' = \xs -> (foldr' True (&&) xs)

-- or'' :: [Bool] -> Bool
-- or'' = \xs -> (foldr' False (||) xs)
			
			
-- -- 3

-- map' :: (a-> b) -> [a] -> [b]
-- map' = \f -> \xs -> case xs of {
					-- [] -> [];
					-- k:ks -> (f k):(map' f ks)
					-- }
					
-- all' :: (a -> Bool) -> [a] -> Bool
-- all' = \p -> \xs -> case xs of {
					-- [] -> True;
				    -- k:ks -> (p k) && (all' p ks)
					-- }
					
-- all'' :: (a -> Bool) -> [a] -> Bool
-- all'' = \p -> \xs -> and'' (map' p xs)

-- exists' :: (a -> Bool) -> [a] -> Bool
-- exists' = \p -> \xs -> case xs of {
						-- []-> False;
						-- k:ks -> (p k) || (exists' p ks)
						-- }
						
-- exists'' :: (a -> Bool) -> [a] -> Bool
-- exists'' = \p -> \xs -> or'' (map' p xs)

-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' = \f -> \xs -> \ys -> case xs of {
								-- [] -> [];
								-- k:ks -> case ys of {
											-- []->[];
											-- z:zs -> (f k z):(zipWith' f ks zs) 
											-- }
-- }

-- head' :: [a] -> a
-- head' = \xs -> case xs of {
			-- [] -> error "esto es logicamente imposible";
			-- k:ks -> k
			-- }

-- tail' :: [a] -> [a]
-- tail' = \xs -> case xs of {
			-- [] -> error "esto es logicamente imposible";
			-- k:ks -> ks
			-- }		

-- last' :: [a] -> a
-- last' = \xs -> case xs of {
				-- [] ->  error "esto es logicamente imposible";;
				-- k:ks -> case ks of {
						-- [] -> k;
						-- z:zs -> last' ks
						-- }
-- }

-- last''' :: [a] -> a
-- last''' = \xs -> head' ( reverse' xs)
			
		
-- init' :: [a] -> [a]
-- init' = \xs -> case xs of {
				-- [] ->  error "esto es logicamente imposible";;
				-- k:ks -> case ks of {
						-- [] -> [];
						-- z:zs -> k:(init' ks)
						-- }
-- }

-- (!!!) :: [a] -> N -> a
-- (!!!) = \xs -> \n -> case n of {
                    -- O   -> case xs of {
					           -- [] ->  error "esto es logicamente imposible";;
							   -- z:zs -> z;
						   -- };
					-- S k -> case xs of {
					          -- [] ->  error "esto es logicamente imposible";;
							  -- z:zs -> zs !!! k
					
					       -- }
	-- }

-- take' :: N -> [a] -> [a]
-- take' = \n -> \xs -> case xs of {
                       -- [] -> [];
                       -- k:ks -> case n of {
		                    -- O -> [];
			                -- S j -> k:(take' j ks)
		                    -- }
                    -- }
					
-- drop' :: N -> [a] -> [a]
-- drop' = \n -> \xs -> case xs of {
                       -- [] -> [];
                       -- k:ks -> case n of {
		                    -- O -> xs;
			                -- S j -> (drop' j ks)
		                    -- }
                    -- }
					

-- splitAt' :: N -> [a] -> ([a], [a])
-- splitAt' = \n -> \xs -> case xs of {
                           -- [] -> ([], []);
						   -- k:ks -> case n of {
                               -- O -> ([], xs);
							   -- S j -> ( k:( fst (splitAt' j ks)) , snd (splitAt' j ks))
                           -- }							   
                        -- }
						
-- -- splitAt'' :: N -> [a] -> ([a], [a])
-- -- splitAt'' = \n -> \xs -> let a = (take n xs)
                             -- -- b = (drop n xs)
                         -- -- in (a, b)			
						
						
-- maxx' :: [a] -> a
-- maxx' = \xs -> case xs of {
						-- [] -> error "dddd";
						-- k:ks -> case ks of {
						    -- [] -> k;
							-- z:zs -> maxx' ks
						-- }
				-- }
			
-- divisores :: N -> [N]
-- divisores = \n -> let a = [uno..n]
                  -- in a