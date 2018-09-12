module Lab3 where
import Prelude(Show)
import Lab1
import Lab2 hiding ((%))

doble :: N -> N
doble = \n -> case n of {
					O -> O;
					S k -> S(S(doble k))
			}
					
count :: N -> (N -> Bool) -> N 
count = \n -> \p -> case n of{
						O -> case p O of{
								True -> S O;
								False -> O
							};
						S k -> case p(S k) of {
								True -> S ( count k p);
								False -> count k p					
							}						
						}
						
anyleq :: N -> (N -> Bool) -> Bool
anyleq = \n -> \p -> case n of {
						O -> p O;
						S k -> case p (S k) of {
						      True -> True;
							  False -> (anyleq k p)
						}
					}
						
-- allleq :: N -> (N -> Bool) -> Bool
-- allleq = \n -> \p -> case n of{
							-- O -> p O;
							-- S k -> (allleq k p) && (p (S k))
					-- }
					
					
(/)::N -> N -> N
(/) = \m -> \n -> case (m >= n) of {
                       False -> O;
					   True -> S ((m-n)/n)   -- Equivalente a: True -> ((m-n)/n) + uno
}		

(%)::N -> N -> N
(%) = \m -> \n -> case (m >= n) of {
                       False -> m;
					   True -> (m-n)%n
}								
			