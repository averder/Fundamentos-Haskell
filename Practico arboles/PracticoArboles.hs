-- 1
-- Arbol con dato de tipo "a" en las hojas, (nodos sin dato(
data AB a = Hoja a | Node (AB a) (AB a)
            deriving (Show)
	
	
hojas :: AB a -> [a]
hojas = \t -> case t of {
                Hoja h -> [h];
				Node i d -> (hojas i) ++ (hojas d)
}

cantHojas :: AB a -> Int
cantHojas = \t -> case t of {
                Hoja h -> 1;
				Node i d -> (cantHojas i) + (cantHojas d)
}

cantNodos :: AB a -> Int
cantNodos = \t -> case t of {
                Hoja h -> 0;
				Node i d -> (cantNodos i) + (cantNodos d) + 1
}

altura :: AB a -> Int
altura = \t -> case t of {
                Hoja h -> 1;
				Node i d -> case ((altura i) + 1) > ((altura d) + 1) of {
				                 True -> (altura i) + 1;
								 False -> (altura d) + 1;
				            }
}

altura' :: AB a -> Int
altura' = \t -> case t of {
                Hoja h -> 1;
				Node i d -> let a1 = (altura' i) + 1
				                a2 = (altura' d) + 1
				            in case (a1 > a2) of {
				                 True -> a1;
								 False -> a2;
				            }
}

-- altura'' ::  AB a -> Int
-- altura'' = \t -> case t of {
                -- Hoja h -> 1;
				-- Node i d -> case (a1 > a2) of {
				                 -- True -> a1;
								 -- False -> a2
				            -- };
				-- where a1 = ((altura'' i) + 1)
				      -- a2 = ((altura'' d) + 1)
-- }

espejo :: AB a -> AB a
espejo = \t -> case t of {
                Hoja h -> Hoja h;
				Node i d -> Node (espejo d) (espejo i)
}

-- maxHoja :: AB a -> a
-- maxHoja =  \t -> case t of {
                -- Hoja h -> h;
				-- Node i d -> case (maxHoja d) > (maxHoja i) of {
				                 -- True -> d;
								 -- False -> i;
				            -- }
-- }		

mapAB :: (a -> b) -> AB a -> AB b
mapAB = \f -> \t -> case t of {
                        Hoja h -> Hoja (f h);
                        Node i d -> Node (mapAB f i) (mapAB f d)						
}

subTrees :: AB a -> [AB a]
subTrees = \t -> case t of {
                        Hoja h -> [Hoja h];
                        Node i d -> (Node (head(subTrees i)) (head(subTrees d))):((subTrees i) ++ (subTrees d))		
}


-- abTree1: Ejemplo de arbol AB con datos de string en las hojas
-- Tiene dos nodos internos, y tres hojas.
-- Tiene altura 3
-- Tiene 4 sub arboles (cinco contando al propio arbol)
--          -
--        /   \
--       -     C
--      / \
--     A   B
abTree1 = Node (Node (Hoja "A") (Hoja "B")) (Hoja "C")


-- abTree2: Espejo de abTree1
--          -
--        /   \
--       C     -     
--            / \
--           B   A
abTree2 = Node (Hoja "C") (Node (Hoja "B") (Hoja "A")) 

-- abTree1: Ejemplo de arbol AB con datos de numeros en las hojas
--          -
--        /   \
--       -     3
--      / \
--     1   2
abTree3 = Node (Node (Hoja 1) (Hoja 2)) (Hoja 3)



-- 3
-- Arbol con dato de tipo "a" en los nodos
data ABB a = Vacio | Nodo (ABB a) a (ABB a)
            deriving (Show)
			
largo :: ABB a -> Int
largo = \t -> case t of {
                 Vacio -> 0;
                 Nodo i x d -> (largo i) + (largo d) + 1
		}
			
inOrder :: ABB a -> [a]
inOrder = \t -> case t of {
                   Vacio -> [];
                   Nodo i x d -> (inOrder i) ++ [x] ++ (inOrder d)
}

preOrder :: ABB a -> [a]
preOrder = \t -> case t of {
                   Vacio -> [];
                   Nodo i x d ->  [x] ++ (preOrder i) ++ (preOrder d)
}

member :: (Eq a) => a -> ABB a -> Bool
member = \e -> \t -> case t of {
                   Vacio -> False;
                   Nodo i x d -> case (x == e) of {
				                      True -> True;
									  False -> (member e i) || (member e d)
				                 }
}

ordenado :: (Ord a) => ABB a -> Bool
ordenado = \t -> case t of {
                    Vacio -> True;
					Nodo i x d -> case i of {
					                 Vacio -> case d of {
									            Vacio -> True;
												Nodo di dx dd -> (x<= dx) && (ordenado d)
											};
									 Nodo ii xi id -> case d of {
									                       Vacio -> (x>=xi) && (ordenado i);
														   Nodo di dx dd -> (x >= xi) && (x <= dx) && (ordenado i) && (ordenado d)
									                 };
									 };

}

insert :: (Ord a) => a -> ABB a -> ABB a
insert = \e -> \t -> case t of {
                    Vacio -> Nodo Vacio e Vacio;
					Nodo i x d -> case (e <= x) of {
					                  True -> Nodo (insert e i) x d;
									  False -> Nodo i x (insert e d)
								  }
					}
					
list2ABB :: (Ord a) => [a] -> ABB a
list2ABB = \xs -> case xs of {
                    [] -> Vacio;
					k:ks-> insert k (list2ABB ks)
}

treeSort :: Ord a=> [a] -> [a]
treeSort = \xs -> inOrder  (list2ABB xs)


merge :: (Ord a) => ABB a -> ABB a -> ABB a
merge = \t1 -> \t2 -> list2ABB ( (inOrder t1) ++ (inOrder t2))
 
delete :: (Ord a) => a -> ABB a -> ABB a
delete = \e -> \t -> case t of {
                        Vacio -> Vacio;
						Nodo i x d -> case (e == x) of {
						                   True -> merge (delete e i) (delete e d);
										   False -> Nodo (delete e i) x (delete e d)
						}
}

min' :: ABB a -> a
min' = \t -> case t of {
              Vacio ->  error "DFFDF";
			  Nodo i x d -> case i of {
			                   Vacio -> x;
							   Nodo ii ix id -> min' i
							  }
			}
			
max' :: ABB a -> a
max' = \t -> case t of {
              Vacio ->  error "DFFDF";
			  Nodo i x d -> case i of {
			                   Vacio -> x;
							   Nodo id dx dd -> max' d
							  }
			}
			  
menor :: (Ord a) => ABB a -> ABB a -> Bool
menor = \t1 -> \t2 -> ((max' t1) < (min' t2))

prefijo :: (Eq a) => [a] -> [a] -> Bool
prefijo = \ps -> \ys -> case ps of {
                           [] -> True;
						   k:ks -> case ys of {
						              [] -> False;
									  z:zs -> (k==z) && (prefijo ks zs)
									}
						}

-- abbTree1: Ejemplo de arbol ABB
-- Tiene 5 nodos en total. Con 3 hojas.
-- Tiene altura 3
-- Tiene 4 sub arboles (cinco contando al propio arbol)
--          D
--        /   \
--       B     E
--      / \
--     A   C
abbTree1 = Nodo (Nodo (Nodo Vacio "A" Vacio) "B" (Nodo Vacio "C" Vacio)) "D" (Nodo Vacio "E" Vacio)

-- abbTree2: Ejemplo de arbol ABB ordenado
--          4
--        /   \
--       2     5
--      / \
--     1   3
abbTree2 = Nodo (Nodo (Nodo Vacio 1 Vacio) 2 (Nodo Vacio 3 Vacio)) 4 (Nodo Vacio 5 Vacio)

-- abbTree3: Ejemplo de arbol ABB NO ordenado
--          4
--        /   \
--       2     1
abbTree3 = Nodo (Nodo Vacio 2 Vacio) 4 (Nodo Vacio 1 Vacio)

-- abbTree4
--          2
--        /   \
--       1     3
abbTree4 = Nodo (Nodo Vacio 1 Vacio) 2 (Nodo Vacio 3 Vacio)

-- abbTree5
--          5
--        /   \
--       4     6
abbTree5 = Nodo (Nodo Vacio 4 Vacio) 5 (Nodo Vacio 6 Vacio)

-- abbTree6
--          4
--        /   \
--       2     4
--      / \
--     2   3
abbTree6 = Nodo (Nodo (Nodo Vacio 2 Vacio) 2 (Nodo Vacio 3 Vacio)) 4 (Nodo Vacio 4 Vacio)



data BT = NodoH Bool | NodoI Bool BT BT
          deriving (Show)
			   
-- btTree1
--          True
--        /      \
--       False    False
btTree1 = NodoI True (NodoH False) (NodoH False)

-- btTree2
--          True
--        /      \
--      False    True
--     /     \
--    True  True
btTree2 = NodoI True (NodoI False (NodoH True) (NodoH True)) (NodoH True)

btTree3 = NodoH False

raiz :: BT -> Bool
raiz = \t -> case t of {
                NodoH x -> x;
				NodoI x i d -> x
}

alt :: BT -> Bool
alt = \t -> case t of {
              NodoH x -> True;
			  NodoI x i d -> (x /= (raiz i)) && (x /= (raiz d)) && (alt i) && (alt d)
}