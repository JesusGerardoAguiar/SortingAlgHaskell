{-# OPTIONS_GHC -fno-warn-tabs #-}
module Sorting where

ordenada :: Ord a => [a] -> Bool
ordenada = \l -> case l of {[]-> True;
							[x]-> True;
							x:y:ys -> x<=y && ordenada (y:ys)}

-- rec. estructural: EX-EX, llam. rec contenida

perm ::  Eq a => [a] -> [a] -> Bool
perm = \l1 l2 -> case l1 of {[]-> null l2;
							x:xs -> elem x l2 &&
									perm xs (borrar1 x l2)}
-- rec. primitiva en l1

borrar1 :: Eq a => a -> [a] -> [a]
borrar1 = \x l -> case l of {[]-> error "el elemento no esta en la lista";
							z:zs-> case x==z of {False-> z:borrar1 x zs;
												True-> zs}}
-- rec. primitiva en l

-- INSERTSORT ---
insert :: Ord a => a -> [a] -> [a]
-- inserta un elemento en una lista ordenada dejandola ordenada
insert = \x l -> case l of {[]-> [x];
							z:zs-> case x<z of {False-> z:insert x zs;
												True-> x:z:zs}}
-- rec. primitiva en l

insertSort :: Ord a => [a] -> [a]
insertSort = \l -> case l of {[]-> [];
							z:zs-> insert z (insertSort zs)}
-- rec. primitiva en l

--- SELECT SORT ---
minL :: Ord a => [a] -> a
-- minimo elemento de una lista no vacia
minL = \l -> case l of {[]-> error "lista vacia";
						[x] -> x;
						x:y:ys -> min x (minL (y:ys))}
-- rec. estructural. EX-EX. Llam rec. contenida

selectSort :: Ord a => [a] -> [a]
selectSort = \l -> case l of {[]-> [];
				z:zs->minL l:selectSort(borrar1 (minL l) l)}
-- rec. bien fundada: la llamada recursiva se hace sobre una
-- lista que tiene un elemento menos que la lista original
-- porque el minimo de l pertenece a l

-- MERGESORT ---
split :: [a] -> ([a],[a])
-- separa una lista en dos "repartiendo" como un mazo de cartas
split = \l -> case l of {[] ->([],[]);
						[x] -> ([x],[]);
						x:y:ys -> case split ys of
								{(xs,zs) ->(x:xs,y:zs)}}
-- rec. estructural. EX-EX. Llam. recursiva contenida

merge :: Ord a => [a] -> [a]-> [a]
-- junta dos listas ordenadas dejando el resultado ordenado
merge = \l1 l2 -> case l1 of {[] -> l2;
						                  x:xs -> case l2 of{[]-> l1; z:zs -> case x<z of{False-> z:merge l1 zs; True-> x:merge xs l2}}}
-- rec. primitiva en l1 y despues en l2

mergeSort :: Ord a => [a] -> [a]
mergeSort = \l -> case l of {[]->[];
							               [x] -> [x];
							               _ -> case split l of{(xs,ys)-> merge (mergeSort xs)(mergeSort ys)}}
-- rec. bien fundada. Casos Ex-Ex, la llamada recursiva se hace
-- sobre dos listas mas chicas (hubo que hacer 2 casos base para
-- 0 y 1 elementos, para que esta condicion se cumpla

--- QUICKSORT ---
quickSort :: Ord a => [a] -> [a]
quickSort = \l -> case l of
					{[]->[];
				x:xs -> quickSort(filter(<=x) xs) ++ x:quickSort(filter(>x) xs)}
-- rec. bien fundada: Casos Ex-Ex, la llamada recursiva se hace
-- sobre (filer ... xs) que tiene menos elementos que x:xs, porque
-- el filter nunca agranda la lista.
