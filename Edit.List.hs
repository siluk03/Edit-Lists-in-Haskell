module Edit.List 
( rotatel
, rotater
, movel
, mover
, rotateRu
, rotateLu
, moveRu
, moveLu
, replace
, replaceFr
, rotateEr
, rotateEl
, replaceFl
, replaceA
, elementr
, elementl
, newLine
) where


rotater a x = if a == 0 then x else rotater (a-1) ([last x] ++ (init x))

rotatel a x = if a == 0 then x else rotatel (a-1) ((tail x) ++ [head x])

mover a x = if a == 0 then x else  mover (a-1) (init x)

movel a x = if a == 0 then x else movel (a-1) (tail x)


rotateRu a (x:xs) = if a `elem` (x:xs) then if a == (last xs) then (x:xs) else rotateRu a ([last (x:xs)] ++ (init (x:xs))) else error "Element ist in der Liste nicht vorhanden"

rotateLu a (x:xs) = if a `elem` (x:xs) then if a == x then (x:xs) else rotateLu a ((tail (x:xs)) ++ [head (x:xs)]) else error "Element ist in der Liste nicht vorhanden"

moveRu a x = if a `elem` x then if a == (last x) then x else  moveRu a (init x) else error "Element ist in der Liste nicht vorhanden"

moveLu a x = if a `elem` x then if a == (head x) then x else moveLu a (tail x) else error "Element ist in der Liste nicht vorhanden"


replace a x = [a] ++ (tail x)


replaceFr e a x =
			let y = x
			in if e `elem` x then rotatel ((rotateEr e y)+1) $ replace a $ rotater 1 $ rotateRu e x else error "Element ist in der Liste nicht vorhanden"
			
			
replaceFl e a x =
			let y = x
			in 	if e `elem` x then rotater (rotateEl e y) $ replace a $ rotateLu e x else error "Element ist in der Liste nicht vorhanden"


rotateEr e x = repl e x 0
				where repl e x y = if e `elem` x then if last x == e then y else repl e (rotater 1 x) (y+1) else error "Element ist in der Liste nicht vorhanden"
				
				
rotateEl e x = repl e x 0
				where repl e x y = if e `elem` x then if head x == e then y else repl e (rotatel 1 x) (y+1) else error "Element ist in der Liste nicht vorhanden"


replaceA e a x = if e `elem` x then replaceA e a (replaceFl e a x) else x


elementl a (x:xs) = head $ rotatel (a-1) (x:xs)

elementr a (x:xs) = last $ rotater (a-1) (x:xs)


newLine x =  putStrLn (init $ unlines $ [b | a <- x, let b = show(a)])
