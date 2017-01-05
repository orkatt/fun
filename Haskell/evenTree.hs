-- inspired by http://avva.livejournal.com/2994023.html
-- description https://www.hackerrank.com/challenges/even-tree

module Tree where

data Tree n = Nil | Leaf n | Tree n [Tree n]
      deriving (Show)
      
    
add :: Eq n => Tree n -> n -> n -> Tree n

add Nil s t = Tree s [(Leaf t)]

add (Leaf n) s t
    | n == s        = Tree n [(Leaf t)]
    | otherwise     = Leaf n

add (Tree n cs) s t 
    | n == s        = Tree n (cs ++ [Leaf t])
    | otherwise     = Tree n (map f cs)
        where f x = add x s t

        
count :: Tree n -> Int
 
count Nil         = 0
count (Leaf _)    = 1
count (Tree _ cs) = 1 + sum (map count cs)


removableLinkCount :: Tree n -> Int

removableLinkCount Nil         = 0    

removableLinkCount (Leaf _)    = 0

removableLinkCount (Tree _ cs) = 
    sum (map f cs) + sum (map removableLinkCount cs)
    where f x = if even(count(x)) then 1 else 0


 
----

parseLine :: (String -> n) -> String -> (n,n)
parseLine f s = do
    let l = map f (words s)
    tuplify2 l
    where tuplify2 (x:y:_) = (x,y)


parseTree :: Eq n => (String -> n) -> Tree n -> [String] -> Tree n
parseTree f t (x:xs) = do
    let l = parseLine f x
    let tn = add t (snd l) (fst l)
    if (length xs) > 0 
    then parseTree f tn xs
    else tn


----   

readLines :: Int -> IO [String]
readLines n 
    | n <= 0    = return []
    | otherwise = do 
         x <- getLine
         xs <- readLines (n-1)
         return (x:xs)


main = 
  do fl <- getLine
     let n = read (last (words fl)) :: Int
     lines <- readLines n
     let t = parseTree id Nil lines
     putStrLn (show (removableLinkCount(t)))     


