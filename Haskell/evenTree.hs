-- inspired by http://avva.livejournal.com/2994023.html
-- description https://www.hackerrank.com/challenges/even-tree

module EvenTree where

data Tree n = Nil | Leaf n | Tree n [Tree n] | Fail
      deriving (Show)

    
link :: Tree String -> String -> String -> Tree String

link Nil s t = 
    Tree s [(Leaf t)]

link (Leaf n) s t
    | n == s        = Tree n [(Leaf t)]
    | otherwise     = Leaf n

link (Tree n cs) s t 
    | n == s        = Tree n (cs ++ [Leaf t])
    | otherwise     = Tree n (map f cs)
        where f x = link x s t


 
--
class Countable t where
   count :: t -> Int

 
instance Countable (Tree n) where 
   count Nil         = 0
   count (Leaf _)    = 1
   count (Tree _ cs) = 1 + sum (map count cs)


removableEdges :: Tree String -> Int
removableEdges Nil         = 0    
removableEdges (Leaf _)    = 0
removableEdges (Tree _ cs) = sum (map f cs) + sum (map removableEdges cs)
                             where f x = if even(count(x)) then 1 else 0


readN :: Int -> Tree String -> IO (Tree String)

readN n t = 
   if n /= 0 
   then   
      do l <- getLine
         if l /= ""
         then readN (n-1) (join t (words l))
         else return t
   else return t
   where join x (n1:n2:_) = link x n2 n1





--   
main = 
  do fl <- getLine
     let n = read (last (words fl)) :: Int
     --putStrLn (show n)
     t <- readN n Nil
     --putStrLn (show t)
     putStrLn (show (removableEdges(t)))     


