module Lists where

-- assumes that Eq exists for type a
removeAll :: Eq a => [a] -> [a] -> [a]
removeAll list [] = list
removeAll list (x:xs) = removeAll (lazyRemoveOne list x) xs

lazyRemoveOne :: Eq a => [a] -> a -> [a]
lazyRemoveOne list elemToRem = 
  case list of
    [] -> error "Empty List"
    x:xs | (x == elemToRem) -> xs
    x:xs -> x:lazyRemoveOne xs elemToRem

removeLast :: [a] -> [a]
removeLast = undefined

contains :: Eq a => [a] -> a -> Bool
contains list elem = do 
  case list of
        [] -> False
        x:xs -> if (x == elem)
          then True
          else contains xs elem
