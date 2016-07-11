module SupportFunctions where

findNext :: Eq a => a -> [a] -> a
findNext x y = case maybefindNext x y of
                    Just b  -> b
                    Nothing -> head y
                    
maybefindNext :: Eq a => a -> [a] -> Maybe a
maybefindNext _ [a] = Nothing
maybefindNext x (y :ys) = if x == y then Just (head ys) else maybefindNext x ys


condMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
condMap c p []       = []
condMap c p (x : xs) = if c x then ((p x) : (condMap c p xs)) else (x : (condMap c p xs)) 


findA :: Eq a => a -> [a] -> Bool
findA x [] = False
findA x (y:ys) = if x == y then True else findA x ys


allAccept :: a -> Bool
allAccept _ = True


applyAll :: [(a -> a)] -> a -> a
applyAll [] v = v
applyAll (x : xs) v = applyAll xs (x v)
 

ioapplyAll :: [(a -> IO a)] -> a -> IO a
ioapplyAll [] v = return v
ioapplyAll (x : xs) v = do v' <- x v
                           ioapplyAll xs v'
                           
                           
ioFold :: (a -> b -> IO b) -> b -> [a] -> IO b
ioFold _ v []       = return v
ioFold f v (x : xs) = do v' <- f x v
                         ioFold f v' xs