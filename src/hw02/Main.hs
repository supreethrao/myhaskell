fold :: (a -> Bool) -> [a] -> b
fold f []     = 0
fold f (x:xs) = f x (fold f z xs)