

something :: [a] -> Int
something [] = 0;
something (x:xs) = 1 + (something xs)

