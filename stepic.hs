perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = concat (map (\y -> (perms' x y)) (perms xs))
                    where perms' x y =  map (\n -> (take n y) ++ [x] ++ (drop n y)) [0..(length y)]
