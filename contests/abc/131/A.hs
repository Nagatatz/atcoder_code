main = getLine >>= \s -> putStrLn $ if or $ zipWith (==) s $ tail s then "Bad" else "Good"
