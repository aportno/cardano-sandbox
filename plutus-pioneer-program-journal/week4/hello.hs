main :: IO()
main = putStrLn "Hello, world!"

bar :: IO()
bar = getLine >>= \s ->
      getLine >>= \t ->
      putStrLn (s ++ t)

