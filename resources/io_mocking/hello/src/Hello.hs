module Hello where

main :: IO ()
main = do
  name <- getLine
  putStrLn $ "Hello " ++ name
