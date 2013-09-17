import System.IO
import System.Process
import System.Environment

cls :: IO ()
cls = system "clear" >> return ()

main = do
  (input:_) <- getArgs
  hSetBuffering stdin NoBuffering
  ls <- readFile input 
  flip mapM_ (go [] [] . lines $ ls) $ \s -> do
    cls
    putStr (unlines s)
    getChar
    return ()
  where
  go [] xss [] = xss
  go xs xss [] = xss ++ [xs]
  go xs xss (x@('#':'#':_):rest) = go [x] (xss ++ [xs]) rest
  go xs xss (x:rest) = go (xs ++ [x]) xss rest
