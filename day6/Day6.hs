import Data.List (nub)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

findDiffs :: Int -> String -> Maybe Int
findDiffs l = go 0
  where go pos xs@(_:_:_:_) =
                    if l == (length $ nub $ take l xs) then Just (pos + l)
                    else go (pos + 1) (drop 1 xs)
        go pos _ = Nothing

main :: IO ()
main = do [file, numString] <- getArgs
          s <- readFile file
          let num = read numString
          putStrLn $ show $ fromMaybe (-1) $ findDiffs num s
