import Data.List 
import Data.Char
import System.IO
import System.Random
import Control.Monad (replicateM)


-- Things to do 
--     1. Take word from a word dump using random : Done    
--     2. Make it prettier i.e images
--     3. Refactor
--     4. Modular : Have a config file for images , doc location etc
--     5. Type Checking : Function declarations everywhere



strip list = filter(/='\"') list
splitter x y = func x y [[]]
    where
        func x [] z = reverse $ map (reverse) z
        func x (y:ys) (z:zs) = if y==x then func x ys ([]:(z:zs)) else func x ys ((y:z):zs)

wrong word done = length $ filter (`notElem`  word) done
printer word done = map (\x-> if x `elem` done then x else '_') word
hangman :: String -> [Char] -> IO(Int)
hangman word done 
    | all (`elem` done) (word) = do
        putStrLn "You Win!" 
        return 10
        
    | wrong word done > 7 = do
        putStrLn "You Lost"
        putStr "The Right answer was "
        putStrLn word
        return 0
        
    | otherwise = do
                    putStrLn $ printer word done
                    putStrLn "Enter the letter"
                    l<-getChar
                    useless<-getChar
                    if isAlpha l then
                        do
                            if (toLower) l `elem` done then
                                do
                                    putStrLn "Letter is already done. Please try again"
                                    hangman word done 
                            else 
                                do
                                    let guessed = (toLower l):done
                                    if (toLower l) `elem` word then do
                                        putStrLn "Right Answer"
                                    else do
                                        putStrLn "Wrong Answer"
                                        let left = show $ 7 - wrong word done 
                                        let out = left ++ " lives left"
                                        putStrLn out
                                    putStr "Letters done : "
                                    putStrLn $  unwords $ map (\x-> [x] ) $ sort guessed
                                    hangman word guessed
                    else
                        do
                            putStrLn "Input only letters."
                            hangman word done 
                        



mainPart l score = do
    random <- replicateM 1 (randomIO :: IO Int) 
    let index = (abs $ head random) `mod` (length l) 
    let line =   l !! index
    let mwords = splitter ',' line
    let uword = strip (mwords !! 0)
    let word  = map (toLower) uword
    -- print word
    n <- hangman word [] 
    let meaning = (unwords ( tail ( tail mwords)))
    putStr "Meaning of the word : "
    putStrLn  (strip meaning)
    let newScore = score  + n
    putStrLn "Enter q to quit. Any other key and enter to continue"
    y<-getChar
    useless<-getChar
    if (y=='q') then gameover newScore else mainPart l newScore
    
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    let s ="dictionary.csv"
    database <- readFile s
    let l = lines database
    mainPart l 0
    
gameover n = do
    putStr "The Game is over. Your score is "
    putStrLn $ show n

                