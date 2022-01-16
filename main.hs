import System.IO  
import Control.Monad
import System.Directory
import Data.List


getLines = liftM lines . readFile


add :: String -> String -> IO()
add file input = do
    list <- getLines file
    seq (length list) return()
    let list2 =  lines input
    let updatedList =  list ++ list2
    let sortedList = sort updatedList
    let addedContents = unlines sortedList
    writeFile file addedContents


remove :: String  -> Int -> IO()
remove file index = do
    list <- getLines file
    seq (length list) return()
        
    if index < length list then do
        let (a, b) = splitAt index list
        let updatedList = a ++ tail b
        let updatedContents = unlines updatedList
        writeFile file updatedContents
    else
        putStrLn "Out of bounds error"


userInput :: String -> IO()
userInput file = do
    line <- getLine
    unless (line == "x") $ do
        if head line == 'a' then do
            let remainingString = drop 2 line
            add file remainingString
        else if head line == 'r' then do
                let remainingString = drop 2 line
                let contentToRemove = read remainingString
                remove file contentToRemove
        else if line == "l" then do
                contents <- readFile file
                putStrLn contents
        else putStrLn "Invalid command, choose x l a or r"
        userInput(file)

main :: IO()
main = do 
    let file = "file" 
    isFile <- doesFileExist file
    if not isFile
        then writeFile file ""
    else return()
    userInput(file)
    putStrLn "Exiting Phone Book Program"
