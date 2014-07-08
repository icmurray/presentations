#!/usr/bin/env runhaskell

-- include-scala-code.hs
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (takeExtension, (</>), takeFileName)
import Text.Pandoc.JSON

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
    case lookup "include-scala" namevals of
        Just f  -> return . (CodeBlock (id, scalaClasses, namevals)) =<< readScalaFile f
        Nothing -> return cb
    where scalaClasses= "scala":classes
doInclude x = return x

readScalaFile :: FilePath -> IO [Char]
readScalaFile f = do
    scalaFileE <- findScalaFile f
    case scalaFileE of
        Right scalaFile -> readFile scalaFile
        Left err        -> fail err

findScalaFile :: FilePath -> IO (Either String FilePath)
findScalaFile name = do
    candidates <- allFilesIn "code"
    return $ selectSingle (filter matches candidates)
        where matches f = (takeFileName f) == name

allFilesIn :: FilePath -> IO [FilePath]
allFilesIn root = do
    fileNames <- filesIn root
    paths <- forM fileNames $ \name -> do
        let path = root </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then allFilesIn path
            else return [path]
    return (concat paths)

filesIn :: FilePath -> IO [FilePath]
filesIn dir = fmap filterFiles (getDirectoryContents dir)
    where filterFiles = filter ( `notElem` [".", ".."])

selectSingle :: [a] -> Either String a
selectSingle []  = Left "Does not exist"
selectSingle [a] = Right a
selectSingle as  = Left "Not unique"

main :: IO ()
main = toJSONFilter doInclude
