{-# LANGUAGE ImportQualifiedPost #-}

module FileSystem where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split qualified as Split
import Data.Maybe (isJust, isNothing, mapMaybe)
import System.IO

type Name = String

type Content = String

data RegularFile = RegularFile Name Content
  deriving (Eq, Show)

data Dir = Dir Name [FileTree]
  deriving (Eq, Show)

-- File tree representation
data FileTree
  = File RegularFile
  | Directory Dir
  deriving (Eq, Show)

type Arg = String

type Path = [String]

-- F - file
-- D - directory
data FileType = F | D
  deriving (Eq, Show)

smallTree :: FileTree
smallTree =
  Directory $
    Dir
      "/"
      [ File $ RegularFile "file1" "my file1\nworking very hard",
        File $ RegularFile "file2" "file2 is here",
        File $ RegularFile "file3" "file3 is here",
        Directory $
          Dir
            "dir1"
            [ File $ RegularFile "file1" "/dir1/file1 is here",
              Directory $
                Dir
                  "dir3"
                  [ File $ RegularFile "file4" "/dir1/dir3/file4 is here",
                    Directory $
                      Dir "dir5" [File $ RegularFile "file6" "/dir1/dir3/dir5/file6"]
                  ],
              Directory $
                Dir "dir4" []
            ],
        Directory $
          Dir
            "dir2"
            [File $ RegularFile "file5" "/dir2/file5 is here"]
      ]

bigTree :: FileTree
bigTree =
  Directory $
    Dir
      "/"
      [ Directory $
          Dir
            "ip"
            [ Directory $
                Dir
                  "homeworks"
                  [ File $ RegularFile "hw1" "make a calculator",
                    File $ RegularFile "hw2" "building bulls-and-cows interactive game\n aditional information",
                    File $ RegularFile "hw3" "string to date time parser"
                  ],
              Directory $
                Dir
                  "exercises"
                  [File $ RegularFile "ex1" "for loops in C++"]
            ],
        Directory $
          Dir
            "oop"
            [ Directory $
                Dir
                  "homeworks"
                  [ File $ RegularFile "hw1" "constructors",
                    File $ RegularFile "hw2" "vector"
                  ],
              Directory $
                Dir
                  "project"
                  [ Directory $
                      Dir
                        "src"
                        [ File $ RegularFile "main.cpp" "this is the main file",
                          File $ RegularFile "FileSystem.cpp" "this is the FileSystem project"
                        ]
                  ]
            ],
        Directory $
          Dir
            "dsa"
            [ File $ RegularFile "content" "link to the online lectures you can find here:\nhttps://www.youtube.com/lectures-fmi",
              Directory $
                Dir
                  "homeworks"
                  [ File $ RegularFile "hw1" "implement an elevator",
                    File $ RegularFile "hw2" "create an avl tree",
                    File $ RegularFile "hw3" "create an electronic table (like excell)"
                  ],
              Directory $
                Dir
                  "project"
                  [ File $ RegularFile "lzw" "implement lzw compressing program",
                    Directory $
                      Dir
                        "implementation"
                        [File $ RegularFile "impl" "lzw algorithm"]
                  ]
            ],
        Directory $
          Dir
            "fp"
            [ Directory $
                Dir
                  "homeworks"
                  [ File $ RegularFile "hw1" "scheme",
                    File $ RegularFile "hw2" "haskell"
                  ],
              Directory $
                Dir
                  "project"
                  [File $ RegularFile "file system" "FileSystem project"]
            ]
      ]

-- >>> strToPath "Users/Pesho"
-- ["Users","Pesho"]
strToPath :: String -> Path
strToPath [] = []
strToPath [only] = case only of '/' -> ["/"]; _ -> [[only]]
strToPath (x : xs) = case x of
  '/' -> "/" : Split.splitOn "/" xs
  _ -> Split.splitOn "/" (x : xs)

isDir :: FileTree -> Maybe Dir
isDir (Directory dir) = Just dir
isDir _ = Nothing

isFile :: FileTree -> Maybe RegularFile
isFile (File file) = Just file
isFile _ = Nothing

getFileName :: RegularFile -> Name
getFileName (RegularFile nm _) = nm

getFileContent :: RegularFile -> Content
getFileContent (RegularFile _ con) = con

rewriteFileContent :: String -> RegularFile -> RegularFile
rewriteFileContent newCon (RegularFile nm _) = RegularFile nm newCon

getDirName :: Dir -> Name
getDirName (Dir dirName _) = dirName

getDirChildren :: Dir -> [FileTree]
getDirChildren (Dir _ children) = children

find :: Path -> FileType -> FileTree -> Maybe FileTree
find [] _ _ = Nothing
find [only] F f@(File (RegularFile fileName _)) = if only == fileName then Just f else Nothing
find [only] D dir@(Directory (Dir dirName _)) = if only == dirName then Just dir else Nothing
find _ _ (File (RegularFile _ _)) = Nothing
find (lead : rest) fileType (Directory (Dir dirName children))
  | dirName == lead = case checkChildren of
      [] -> Nothing
      (x : _) -> Just x
  | otherwise = Nothing
  where
    checkChildren = mapMaybe (find rest fileType) children

findFileByPath :: Path -> FileTree -> Maybe RegularFile
findFileByPath path tree = case find path F tree of
  Just (File f) -> Just f
  _ -> Nothing

findDirByPath :: Path -> FileTree -> Maybe Dir
findDirByPath path tree = case find path D tree of
  Just (Directory d) -> Just d
  _ -> Nothing

-- Prints the contents of the given directory
printDirectory :: Dir -> String
printDirectory (Dir _ children) = concat [go x | x <- children]
  where
    go (File (RegularFile name _)) = name ++ " "
    go (Directory (Dir name _)) = name ++ " "

------------------pwd------------------

-- >>> pwd ["/","Users","Pesho","Programs(x86)"]
-- "/Users/Pesho/Programs(x86)"
pwd :: Path -> String
pwd [] = ""
pwd (lead : rest)
  | lead == "/" = lead ++ pwd rest
  | otherwise =
      if null rest
        then lead
        else lead ++ "/" ++ pwd rest

------------------cd-------------------

-- cd foo/bar -> relative
-- cd /foo/bar -> full path
-- cd -> current working dir (cd with no arguments)
newtype CdArgs = CdArgs {cdPath :: Path}
  deriving (Eq, Show)

-- >>> break (== ">") ["/file1", "/dir1/file1","/dir1/dir3/file4", ">", "output.txt"]
-- (["/file1","/dir1/file1","/dir1/dir3/file4"],[">","output.txt"])
needsToBeCleaned :: Path -> Bool
needsToBeCleaned = foldr (\x -> (||) (x == "..")) False

formatCdInput :: Path -> Path
formatCdInput [] = []
formatCdInput [only] = [only | only /= ".."]
formatCdInput input = go input input
  where
    go [] _ = []
    go _ [] = []
    go _ [only] = [only]
    go original whole@(x : y : xys)
      | y == ".." && x /= ".." = go original xys
      | x == ".." && length original == length whole = y : xys
      | otherwise = x : go original (y : xys)

relativeToFullPath :: Path -> Path -> Path
relativeToFullPath workingDir relative = workingDir ++ relative

-- >>> parseCdArgs ["/"] "../../.."
-- Nothing
parseCdArgs :: Path -> Arg -> Maybe CdArgs
parseCdArgs [] _ = Nothing
parseCdArgs workingDir [] = Just $ CdArgs workingDir
parseCdArgs workingDir arg@(x : _)
  | x == '/' = Just $ CdArgs $ strToPath arg
  | needsToBeCleaned (strToPath arg) = go (relativeToFullPath workingDir (strToPath arg))
  | otherwise = Just $ CdArgs (workingDir ++ strToPath arg)
  where
    go fullPath
      | null fullPath = Nothing
      | all (== "..") fullPath = Just $ CdArgs workingDir
      | needsToBeCleaned fullPath = go (formatCdInput fullPath)
      | otherwise = Just $ CdArgs fullPath

cd :: CdArgs -> FileTree -> Maybe Dir
cd (CdArgs path) = findDirByPath path

------------------ls-------------------

newtype LsArgs = LsArgs {lsPath :: Path}
  deriving (Eq, Show)

parseLsArgs :: Path -> Arg -> Maybe LsArgs
parseLsArgs path arg = case parseCdArgs path arg of
  Just p -> Just $ LsArgs $ cdPath p
  _ -> Nothing

ls :: LsArgs -> FileTree -> Maybe String
ls (LsArgs path) tree = case findDirByPath path tree of
  Just foundDir -> Just $ printDirectory foundDir
  _ -> Nothing

-----------------cat-------------------

type Output = String

data PartialCatArgs
  = FilesAndOutput (NonEmpty Path) Path
  | Output Path
  | Files (NonEmpty Path)
  deriving (Eq, Show)

data CatArgs
  = Both (NonEmpty Path) Path
  | OutputArgs Path String
  | FileArgs (NonEmpty Path)
  deriving (Eq, Show)

-- >>> parseCatArgs ["/","dir1"] ["/file1", "file1/","/dir2/file5",">", "dir5/../../../file6"]
-- Just (FilesAndOutput (["/","file1"] :| [["/","dir1","file1"],["/","dir2","file5"]]) ["file6"])
parseCatArgs :: Path -> [String] -> Maybe PartialCatArgs
parseCatArgs path args = case break (== ">") args of
  ([], _ : y : _) -> case parseCdArgs path y of -- e.g cat > output.txt (no input files)
    Just (CdArgs p) -> Just $ Output p
    _ -> Nothing
  (x : xs, []) -> case mapMaybe (parseCdArgs path) (x : xs) of -- e.g cat file1 file2 ... fileN  (no output file)
    (c : cs) -> Just $ Files $ NonEmpty.map cdPath (c :| cs)
    _ -> Nothing
  (z : zs, _ : y : _) -> case mapMaybe (parseCdArgs path) (z : zs) of -- e.g cat file1 file2 ... fileN > output.txt (has both)
    (c : cs) -> case parseCdArgs path y of
      Just (CdArgs p) -> Just $ FilesAndOutput (NonEmpty.map cdPath (c :| cs)) p
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing -- e.g cat (no arguments)

finaliseCatArgs :: String -> PartialCatArgs -> Maybe CatArgs
finaliseCatArgs _ (FilesAndOutput _ []) = Nothing
finaliseCatArgs _ (Output []) = Nothing
finaliseCatArgs _ (FilesAndOutput files output) = Just $ Both files output
finaliseCatArgs newContent (Output output) = Just $ OutputArgs output newContent
finaliseCatArgs _ (Files files) = Just $ FileArgs files

-- >>> catFilesToString smallTree (fromList [["/","file1"],["/","file2"]])
catFilesToString :: FileTree -> NonEmpty Path -> Maybe String
catFilesToString tree (lead :| rest) =
  Just $ foldl (\acc x -> case findFileByPath x tree of Just f -> acc ++ "\n" ++ getFileContent f; _ -> "") (maybe "" getFileContent (findFileByPath lead tree)) rest

catOutputRewrite :: Path -> String -> FileTree -> FileTree
catOutputRewrite [] _ f@(File _) = f
catOutputRewrite [] _ dir@(Directory _) = dir
catOutputRewrite [only] newContent f@(File (RegularFile fName _)) = if only == fName then File (RegularFile fName newContent) else f
catOutputRewrite [_] _ dir@(Directory _) = dir
catOutputRewrite _ _ f@(File (RegularFile _ _)) = f
catOutputRewrite (_ : rest) newContent (Directory (Dir dirName children)) = Directory (Dir dirName (map (catOutputRewrite rest newContent) children))

-- >>> cat (Files (fromList [["/","dir1","file1"], ["/","dir2","file5"]])) smallTree
-- Just "\nfile1\nfile5"
cat :: CatArgs -> FileTree -> Maybe String
cat (FileArgs files) tree = if any (\f -> isNothing $ findFileByPath f tree) files then Nothing else catFilesToString tree files
cat (OutputArgs path newContent) tree = if isNothing $ findFileByPath path tree then Nothing else Just newContent
cat (Both files _) tree = if any (\f -> isNothing $ findFileByPath f tree) files then Nothing else catFilesToString tree files

------------------rm------------------

newtype RmArgs = RmArgs (NonEmpty Path)
  deriving (Eq, Show)

-- >>> cdParseTweaker ["/","dir1"] ""
-- Nothing
cdParseTweaker :: Path -> Arg -> Maybe CdArgs
cdParseTweaker [] _ = Nothing
cdParseTweaker _ [] = Nothing
cdParseTweaker path arg = parseCdArgs path arg

-- >>> parseRmArgs ["/","dir1"] ["/full/path","relative/path/to/file","relative/path/but/../with/../../dots","","","/new/full/path"]
-- Just (RmArgs (["/","full","path"] :| [["/","dir1","relative","path","to","file"],["/","dir1","relative","dots"],["/","dir1"]]))
parseRmArgs :: Path -> [String] -> Maybe RmArgs
parseRmArgs [] _ = Nothing
parseRmArgs _ [] = Nothing
parseRmArgs path args = case mapMaybe (cdParseTweaker path) args of
  (c : cs) -> Just $ RmArgs (NonEmpty.map cdPath (c :| cs))
  _ -> Nothing

remove :: Path -> FileTree -> Maybe FileTree
remove [] f@(File _) = Just f
remove [] dir@(Directory _) = Just dir
remove [p] f@(File (RegularFile nm _)) = if nm == p then Nothing else Just f
remove [p] dir@(Directory (Dir nm _)) = if nm == p then Nothing else Just dir
remove _ file@(File _) = Just file
remove (_ : rest) (Directory (Dir nm children)) = Just $ Directory (Dir nm (mapMaybe (remove rest) children))

rm :: RmArgs -> FileTree -> Maybe FileTree
rm (RmArgs args) tree =
  if any (\f -> isNothing (findFileByPath f tree) && isNothing (findDirByPath f tree)) args
    then Nothing
    else foldl (\acc x -> case acc of Just currTree -> remove x currTree; _ -> acc) (Just tree) args

----------------------main----------------------

data Command = Pwd | Ls LsArgs | Cd CdArgs | Cat PartialCatArgs | Rm RmArgs | Exit
  deriving (Eq, Show)

readInput :: IO String
readInput = do
  line <- getLine
  if line == "."
    then return ""
    else do
      rest <- readInput
      return (if null rest then line else line ++ "\n" ++ rest)

parseCommand :: Path -> String -> Maybe Command
parseCommand _ [] = Nothing
parseCommand [] _ = Nothing
parseCommand path cmd = case Split.splitOn " " cmd of
  [] -> Nothing
  ["exit"] -> Just Exit
  ["pwd"] -> Just Pwd
  ["ls"] -> case parseLsArgs path [] of
    Nothing -> Nothing
    Just lsArgs -> Just $ Ls lsArgs
  ("ls" : location : _) -> case parseLsArgs path location of
    Nothing -> Nothing
    Just lsArgs -> Just $ Ls lsArgs
  ["cd"] -> case parseCdArgs path [] of
    Nothing -> Nothing
    Just cdArgs -> Just $ Cd cdArgs
  ("cd" : location : _) -> case parseCdArgs path location of
    Nothing -> Nothing
    Just cdArgs -> Just $ Cd cdArgs
  ("cat" : xs) -> case parseCatArgs path xs of
    Nothing -> Nothing
    Just partialCatArgs -> Just $ Cat partialCatArgs
  ("rm" : xs) -> case parseRmArgs path xs of
    Nothing -> Nothing
    Just rmArgs -> Just $ Rm rmArgs
  _ -> Nothing

data FileSystemState = MkFileSystemState
  { activeDir :: Path,
    fs :: FileTree
  }

root :: Path
root = ["/"]

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

interaction :: FileSystemState -> IO ()
interaction fss = do
  input <- prompt "$ "
  case parseCommand (activeDir fss) input of
    Nothing -> do putStrLn "Invalid Command"; interaction fss
    Just cmd -> case cmd of
      Pwd -> do putStrLn (pwd (activeDir fss)); interaction fss
      Ls lsArgs -> case ls lsArgs (fs fss) of
        Nothing -> do putStrLn ("ls:" ++ pwd (lsPath lsArgs) ++ " directory not found"); interaction fss
        Just s -> do putStrLn s; interaction fss
      Cd cdArgs -> case cd cdArgs (fs fss) of
        Nothing -> do putStrLn ("cd: directory " ++ pwd (cdPath cdArgs) ++ " not found"); interaction fss
        Just _ -> interaction (MkFileSystemState (cdPath cdArgs) (fs fss))
      Cat partialCatArgs -> case partialCatArgs of
        FilesAndOutput files output -> case cat (Both files output) (fs fss) of
          Nothing -> do putStrLn "cat: some of the files or the output file do not exist"; interaction fss
          Just s -> interaction (MkFileSystemState (activeDir fss) (catOutputRewrite output s (fs fss)))
        Output output -> do userInput <- readInput; interaction (MkFileSystemState (activeDir fss) (catOutputRewrite output userInput (fs fss)))
        Files _ -> case finaliseCatArgs "" partialCatArgs of
          Nothing -> do putStrLn "cat : there was an error reading some of the files"; interaction fss
          Just catArgs -> case cat catArgs (fs fss) of
            Nothing -> do putStrLn "cat : couldn't find some of the files"; interaction fss
            Just s -> do putStrLn s; interaction fss
      Rm rmArgs -> case rm rmArgs (fs fss) of
        Nothing -> do putStrLn "rm: some of the files couldn't be found"; interaction fss
        Just ft -> interaction (MkFileSystemState (activeDir fss) ft)
      Exit -> putStrLn "Exiting..."

main :: IO ()
main = interaction (MkFileSystemState root bigTree)