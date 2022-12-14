module FileSystem where

type Name = String

type Content = String

-- File tree representation
data FileTree
  = Empty
  | File Name Content
  | Directory Name [FileTree]
  deriving (Eq, Show)

type Path = [String]

simpleTree :: FileTree
simpleTree =
  Directory
    "/"
    [ File "file1" "This is file1",
      File "file2" "This is file2",
      File "file3" "",
      Directory
        "dir1"
        [ File "file1" "This is file3",
          Directory
            "dir3"
            [File "file4" "This is file4"],
          Directory "dir4" []
        ],
      Directory
        "dir2"
        [File "file5" "This is file5"]
    ]

bigTree :: FileTree
bigTree =
  Directory
    "/"
    [ Directory
        "ip"
        [ Directory
            "homeworks"
            [ File "hw1" "simple clock implementation.",
              File "hw2" "working with dates.",
              File "hw3" "sorting rows of a matrix."
            ],
          Directory
            "exercises"
            [File "exc1" "this is introducing to the syntax in C++"]
        ],
      Directory
        "oop"
        [ Directory
            "homeworks"
            [ File "hw1" "big four",
              File "hw2" "dynamic array"
            ],
          Directory
            "project"
            [ Directory
                "source"
                [ File "s" "this is source file",
                  File "et" "this is electonic table project"
                ]
            ]
        ],
      Directory
        "dsa"
        [ File "content" "here are the recourses from dsa",
          Directory
            "homeworks"
            [ File "hw1" "store with client lists",
              File "hw2" "tree hierarchy with workers",
              File "hw3" "hash table for file comparison"
            ],
          Directory
            "project"
            [ File "db" "this is database project",
              Directory
                "implementation"
                [File "impl" "this is the database"]
            ]
        ],
      Directory
        "fp"
        [ Directory
            "homeworks"
            [ File "hw1" "some scheme tasks",
              File "hw2" "and some haskell tasks"
            ],
          Directory
            "project"
            [File "file system" "this is the project"]
        ]
    ]

-- >>> strToPath "/"
-- ["/"]
strToPath :: String -> Path
strToPath str = go str "" []
  where
    go original word path
      | null original = if (not . null) word then path ++ [word] else path
      | head original == '/' && length original == length str = go (tail original) word ("/" : path)
      | head original == '/' = go (tail original) "" (path ++ [word])
      | otherwise = go (tail original) (word ++ [head original]) path

isDir :: FileTree -> Bool
isDir (Directory _ _) = True
isDir _ = False

isFile :: FileTree -> Bool
isFile (File _ _) = True
isFile _ = False

getFileName :: FileTree -> Name
getFileName (File fileName _) = fileName
getFileName _ = ""

-- >>> getFileContent (File)
getFileContent :: FileTree -> Content
getFileContent (File _ con) = con
getFileContent _ = ""

getDirName :: FileTree -> Name
getDirName (Directory dirName _) = dirName
getDirName _ = ""

getDirChildren :: FileTree -> [FileTree]
getDirChildren (Directory _ children) = children
getDirChildren _ = []

findFileByName :: Path -> FileTree -> FileTree
findFileByName [] _ = Empty
findFileByName _ Empty = Empty
findFileByName [only] f@(File fileName _) = if only == fileName then f else Empty
findFileByName path (File _ _) = Empty
findFileByName (lead : rest) dir@(Directory dirName children)
  | dirName == lead = if null checkChildren then Empty else head checkChildren
  | otherwise = Empty
  where
    checkChildren = [child | child <- map (findFileByName rest) children, child /= Empty]

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

cd :: String -> FileTree -> FileTree
cd [] _ = error "Not enough arguments for cd"
cd str tree = undefined
