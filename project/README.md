# Файлова система
## Инструкции
- за да инсталирате нужните библиотеки за работа с проекта трябва да пуснете командата `stack setup` преди да започнете работа с програмата
- програмата се стартира със `stack run` откъдето директно влизате в интерактивен режим за приемане на команди
- поддържат се командите `pwd`, `cd`, `ls`, `cat`, `rm`, `touch`, `mkdir`
- командите работят, както е описано в примерите в темата за проекта.
## Структура
```Haskell
type Name = String
type Content = String

data RegularFile = RegularFile Name Content
  deriving (Eq, Show)

data Dir = Dir Name [FileTree]
  deriving (Eq, Show)

data FileTree
  = File RegularFile
  | Directory Dir
  deriving (Eq, Show)
```
## Данни
Във `FileSystem.hs` има две предварително подготвени файлови дървета с имена `smallTree` и `bigTree`. За да стартирате програмата със съответното дърво трябва във файл `FileSystem.hs`, където е входната точка на програмата `main` да промените началното извикване на `interaction` да приема `bigTree` или `smallTree`
