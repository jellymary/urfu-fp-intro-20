{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09 where

import System.Directory
import System.FilePath
import System.Random
import Data.List (sortBy)

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show)

newtype Title = Title String deriving (Eq, Show)

newtype Deadline = Deadline String deriving (Eq, Show)

newtype Content = Content String deriving (Eq, Show)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show)

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  doesExist <- doesDirectoryExist rootFolder
  if doesExist then removeDirectoryRecursive rootFolder else return ()
  createDirectory rootFolder
  return (TodoList rootFolder)

writeTodo :: TodoList -> Todo -> IO ()
writeTodo (TodoList folderPath) (Todo (Id id) (Title title) (Content text) (Deadline deadline) isDone) = do
  let path = folderPath </> id
  doesExist <- doesDirectoryExist folderPath
  if doesExist
    then writeFile path (unlines [id, title, deadline, if isDone then "true" else "false", text])
    else error "TodoList doesn't exist"
  return ()

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList (Title title) text (Deadline deadline) = do
  let id = title ++ deadline
  writeTodo todoList (Todo (Id id) (Title title) text (Deadline deadline) False)
  return (Id id)

readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList folderPath) (Id id) =
  do
    raw <- readFile $ folderPath </> id
    let [id, title, deadline, rawIsDone, text] = lines raw
    let isDone = parseIsDone rawIsDone
    return (Todo (Id id) (Title title) (Content text) (Deadline deadline) isDone)
  where
    parseIsDone :: String -> Bool
    parseIsDone rawIsDone = case rawIsDone of
      "true" -> True
      "false" -> False
      _ -> error "isDone cannot be deserialized"

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = do
  (Todo (Id id) (Title title) (Content text) (Deadline deadline) isDone) <- readTodo todoList id
  putStrLn title
  putStrLn $ "Deadline: " ++ deadline
  putStrLn $ if isDone then "Done :)" else "Not done :("
  putStrLn "~~~"
  putStrLn text
  putStrLn "\n"

removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList folderPath) (Id id) = do
  let filePath = folderPath </> id
  doesExist <- doesFileExist filePath
  if doesExist then removeFile filePath else return ()

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id (TodoEdit newTitle newText newDeadline) = do
  Todo {todoId=id, isDone=isDone} <- readTodo todoList id
  writeTodo todoList (Todo id newTitle newText newDeadline isDone)

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do
  Todo id title content deadline _ <- readTodo todoList id
  writeTodo todoList (Todo id title content deadline True)

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList folderPath) = do
  todoIds <- listDirectory folderPath
  todos <- mapM (\id -> readTodo todoList (Id id)) todoIds
  return $ sortBy (\(Todo{deadline=(Deadline first)}) (Todo{deadline=(Deadline second)}) -> compare first second) todos

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  todos <- readAllTodo todoList
  return $ filter (\(Todo{isDone=isDone}) -> not isDone) todos

showAllTodo :: TodoList -> IO ()
showAllTodo todoList@(TodoList folderPath) = do
  todos <- readAllTodo todoList
  _ <- mapM (\(Todo{todoId=id}) -> showTodo todoList id) todos
  return ()

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do
  todos <- readUnfinishedTodo todoList
  _ <- mapM (\(Todo{todoId=id}) -> showTodo todoList id) todos
  return ()

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37
  > Yep, that's the number!
-}

guessNumber :: Integer -> IO ()
guessNumber secretNumber = do
  putStrLn "Your number: "
  line <- getLine
  let alleged = read line :: Integer
  case compare alleged secretNumber of
    LT -> do
      putStrLn "Too small\n"
      guessNumber secretNumber
    GT -> do
      putStrLn "Too big\n"
      guessNumber secretNumber
    EQ -> putStrLn "Yep, that's the number!"

playGuessGame :: IO ()
playGuessGame = do
  number <- randomRIO (0, 100)
  guessNumber number

-- </Задачи для самостоятельного решения>