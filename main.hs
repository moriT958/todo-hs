import Data.List (delete)
import System.Environment (getArgs)
import System.IO

-- タスクを保存するファイル
todoFile :: FilePath
todoFile = "store.txt"

-- MAIN 関数
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["add", task] -> addTask task
    ["list"] -> listTasks
    ["done", num] -> doneTask (read num)
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  todo add \"<task>\""
  putStrLn "  todo list"
  putStrLn "  todo done <id>"

-- タスクを追加
addTask :: String -> IO ()
addTask task = do
  appendFile todoFile (task ++ "\n")
  putStrLn "Added a new task!"

-- タスク一覧表示
listTasks :: IO ()
listTasks = do
  contents <- readFile todoFile
  let tasks = lines contents
  mapM_ putStrLn $
    zipWith (\i t -> show i ++ ": " ++ t) [1 :: Int ..] tasks

-- タスク完了 (削除)
doneTask :: Int -> IO ()
doneTask n = do
  contents <- readFile todoFile
  let tasks = lines contents
      task = tasks !! (n - 1)
      newTasks = delete task tasks
  length contents `seq` writeFile todoFile (unlines newTasks)
  putStrLn ("Done: " ++ task)
