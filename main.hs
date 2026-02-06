import System.Environment (getArgs)

-- タスクを保存するファイル
todoFile :: FilePath
todoFile = "store.txt"

formatTasks :: [String] -> [String]
formatTasks = zipWith (\n t -> show n ++ ": " ++ t) [1 :: Int ..]

removeTaskAt :: Int -> [String] -> Maybe ([String], String)
removeTaskAt n tasks
  | n < 1 || n > length tasks = Nothing
  | otherwise = Just (take (n - 1) tasks ++ drop n tasks, tasks !! (n - 1))

parseTaskNumber :: String -> Maybe Int
parseTaskNumber s = case reads s of
  [(n, "")] | n > 0 -> Just n
  _ -> Nothing

usage :: IO ()
usage =
  mapM_
    putStrLn
    [ "Usage:",
      "  todo add \"<task>\"",
      "  todo list",
      "  todo done <id>"
    ]

-- タスクを追加
addTask :: String -> IO ()
addTask task =
  appendFile todoFile (task ++ "\n")
    >> putStrLn "Added a new task!"

-- タスク一覧表示
listTasks :: IO ()
listTasks =
  readFile todoFile
    >>= mapM_ putStrLn . formatTasks . lines

-- タスク完了 (削除)
doneTask :: Int -> IO ()
doneTask n = do
  tasks <- lines <$> readFile todoFile
  case removeTaskAt n tasks of
    Nothing -> putStrLn $ "Error: Invalid task number " ++ show n
    Just (newTasks, removed) ->
      writeFile todoFile (unlines newTasks)
        >> putStrLn ("Done: " ++ removed)

main :: IO ()
main = getArgs >>= dispatch
  where
    dispatch ["add", task] = addTask task
    dispatch ["list"] = listTasks
    dispatch ["done", num] = maybe invalidNum doneTask (parseTaskNumber num)
    dispatch _ = usage
    invalidNum = putStrLn "Error: Please provide a valid task number"
