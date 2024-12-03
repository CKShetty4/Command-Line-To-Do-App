import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Data.List (sortBy)  -- For sorting tasks
import Data.Ord (comparing) -- For comparison function
import Data.Char (toLower) -- For case-insensitive sorting

type Task = (String, Bool) 
type TodoList = [Task]

-- File to save tasks
tasksFile :: FilePath
tasksFile = "tasks.txt"

-- ANSI escape codes for colors
setColor :: String -> String -> String
setColor color text = "\ESC[" ++ color ++ "m" ++ text ++ "\ESC[0m"

red, green, yellow, blue, reset :: String
red = "31"
green = "32" 
yellow = "33"
blue = "34"
reset = "0"   -- Reset to default

-- Function to add a task
addTask :: TodoList -> String -> TodoList
addTask tasks newTask = tasks ++ [(newTask, False)]

-- Function to delete a task with error handling
deleteTask :: TodoList -> Int -> TodoList
deleteTask tasks index
    | index < 0 || index >= length tasks = tasks -- Return unchanged list for invalid index
    | otherwise = take index tasks ++ drop (index + 1) tasks

-- Function to mark a task as completed
markTaskCompleted :: TodoList -> Int -> TodoList
markTaskCompleted tasks index
    | index < 0 || index >= length tasks = tasks -- Return unchanged list for invalid index
    | otherwise = 
        let (desc, _) = tasks !! index
        in take index tasks ++ [(desc, True)] ++ drop (index + 1) tasks

-- Function to view tasks
viewTasks :: TodoList -> IO ()
viewTasks [] = putStrLn (setColor yellow "No tasks to show. Your todo list is empty!")
viewTasks tasks = mapM_ putStrLn (zipWith formatTask [0..] tasks)
  where
    formatTask i (desc, completed) =
        setColor blue (show i ++ ". ") ++
        (if completed then setColor green "[x] " else setColor red "[ ] ") ++ desc

-- Function to load tasks from a file
loadTasks :: FilePath -> IO TodoList
loadTasks filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            contents <- readFile filePath
            return (map read (lines contents)) -- Read tasks as (String, Bool)
        else return []

-- Function to save tasks to a file
saveTasks :: FilePath -> TodoList -> IO ()
saveTasks filePath tasks = writeFile filePath (unlines (map show tasks))

-- Function to sort tasks alphabetically (ignoring case)
sortTasksAlphabetically :: TodoList -> TodoList
sortTasksAlphabetically = sortBy (comparing (map toLower . fst)) -- Convert both strings to lowercase for case-insensitive comparison

-- Finds the original index of a task in the timeline list
findOriginalIndex :: TodoList -> TodoList -> Int -> Int
findOriginalIndex sortedTasks timelineTasks sortedIndex =
    let (sortedDesc, _) = sortedTasks !! sortedIndex
    in head [i | (i, (desc, _)) <- zip [0..] timelineTasks, desc == sortedDesc]

-- Main loop
main :: IO ()
main = do
    putStrLn $ setColor yellow "Welcome to the Todo App!"
    tasks <- loadTasks tasksFile -- Load tasks from file
    putStrLn $ setColor green "Tasks loaded successfully!"
    appLoop tasks tasks False

-- Recursive loop for the app
appLoop :: TodoList -> TodoList -> Bool -> IO ()
appLoop tasks timelineTasks isSorted = do
    putStrLn "\nChoose an option:"
    putStrLn $ setColor blue "1. View Tasks"
    putStrLn $ setColor blue "2. Add Task"
    putStrLn $ setColor blue "3. Delete Task"
    putStrLn $ setColor blue "4. Mark Task as Completed"
    putStrLn $ setColor blue "5. Sort Tasks Alphabetically"
    putStrLn $ setColor blue "6. Exit"
    putStr $ setColor yellow "Your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            viewTasks tasks
            appLoop tasks timelineTasks isSorted
        "2" -> do
            putStr $ setColor yellow "Enter new task: "
            hFlush stdout
            newTask <- getLine
            let updatedTasks = addTask tasks newTask
            let updatedTimelineTasks = addTask timelineTasks newTask
            saveTasks tasksFile updatedTimelineTasks
            putStrLn $ setColor green ("Task added: " ++ newTask)
            appLoop updatedTasks updatedTimelineTasks isSorted
        "3" -> do
            if null tasks
                then do
                    putStrLn (setColor red "Your todo list is empty. Nothing to delete!")
                    appLoop tasks timelineTasks isSorted
                else do
                    putStr $ setColor yellow "Enter task number to delete: "
                    hFlush stdout
                    numStr <- getLine
                    let num = read numStr :: Int
                    if num < 0 || num >= length tasks
                        then do
                            putStrLn (setColor red "Invalid task number.")
                            appLoop tasks timelineTasks isSorted
                        else do
                            let originalIndex = if isSorted then findOriginalIndex tasks timelineTasks num else num
                            putStrLn (setColor red ("Deleted task: " ++ fst (timelineTasks !! originalIndex)))
                            let updatedTimelineTasks = deleteTask timelineTasks originalIndex
                            saveTasks tasksFile updatedTimelineTasks
                            appLoop updatedTimelineTasks updatedTimelineTasks False
        "4" -> do
            if null tasks
                then do
                    putStrLn (setColor red "Your todo list is empty. Nothing to mark as completed!")
                    appLoop tasks timelineTasks isSorted
                else do
                    putStr $ setColor yellow "Enter task number to mark as completed: "
                    hFlush stdout
                    numStr <- getLine
                    let num = read numStr :: Int
                    if num < 0 || num >= length tasks
                        then do
                            putStrLn (setColor red "Invalid task number.")
                            appLoop tasks timelineTasks isSorted
                        else do
                            let originalIndex = if isSorted then findOriginalIndex tasks timelineTasks num else num
                            let updatedTimelineTasks = markTaskCompleted timelineTasks originalIndex
                            saveTasks tasksFile updatedTimelineTasks
                            putStrLn $ setColor green ("Task marked as completed: " ++ fst (timelineTasks !! originalIndex))
                            appLoop updatedTimelineTasks updatedTimelineTasks False
        "5" -> do
            let sortedTasks = sortTasksAlphabetically tasks
            putStrLn $ setColor green "Tasks sorted alphabetically!"
            appLoop sortedTasks timelineTasks True
        "6" -> do
            saveTasks tasksFile timelineTasks
            putStrLn (setColor yellow "Goodbye!")
            return ()
        _   -> do
            putStrLn (setColor red "Invalid choice, try again.")
            appLoop tasks timelineTasks isSorted
