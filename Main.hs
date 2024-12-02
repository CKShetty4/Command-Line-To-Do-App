import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)

type Task = (String, Bool) -- Updated: Task with description and completion status
type TodoList = [Task]

-- File to save tasks
tasksFile :: FilePath
tasksFile = "tasks.txt"

-- ANSI escape codes for colors
setColor :: String -> String -> String
setColor color text = "\ESC[" ++ color ++ "m" ++ text ++ "\ESC[0m"

red, green, yellow, blue, reset :: String
red = "31"    -- Red
green = "32"  -- Green
yellow = "33" -- Yellow
blue = "34"   -- Blue
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

-- Main loop
main :: IO ()
main = do
    putStrLn $ setColor yellow "Welcome to the Todo App!"
    tasks <- loadTasks tasksFile -- Load tasks from file
    putStrLn $ setColor green "Tasks loaded successfully!"
    appLoop tasks

-- Recursive loop for the app
appLoop :: TodoList -> IO ()
appLoop tasks = do
    putStrLn "\nChoose an option:"
    putStrLn $ setColor blue "1. View Tasks"
    putStrLn $ setColor blue "2. Add Task"
    putStrLn $ setColor blue "3. Delete Task"
    putStrLn $ setColor blue "4. Mark Task as Completed"
    putStrLn $ setColor blue "5. Exit"
    putStr $ setColor yellow "Your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            viewTasks tasks
            appLoop tasks
        "2" -> do
            putStr $ setColor yellow "Enter new task: "
            hFlush stdout
            newTask <- getLine
            let updatedTasks = addTask tasks newTask
            saveTasks tasksFile updatedTasks -- Save updated tasks to file
            putStrLn $ setColor green ("Task added: " ++ newTask)
            appLoop updatedTasks
        "3" -> do
            if null tasks
                then do
                    putStrLn (setColor red "Your todo list is empty. Nothing to delete!")
                    appLoop tasks
                else do
                    putStr $ setColor yellow "Enter task number to delete: "
                    hFlush stdout
                    numStr <- getLine
                    let num = read numStr :: Int
                    if num < 0 || num >= length tasks
                        then do
                            putStrLn (setColor red "Invalid task number.")
                            appLoop tasks
                        else do
                            putStrLn (setColor red ("Deleted task: " ++ fst (tasks !! num)))
                            let updatedTasks = deleteTask tasks num
                            saveTasks tasksFile updatedTasks -- Save updated tasks to file
                            appLoop updatedTasks
        "4" -> do
            if null tasks
                then do
                    putStrLn (setColor red "Your todo list is empty. Nothing to mark as completed!")
                    appLoop tasks
                else do
                    putStr $ setColor yellow "Enter task number to mark as completed: "
                    hFlush stdout
                    numStr <- getLine
                    let num = read numStr :: Int
                    if num < 0 || num >= length tasks
                        then do
                            putStrLn (setColor red "Invalid task number.")
                            appLoop tasks
                        else do
                            let updatedTasks = markTaskCompleted tasks num
                            saveTasks tasksFile updatedTasks -- Save updated tasks to file
                            putStrLn $ setColor green ("Task marked as completed: " ++ fst (tasks !! num))
                            appLoop updatedTasks
        "5" -> do
            putStrLn (setColor yellow "Goodbye!")
            return () -- Exit the appLoop explicitly
        _   -> do
            putStrLn (setColor red "Invalid choice, try again.")
            appLoop tasks