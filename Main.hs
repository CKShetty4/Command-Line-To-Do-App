import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)

type Task = String
type TodoList = [Task]

-- File to save tasks
tasksFile :: FilePath
tasksFile = "tasks.txt"

-- Function to add a task
addTask :: TodoList -> Task -> TodoList
addTask tasks newTask = tasks ++ [newTask]

-- Function to delete a task with error handling
deleteTask :: TodoList -> Int -> TodoList
deleteTask tasks index
    | index < 0 || index >= length tasks = tasks -- Return unchanged list for invalid index
    | otherwise = take index tasks ++ drop (index + 1) tasks

-- Function to view tasks
viewTasks :: TodoList -> IO ()
viewTasks [] = putStrLn "No tasks to show. Your todo list is empty!"
viewTasks tasks = mapM_ putStrLn (zipWith (\i task -> show i ++ ". " ++ task) [0..] tasks)

-- Function to load tasks from a file
loadTasks :: FilePath -> IO TodoList
loadTasks filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            contents <- readFile filePath
            return (lines contents) -- Each line represents a task
        else return []

-- Function to save tasks to a file
saveTasks :: FilePath -> TodoList -> IO ()
saveTasks filePath tasks = writeFile filePath (unlines tasks)

-- Main loop
main :: IO ()
main = do
    putStrLn "Welcome to the Todo App!"
    tasks <- loadTasks tasksFile -- Load tasks from file
    putStrLn "Tasks loaded successfully!"
    appLoop tasks

-- Recursive loop for the app
appLoop :: TodoList -> IO ()
appLoop tasks = do
    putStrLn "\nChoose an option:"
    putStrLn "1. View Tasks"
    putStrLn "2. Add Task"
    putStrLn "3. Delete Task"
    putStrLn "4. Exit"
    putStr "Your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            viewTasks tasks
            appLoop tasks
        "2" -> do
            putStr "Enter new task: "
            hFlush stdout
            newTask <- getLine
            let updatedTasks = addTask tasks newTask
            saveTasks tasksFile updatedTasks -- Save updated tasks to file
            putStrLn $ "Task added: " ++ newTask
            appLoop updatedTasks
        "3" -> do
            if null tasks
                then putStrLn "Your todo list is empty. Nothing to delete!"
                else do
                    putStr "Enter task number to delete: "
                    hFlush stdout
                    numStr <- getLine
                    let num = read numStr :: Int
                    if num < 0 || num >= length tasks
                        then putStrLn "Invalid task number."
                        else do
                            putStrLn ("Deleted task: " ++ tasks !! num)
                            let updatedTasks = deleteTask tasks num
                            saveTasks tasksFile updatedTasks -- Save updated tasks to file
                            appLoop updatedTasks
            return () -- Return explicitly to avoid re-entering the loop
        "4" -> do
            putStrLn "Goodbye!"
            return () -- Exit the appLoop explicitly
        _   -> do
            putStrLn "Invalid choice, try again."
            appLoop tasks
