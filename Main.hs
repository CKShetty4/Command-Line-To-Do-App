import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)

type Task = (String, Bool) -- Updated: Task with description and completion status
type TodoList = [Task]

-- File to save tasks
tasksFile :: FilePath
tasksFile = "tasks.txt"

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
viewTasks [] = putStrLn "No tasks to show. Your todo list is empty!"
viewTasks tasks = mapM_ putStrLn (zipWith formatTask [0..] tasks)
  where
    formatTask i (desc, completed) =
        show i ++ ". " ++ (if completed then "[x] " else "[ ] ") ++ desc

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
    putStrLn "4. Mark Task as Completed"
    putStrLn "5. Exit"
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
                            putStrLn ("Deleted task: " ++ fst (tasks !! num))
                            let updatedTasks = deleteTask tasks num
                            saveTasks tasksFile updatedTasks -- Save updated tasks to file
                            appLoop updatedTasks
            return () -- Return explicitly to avoid re-entering the loop
        "4" -> do
            if null tasks
                then putStrLn "Your todo list is empty. Nothing to mark as completed!"
                else do
                    putStr "Enter task number to mark as completed: "
                    hFlush stdout
                    numStr <- getLine
                    let num = read numStr :: Int
                    if num < 0 || num >= length tasks
                        then putStrLn "Invalid task number."
                        else do
                            let updatedTasks = markTaskCompleted tasks num
                            saveTasks tasksFile updatedTasks -- Save updated tasks to file
                            putStrLn $ "Task marked as completed: " ++ fst (tasks !! num)
                            appLoop updatedTasks
            return ()
        "5" -> do
            putStrLn "Goodbye!"
            return () -- Exit the appLoop explicitly
        _   -> do
            putStrLn "Invalid choice, try again."
            appLoop tasks
