import System.IO (hFlush, stdout)

type Task = String
type TodoList = [Task]

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

-- Main loop
main :: IO ()
main = do
    let tasks = [] -- Start with an empty list
    putStrLn "Welcome to the Todo App!"
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
            putStrLn $ "Task added: " ++ newTask
            appLoop (addTask tasks newTask)
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
                            appLoop (deleteTask tasks num)
            appLoop tasks
        "4" -> putStrLn "Goodbye!"
        _   -> do
            putStrLn "Invalid choice, try again."
            appLoop tasks
