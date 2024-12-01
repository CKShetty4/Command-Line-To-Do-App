import System.IO (hFlush, stdout)

type Task = String
type TodoList = [Task]

-- Function to add a task
addTask :: TodoList -> Task -> TodoList
addTask tasks newTask = tasks ++ [newTask]

-- Function to delete a task
deleteTask :: TodoList -> Int -> TodoList
deleteTask tasks index = take index tasks ++ drop (index + 1) tasks

-- Function to view tasks
viewTasks :: TodoList -> IO ()
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
            appLoop (addTask tasks newTask)
        "3" -> do
            putStr "Enter task number to delete: "
            hFlush stdout
            numStr <- getLine
            let num = read numStr :: Int
            appLoop (deleteTask tasks num)
        "4" -> putStrLn "Goodbye!"
        _   -> do
            putStrLn "Invalid choice, try again."
            appLoop tasks
