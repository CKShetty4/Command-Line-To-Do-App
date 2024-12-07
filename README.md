# Command Line To-Do App

A simple and efficient command-line to-do list application built in Haskell. This app is designed to help users manage their tasks directly from the terminal with a clean and colorful interface for better readability.

## Features
- **Add Tasks**: Quickly add tasks to your to-do list.
- **Delete Tasks**: Remove completed or unwanted tasks.
- **View Tasks**: Display all tasks in your list.
- **Sort Tasks**:
  - Alphabetically.
  - By order of time added.
- **Mark Tasks as Completed**: Track progress by marking tasks as completed.
- **Save and Load Tasks**: Save tasks to a file and load them back to retrieve your to-do list even after exiting the app.
- **Color-coded CLI Output**:
  - **Green**: Success messages.
  - **Blue**: Menu options.
  - **Red**: Errors or warnings.
  - **Yellow**: Exit message.
- **Continuous Loop**: The app remains in a loop until exited, allowing you to manage tasks as much as needed.

## Installation and Usage

### Prerequisites
- **GHC (Glasgow Haskell Compiler)** installed on your system.
  - To install GHC on Arch Linux: 
    ```bash
    sudo pacman -S ghc
    ```

### Running the Application
1. Clone the repository:
   ```bash
   git clone https://github.com/CKShetty4/Command-Line-To-Do-App.git
   cd Command-Line-To-Do-App
   ```

2. Compile the program:
   ```bash
   ghc Main.hs -o todoApp
   ```

3. Run the compiled executable:
   ```bash
   ./todoApp
   ```

### File Structure
```
Command-Line-To-Do-App
├── Main.hs       # Main source code for the application.
├── LICENSE       # License for the project.
├── README.md     # Documentation for the application.
```

## How It Works
1. After running the application, you’ll be greeted with a menu displaying options in **blue**.
2. Select options by typing the corresponding number (e.g., `2` to add a task).
3. Follow the prompts to manage your tasks:
   - Success messages will appear in **green**.
   - Errors or invalid inputs will appear in **red**.
4. Exit the application by selecting `6`, and you'll see a farewell message in **yellow**.


## About
- **Built on**: Arch Linux
- **Language**: Haskell
- **GitHub Repository**: [Command-Line-To-Do-App](https://github.com/CKShetty4/Command-Line-To-Do-App)

---

Feel free to fork this repository, open issues, or contribute to make it even better. 🚀
