import std;

// Struct representing an asynchronous task
// - task_fn: A function pointer to the task that will be run asynchronously
// - data: A void pointer to the data that will be passed to the task function
// - thread: A Thread object to manage the execution of the task in a separate thread
struct AsyncTask {
    task_fn: fn(void*) void;
    data: void*;
    thread: Thread;
}

// Function to create a new asynchronous task
// @param task_fn: The function to be run asynchronously
// @param data: The data to be passed to the task function
// @return: An AsyncTask struct initialized with the provided function and data
fn async_task_create(task_fn: fn(void*) void, data: void*) AsyncTask {
    task: AsyncTask;
    task.task_fn = task_fn;
    task.data = data;
    return task;
}

// Function to run an asynchronous task
// @param task: A pointer to an AsyncTask struct that will be run asynchronously
// @return: An integer status code (0 for success, non-zero for error)
fn async_run(task: AsyncTask*) int {
    // Create a new thread to run the task function with the provided data
    return thread_create(&task.thread, task.task_fn, task.data);
}

// Function to wait for an asynchronous task to complete
// @param task: A pointer to an AsyncTask struct whose completion is being waited on
// @return: An integer status code (0 for success, non-zero for error)
fn async_wait(task: AsyncTask*) int {
    // Join the thread to wait for the task to finish
    return thread_join(&task.thread);
}

// Function to run and wait for multiple asynchronous tasks
// @param tasks: An array of AsyncTask structs representing the tasks to be run
// @param count: The number of tasks in the array
// @return: An integer status code (0 for success, non-zero for error)
fn async_all(tasks: AsyncTask[], count: usize) int {
    int result = 0;

    // Run each task in the array asynchronously
    for (i in 0..count) {
        result = async_run(&tasks[i]);
        if (result != 0) return result; // Return if there's an error
    }

    // Wait for each task in the array to complete
    for (i in 0..count) {
        result = async_wait(&tasks[i]);
        if (result != 0) return result; // Return if there's an error
    }

    return result; // Return 0 if all tasks completed successfully
}
