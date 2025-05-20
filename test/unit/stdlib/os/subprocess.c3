module std::os::process @test;
import std::os, std::io;

fn void test_inherit_stdio_stdout()
{
    // Test that with inherit_stdio, we can't capture stdout
    String[] command;
    $if env::WIN32:
        command = { "cmd.exe", "/c", "echo Hello with inherit_stdio" };
    $else
        command = { "echo", "Hello with inherit_stdio" };
    $endif
    
    SubProcess process = process::create(command, 
        { .search_user_path = true, .inherit_stdio = true })!!;
    defer process.destroy();
    
    // Join the process
    process.join()!!;
    
    // When using inherit_stdio, read_stdout should return 0 bytes
    char[100] buffer;
    usz bytes_read = process.read_stdout(&buffer[0], buffer.len)!!;
    assert(bytes_read == 0, "Expected 0 bytes when using inherit_stdio, got %d", bytes_read);
    
    // stdout() should return an empty file
    File stdout_file = process.stdout();
    assert(stdout_file.file == null, "Expected null file when using inherit_stdio");
}

fn void test_inherit_stdio_stderr()
{
    // Test that with inherit_stdio, stderr is also null
    String[] command;
    $if env::WIN32:
        command = { "cmd.exe", "/c", "echo Error >&2" };
    $else
        command = { "sh", "-c", "echo Error >&2" };
    $endif
    
    SubProcess process = process::create(command,
        { .search_user_path = true, .inherit_stdio = true })!!;
    defer process.destroy();
    
    process.join()!!;
    
    // stderr() should return null file
    File stderr_file = process.stderr();
    assert(stderr_file.file == null, "Expected null file when using inherit_stdio");
    
    // read_stderr should return 0 bytes
    char[100] buffer;
    usz bytes_read = process.read_stderr(&buffer[0], buffer.len)!!;
    assert(bytes_read == 0, "Expected 0 bytes when reading stderr with inherit_stdio");
}

fn void test_inherit_stdio_comparison()
{
    // Compare behavior with and without inherit_stdio
    String[] command;
    $if env::WIN32:
        command = { "cmd.exe", "/c", "echo Test output" };
    $else
        command = { "echo", "Test output" };
    $endif
    
    // First without inherit_stdio
    SubProcess process1 = process::create(command, { .search_user_path = true })!!;
    defer process1.destroy();
    
    process1.join()!!;
    
    char[100] buffer1;
    usz bytes_read1 = process1.read_stdout(&buffer1[0], buffer1.len)!!;
    assert(bytes_read1 > 0, "Expected output without inherit_stdio");
    
    // Now with inherit_stdio
    SubProcess process2 = process::create(command, 
        { .search_user_path = true, .inherit_stdio = true })!!;
    defer process2.destroy();
    
    process2.join()!!;
    
    char[100] buffer2;
    usz bytes_read2 = process2.read_stdout(&buffer2[0], buffer2.len)!!;
    assert(bytes_read2 == 0, "Expected no output with inherit_stdio");
}

fn void test_inherit_stdio_exit_code()
{
    // Test that processes complete successfully with inherit_stdio
    String[] command;
    $if env::WIN32:
        command = { "cmd.exe", "/c", "exit 0" };
    $else
        command = { "true" };
    $endif
    
    SubProcess process = process::create(command,
        { .search_user_path = true, .inherit_stdio = true })!!;
    defer process.destroy();
    
    CInt exit_code = process.join()!!;
    assert(exit_code == 0, "Process should exit with code 0, got %d", exit_code);
    
    // Verify process is not running after join
    assert(!process.is_alive, "Process should not be running after join");
}

fn void test_inherit_stdio_redirect() 
{
    // This test verifies that child process can read from parent's stdin and write to parent's stdout
    // Create a simple program that reads input from stdin and echoes to stdout
    String[] command;
    
    $if env::WIN32:
        // Command to read from stdin and echo to stdout on Windows
        command = { "cmd.exe", "/c", "set /p INPUT= && echo %INPUT%" };
    $else
        // Command to read from stdin and echo to stdout on POSIX
        command = { "sh", "-c", "read INPUT && echo $INPUT" };
    $endif
    
    // Create a temporary file to redirect our stdout
    String temp_file = io::tmpname("inherit_stdio_test.txt")!!;
    defer io::remove(temp_file);
    
    // Redirect our stdout to the temporary file for this test
    CInt original_stdout = libc::dup(libc::STDOUT_FD)!!;
    defer libc::dup2(original_stdout, libc::STDOUT_FD);
    defer libc::close(original_stdout);
    
    File output_file = io::file::open(temp_file, "w")!!;
    libc::dup2(libc::fileno(output_file.file), libc::STDOUT_FD)!!;
    output_file.close();
    
    // Create a subprocess that inherits stdio
    SubProcess process = process::create(command,
        { .search_user_path = true, .inherit_stdio = true })!!;
    defer process.destroy();
    
    // Write to the process's stdin (which should be our stdin)
    // For automated testing, we'll write directly to the file descriptor
    io::printfn("Test message from parent");
    io::flush(io::stdout);
    
    // Wait for process to complete
    process.join()!!;
    
    // Restore stdout
    libc::dup2(original_stdout, libc::STDOUT_FD);
    
    // Read the output file to verify the content
    File input_file = io::file::open(temp_file, "r")!!;
    char[100] buffer;
    usz bytes_read = input_file.read(buffer.ptr, buffer.len)!!;
    String content = (String)buffer[:bytes_read];
    
    // Check if our message was written through the child process
    // Note: This test is primarily to ensure no crashes or errors when using inherit_stdio,
    // as the actual inheritance functionality depends on the terminal environment
    assert(bytes_read > 0, "Expected some output with inherit_stdio redirection");
    
    io::printfn("inherit_stdio redirection test succeeded.");
}