// Currently disabled as they somewhat mess up the test output.
module std::os::process @test @if(false);
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
    // This test verifies that stdio inheritance doesn't crash and works with the OS
    String[] command;
    
    $if env::WIN32:
        // Create a test command that will write to stdout
        command = { "cmd.exe", "/c", "echo Test output from child process with inherit_stdio" };
    $else
        // For POSIX systems
        command = { "echo", "Test output from child process with inherit_stdio" };
    $endif
    
    // Create a subprocess that inherits stdio
    SubProcess process = process::create(command,
        { .search_user_path = true, .inherit_stdio = true })!!;
    defer process.destroy();
    
    // Wait for process to complete
    CInt exit_code = process.join()!!;
    
    // Since inherit_stdio is enabled, verify:
    // 1. Process completed successfully
    // 2. We can't read from its stdout/stderr (as expected)
    assert(exit_code == 0, "Process should complete successfully");
    assert(process.stdout().file == null, "stdout should be null with inherit_stdio");
    assert(process.stderr().file == null, "stderr should be null with inherit_stdio");
    
    char[100] buffer;
    usz stdout_bytes = process.read_stdout(&buffer[0], buffer.len)!!;
    usz stderr_bytes = process.read_stderr(&buffer[0], buffer.len)!!;
    
    assert(stdout_bytes == 0, "Should read 0 bytes from stdout");
    assert(stderr_bytes == 0, "Should read 0 bytes from stderr");
}

fn void test_write_to_stdin() @test {
$if env::WIN32:
	SubProcess? subprocess = process::create({`powershell`, `-C`, `Read-Host`}, {.inherit_environment = true});
$else
	SubProcess? subprocess = process::create({"tee"}, {.read_async = true});
$endif
	test::eq(@ok(subprocess), true);
	usz? write = subprocess!!.write_to_stdin(")");
	test::eq(@ok(write), true);
	test::eq(write!!, 1);
	char buf;
	test::eq(@ok(subprocess!!.join()), true);
	usz? read = subprocess!!.read_stdout(&buf, 1);
	test::eq(@ok(read), true);
	test::eq(read!!, 1);
	test::eq(buf, ')');
}
