// Currently disabled as they somewhat mess up the test output.
module std::os::process @test @if(!env::EMSCRIPTEN);
import std::os, std::io, std::net::os, std::time::clock, libc;

fn void test_inherit_stdio_stdout()
{
    // Test that with inherit_stdio, we can't capture stdout
    String[] command;
    $if env::WIN32:
        command = { "cmd.exe", "/c", "echo Hello with inherit_stdio" };
    $else
        command = { "echo", "Hello with inherit_stdio" };
    $endif
    
    Process process = process::spawn(command, INHERIT_STDIO)!!;
    defer process.destroy();
    
    // Join the process
    process.join()!!;
    
    // When using inherit_stdio, read_stdout should return 0 bytes
    char[100] buffer;
    sz bytes_read = process.read_stdout(&buffer[0], buffer.len)!!;
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
    
    Process process = process::spawn(command, INHERIT_STDIO)!!;
    defer process.destroy();
    
    process.join()!!;
    
    // stderr() should return null file
    File stderr_file = process.stderr();
    assert(stderr_file.file == null, "Expected null file when using inherit_stdio");
    
    // read_stderr should return 0 bytes
    char[100] buffer;
    sz bytes_read = process.read_stderr(&buffer[0], buffer.len)!!;
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
    Process process1 = process::spawn(command)!!;
    defer process1.destroy();
    
    process1.join()!!;
    
    char[100] buffer1;
    sz bytes_read1 = process1.read_stdout(&buffer1[0], buffer1.len)!!;
    assert(bytes_read1 > 0, "Expected output without inherit_stdio");
    
    // Now with inherit_stdio
    Process process2 = process::spawn(command, INHERIT_STDIO)!!;
    defer process2.destroy();
    
    process2.join()!!;
    
    char[100] buffer2;
    sz bytes_read2 = process2.read_stdout(&buffer2[0], buffer2.len)!!;
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
    
    Process process = process::spawn(command, INHERIT_STDIO)!!;
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
    Process process = process::spawn(command, INHERIT_STDIO)!!;
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
    sz stdout_bytes = process.read_stdout(&buffer[0], buffer.len)!!;
    sz stderr_bytes = process.read_stderr(&buffer[0], buffer.len)!!;
    
    assert(stdout_bytes == 0, "Should read 0 bytes from stdout");
    assert(stderr_bytes == 0, "Should read 0 bytes from stderr");
}

fn void test_pipe_cloexec_leak() @test @if(env::POSIX)
{
	// Process A reads from stdin and exits when its stdin pipe closes
	Process a = process::spawn({"cat"})!!;

	// Process B is a long running process that shouldn't interfere with A
	Process b = process::spawn({"sleep", "2"})!!;

	// We close the parent's handle to A's stdin.
	// If FD_CLOEXEC was missing on A's pipe, B would have inherited a write-handle to A's stdin.
	// A would stay alive waiting for B to exit, even though the parent closed it.
	libc::fclose(a.stdin_file);
	a.stdin_file = null;

	Clock start = clock::now();
	a.join()!!; // Join A, waiting for it to exit

	long ms = start.to_now().to_ms();

	a.destroy();

	b.terminate()!!;
	b.join()!!;
	b.destroy();

	// If missing FD_CLOEXEC, A's join would take >= 2000 milliseconds
	// Since it's fixed, it should return near instantly (< 1000 ms)
	assert(ms < 1000, "Process A hung waiting for Process B to close its leaked stdin fd!");
}

fn void test_write_to_stdin() @test {
$if env::WIN32:
	Process? subprocess = process::spawn({`powershell`, `-C`, `Read-Host`}, INHERIT_ENV);
$else
	Process? subprocess = process::spawn({"tee"}, ASYNC_OUTPUT);
$endif
	test::eq(@ok(subprocess), true);
	sz? write = subprocess!!.write_stdin(")");
	test::eq(@ok(write), true);
	test::eq(write!!, 1);
	char buf;
	test::eq(@ok(subprocess!!.join()), true);
	sz? read = subprocess!!.read_stdout(&buf, 1);
	test::eq(@ok(read), true);
	test::eq(read!!, 1);
	test::eq(buf, ')');
}
