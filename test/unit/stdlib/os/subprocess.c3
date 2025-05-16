module std::os::process @test;
import std::os, std::io;

fn void test_inherit_stdio()
{
    // Test 1: With inherit_stdio - we can't capture output
    {
        // Create a process with inherit_stdio
        String[] command;
        command = { "cmd.exe", "/c", "echo Hello with inherit_stdio" };
        
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
    
    // Test 2: Verify stderr is also null with inherit_stdio
    {
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
    
    // Test 3: Compare behavior with and without inherit_stdio
    {
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
    
    // Test 4: Process should complete successfully even with inherit_stdio
    {
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
}