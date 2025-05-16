module std::os::process @test;
import std::os, std::io;

fn void test_inherit_stdio()
{
    // Test 2: With inherit_stdio - we can't capture output
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
}