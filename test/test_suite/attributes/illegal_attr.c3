
attrmacro @FOO = @test; // #error: Expected an
attrmacro 1 = @test; // #error: Expected an
attrmacro @foo = @test; // #error: Expected an
attrmacro foo = @test; // #error: Expected an
attrmacro $foo = @test; // #error: Expected an
attrmacro #foo = @test; // #error: Expected an
attrmacro "foo" = @test; // #error: Expected an
attrmacro 'f' = @test; // #error: Expected an