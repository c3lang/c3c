
attrdef @FOO = @test; // #error: Expected an
attrdef 1 = @test; // #error: Expected an
attrdef @foo = @test; // #error: Expected an
attrdef foo = @test; // #error: Expected an
attrdef $foo = @test; // #error: Expected an
attrdef #foo = @test; // #error: Expected an
attrdef "foo" = @test; // #error: Expected an
attrdef 'f' = @test; // #error: Expected an