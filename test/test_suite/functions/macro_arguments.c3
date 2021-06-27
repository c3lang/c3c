
func void foo1(int #foo) { } // #error: Only regular parameters are allowed for functions.

func void foo2(int $foo) { } // #error: Only regular parameters are allowed for functions.

func void foo3(bar) { } // #error: Only typed parameters are allowed for functions

func void foo4($Type) { } // #error: Only regular parameters are allowed for functions.

func void foo8(int &foo) {} // #error: Only regular parameters are allowed for functions.