module foo;

fn void hello() {}

$if (true)

import bar; // #error: 'import' may not appear inside a compile

$endif