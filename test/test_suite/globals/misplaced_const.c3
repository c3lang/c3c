const $FOO = 3;
$BAR = 4; // #error: Did you forget a 'const' before the name of this compile time constant?

const $BAZ = "ofke";

$FOOBAR; // #error: Compile time constant unexpectedly found