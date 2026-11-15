module std::core::runtime::panics;

fn int index() => 42; // #error: is reserved in this module
fn int index_u() => 42;
int message; // #error: is reserved in this module
fn int require_message() @private => 42; // #error: is reserved in this module
fn int assert_violation() @local => 42;
fn int index_v() => 42;
