module contract_err_tests;
import std;
<*
Hello world
 @param [in] a
   : "param description"
 @require 1 == 2 : "Some text, "
  "that" " continues "
  `-here `
 @require 1 = 1
 @return "hello"
 "world"
 @return? io::EOF :
  "errors"
*>
macro usz @a(a) => a;

<*
Hello world
 @param [in] a :
    "param description"
 @require 1 == 2
    :
    "Some text, "
  "that" " continues "
  `-here `
 @require 1 = 1
 @return
 "hello"
 "world"
 @return? io::EOF
    : "errors"
*>
macro usz @b(a) => a;

fn int main()
{
    usz t = @a(1); // #error: 'Some text, that continues -here '
    return 0;
}