module std::collections::result <OkType, ErrType>;
import std::io;

faultdef RESULT_ERROR, RESULT_OK;

<*
 Result{OkType, ErrType} holds either a success value or an error value with a payload.

 **Prefer C3 optionals for normal error handling.**
 C3's built-in optional/fault system handles most error cases with less overhead.
 See: https://c3-lang.org/language-common/optionals-essential/

 Use Result only when the error path must carry structured data that a bare fault cannot express.

 Good Example:
 ```c3
 struct ParseError { int line; String msg; }

 fn Result{int, ParseError} parse_number(String s)
 {
	 int! v = s.to_int();
	 if (catch v) return result::err({ .line = 1, .msg = string::tformat("not a number: %s", s) });
	 return result::ok(v);
 }

 fn void main()
 {
	 Result{int, ParseError} r = parse_number("42");
	 if (try int v = r.ok()) 
	 {
		 io::printfn("got %d", v);
	 }

	 Result{int, ParseError} bad = parse_number("abc");
	 if (try ParseError e = bad.err()) 
	 {
		 io::printfn("error on line %d: %s", e.line, e.msg);
	 }
 }
 ```

 Bad Example (should be a fault):
 ```c3
 fn Result{int, String} div(int a, int b) 
 {
	 if (b == 0) return result::err("division by zero");
	 return result::ok(a / b);
 }

 fn void print_int(int x) 
 {
	 io::printfn("Integer %d", x);
 }

 fn void main() 
 {
	 print_int(div(4, 1).ok())!!;
	 print_int(div(4, 0).ok())!!;
 }
 ```
 *>
struct Result (Printable)
{
	union
	{
		OkType value;
		ErrType error;
	}
	bool is_ok;
}

fn Result ok(OkType val) => {.is_ok = true, .value = val};
fn Result err(ErrType err) => {.is_ok = false, .error = err};

fn sz? Result.to_format(&self, Formatter* f) @dynamic
{
	if (self.is_ok) return f.printf("OK[%s]", self.value);
	return f.printf("ERR[%s]", self.error);
}

<*
	Returns the ok value or a fault

	@return "The ok value of the result"
	@return? RESULT_ERROR
*>
fn OkType? Result.ok(&self)
{
	return self.is_ok ? self.value : RESULT_ERROR~;
}

<*
	Returns the error value or a fault

	@return "The error of the result"
	@return? RESULT_OK
*>
fn ErrType? Result.err(&self)
{
	return self.is_ok ? RESULT_OK~ : self.error;
}

fn bool Result.equals(self, Result other) @operator(==) 
@if(types::is_equatable_type(OkType) &&& types::is_equatable_type(ErrType))
{
	if (self.is_ok)
	{
		return other.is_ok && equals(self.value, other.value);
	}
	return !other.is_ok && equals(self.error, other.error);
}

<*
	This macro allows the result to diverge on error (return/break/continue)

	**Aborts the program if the body doesn't diverge**

	Examples:
	```c3
	fn Result{int, String} foo()
	{
		Result{int, String} res = result::err("Error");
		int value = res.@or_else(;it) { return it; }; // early function return
		// ...
	}

	fn Result{int, String} bar()
	{
		Result{int, String} res = result::ok(22);
		int value = res.@or_else(;it) { return it; };
		// value is 22
	}
	```
*>
macro OkType Result.@or_else(&self; @body(Result res))
{
	if (!self.is_ok) 
	{
		@body(*self);
		unreachable("@or_else must diverge on error.");
	}
	return self.value;
}
