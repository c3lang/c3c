module test;
import std::io;

fn void foo()
{
	int a;
	$switch ($typeof(a)):
		$case int..float: // #error: $case ranges are only allowed for floats
			io::printn("Hello");
		$default:
			io::printn("World");
	$endswitch;
}

fn void foo2()
{
	int a;
	$switch ($typeof(a)):
		$case true..false: // #error: $case ranges are only allowed
			io::printn("Hello");
		$default:
			io::printn("World");
	$endswitch;
}

fn void foo3()
{
	int a;
	$switch ({ 1, 3 }): // #error: Only types, strings, enums, integers, floats and booleans
		$case 1:
			io::printn("Hello");
		$default:
			io::printn("World");
	$endswitch;
}

fn void main()
{
	int a;
	$switch ("abc"):
		$case "cde":
			io::printn("!!!");
		$case "abc":
			io::printn("Hello");
		$default:
			io::printn("World");
	$endswitch;
}
