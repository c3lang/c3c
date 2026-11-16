faultdef BAD, OTHER;

fn void test_bool(bool b)
{
	switch (b)
	{
		case true: break;
		case true: break; // #error: The same case value appears more than once
		default: break;
	}
}

fn void test_string(String s)
{
	switch (s)
	{
		case "a": break;
		case "b": break;
		case "a": break; // #error: The same case value appears more than once
		default: break;
	}
}

fn void test_float(double d)
{
	switch (d)
	{
		case 0.0: break;
		case -0.0: break; // #error: The same case value appears more than once
		default: break;
	}
}

fn void test_nan(double d, float f)
{
	switch (d)
	{
		case double::nan: break; // #error: A NaN case can never match
		default: break;
	}
	switch (f)
	{
		case 0.0f / 0.0f: break; // #error: A NaN case can never match
		case float::inf: break;
		default: break;
	}
}

fn void test_fault(fault f)
{
	switch (f)
	{
		case BAD: break;
		case OTHER: break;
		case BAD: break; // #error: The same case value appears more than once
		default: break;
	}
}

fn void test_pointer(char* p)
{
	switch (p)
	{
		case null: break;
		case null: break; // #error: The same case value appears more than once
		default: break;
	}
}

fn void test_runtime_and_distinct(String s, double d, double e, bool b)
{
	switch (s)
	{
		case "a": break;
		case "ab": break;
		default: break;
	}
	switch (d)
	{
		case e: break;
		case e: break;
		default: break;
	}
	switch (b)
	{
		case true: break;
		case false: break;
	}
}
