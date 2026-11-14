module test;

// An explicit cast of a slice to a pointer works for any pointer type, like a cast between pointers.
struct Plain
{
	int a;
}

fn void to_char_ptr(int[] s) { char* p = (char*)s; }
fn void to_struct_ptr(int[] s) { Plain* p = (Plain*)s; }
fn void to_vector_ptr(int[] s) { int[<4>]* p = (int[<4>]*)s; }
fn void to_array_ptr(int[] s) { int[4]* p = (int[4]*)s; }
fn void to_other_array_ptr(int[] s) { char[4]* p = (char[4]*)s; }

// Without the cast only compatible pointers work.
fn void to_void_ptr(int[] s) { void* p = s; }
fn void to_int_ptr(int[] s) { int* p = s; }
fn void implicit_char_ptr(int[] s) { char* p = s; } // #error: 'char*'
fn void implicit_struct_ptr(int[] s) { Plain* p = s; } // #error: 'Plain*'
fn void implicit_vector_ptr(int[] s) { int[<4>]* p = s; } // #error: 'int[<4>]*'
