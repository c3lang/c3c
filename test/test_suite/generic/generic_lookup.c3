module test;

fn void a()
{
	List {int} x; // #error: Did you mean the struct 'List' in module std::collections::list? If so please add
}

fn void b()
{
	list::ONHEAP{int}; // #error: Did you mean the constant 'std::collections::list::ONHEAP' in module std::collections::list?
}

fn void c()
{
	list::type_is_overaligned{int}(); // #error: Did you mean the macro 'std::collections::list::type_is_overaligned' in module std::collections::list?
}

fn void d()
{
	list::type_is_overaligned(); // #error: Did you mean the macro 'type_is_overaligned' in the generic module std::collections::list? If so, use 'type_is_overaligned{...}' instead
}

fn void e()
{
	List x; // #error: Did you mean the struct 'List' in the generic module std::collections::list? If so, use 'List{...}'
}

fn int main(String[] args)
{
	return 0;
}
