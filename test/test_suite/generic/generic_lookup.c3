module test;
import std::collections::elastic_array;
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
	list::type_is_overaligned(); // #error: Did you mean the macro 'std::collections::list::type_is_overaligned' in module std::collections::list? If so please add 'import std::collections::list'
}

fn void d2()
{
	elastic_array::type_is_overaligned(); // #error: 'type_is_overaligned' is a generic macro, did you forget the parameters '{ ... }'
}

fn void e()
{
	List x; // #error: Did you mean the struct 'List' in module std::collections::list? If so please add 'import std::collections::list'
}

fn void f()
{
	ElasticArray x; // #error: 'ElasticArray' is a generic struct, did you forget the parameters '{ ... }'?
}

fn void g()
{
	(ElasticArray){}; // #error: 'ElasticArray' is a generic struct, did you forget the parameters '{ ... }'?
}

fn int main(String[] args)
{
	return 0;
}