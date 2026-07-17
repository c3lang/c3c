module json_benchmarks;
import std::io;
import std::encoding::json;
import std::collections::object;

struct Address
{
	String street;
	String city;
	int zip;
}

struct Record
{
	int id;
	String name;
	String email;
	bool active;
	double balance;
	int[] scores;
	Address address;
}

Record[] small_records;
Record[] medium_records;
Record[] large_records;

String small_json;
String medium_json;
String large_json;

Object* small_dom;
Object* medium_dom;
Object* large_dom;

fn Record[] make_records(Allocator allocator, sz count)
{
	Record[] records = alloc::new_array(allocator, Record, count);
	for (sz i = 0; i < count; i++)
	{
		int[] scores = alloc::new_array(allocator, int, 5);
		for (sz j = 0; j < 5; j++) scores[j] = (int)((i * 7 + j * 13) % 100);
		records[i] = {
			.id = (int)i,
			.name = string::format(allocator, "user_%d", i),
			.email = string::format(allocator, "user_%d@example.com", i),
			.active = (i & 1) == 0,
			.balance = i * 1.5,
			.scores = scores,
			.address = {
				.street = string::format(allocator, "%d Main Street", i),
				.city = "Springfield",
				.zip = (int)(10000 + i),
			},
		};
	}
	return records;
}

fn void bench_setup() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(100_000);

	set_benchmark_func_iterations($reflect(marshal_small).qname, 100_000);
	set_benchmark_func_iterations($reflect(marshal_medium).qname, 2_400);
	set_benchmark_func_iterations($reflect(marshal_large).qname, 120);

	set_benchmark_func_iterations($reflect(unmarshal_small).qname, 70_000);
	set_benchmark_func_iterations($reflect(unmarshal_medium).qname, 1_400);
	set_benchmark_func_iterations($reflect(unmarshal_large).qname, 65);

	set_benchmark_func_iterations($reflect(parse_dom_small).qname, 45_000);
	set_benchmark_func_iterations($reflect(parse_dom_medium).qname, 900);
	set_benchmark_func_iterations($reflect(parse_dom_large).qname, 25);

	set_benchmark_func_iterations($reflect(unmarshal_dom_small).qname, 120_000);
	set_benchmark_func_iterations($reflect(unmarshal_dom_medium).qname, 3_000);
	set_benchmark_func_iterations($reflect(unmarshal_dom_large).qname, 150);

	small_records  = make_records(mem, 1);
	medium_records = make_records(mem, 50);
	large_records  = make_records(mem, 1000);

	small_json  = json::marshal(mem, small_records)!!;
	medium_json = json::marshal(mem, medium_records)!!;
	large_json  = json::marshal(mem, large_records)!!;

	small_dom  = json::parse(mem, small_json)!!;
	medium_dom = json::parse(mem, medium_json)!!;
	large_dom  = json::parse(mem, large_json)!!;
}

module json_benchmarks @benchmark;

import std::encoding::json;

// 1. Compile-Time Marshaling
fn void marshal_small()   { json::marshal(tmem, small_records)!!; }
fn void marshal_medium()  { json::marshal(tmem, medium_records)!!; }
fn void marshal_large()   { json::marshal(tmem, large_records)!!; }

// 2. Compile-Time Unmarshaling
fn void unmarshal_small()  { json::unmarshal(tmem, small_json, Record[])!!; }
fn void unmarshal_medium() { json::unmarshal(tmem, medium_json, Record[])!!; }
fn void unmarshal_large()  { json::unmarshal(tmem, large_json, Record[])!!; }

// 3. Dynamic DOM Parsing
fn void parse_dom_small()  { json::tparse(small_json)!!; }
fn void parse_dom_medium() { json::tparse(medium_json)!!; }
fn void parse_dom_large()  { json::tparse(large_json)!!; }

// 4. Dynamic DOM-to-Struct mapping
fn void unmarshal_dom_small()
{
	Record[] decoded;
	json::@unmarshal_to(tmem, small_dom, decoded);
}

fn void unmarshal_dom_medium()
{
	Record[] decoded;
	json::@unmarshal_to(tmem, medium_dom, decoded);
}

fn void unmarshal_dom_large()
{
	Record[] decoded;
	json::@unmarshal_to(tmem, large_dom, decoded);
}
