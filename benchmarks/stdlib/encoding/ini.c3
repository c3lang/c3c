module ini_benchmarks;
import std::encoding::ini;
import std::collections::object;

// Small: typical app config (~250 B)
const String SMALL_INI = `
name = myapp
version = 1.0
debug = false

[server]
host = localhost
port = 8080
timeout = 30

[database]
host = db.internal
port = 5432
name = appdb
user = admin
password = secret
pool_size = 10

[logging]
level = info
file = /var/log/myapp.log
`;

Object* small_map;
Object* medium_map;
Object* large_map;

String medium_ini;
String large_ini;

fn void bench_setup() @init
{
    set_benchmark_warmup_iterations(3);
    set_benchmark_max_iterations(100_000);

    set_benchmark_func_iterations($reflect(parse_small).qname,   100_000);
    set_benchmark_func_iterations($reflect(encode_small).qname,  100_000);
    set_benchmark_func_iterations($reflect(parse_medium).qname,   10_000);
    set_benchmark_func_iterations($reflect(encode_medium).qname,  10_000);
    set_benchmark_func_iterations($reflect(parse_large).qname,     1_000);
    set_benchmark_func_iterations($reflect(encode_large).qname,    1_000);

    // Medium: 20 sections × 20 keys (~6 KB)
    DString med;
    for (int s = 0; s < 20; s++)
    {
        med.appendf("[section_%d]\n", s);
        for (int k = 0; k < 20; k++)
        {
            med.appendf("key_%d = value_number_%d_in_section_%d\n", k, k, s);
        }
    }
    medium_ini = med.str_view().copy(mem);

    // Large: 100 sections × 30 keys (~50 KB)
    DString big;
    for (int s = 0; s < 100; s++)
    {
        big.appendf("[module_%d]\n", s);
        for (int k = 0; k < 30; k++)
        {
            big.appendf("option_%d = some_value_%d\n", k, k * s + 1);
        }
    }
    large_ini = big.str_view().copy(mem);

    small_map  = ini::parse(mem, SMALL_INI)!!;
    medium_map = ini::parse(mem, medium_ini)!!;
    large_map  = ini::parse(mem, large_ini)!!;
}

// =======================================================================================
module ini_benchmarks @benchmark;

import std::encoding::ini;

fn void parse_small()  { @pool() { ini::tparse(SMALL_INI)!!; }; }
fn void parse_medium() { @pool() { ini::tparse(medium_ini)!!; }; }
fn void parse_large()  { @pool() { ini::tparse(large_ini)!!; }; }

fn void encode_small()  { @pool() { ini::tencode(small_map); }; }
fn void encode_medium() { @pool() { ini::tencode(medium_map); }; }
fn void encode_large()  { @pool() { ini::tencode(large_map); }; }
