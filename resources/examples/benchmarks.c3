import std::thread;

fn void bench1() @benchmark
{
    std::thread::sleep_ms(1)!!;
}

fn void bench123456789() @benchmark
{
    std::thread::sleep_ms(2)!!;
}

fn void long_name_bench() @benchmark
{
    std::thread::sleep_ms(3)!!;
}

fn void very_long_name_bench() @benchmark
{
    return std::thread::sleep_ms(10);
}

fn void initialize() @init
{
    set_benchmark_warmup_iterations(5);
    set_benchmark_max_iterations(1000);
}


