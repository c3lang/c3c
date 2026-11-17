// #target: elf-aarch64
module test;
import std::core::cpudetect;

fn void main()
{
	cpudetect::aarch64_initialize_cpu_features();
}
