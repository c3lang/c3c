import std::io;

fn void main()
{
    io::printfln("LLVM version:       %s", env::LLVM_VERSION);
    io::printfln("Optimization level: %s", env::COMPILER_OPT_LEVEL);
    io::printfln("Safe mode:          %s", env::COMPILER_SAFE_MODE);
    io::printfln("OS type:            %s", env::OS_TYPE);
    io::printfln("Big endian:         %s", env::BIG_ENDIAN);
    io::printfln("i128 support:       %s", env::I128_SUPPORT);
    io::printfln("f16 support:        %s", env::F16_SUPPORT);
    io::printfln("f128 support:       %s", env::F128_SUPPORT);
    io::printfln("Benchmarking:       %s", env::BENCHMARKING);
    io::printfln("Testing:            %s", env::TESTING);
}