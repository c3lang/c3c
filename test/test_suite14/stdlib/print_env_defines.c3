import std::io;

fn void main()
{
    io::printfn("LLVM version:       %s", env::LLVM_VERSION);
    io::printfn("Optimization level: %s", env::COMPILER_OPT_LEVEL);
    io::printfn("Safe mode:          %s", env::COMPILER_SAFE_MODE);
    io::printfn("OS type:            %s", env::OS_TYPE);
    io::printfn("Big endian:         %s", env::BIG_ENDIAN);
    io::printfn("i128 support:       %s", env::I128_NATIVE_SUPPORT);
    io::printfn("f16 support:        %s", env::F16_SUPPORT);
    io::printfn("f128 support:       %s", env::F128_SUPPORT);
    io::printfn("Benchmarking:       %s", env::BENCHMARKING);
    io::printfn("Testing:            %s", env::TESTING);
}