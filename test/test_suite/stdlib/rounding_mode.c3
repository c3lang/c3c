import std::io;
import std::math;

fn void main()
{
    io::printfln("Current rounding mode: %s", $$get_rounding_mode());
    float f1 = 11.5;
    float f2 = -11.5;

    foreach (int mode : RoundingMode.values)
    {
        $$set_rounding_mode(mode);
        io::printfln("Rounding mode: %s", $$get_rounding_mode());

        io::printfln("  ceil(%s) == %s", f1, math::ceil(f1));
        io::printfln("  ceil(%s) == %s", f2, math::ceil(f2));
        io::printfln("  floor(%s) == %s", f1, math::floor(f1));
        io::printfln("  floor(%s) == %s", f2, math::floor(f2));
        io::printfln("  nearbyint(%s) == %s", f1, math::nearbyint(f1));
        io::printfln("  nearbyint(%s) == %s", f2, math::nearbyint(f2));
        io::printfln("  rint(%s) == %s", f1, math::rint(f1));
        io::printfln("  rint(%s) == %s", f2, math::rint(f2));
        io::printfln("  round(%s) == %s", f1, math::round(f1));
        io::printfln("  round(%s) == %s", f2, math::round(f2));
        io::printfln("  roundeven(%s) == %s", f1, math::roundeven(f1));
        io::printfln("  roundeven(%s) == %s", f2, math::roundeven(f2));
        io::printfln("  trunc(%s) == %s", f1, math::trunc(f1));
        io::printfln("  trunc(%s) == %s", f2, math::trunc(f2));
    }
}
