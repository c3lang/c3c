module std::math;

extern func double _log10(double) @cname("log10");

public func double log10(double d) @inline
{
    return _log10(d);
}

