module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

fn double pow(double x, double y) @extern("pow")
{
    if (x != x || y != y) return double.nan;

    if (y == double.inf)
    {
        if (x == 1.0 || x == -1.0) return 1.0;
        return (_fabs(x) < 1.0) ? 0.0 : double.inf;
    }
    if (y == -double.inf)
    {
        if (x == 1.0 || x == -1.0) return 1.0;
        return (_fabs(x) < 1.0) ? double.inf : 0.0;
    }
    if (x == double.inf)
    {
        return (y < 0.0) ? 0.0 : double.inf;
    }
    if (x == -double.inf)
    {
        if (y != _floor(y)) return -double.nan;
        if (y < 0.0) return 0.0;
        return ((int)y & 1) ? -double.inf : double.inf;
    }

    if (y == 0.0) return 1.0;

    if (x == 0.0)
    {
        if (y < 0.0) return double.inf;
        if (y > 0.0) return 0.0;
        return 1.0;
    }

    if (y == 1.0) return x;
    if (x == 1.0) return 1.0;

    if (x < 0.0)
    {
        if (y != _floor(y)) return double.nan;
        return ((int)y & 1) ? -pow(-x, y) : pow(-x, y);
    }

    double result = exp(y * log(x));

    if (result == double.inf || result == -double.inf)
    {
        return (y < 0.0) ? 0.0 : double.inf;
    }
    if (result == 0.0) return 0.0;

    return result;
}

fn float powf(float x, float y) @extern("powf")
{
    if (x != x || y != y) return float.nan;

    if (y == float.inf)
    {
        if (x == 1.0f || x == -1.0f) return 1.0f;
        return (_fabsf(x) < 1.0f) ? 0.0f : float.inf;
    }
    if (y == -float.inf)
    {
        if (x == 1.0f || x == -1.0f) return 1.0f;
        return (_fabsf(x) < 1.0f) ? float.inf : 0.0f;
    }
    if (x == float.inf)
    {
        return (y < 0.0f) ? 0.0f : float.inf;
    }
    if (x == -float.inf)
    {
        if (y != _floorf(y)) return float.nan;
        if (y < 0.0f) return 0.0f;
        return ((int)y & 1) ? -float.inf : float.inf;
    }

    if (y == 0.0f) return 1.0f;

    if (x == 0.0f)
    {
        if (y < 0.0f) return float.inf;
        if (y > 0.0f) return 0.0f;
        return 1.0f;
    }

    if (y == 1.0f) return x;
    if (x == 1.0f) return 1.0f;

    if (x < 0.0f)
    {
        if (y != _floorf(y)) return float.nan;
        return ((int)y & 1) ? -powf(-x, y) : powf(-x, y);
    }

    float result = expf(y * logf(x));

    if (result == float.inf || result == -float.inf)
    {
        return (y < 0.0f) ? 0.0f : float.inf;
    }
    if (result == 0.0f) return 0.0f;

    return result;
}