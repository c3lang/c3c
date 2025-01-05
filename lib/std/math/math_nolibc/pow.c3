module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

fn double pow(double x, double y) @extern("pow")
{
    if (x != x || y != y) return NAN;

    if (y == INFINITY)
    {
        if (x == 1.0 || x == -1.0) return 1.0;
        return (fabs(x) < 1.0) ? 0.0 : INFINITY;
    }
    if (y == -INFINITY)
    {
        if (x == 1.0 || x == -1.0) return 1.0;
        return (fabs(x) < 1.0) ? INFINITY : 0.0;
    }
    if (x == INFINITY)
    {
        return (y < 0.0) ? 0.0 : INFINITY;
    }
    if (x == -INFINITY)
    {
        if (y != floor(y)) return NAN;
        if (y < 0.0) return 0.0;
        return ((int)y & 1) ? -INFINITY : INFINITY;
    }

    if (y == 0.0) return 1.0;

    if (x == 0.0)
    {
        if (y < 0.0) return INFINITY;
        if (y > 0.0) return 0.0;
        return 1.0;
    }

    if (y == 1.0) return x;
    if (x == 1.0) return 1.0;

    if (x < 0.0)
    {
        if (y != floor(y)) return NAN;
        return ((int)y & 1) ? -pow(-x, y) : pow(-x, y);
    }

    double result = exp(y * log(x));

    if (result == INFINITY || result == -INFINITY)
    {
        return (y < 0.0) ? 0.0 : INFINITY;
    }
    if (result == 0.0) return 0.0;

    return result;
}

fn float powf(float x, float y) @extern("powf")
{
    if (x != x || y != y) return NAN;

    if (y == INFINITY)
    {
        if (x == 1.0f || x == -1.0f) return 1.0f;
        return (fabsf(x) < 1.0f) ? 0.0f : INFINITY;
    }
    if (y == -INFINITY)
    {
        if (x == 1.0f || x == -1.0f) return 1.0f;
        return (fabsf(x) < 1.0f) ? INFINITY : 0.0f;
    }
    if (x == INFINITY)
    {
        return (y < 0.0f) ? 0.0f : INFINITY;
    }
    if (x == -INFINITY)
    {
        if (y != floorf(y)) return NAN;
        if (y < 0.0f) return 0.0f;
        return ((int)y & 1) ? -INFINITY : INFINITY;
    }

    if (y == 0.0f) return 1.0f;

    if (x == 0.0f)
    {
        if (y < 0.0f) return INFINITY;
        if (y > 0.0f) return 0.0f;
        return 1.0f;
    }

    if (y == 1.0f) return x;
    if (x == 1.0f) return 1.0f;

    if (x < 0.0f)
    {
        if (y != floorf(y)) return NAN;
        return ((int)y & 1) ? -powf(-x, y) : powf(-x, y);
    }

    float result = expf(y * logf(x));

    if (result == INFINITY || result == -INFINITY)
    {
        return (y < 0.0f) ? 0.0f : INFINITY;
    }
    if (result == 0.0f) return 0.0f;

    return result;
}
