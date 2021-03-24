

generic abs(x)
{
    case double:
        return fabs(x);
    case int:
        return abs(x);
}

generic add(x, y)
{
    case double, double:
        return x + y;
    case int, double:
        return cast(x as double) + y;
}

generic addBad(x, y)
{
    case double: // #error: Expected 2 types in the case statement
        return x + y;
    case int, double:
        return cast(x as double) + y;
}

generic addBad2() // #error: generic function needs at least 1 parameter
{
    case double:
        return x + y;
    case int, double:
        return cast(x as double) + y;
}

generic addBad3(x)
{
    case 123: // #error: Expected a type as the argument
        return x + y;
    case double:
        return cast(x as double) + y;
}
