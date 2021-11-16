module comparable;
import std::math;

interface Geometry
{
    fn double area();
    fn double perim();
}

struct Rect
{
    double width, height;
}

struct Circle
{
    double radius;
}

fn double Rect.area(Rect *r)
{
    return r.width * r.height;
}

fn double Rect.perim(Rect *r)
{
    return 2 * r.width + 2 * r.height;
}

fn double Circle.area(Circle *c)
{
    return math::PI * c.radius * c.radius
}

fn double Circle.perim(Circle *c)
{
    return math::PI * c.radius * 2;
}

fn void measure(virtual Geometry g)
{
    printf("area: %f, perimeter: %f\n", g.area(), g.perim());
}

fn void main()
{
    Rect r = { 3, 4 };
    Circle c = { 5 };
    measure(&r);
    measure(&c);
}
