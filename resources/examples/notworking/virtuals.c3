module comparable;
import std::math;

interface Geometry
{
    func double area();
    func double perim();
}

struct Rect
{
    double width, height;
}

struct Circle
{
    double radius;
}

func double Rect.area(Rect *r)
{
    return r.width * r.height;
}

func double Rect.perim(Rect *r)
{
    return 2 * r.width + 2 * r.height;
}

func double Circle.area(Circle *c)
{
    return math::PI * c.radius * c.radius
}

func double Circle.perim(Circle *c)
{
    return math::PI * c.radius * 2;
}

func void measure(virtual Geometry g)
{
    printf("area: %f, perimeter: %f\n", g.area(), g.perim());
}

func void main()
{
    Rect r = { 3, 4 };
    Circle c = { 5 };
    measure(&r);
    measure(&c);
}
