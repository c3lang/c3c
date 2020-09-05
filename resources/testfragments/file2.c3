module bar;

struct Type { int x; }

public func Type blub(Type type)
{
int i = 0;
    type.x = i;
    return type;
}