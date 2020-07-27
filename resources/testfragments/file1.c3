module baz;
import bar;

public func void runBar()
{
    bar::notVisible(); // #error: 'notVisible' is not visible from this module.
}