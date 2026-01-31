module gui::widget <Type>;
import gui::widget_types;
import std::collections::list;
import std::io;

struct Widget
{
	int id;
	Type type;
	List {any} children;
}

fn void Widget{Label}.draw(Widget* self) // #error: This method is already defined for 'Widget{Label}'
{
	io::printfn("Hello Label");
}

module gui::widget_types;
import gui::widget;
struct Label
{
	String text;
}

fn void main()
{
	Widget {int} y;
	y.draw();
}