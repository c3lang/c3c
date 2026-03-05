module glfw;

import std::io;

typedef Window @public = inline _Window*;
typedef Monitor @public = inline _Monitor*;

fn void Window.create(&self, int width, int height, String title, Monitor monitor = {}, Window share = {}
) @public
{
	*self = (Window)_glfwCreateWindow(width, height, title, monitor, share);
}

fn void Window.destroy(self) @public
{
	_glfwDestroyWindow((_Window*)self);
}

fn bool Window.shouldClose(self) @public
{
	return _glfwWindowShouldClose((_Window*)self);
}

fn void Window.swapBuffers(self) @public
{
	return _glfwSwapBuffers((_Window*)self);
}

fn void Window.makeContextCurrent(self) @public
{
	return _glfwMakeContextCurrent((_Window*)self);
}

extern fn void initialize() @cname("glfwInit") @public;

extern fn void terminate() @cname("glfwTerminate") @public;

extern fn void pollEvents() @cname("glfwPollEvents") @public;

typedef _Window @private = void;
typedef _Monitor @private = void;

extern fn _Window* _glfwCreateWindow(int width, int height, char* title, _Monitor* monitor, _Window* share) @cname("glfwCreateWindow") @private;

extern fn bool _glfwWindowShouldClose(_Window* window) @cname("glfwWindowShouldClose") @private;

extern fn void _glfwDestroyWindow(_Window* window) @cname("glfwDestroyWindow") @private;

extern fn void _glfwSwapBuffers(_Window* window) @cname("glfwSwapBuffers") @private;

extern fn void _glfwMakeContextCurrent(_Window* window) @cname("glfwMakeContextCurrent") @private;