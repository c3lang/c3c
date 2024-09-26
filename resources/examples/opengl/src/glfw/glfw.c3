module glfw;

import std::io;

distinct Window @public = _Window*;
distinct Monitor @public = _Monitor*;

fn void Window.create(
    &self,
    int width,
    int height,
    String title,
    Monitor monitor = {},
    Window share = {}
) @public {
    *self = (Window)_glfwCreateWindow(
        width,
        height,
        title,
        (_Monitor*)monitor,
        (_Window*)share
    );
}

fn void Window.destroy(self) @public {
    _glfwDestroyWindow((_Window*)self);
}

fn bool Window.shouldClose(self) @public {
    return _glfwWindowShouldClose((_Window*)self);
}

fn void Window.swapBuffers(self) @public {
    return _glfwSwapBuffers((_Window*)self);
}

fn void Window.makeContextCurrent(self) @public {
    return _glfwMakeContextCurrent((_Window*)self);
}

extern fn void initialize() @extern("glfwInit") @public;

extern fn void terminate() @extern("glfwTerminate") @public;

extern fn void pollEvents() @extern("glfwPollEvents") @public;

distinct _Window @private = void;
distinct _Monitor @private = void;

extern fn _Window* _glfwCreateWindow(
    int width, int height, char* title, _Monitor* monitor, _Window* share
) @extern("glfwCreateWindow") @private;

extern fn bool _glfwWindowShouldClose(_Window* window)
    @extern("glfwWindowShouldClose") @private;

extern fn void _glfwDestroyWindow(_Window* window)
    @extern("glfwDestroyWindow") @private;

extern fn void _glfwSwapBuffers(_Window* window)
    @extern("glfwSwapBuffers") @private;

extern fn void _glfwMakeContextCurrent(_Window* window)
    @extern("glfwMakeContextCurrent") @private;