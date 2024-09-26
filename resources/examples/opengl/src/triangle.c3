import std::io;
import glfw;
import gl;

fn void main() {
    glfw::initialize();

    Window window;
    window.create(1280, 720, "Triangle example");
    window.makeContextCurrent();

    while(!window.shouldClose()) {
        gl::clear(BufferBit.COLOR.value);

        gl::begin(Primitive.TRIANGLES.value);
            gl::color3f(1, 0, 0); gl::vertex3f(-0.6, -0.75, 0.5);
            gl::color3f(0, 1, 0); gl::vertex3f(0.6, -0.75, 0);
            gl::color3f(0, 0, 1); gl::vertex3f(0, 0.75, 0);
        gl::end();

        gl::flush();

        window.swapBuffers();
        glfw::pollEvents();
    }

    window.destroy();

    glfw::terminate();
}
