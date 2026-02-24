module gl;

constdef BufferBit : int
{
    COLOR = 0x00004000,
    STENCIL = 0x00000400,
    DEPTH = 0x00000100,
}

constdef Primitive : int
{
    POINTS = 0,
    LINES = 1,
    LINE_LOOP = 2,
    LINE_STRIP = 3,
    TRIANGLES = 4,
    TRIANGLE_STRIP = 5,
    TRIANGLE_FAN = 6,
    QUADS = 7,
    QUAD_STRIP = 8,
    POLYGON = 9,
}

extern fn void clear(BufferBit mask) @cname("glClear") @public;

extern fn void begin(BufferBit mask) @cname("glBegin") @public;

extern fn void end() @cname("glEnd") @public;

extern fn void flush() @cname("glFlush") @public;

extern fn void color3f(float r, float g, float b) @cname("glColor3f") @public;

extern fn void vertex3f(float x, float y, float z) @cname("glVertex3f") @public;