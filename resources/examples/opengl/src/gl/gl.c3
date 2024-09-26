module gl;

def BitField = int;

enum BufferBit : int(int value) {
    COLOR = 0x00004000,
    STENCIL = 0x00000400,
    DEPTH = 0x00000100,
}

enum Primitive : int(int value) {
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

extern fn void clear(BitField mask) @extern("glClear") @public;

extern fn void begin(BitField mask) @extern("glBegin") @public;

extern fn void end() @extern("glEnd") @public;

extern fn void flush() @extern("glFlush") @public;

extern fn void color3f(float r, float g, float b)
    @extern("glColor3f") @public;

extern fn void vertex3f(float x, float y, float z)
    @extern("glVertex3f") @public;