module sourcelib;

extern fn int sourcelib_c_add(int a, int b);

fn int add(int a, int b) => sourcelib_c_add(a, b);
