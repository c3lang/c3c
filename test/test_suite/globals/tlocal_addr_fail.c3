tlocal int g_tls;
tlocal int[3] g_tls_arr;
struct TlsFoo { int x; int[2] y; }
tlocal TlsFoo g_tls_foo;

int* p1 = &g_tls; // #error: must be a constant
int* p2 = &g_tls_arr[1]; // #error: must be a constant
int* p3 = &g_tls_foo.y[1]; // #error: must be a constant
tlocal int* p4 = &g_tls; // #error: must be a constant
tlocal int* p5 = &g_tls_arr[2]; // #error: must be a constant
