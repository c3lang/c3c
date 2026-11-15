struct Inner { char c; long l; }

Inner[3] g_inners;
Inner* g_ptr = &g_inners[0];
Inner[] g_slice;
int g_index = 1;

long* a = &g_inners[1].l;
long* b = &g_ptr[1].l; // #error: must be a constant
long* c = &g_slice[1].l; // #error: must be a constant
long* d = &g_inners[g_index].l; // #error: must be a constant
long* e = &(*g_ptr).l; // #error: must be a constant

