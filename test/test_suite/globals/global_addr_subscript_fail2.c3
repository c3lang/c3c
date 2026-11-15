struct Pair { int[2] a; }

int[4] g_arr = { 1, 2, 3, 4 };
int* g_ptr = &g_arr[1];
Pair g_pair;
Pair* g_pair_ptr = &g_pair;
const String C_STR = "hello";
int g_index = 1;

int* a = &g_ptr[1]; // #error: must be a constant
int* b = &g_pair_ptr.a[1]; // #error: must be a constant
char* c = &C_STR[1]; // #error: must be a constant
int* d = &g_arr[g_index]; // #error: must be a constant
int* e = &(*g_ptr); // #error: must be a constant
