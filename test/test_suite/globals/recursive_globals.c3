void *x = &x;

int y = 1 + y; // #error: This looks like the initialization of the variable was circular.