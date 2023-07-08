module std::sort;

fn int cmp_int_ref(int* x, int* y) {
    return *x - *y;
}

fn int cmp_int_value(int x, int y) {
    return x - y;
}