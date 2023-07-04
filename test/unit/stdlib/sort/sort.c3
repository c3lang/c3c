module std::sort;

fn int cmp_int(void* x, void* y) {
    return *(int*)x - *(int*)y;
}

fn int cmp_int2(int x, int y) {
    return x - y;
}