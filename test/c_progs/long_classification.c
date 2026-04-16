long num_use(void) {
    long n = 42;
    return n + 1;
}

long ptr_use(void *p) {
    long x = (long)p;
    return x;
}

long size_use(void) {
    long sz = sizeof(int);
    return sz;
}

long offset_use(int *a, int *b) {
    long d = a - b;
    return d;
}

long bits_use(long val) {
    long m = val & 255;
    return m;
}

unsigned long unum_use(void) {
    unsigned long n = 100;
    return n + 1;
}
