int printf(const char *, ...);

long          g_count = 0;
unsigned long g_mask  = 0xFFFFFFFFUL;

void add(long n) {
    g_count = g_count + n;
}

int main(void) {
    add(3);
    add(2);
    long sz = (long)sizeof(long);
    printf("%ld %lu %ld\n", g_count, g_mask, sz);
    return 0;
}
