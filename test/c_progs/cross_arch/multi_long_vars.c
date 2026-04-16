int printf(const char *, ...);

int main(void) {
    long          count   = 3;
    long          sz      = (long)sizeof(long);
    unsigned long mask    = 0xFFFFFFFFUL;
    unsigned long wrapped = mask + 1;
    printf("%ld %ld %lu %lu\n", count, sz, mask, wrapped);
    return 0;
}
