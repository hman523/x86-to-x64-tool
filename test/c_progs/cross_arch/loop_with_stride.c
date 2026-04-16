int printf(const char *, ...);

int main(void) {
    long stride = (long)sizeof(long);
    long total  = 0;
    for (long i = 0; i < 4; i++) {
        total = total + stride;
    }
    printf("%ld %ld\n", total, stride);
    return 0;
}
