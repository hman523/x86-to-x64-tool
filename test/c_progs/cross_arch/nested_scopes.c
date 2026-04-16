int printf(const char *, ...);

int main(void) {
    long count = 3;
    long total = 0;

    if (count > 0) {
        long step = (long)sizeof(long);
        for (long i = 0; i < count; i++) {
            total = total + step;
        }
    }

    printf("%ld %ld\n", count, total);
    return 0;
}
