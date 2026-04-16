int printf(const char *, ...);

int main(void) {
    long sum_even = 0;
    long sum_odd  = 0;

    for (long i = 0; i < 5; i++) {
        if (i % 2 == 0) {
            long v = i;
            sum_even = sum_even + v;
        } else {
            long v = i;
            sum_odd = sum_odd + v;
        }
    }

    long stride = (long)sizeof(long);
    long total  = sum_even + sum_odd + stride;

    printf("%ld %ld %ld %ld\n", sum_even, sum_odd, stride, total);
    return 0;
}
