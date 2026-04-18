int printf(const char *, ...);

int main(void) {
    int flag = 1;

    long a = __alignof__(a);

    long b = 0;
    b = (long)__alignof__(b);

    long c = 0;
    c = flag ? (long)__alignof__(c) : 1;

    long d = (long)_Alignof(long);

    printf("%ld %ld %ld %ld\n", a, b, c, d);
    return 0;
}
