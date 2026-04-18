int printf(const char *, ...);

int main(void) {
    int flag = 1;

    long a = 0;
    a = sizeof(a);

    long b = sizeof(b);

    long c = 0;
    c = (long)sizeof(c);

    long d = 0;
    d = flag ? (long)sizeof(d) : 1;

    long e = 0;
    e = flag ? 1 : (long)sizeof(e);

    printf("%ld %ld %ld %ld %ld\n", a, b, c, d, e);
    return 0;
}
