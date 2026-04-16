int printf(const char *, ...);

int main(void) {
    long a = 2147483647L;
    long b = a + 1;
    printf("%ld\n", b);
    return 0;
}
