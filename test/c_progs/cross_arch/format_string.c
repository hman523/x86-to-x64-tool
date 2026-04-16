int printf(const char *, ...);

int main(void) {
    long n = (long)sizeof(long);
    printf("%ld hello\n", n);
    return 0;
}
