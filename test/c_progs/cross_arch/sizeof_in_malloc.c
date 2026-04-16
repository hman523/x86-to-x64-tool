int printf(const char *, ...);

int main(void) {
    unsigned long n = 10;
    unsigned long bytes = n * (unsigned long)sizeof(long);
    printf("%lu\n", bytes);
    return 0;
}
