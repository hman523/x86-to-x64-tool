int printf(const char *, ...);

int main(void) {
    unsigned long mask = 0xFFFFFFFFUL;
    unsigned long val  = mask + 1;
    printf("%lu\n", val);
    return 0;
}
