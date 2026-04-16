int printf(const char *, ...);

typedef long myint;

int main(void) {
    printf("%lu\n", (unsigned long)sizeof(myint));
    return 0;
}
