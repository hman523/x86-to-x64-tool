int printf(const char *, ...);

struct S {
    char  c;
    long  x;
    long  y;
};

int main(void) {
    printf("%lu\n", (unsigned long)sizeof(struct S));
    return 0;
}
