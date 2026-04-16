int printf(const char *, ...);

struct Pair {
    long first;
    long second;
};

int main(void) {
    struct Pair p;
    p.first  = 10;
    p.second = 20;
    long sum         = p.first + p.second;
    long elem_size   = (long)sizeof(long);
    long struct_size = (long)sizeof(struct Pair);
    printf("%ld %ld %ld\n", sum, elem_size, struct_size);
    return 0;
}
