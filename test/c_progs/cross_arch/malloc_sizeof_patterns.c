int printf(const char *, ...);

int main(void) {
    long count = 10;

    long self_size = sizeof(self_size) * count;

    long element;
    long elem_size = sizeof(element) * count;

    printf("%ld %ld\n", self_size, elem_size);
    return 0;
}
