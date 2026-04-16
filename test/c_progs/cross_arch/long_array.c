int printf(const char *, ...);

int main(void) {
    long arr[5] = {1, 2, 3, 4, 5};
    printf("%lu\n", (unsigned long)sizeof(arr));
    return 0;
}
