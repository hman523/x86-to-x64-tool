struct Point {
    long x;
    long y;
    int label;
};

struct Mixed {
    unsigned long count;
    int flags;
};

typedef long word_t;
typedef unsigned long uword_t;

long num_return(void) {
    return 42;
}

long ptr_return(void *p) {
    return (long)p;
}

void sizeof_use(void) {
    int n;
    n = sizeof(long);
}

void cast_sync_use(void *p) {
    long x = (long)p;
}
