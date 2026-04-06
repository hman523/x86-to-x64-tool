int cast_ptr_to_int(void) {
    int *ptr = 0;
    int addr = (int)ptr;
    return addr;
}

int ptr_diff(void) {
    int *p = 0;
    int *q = 0;
    int diff;
    diff = p - q;
    return diff;
}

int sizeof_to_int(void) {
    int n;
    n = sizeof(int);
    return n;
}

int return_ptr(void) {
    int *ptr = 0;
    return ptr;
}


struct Mixed {
    int *ptr;
    int  val;
};

void use_mixed(void) {
    struct Mixed m;
    m.val = 0;
}
