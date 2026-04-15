struct Mixed { int *ptr; int val; };
union Handler { int fd; int *ptr; };
int ret_fn() { int *p; return p; }
void param_fn(int h) { int *r; h = r; }
void big_problem() {
  int *a; int *b;
  int addr = (int)a;
  long base; int *dp = (int*)base;
  if (sizeof(int) == sizeof(void*)) { }
  unsigned int usiz; void *um = malloc(usiz);
  int idiff; idiff = a - b;
  int n; int *fwd = a + n;
  unsigned int un; int *bwd = a - un;
  int arr[10]; int idx; int el = arr[idx];
  int nm; int mm; char *p1 = malloc(nm * mm);
  char *p2 = malloc(nm + mm);
  int sz; sz = sizeof(void*);
  char *magic = malloc(4);
  int *hc = (int*)0xDEAD;
  long masked = (long)(a & 0xFFFFFFFF);
  unsigned long usz; usz = 128;
  int flags; int packed = (int)(a | flags);
  long shifted = a << n;
  int bits = (int)(a >> 32);
  int *start; int *end;
  for (int i = 0; i < (end - start); i++) { }
  if (a < 4096) { }
  int off; fseek(0, off, 0);
  printf("%d", a);
  long lng; printf("%ld", lng);
  __asm__("movl %eax, %ebx");
  if (sizeof(int) == 4) { }
  _mm_set1_ps(0);
  int **pp; FILE *f; fwrite(pp, sizeof(*pp), 1, f);
  struct Mixed r; fwrite(&r, sizeof(r), 1, f);
  int sock; send(sock, pp, sizeof(*pp), 0);
  int sfd = shm_open("/test", 0, 0);
  void *msz = malloc(32);
}