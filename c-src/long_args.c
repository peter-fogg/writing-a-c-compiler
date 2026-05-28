int test(long a, long b, int c, int d, int e, int f, int g, int h, long i) {
  return 0;
}

int main(void) {
  return test(4294967296l, 4294967297l, 1, 2, 3, 4, 5, 6, 4294967298l);
}
