int f(int x) {
  static int i = 0;
  return i++ + x;
}

int main(void) { return f(1) + f(2); }
