int main(void) {
  int x = 10;
  while (1) {
    x = x - 1;
    for (int i = 0; i < 10; i++) {
      if (i > 10)
        break;
    }
    if (x < 10)
      break;
  }
  return 0;
}
