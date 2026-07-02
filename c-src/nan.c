int main(void) {
  int nan_nonzero = 0;
  double nan = 0.0 / 0.0;
  do {
    if (nan_nonzero) {
      return 0;
    }
    nan_nonzero += 1;
  } while (nan);
  return 1;
}
