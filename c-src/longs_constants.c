long x = 5l;

int mul(void) {
  x = x * 4294967290l;
  return (x == 21474836450l);
}

int main(void) {
  if (!mul()) {
    return 1;
  } else {
    return 0;
  }
}
