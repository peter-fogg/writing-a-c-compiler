int transform_bits(int n, int mask) {
  int result;
  int i;
  int count;

  result = 0;
  count = 0;

  while (count < 5) {
    result += (n & mask);
    n = n >> 1;
    count++;
  }

  do {
    --result;
    mask++;
  } while (mask < 0);

  if ((result ^ mask) != 0) {
    result = ~result;
  } else {
    result = result | 15;
  }

  return result;
}

int main(void) {
  int a;
  int b;
  int c;
  int i;

  a = 10;
  b = 20;
  c = 0;

  for (i = 0; i < 10; i = i + 1) {
    a = (a * 2) / (i + 1);

    switch (i % 3) {
    case 0:
      a |= 1;
      break;
    case 1:
      a &= ~2;
      break;
    case 2:
      a ^= i;
      break;
    default:
      a = 0;
    }
  }

  if (a > b) {
    goto error_state;
  } else {
    c = transform_bits(a, b);
  }

  c = (a << 2) + (b >> 1) * (a % 3) - (~b);

start_loop:
  if (c < 100) {
    c *= 2;
    if (c == 42) {
      goto exit_label;
    }
    goto start_loop;
  }

error_state:
  a = -1;

exit_label:
  return a + b + c;
}
