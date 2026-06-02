long signed l;
long l = 7;
int long l;
signed long int l;
int main(void) {
  extern signed long l;
  if (l != 7) {
    return 1;
  }
  return 0;
}
