extern void IO::print_int(int n);

int test(int x) {
  x = x+5;
  IO::print_int(x);
  return x;
};
