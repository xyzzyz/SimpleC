int fib(int n) {
  if(n == 0) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}
