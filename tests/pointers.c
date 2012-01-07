int test(int n) {
  let (*int p = &n;) {
    *p = 123;
    return n;
  }
};
