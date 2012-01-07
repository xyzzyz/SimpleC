int test(int n) {
  let (*int p = &n;) {
    let (**int pp = &p;) {
      *(*pp) = 123;
      return n + *(*pp);
    }
  }
};
