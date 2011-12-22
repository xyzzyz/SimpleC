void test(int foo) {
  let (int x = 32; int y = 2) {
  if(foo+2) {
    x = 3;
  }
  for(x=0; x<10; x++) {
    printf("x %d\n", x);
  }
}
