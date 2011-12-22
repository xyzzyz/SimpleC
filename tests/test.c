void test(int foo) {
  let (int x = 32;
       int y = 2;
       char z = 'd';) {
    if(foo < 3) {
      x = 3;
    }
    for(x=0; x<10; x++) {
      test(y);
    }
  }
};
