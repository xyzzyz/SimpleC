

int gcd_rec(int a, int b){
  let (int c;){
    c=a-(a/b)*b;
    if(c==0){
      return b;
    }
    else{
      return gcd_rec(b,c);
    }
  }   
}

int gcd_iter(int a, int b){
  let(int c=1;){
    while(c!=0){
      c=a-(a/b)*b;
      if(c!=0){
        a=b;
        b=c;
      }
    }
    return b;
  }
}
