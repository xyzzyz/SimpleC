extern void IO::print_string(*char x);
extern void IO::print_char(char c);

void main() {
  let (*char x; char c;) {
    c = 'q';
    //IO::print_char(c);
    x = "Hello, World!\n";
    x[0] = c;
    IO::print_string(x);
  }
  return;
};
