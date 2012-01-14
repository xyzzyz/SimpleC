extern void IO::print_string(*char x);

void main() {
  let (*char x;) {
    x = "Hello, World!\n";
    IO::print_string(x);
  }
  return;
};
