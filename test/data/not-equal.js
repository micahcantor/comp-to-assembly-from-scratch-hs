function main() {
  assert(0 != 42);
  assert(!(42 != 42));
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}