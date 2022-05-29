function main() {
  assert(42 == 42);
  assert(!(42 == 0));
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}