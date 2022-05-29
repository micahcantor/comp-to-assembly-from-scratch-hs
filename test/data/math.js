function main() {
  assert(2 + 2 == 4);
  assert(4 * 2 == 8);
  assert(4 - 2 == 2);
  assert(42 == 4 + 2 * (12 - 2) + 3 * (5 + 1));
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}