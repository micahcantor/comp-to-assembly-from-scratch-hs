function main() {
  assert(true);
  assert(!false);
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}