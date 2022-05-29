function main() {
  assert(!null);
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}