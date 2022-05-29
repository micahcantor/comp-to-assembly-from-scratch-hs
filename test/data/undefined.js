function main() {
  assert(!undefined);
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}