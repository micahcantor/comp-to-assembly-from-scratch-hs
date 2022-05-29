function main() {
  assert(1);
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}