function main() {
  var x = 4 + 2 * (12 - 2);
  var y = 3 * (5 + 1);
  var z = x + y;
  assert(z == 42);
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}