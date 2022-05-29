function main() {
  var a = 1;
  assert(a == 1);
  a = 0;
  assert(a == 0);
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}