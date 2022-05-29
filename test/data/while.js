function main() {
  var i = 0;
  while (i != 3) {
    i = i + 1;
  }
  assert(i == 3);
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}