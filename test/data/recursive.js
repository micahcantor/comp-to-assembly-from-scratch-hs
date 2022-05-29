function main() {
  assert(factorial(5) == 120);
}

function factorial(n) {
  if (n == 1) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}