function main() {
  assert(return42() == 42);
  assert(returnTrue());
  assert(!returnFalse());
  assert(!returnNull());
  assert(!returnNothing());
}

function return42() {
  return 42;
}

function returnTrue() {
  return true;
}

function returnFalse() {
  return false;
}

function returnNull() {
  return null;
}

function returnNothing() {}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}