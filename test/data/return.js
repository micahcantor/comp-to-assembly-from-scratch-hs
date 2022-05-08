function main() {
  assert(return42() == 42);
  assert(!returnNothing());
}

function return42() {
  return 42;
}

function returnNothing() {}

