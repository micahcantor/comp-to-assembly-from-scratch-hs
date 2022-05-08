function main() {
  assert42(42);
  assert1234(1, 2, 3, 4);
}

function assert42(x) {
  assert(x == 42);
}
function assert1234(a, b, c, d) {
  assert(a == 1);
  assert(b == 2);
  assert(c == 3);
  assert(d == 4);
}