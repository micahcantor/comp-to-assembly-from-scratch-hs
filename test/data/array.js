function main() {
  var arr = [1, 2, 3];
  assert(arr[0] == 1);
  assert(arr[1] == 2);
  assert(arr[2] == 3);
  assert(length(arr) == 3);
}

function assert(x) {
  if (x) {
      putchar(46);
  } else {
      putchar(70);
  }
}