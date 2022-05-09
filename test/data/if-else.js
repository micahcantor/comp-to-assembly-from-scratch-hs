function main() {
  if (true) {
    assert(1);
  } else {
    assert(0);
  }
  
  if (false) {
    assert(0);
  } else {
    assert(1);
  }
}