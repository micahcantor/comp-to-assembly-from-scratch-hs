function main() {
    assert(factorial(5) == 120);
}

function factorial(n) {
    var result = 1;
    while (n != 1) {
        result = result * n;
        n = n - 1;
    }
    return result;
}

function assert(x) {
    if (x) {
        putchar(46);
    } else {
        putchar(70);
    }
}