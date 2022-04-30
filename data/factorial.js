function factorial(n) {
    var result = 1;
    while (n) {
        result = result * n;
        n = n - 1;
    }
    return result;
}