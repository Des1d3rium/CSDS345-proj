function fib(a) {
  if (a <= 1) {
    return a;
  }
  return fib(a - 1) + fib(a - 2);
}

function main() {
  return fib(10);
}