var b = 7;

function fib(a) {
  if (a <= 1) {
    return a;
  }
  return fib(a - 1) + fib(a - 2);
}

function main() {
  var a = 10;
  if (a < 0) {
    return -1;
  }
  return fib(b);
}