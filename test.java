var x = 0;
while (x < 10) {
  x = x - 1;
  if (x == -1)
    continue;
  else 
    x = x + 2;
}
return x;