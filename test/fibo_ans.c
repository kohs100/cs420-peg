#include <stdio.h>
long fibo_repeat(long n)
{
  printf("fibo_repeat(%d)\n", n);
  if (n <= 1)
    return n;
  else
  {
    long result = 0;
    long iterA = 0, iterB = 1, i;

    for (i = 2; i <= n; i++)
    {
      result = iterA + iterB;
      iterA = iterB;
      iterB = result;
    }

    return result;
  }
}

long fibo_recur(long n)
{
  printf("fibo_recur(%d)\n", n);
  if (n <= 1)
    return n;
  else
    return fibo_recur(n - 2) + fibo_recur(n - 1);
}

int main()
{
  printf("main()\n");
  long rep = 0, rec = 0;

  rep = fibo_repeat(6);
  rec = fibo_recur(6);

  printf("%d, %d\n", rep, rec);
  return 0;
}