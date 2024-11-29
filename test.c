#include <stdio.h>
#include <stdbool.h>

char fa = 3;

int testfunc1(int asdf, int fdsa)
{
  return asdf + fdsa;
}

int testfunc2(int a)
{
  return 3;
}

char *cptr_func(int, char *b)
{
  return b;
};

char cval_func(char, int *)
{
  return '3';
}

char cval_func(char a, int *);
char cval_func(char a, int *);

void test_asdf()
{
  char *a[2];
  char b[3];
  char(*c)[2];

  unsigned char asdf;

  // char d();
  // char (*e)() = d;
  char (*f)(char, int *) = cval_func;
  // char (*fov)(char, char *) = cval_func;
  char (**g)(char, int *) = &f;
  char *(*h)(int, char *) = cptr_func;
}

void test_ptr()
{
  char *a[123][234];
  char b[123][321];
  char c[123][562];

  if (b[2] < c[1])
  {
    printf("asdf\n");
  }

  printf("a %d\n", sizeof a);
  printf("a[0] %d\n", sizeof a[0]);
  printf("a[1][1] %d\n", sizeof a[1][1]);

  printf("b %d\n", sizeof b);
  printf("b-1 %d\n", sizeof(b + 1));
  printf("*(b-1) %d\n", sizeof *((b - 1) + 2));
  printf("b[0] %d\n", sizeof b[0]);
  printf("b[0]+1 %d\n", sizeof(b[0] + 1));
  printf("b[1][1] %d\n", sizeof b[1][1]);

  b[3][1] = '2';
  *(b[3] + 1) = '3';

  bool btest = true;
  char *cptr = b[2];

  if (cptr + btest < cptr)
  {
    printf("1<2\n");
  }
  else
  {
    printf("1>=2\n");
  }
}

void test_void()
{
  return test_ptr();
}

int main()
{
  test_ptr();
  test_void();

  float fval = .3;
  printf("%f\n", ++fval);

  char test1 = 3;
  char test2 = 4;
  // (fval == 3 ? test1 : test2) = 2;
  // (test1, test2) = 2;

  char *ptr1 = &test1;
  char *ptr2 = &test2;

  char *ptr3 = 1 + ptr1;
  char **ptr4 = &ptr3;
  char ***ptr5 = &ptr4;

  void *vptr = ptr5;

  char parr[3] = "12";

  bool btest = false;
  btest = true;
  printf("%d\n", 3 * btest);

  int asdf = 3;
  asdf = asdf + -btest;
  printf("asdf %d\n", asdf);

  if (testfunc1)
  {
    printf("asdf\n");
  }
}