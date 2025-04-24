void strncpy(char *dst, char *src, long n)
{
  long cnt = 0;

  for (; *src && cnt < n; dst++, src++, cnt++)
  {
    *dst = *src;
  }
}

long strlen(char *str)
{
  long cnt = 0;
  for (; *str; cnt++, str++)
    ;
  return cnt;
}

int main()
{
  char *str1 = "String literal";
  char str2[124];
  char *str3, i;

  strncpy(str2, str1, 123);
  printf("%d, %d", strlen(str1), strlen(str2));
  return 0;
}