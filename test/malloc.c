int a = 3, c = 2;

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
  char *str2, *str1 = "String literal";
  unsigned str_len;

  str_len = strlen(str1);
  str2 = malloc(str_len + 1);

  strncpy(str2, str1, str_len + 1);

  free(str2);

  return 0;
}