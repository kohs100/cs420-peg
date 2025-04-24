int a, b = 0;

float avg(int count, int *value)
{
  int total, i;
  int sum = 0;
  sum = sum + 1;
  for (i = 1; i < count; i++)
  {
    total = total + value[i];
  }
  return (total / count);
}

int main()
{
  int studentNumber, count, i, sum;
  int mark[4];
  float average;
  count = 4;
  sum = 0;
  for (i = 1; i < count; i++)
  {
    mark[i] = i * 30;
    sum = sum + mark[i];
    average = avg(i + 1, mark);
    if (average > 40)
    {
      printf("%f", average);
    }
  }
  return 0;
}