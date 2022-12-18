#include <stdlib.h>
#include <stdio.h>

int main()
{
  printf("%d %d %d\n", (int) sizeof(int), (int) sizeof(long), (int) sizeof(long *));
  return 0;
}
