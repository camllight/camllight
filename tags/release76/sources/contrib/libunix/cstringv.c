#include <mlvalues.h>
#include <memory.h>
#include "unix.h"

char ** cstringvect(arg)
     value arg;
{
  char ** res;
  mlsize_t size, i;

  size = Wosize_val(arg);
  res = (char **) stat_alloc((size + 1) * sizeof(char *));
  for (i = 0; i < size; i++) res[i] = String_val(Field(arg, i));
  res[size] = NULL;
  return res;
}

  
