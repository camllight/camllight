#include <math.h>
#include <stdio.h>
#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "debugger.h"
#include "mlvalues.h"

#ifdef ALIGN_DOUBLE

double Double_val(val)
     value val;
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.v[0] = Field(val, 0);
  buffer.v[1] = Field(val, 1);
  return buffer.d;
}

void Store_double_val(val, dbl)
     value val;
     double dbl;
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.d = dbl;
  Field(val, 0) = buffer.v[0];
  Field(val, 1) = buffer.v[1];
}

#endif

value format_float(fmt, arg)    /* ML */
     value fmt, arg;
{
  char format_buffer[64];
  int prec, i;
  char * p;
  char * dest;
  value res;

  prec = 64;
  for (p = String_val(fmt); *p != 0; p++) {
    if (*p >= '0' && *p <= '9') {
      i = atoi(p) + 15;
      if (i > prec) prec = i;
      break;
    }
  }
  for( ; *p != 0; p++) {
    if (*p == '.') {
      i = atoi(p+1) + 15;
      if (i > prec) prec = i;
      break;
    }
  }
  if (prec <= sizeof(format_buffer)) {
    dest = format_buffer;
  } else {
    dest = stat_alloc(prec);
  }
  sprintf(dest, String_val(fmt), Double_val(arg));
  res = copy_string(dest);
  if (dest != format_buffer) {
    stat_free(dest);
  }
  return res;
}

value float_of_string(s)        /* ML */
     value s;
{
  extern double atof();
  return copy_double(atof(String_val(s)));
}

value exp_float(f)              /* ML */
     value f;
{
  return copy_double(exp(Double_val(f)));
}

value log_float(f)              /* ML */
     value f;
{
  return copy_double(log(Double_val(f)));
}

value sqrt_float(f)             /* ML */
     value f;
{
  return copy_double(sqrt(Double_val(f)));
}

value power_float(f, g)         /* ML */
     value f, g;
{
  return copy_double(pow(Double_val(f), Double_val(g)));
}

value sin_float(f)              /* ML */
     value f;
{
  return copy_double(sin(Double_val(f)));
}

value cos_float(f)              /* ML */
     value f;
{
  return copy_double(cos(Double_val(f)));
}

value tan_float(f)              /* ML */
     value f;
{
  return copy_double(tan(Double_val(f)));
}

value asin_float(f)             /* ML */
     value f;
{
  return copy_double(asin(Double_val(f)));
}

value acos_float(f)             /* ML */
     value f;
{
  return copy_double(acos(Double_val(f)));
}

value atan_float(f)             /* ML */
     value f;
{
  return copy_double(atan(Double_val(f)));
}

value atan2_float(f, g)        /* ML */
     value f, g;
{
  return copy_double(atan2(Double_val(f), Double_val(g)));
}
