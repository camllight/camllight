#include <math.h>
#include <stdio.h>
#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"

#ifdef CAML_ALIGN_DOUBLE

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
#define MAX_DIGITS 350
/* Max number of decimal digits in a "natural" (not artificially padded)
   representation of a float. Can be quite big for %f format.
   Max exponent for IEEE format is 308 decimal digits.
   Rounded up for good measure. */
  char format_buffer[MAX_DIGITS + 20];
  int prec, i;
  char * p;
  char * dest;
  value res;

  prec = MAX_DIGITS;
  for (p = String_val(fmt); *p != 0; p++) {
    if (*p >= '0' && *p <= '9') {
      i = atoi(p) + MAX_DIGITS;
      if (i > prec) prec = i;
      break;
    }
  }
  for( ; *p != 0; p++) {
    if (*p == '.') {
      i = atoi(p+1) + MAX_DIGITS;
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
  return copy_double(atof(String_val(s)));
}

value exp_float(f)              /* ML */
     value f;
{
  return copy_double(exp(Double_val(f)));
}

value fabs_float(f)              /* ML */
     value f;
{
  return copy_double(fabs(Double_val(f)));
}

value floor_float(f)              /* ML */
     value f;
{
  return copy_double(floor(Double_val(f)));
}

value fmod_float(f1, f2)              /* ML */
     value f1, f2;
{
  return copy_double(fmod(Double_val(f1), Double_val(f2)));
}

value frexp_float(f)              /* ML */
     value f;
{
  int i;
  value res;
  Push_roots(r, 1);

  r[0] = copy_double(frexp (Double_val(f), &i));
  res = alloc_tuple(2);
  Field(res, 0) = r[0];
  Field(res, 1) = Val_int(i);
  Pop_roots();
  return res;
}

value ldexp_float(f, i)              /* ML */
     value f, i;
{
  return copy_double(ldexp(Double_val(f), Int_val(i)));
}

value log_float(f)              /* ML */
     value f;
{
  return copy_double(log(Double_val(f)));
}

value log10_float(f)              /* ML */
     value f;
{
  return copy_double(log10(Double_val(f)));
}

value modf_float(f)              /* ML */
     value f;
{
  double fres;
  value res;
  Push_roots(r, 2);

  r[0] = copy_double(modf (Double_val(f), &fres));
  r[1] = copy_double(fres);

  res = alloc_tuple(2);
  Field(res, 0) = r[0];
  Field(res, 1) = r[1];
  Pop_roots();
  return res;
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

value sinh_float(f)              /* ML */
     value f;
{
  return copy_double(sinh(Double_val(f)));
}

value cos_float(f)              /* ML */
     value f;
{
  return copy_double(cos(Double_val(f)));
}

value cosh_float(f)              /* ML */
     value f;
{
  return copy_double(cosh(Double_val(f)));
}

value tan_float(f)              /* ML */
     value f;
{
  return copy_double(tan(Double_val(f)));
}

value tanh_float(f)              /* ML */
     value f;
{
  return copy_double(tanh(Double_val(f)));
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

value ceil_float(f)              /* ML */
     value f;
{
  return copy_double(ceil(Double_val(f)));
}
