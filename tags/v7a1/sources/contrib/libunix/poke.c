#include <mlvalues.h>
#include <sys/types.h>

value poke_short(str, ofs, num)
     value str, ofs, num;
{
  *((short *) &Byte(str, Long_val(ofs))) = Long_val(num);
  return Atom(0);
}

value poke_int(str, ofs, num)
     value str, ofs, num;
{
  *((int *) &Byte(str, Long_val(ofs))) = Long_val(num);
  return Atom(0);
}

value poke_long(str, ofs, num)
     value str, ofs, num;
{
  *((long *) &Byte(str, Long_val(ofs))) = Long_val(num);
  return Atom(0);
}

#ifdef HAS_SOCKETS

#include <sys/types.h>
#include <netinet/in.h>

value poke_nshort(str, ofs, num)
     value str, ofs, num;
{
  *((unsigned short *) &Byte(str, Long_val(ofs))) = htons(Long_val(num));
  return Atom(0);
}

value poke_nlong(str, ofs, num)
     value str, ofs, num;
{
  *((unsigned long *) &Byte(str, Long_val(ofs))) = htonl(Long_val(num));
  return Atom(0);
}

#else

value poke_nshort() { invalid_argument("poke_nshort not implemented"); }

value poke_nlong() { invalid_argument("poke_nlong not implemented"); }

#endif
