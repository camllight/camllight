#include <mlvalues.h>
#include <alloc.h>

value peek_short(str, ofs)
     value str, ofs;
{
  return Val_long(*((short *) &Byte(str, Long_val(ofs))));
}

value peek_ushort(str, ofs)
     value str, ofs;
{
  return Val_long(*((unsigned short *) &Byte(str, Long_val(ofs))));
}

value peek_int(str, ofs)
     value str, ofs;
{
  return Val_long(*((int *) &Byte(str, Long_val(ofs))));
}

value peek_uint(str, ofs)
     value str, ofs;
{
  return Val_long(*((unsigned int *) &Byte(str, Long_val(ofs))));
}

value peek_long(str, ofs)
     value str, ofs;
{
  return Val_long(*((long *) &Byte(str, Long_val(ofs))));
}

value peek_ulong(str, ofs)
     value str, ofs;
{
  return Val_long(*((unsigned long *) &Byte(str, Long_val(ofs))));
}

#ifdef HAS_SOCKETS

#include <sys/types.h>
#include <netinet/in.h>

value peek_nshort(str, ofs)
     value str, ofs;
{
  return Val_long(ntohs(*((unsigned short *) &Byte(str, Long_val(ofs)))));
}

value peek_nlong(str, ofs)
     value str, ofs;
{
  return Val_long(ntohs(*((unsigned long *) &Byte(str, Long_val(ofs)))));
}

#else

value peek_nshort() { invalid_argument("peek_nshort not implemented"); }

value peek_nlong() { invalid_argument("peek_nlong not implemented"); }

#endif

value peek_cstring(str, ofs)
     value str, ofs;
{
  return copy_string(&Byte(str, Long_val(ofs)));
}
