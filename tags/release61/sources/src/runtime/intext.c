/* structured input/output */

#include "debugger.h"
#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "mlvalues.h"
#include "reverse.h"

typedef unsigned long offset_t;

struct extern_obj {
  value obj;
  offset_t ofs;
};

static offset_t * extern_block;
static asize_t extern_size, extern_pos;
static struct extern_obj * extern_table;
static asize_t extern_table_size, extern_table_used;

#define Hash(v) (((asize_t) ((v) >> 2)) % extern_table_size)

#define Base_magic_number 0x8495A6B9
#define Big_endian_32_magic_number Base_magic_number
#define Little_endian_32_magic_number (Base_magic_number + 1)
#define Big_endian_64_magic_number (Base_magic_number + 2)
#define Little_endian_64_magic_number (Base_magic_number + 3)
#define First_valid_magic_number Base_magic_number
#define Last_valid_magic_number (Base_magic_number + 3)

#ifdef SIXTYFOUR
# ifdef BIG_ENDIAN
#  define Extern_magic_number Big_endian_64_magic_number
# else
#  define Extern_magic_number Little_endian_64_magic_number
# endif
#else
# ifdef BIG_ENDIAN
#  define Extern_magic_number Big_endian_32_magic_number
# else
#  define Extern_magic_number Little_endian_32_magic_number
# endif
#endif

static void alloc_table()
{
  asize_t i;

  extern_table = (struct extern_obj *)
    stat_alloc(extern_table_size * sizeof(struct extern_obj));
  for (i = 0; i < extern_table_size; i++)
    extern_table[i].obj = 0;
}

static void extern_too_big()
{
  stat_free((char *) extern_block);
  stat_free((char *) extern_table);
  failwith("extern: object too big");
}

static void resize_result()
{
  extern_size = 2 * extern_size;
#ifdef MAX_MALLOC_SIZE
  if (extern_size > MAX_MALLOC_SIZE / sizeof(unsigned long))
    extern_too_big();
#endif
  extern_block = (offset_t *)
    stat_resize((char *) extern_block, extern_size * sizeof(offset_t));
}

static void resize_table()
{
  asize_t oldsize;
  struct extern_obj * oldtable;
  asize_t i, h;

  oldsize = extern_table_size;
  oldtable = extern_table;
  extern_table_size = 2 * extern_table_size;
#ifdef MAX_MALLOC_SIZE
  if (extern_table_size > MAX_MALLOC_SIZE / sizeof(struct extern_obj))
    extern_too_big();
#endif
  alloc_table();
  for (i = 0; i < oldsize; i++) {
    h = Hash(oldtable[i].obj);
    while (extern_table[h].obj != 0) {
      h++;
      if (h >= extern_table_size) h = 0;
    }
    extern_table[h].obj = oldtable[i].obj;
    extern_table[h].ofs = oldtable[i].ofs;
  }
  stat_free((char *) oldtable);
}

static offset_t emit(v)
     value v;
{
  mlsize_t size;
  asize_t h;
  offset_t res;
  value * p;
  offset_t * q;
  asize_t end_pos;

  if (Is_long(v)) return (offset_t) v;
  size = Wosize_val(v);
  if (size == 0) return (Tag_val(v) << 2) + 2;
  if (2 * extern_table_used >= extern_table_size) resize_table();
  h = Hash(v);
  while (extern_table[h].obj != 0) {
    if (extern_table[h].obj == v) return extern_table[h].ofs;
    h++;
    if (h >= extern_table_size) h = 0;
  }
  end_pos = extern_pos + 1 + size;
  while (end_pos >= extern_size) resize_result();
  extern_block[extern_pos++] = Hd_val(v);
  res = extern_pos * sizeof(offset_t);
  extern_table[h].obj = v;
  extern_table[h].ofs = res;
  extern_table_used++;
  for (p = &Field(v, 0), q = &extern_block[extern_pos]; size > 0; size--) {
    *q++ = *p++;
  }
  extern_pos = end_pos;
  return res;
}

static offset_t emit_all(root)
     value root;
{
  asize_t read_pos;
  offset_t res;
  header_t hd;
  mlsize_t sz;
  offset_t ofs;

  read_pos = extern_pos;
  res = emit(root);
  while (read_pos < extern_pos) {
    hd = (header_t) extern_block[read_pos++];
    sz = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case String_tag:
    case Double_tag:
      read_pos += sz;
      break;
    case Abstract_tag:
    case Final_tag:
      invalid_argument("extern: abstract value");
      break;
    case Closure_tag:
      invalid_argument("extern: functional value");
      break;
    default:
      while (sz > 0) {
        ofs = emit((value) extern_block[read_pos]);
        extern_block[read_pos] = ofs;
        read_pos++;
        sz--;
      }
      break;
    }
  }
  return res;
}

#ifndef INITIAL_EXTERN_SIZE
#define INITIAL_EXTERN_SIZE 4096
#endif
#ifndef INITIAL_EXTERN_TABLE_SIZE
#define INITIAL_EXTERN_TABLE_SIZE 2039
#endif

value extern_val(chan, v)       /* ML */
     struct channel * chan;
     value v;
{
  offset_t res;

  extern_size = INITIAL_EXTERN_SIZE;

  extern_block =
    (offset_t *) stat_alloc(extern_size * sizeof(unsigned long));
  extern_pos = 0;
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  alloc_table();
  extern_table_used = 0;
  res = emit_all(v);
  if (extern_pos >= Max_wosize) extern_too_big();
  stat_free((char *) extern_table);
  putword(chan, Extern_magic_number);
  putword(chan, extern_pos);
  if (extern_pos == 0)
    putword(chan, res);
  else
    putblock(chan, (char *) extern_block, extern_pos * sizeof(unsigned long));
  stat_free((char *) extern_block);
  return Atom(0);
}

void adjust_pointers(start, size, color)
     value *start;
     mlsize_t size;
     color_t color;
{
  value * p, * q;
  mlsize_t sz;
  header_t hd;
  tag_t tag;
  value v;
  mlsize_t bosize;

  p = start;
  q = p + size;
  bosize = Bsize_wsize(size);
  while (p < q) {
    hd = *p;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    *p++ = Make_header(sz, tag, color);
    if (tag >= No_scan_tag)
      p += sz;
    else
      for( ; sz > 0; sz--, p++) {
        v = *p;
        switch(v & 3) {
        case 0:                 /* 0 -> A bloc represented by its offset. */
          Assert(v >= 0 && v <= bosize && (v & 3) == 0);
          *p = (value) ((offset_t) start + v);
          break;
        case 2:                 /* 2 -> An atom. */
          v = v >> 2;
          Assert(v >= 0 && v < 256);
          *p = Atom(v);
          break;
        default:                /* 1 or 3 -> An integer. */
          break;
        }
      }
  }
}

/* Reverse all words in a block, in case of endianness clash.
   Works with words of the natural word size. */

void rev_pointers(p, size)
     value *p;
     mlsize_t size;
{
  value * q;
  header_t hd;
  mlsize_t n;

  q = p + size;
  while (p < q) {
    Reverse_word(p);
    hd = (header_t) *p++;
    n = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case Abstract_tag:
    case Final_tag:
      Assert (0);       /* Should not happen. Fall through for compatibility */
    case String_tag:
      p += n;
      break;
    case Double_tag:
      Reverse_double(p);
      p += n;
      break;
    default:
      for( ; n > 0; n--, p++) {
        Reverse_word(p);
      }
    }
  }
}

#ifdef SIXTYFOUR

/* Routines to convert 32-bit externed objects to 64-bit memory blocks. */

typedef int32 value32;

/* Reverse all words in a block, in case of endianness clash.
   Works with 32-bit words. */

void rev_pointers_32(p, size)
     value32 * p;
     mlsize_t size;
{
  value32 * q;
  header_t hd;
  mlsize_t n;

  q = p + size;
  while (p < q) {
    Reverse_int32(p);
    hd = (header_t) *p++;
    n = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case Abstract_tag:
    case Final_tag:
      Assert (0);       /* Should not happen. Fall through for compatibility */
    case String_tag:
      p += n;
      break;
    case Double_tag:
      Reverse_double(p);
      p += n;
      break;
    default:
      for( ; n > 0; n--, p++) {
        Reverse_int32(p);
      }
    }
  }
}

/* Compute the size of the expansion of a 32-bit externed block to a
   64-bit block. The size is returned in 64-bit words. */

static mlsize_t size_after_expansion(p, len)
     value32 * p;
     mlsize_t len;              /* length in 32-bit words */
{
  mlsize_t res;
  value32 * q;
  header_t hd;
  mlsize_t n;

  for (q = p + len, res = 0; p < q; /*nothing*/) {
    hd = (header_t) *p++;
    res++;
    n = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case String_tag:            /* round to the next 64-bit word */
      res += (n * sizeof(value32) + sizeof(value) - 1) / sizeof(value);
      break;
    case Double_tag:
      res += sizeof(double) / sizeof(value);
      break;
    case Abstract_tag:
    case Final_tag:
      Assert(0);                /* should not happen. */
      break;
    default:
      res += n;                 /* all fields will be extended 32 -> 64 */
      break;
    }
    p += n;
  }
  return res;
}

/* Convert a 32-bit externed block to a 64-bit block. The resulting block
   is a valid 64-bit object. */

static void expand_block(source, dest, source_len, dest_len, color)
     value32 * source;
     value * dest;
     mlsize_t source_len, dest_len;
     color_t color;
{
  value32 * p, * q;
  value * d, * e;
  header_t hd;
  mlsize_t sz;
  tag_t tag;
  uint32 * forward_addr;
  uint32 dest_ofs;
  value v;

  /* First pass: copy the objects and set up forwarding pointers.
     The pointers contained inside blocks are not resolved. */

  for (p = source, q = source + source_len, d = dest; p < q; /*nothing*/) {
    hd = (header_t) *p++;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    forward_addr = (uint32 *) p;
    dest_ofs = d + 1 - dest;
    switch(tag) {
    case String_tag:
      { mlsize_t ofs_last_byte, len, new_sz;
        ofs_last_byte = sz * sizeof(value32) - 1;
        len = ofs_last_byte - Byte(p, ofs_last_byte);
        new_sz = (sz * sizeof(value32) + sizeof(value) - 1) / sizeof(value);
        *d++ = Make_header(new_sz, String_tag, color);
        Field(d, new_sz - 1) = 0;
        bcopy(p, d, len);
        ofs_last_byte = new_sz * sizeof(value) - 1;
        Byte(d, ofs_last_byte) = ofs_last_byte - len;
        p += sz;
        d += new_sz;
        break;
      }
    case Double_tag:
      *d++ = Make_header(Double_wosize, Double_tag, color);
      /* Cannot do *((double *) d) = *((double *) p) directly
         because p might not be 64-aligned. */
      Assert(sizeof(double) == sizeof(value));
      ((value32 *) d)[0] = p[0];
      ((value32 *) d)[1] = p[1];
      p += sizeof(double) / sizeof(value32);
      d += 1;
      break;
    case Abstract_tag:
    case Final_tag:
      Assert(0);
    default:
      *d++ = Make_header(sz, tag, color);
      for (/*nothing*/; sz > 0; sz--, p++, d++) {
        if ((*p & 1) == 0) {
          *d = *((uint32 *) p);         /* copy, zero expansion */
        } else {
          *d = *((int32 *) p);          /* copy, sign expansion */
        }
      }
      break;
    }
    *forward_addr = dest_ofs;   /* store the forwarding pointer */
  }
  Assert(d == dest + dest_len);

  /* Second pass: resolve pointers contained inside blocks,
     replacing them by the corresponding forwarding pointer. */

  for (d = dest, e = dest + dest_len; d < e; /*nothing*/) {
    hd = (header_t) *d++;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    if (tag >= No_scan_tag) {
      d += sz;
    } else {
      for (/*nothing*/; sz > 0; sz--, d++) {
        v = *d;
        switch(v & 3) {
        case 0:                 /* 0: a block represented by its offset */
          Assert(v >= 0 && v < source_len * sizeof(value32) && (v & 3) == 0);
          *d = (value) (dest + *((uint32 *)((char *) source + v)));
          break;
        case 2:                 /* 2: an atom */
          v = v >> 2;
          Assert(v >= 0 && v < 256);
          *d = Atom(v);
          break;
        default:                /* 1 or 3: an integer */
          break;
        }
      }
    }
  }
}

#else /* !SIXTYFOUR */

#ifndef NO_SIXTYFOUR_INTERN

/* Routines to convert 64-bit externed objects to 32-bit memory blocks. */

typedef double value64;         /* Should work on just about any machine */

#ifdef BIG_ENDIAN
#define MSword(p) (((value*) p)[0])
#define LSword(p) (((value*) p)[1])
#else
#define MSword(p) (((value *) p)[1])
#define LSword(p) (((value *) p)[0])
#endif

/* Reverse all words in a block, in case of endianness clash.
   Works with 64-bit words.
   Returns (-1) if a header too large is encountered, 0 otherwise. */

int rev_pointers_64(p, size)
     value64 * p;
     mlsize_t size;             /* size in 64-bit words */
{
  value64 * q;
  header_t hd;
  mlsize_t n;

  q = p + size;
  while (p < q) {
    Reverse_int64(p);
    hd = (header_t) LSword(p);
    if (MSword(p) != 0) return -1;
    p++;
    n = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case Abstract_tag:
    case Final_tag:
      Assert (0);       /* Should not happen. Fall through for compatibility */
    case String_tag:
      p += n;
      break;
    case Double_tag:
      Reverse_double(p);
      p += n;
      break;
    default:
      for( ; n > 0; n --, p++) {
        Reverse_int64(p);
      }
    }
  }
  return 0;
}

/* Compute the size of the shrinkage of a 64-bit externed block to a
   32-bit block. The size is returned in 32-bit words.
   Return 0 if a block cannot be shrunk because its size is too big. */

static mlsize_t size_after_shrinkage(p, len)
     value64 * p;
     mlsize_t len;              /* length in 64-bit words */
{
  mlsize_t res;
  value64 * q;
  header_t hd;
  mlsize_t n;

  for (q = p + len, res = 0; p < q; /*nothing*/) {
    hd = (header_t) LSword(p);
    if (MSword(p) != 0) return 0;
    p++;
    n = Wosize_hd(hd);
    res++;
    switch(Tag_hd(hd)) {
    case String_tag:
      { mlsize_t ofs_last_byte, len, new_sz;
        ofs_last_byte = n * sizeof(value64) - 1;
        len = ofs_last_byte - Byte(p, ofs_last_byte);
        new_sz = (len + sizeof(value)) / sizeof(value);
        res += new_sz;
        break;
      }
    case Double_tag:
      res += sizeof(double) / sizeof(value);
      break;
    case Abstract_tag:
    case Final_tag:
      Assert(0);                /* should not happen. */
      break;
    default:
      res += n;                 /* all fields will be shrunk 64 -> 32 */
      break;
    }
    p += n;
  }
  return res;
}

/* Convert a 64-bit externed block to a 32-bit block. The resulting block
   is a valid 32-bit object.
   Return -1 if the block cannot be shrunk because some integer literals
   or relative displacements are too large, 0 otherwise. */

static int shrink_block(source, dest, source_len, dest_len, color)
     value64 * source;
     value * dest;
     mlsize_t source_len, dest_len;
     color_t color;
{
  value64 * p, * q;
  value * d, * e;
  header_t hd;
  mlsize_t sz;
  tag_t tag;
  offset_t * forward_addr;
  offset_t dest_ofs;
  value v;

  /* First pass: copy the objects and set up forwarding pointers.
     The pointers contained inside blocks are not resolved. */

  for (p = source, q = source + source_len, d = dest; p < q; /*nothing*/) {
    hd = (header_t) LSword(p);
    p++;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    forward_addr = (offset_t *) p;
    dest_ofs = d + 1 - dest;
    switch(tag) {
    case String_tag:
      { mlsize_t ofs_last_byte, len, new_sz;
        ofs_last_byte = sz * sizeof(value64) - 1;
        len = ofs_last_byte - Byte(p, ofs_last_byte);
        new_sz = (len + sizeof(value)) / sizeof(value);
        *d++ = Make_header(new_sz, String_tag, color);
        Field(d, new_sz - 1) = 0;
        bcopy(p, d, len);
        ofs_last_byte = new_sz * sizeof(value) - 1;
        Byte(d, ofs_last_byte) = ofs_last_byte - len;
        p += sz;
        d += new_sz;
        break;
      }
    case Double_tag:
      *d++ = Make_header(Double_wosize, Double_tag, color);
      Store_double_val(d, Double_val(p));
      p += sizeof(double) / sizeof(value64);
      d += sizeof(double) / sizeof(value);
      break;
    case Abstract_tag:
    case Final_tag:
      Assert(0);
    default:
      *d++ = Make_header(sz, tag, color);
      for (/*nothing*/; sz > 0; sz--, p++, d++) {
        value lsw = LSword(p);
        value msw = MSword(p);
        if ((lsw & 1) == 0) {      /* If relative displacement: */
          if (msw != 0) return -1; /* Check unsigned displacement fits in 32 */
        } else {                   /* Otherwise, it's a signed integer */
          if ((lsw >= 0 && msw != 0) || (lsw < 0 && msw != -1)) return -1;
        }
        *d = lsw;
      }
    }
    *forward_addr = dest_ofs;   /* store the forwarding pointer */
  }
  Assert(d == dest + dest_len);

  /* Second pass: resolve pointers contained inside blocks,
     replacing them by the corresponding forwarding pointer. */

  for (d = dest, e = dest + dest_len; d < e; /*nothing*/) {
    hd = (header_t) *d++;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    if (tag >= No_scan_tag) {
      d += sz;
    } else {
      for (/*nothing*/; sz > 0; sz--, d++) {
        v = *d;
        switch(v & 3) {
        case 0:                 /* 0: a block represented by its offset */
          Assert(v >= 0 && v < source_len * sizeof(value64) && (v & 7) == 0);
          *d = (value) (dest + *((offset_t *)((char *) source + v)));
          break;
        case 2:                 /* 2: an atom */
          v = v >> 2;
          Assert(v >= 0 && v < 256);
          *d = Atom(v);
          break;
        default:                /* 1 or 3: an integer */
          break;
        }
      }
    }
  }
  return 0;
}

#endif /* NO_SIXTYFOUR_INTERN */
#endif /* SIXTYFOUR */

static int really_getblock(chan, p, n)
     struct channel * chan;
     char * p;
     unsigned long n;
{
  unsigned r;
  while (n > 0) {
    r = getblock(chan, p, (unsigned) n);
    if (r == 0) return 0;
    p += r;
    n -= r;
  }
  return 1;
}

value intern_val(chan)          /* ML */
     struct channel * chan;
{
  value res;
  unsigned long magic;
  mlsize_t whsize, wosize;
  unsigned long bhsize;
  color_t color;
  header_t hd;

  magic = getword(chan);
  if (magic < First_valid_magic_number && magic > Last_valid_magic_number)
    failwith("intern: bad object");
  whsize = getword(chan);
  if (whsize == 0) {
    res = (value) getword(chan);
    if (Is_long(res))
      return res;
    else
      return Atom(res >> 2);
  }
  bhsize = Bsize_wsize (whsize);
  wosize = Wosize_whsize (whsize);
#ifdef SIXTYFOUR
  if (magic == Little_endian_32_magic_number ||
      magic == Big_endian_32_magic_number) {
    /* Expansion 32 -> 64 required */
    mlsize_t whsize32;
    value32 * block;
    whsize32 = whsize;
    block = (value32 *) stat_alloc(whsize32 * sizeof(value32));
    if (really_getblock(chan, block, whsize32 * sizeof(value32)) == 0) {
      stat_free((char *) block);
      failwith ("intern : truncated object");
    }
#ifdef BIG_ENDIAN
    if (magic == Little_endian_32_magic_number)
      rev_pointers_32(block, whsize32);
#else
    if (magic == Big_endian_32_magic_number)
      rev_pointers_32(block, whsize32);
#endif
    whsize = size_after_expansion(block, whsize32);
    wosize = Wosize_whsize(whsize);
    res = alloc_shr(wosize, String_tag);
    hd = Hd_val (res);
    color = Color_hd (hd);
    Assert (color == White || color == Black);
    expand_block(block, Hp_val(res), whsize32, whsize, color);
    stat_free((char *) block);
  } else {
    /* Block has natural word size (64) */
    res = alloc_shr(wosize, String_tag);
    hd = Hd_val (res);
    color = Color_hd (hd);
    Assert (color == White || color == Black);
    if (really_getblock(chan, Hp_val(res), bhsize) == 0) {
      Hd_val (res) = hd;                      /* Avoid confusing the GC. */
      failwith ("intern : truncated object");
    }
#ifdef BIG_ENDIAN
    if (magic == Little_endian_64_magic_number)
      rev_pointers(Hp_val (res), whsize);
#else
    if (magic == Big_endian_64_magic_number)
      rev_pointers(Hp_val (res), whsize);
#endif
    adjust_pointers(Hp_val (res), whsize, color);
  }
#else /* !SIXTYFOUR */
  if (magic == Little_endian_64_magic_number ||
      magic == Big_endian_64_magic_number) {
    /* Shrinkage 64 -> 32 required */
#ifdef NO_SIXTYFOUR_INTERN
    failwith("intern: 64-bit object, cannot load");
#else
    mlsize_t whsize64;
    value64 * block;
    whsize64 = whsize;
    block = (value64 *) stat_alloc(whsize64 * sizeof(value64));
    if (really_getblock(chan, block, whsize64 * sizeof(value64)) == 0) {
      stat_free((char *) block);
      failwith ("intern : truncated object");
    }
#ifdef BIG_ENDIAN
    if (magic == Little_endian_64_magic_number) {
#else
    if (magic == Big_endian_64_magic_number) {
#endif
      if (rev_pointers_64(block, whsize64) == -1) {
        stat_free((char *) block);
        failwith("intern: 64-bit object too big");
      }
    }
    whsize = size_after_shrinkage(block, whsize64);
    if (whsize == -1) {
      stat_free((char *) block);
      failwith("intern: 64-bit object too big");
    }
    wosize = Wosize_whsize(whsize);
    res = alloc_shr(wosize, String_tag);
    hd = Hd_val (res);
    color = Color_hd (hd);
    Assert (color == White || color == Black);
    if (shrink_block(block, Hp_val(res), whsize64, whsize, color) == -1) {
      Hd_val (res) = hd;                      /* Avoid confusing the GC. */
      stat_free((char *) block);
      failwith("intern: 64-bit object too big");
    }
    stat_free((char *) block);
#endif /* !NO_SIXTYFOUR_INTERN */
  } else {
    /* Block has natural word size (32) */
    res = alloc_shr(wosize, String_tag);
    hd = Hd_val (res);
    color = Color_hd (hd);
    Assert (color == White || color == Black);
    if (really_getblock(chan, Hp_val(res), bhsize) == 0) {
      Hd_val (res) = hd;                      /* Avoid confusing the GC. */
      failwith ("intern : truncated object");
    }
#ifdef BIG_ENDIAN
    if (magic == Little_endian_32_magic_number)
      rev_pointers(Hp_val (res), whsize);
#else
    if (magic == Big_endian_32_magic_number)
      rev_pointers(Hp_val (res), whsize);
#endif
    adjust_pointers(Hp_val (res), whsize, color);
  }
#endif /* !SIXTYFOUR */
  return res;
}
