/* Processor dependencies */

#undef CAML_SIXTYFOUR

/* Define CAML_SIXTYFOUR if the processor is 64 bits
   (sizeof(long) == 64, sizeof(void *) == 64).
   Leave CAML_SIXTYFOUR undefined if the processor is 32 bits
   (sizeof(long) == 32, sizeof(void *) == 32).
*/

#define CAML_BIG_ENDIAN

/* Define CAML_BIG_ENDIAN if the processor is big endian (the most significant
   byte of an integer stored in memory comes first).
   Leave CAML_BIG_ENDIAN undefined
   if the processor is little-endian (the least significant byte comes first).
*/

#define CAML_ALIGNMENT

/* Define CAML_ALIGNMENT if the processor puts alignment constraints on 
   16-bit and 32-bit memory accesses.
   Leave CAML_ALIGNMENT undefined if the processor can
   access a 16-bit or 32-bit integer at any address.
*/

#define CAML_ALIGN_DOUBLE

/* Define CAML_ALIGN_DOUBLE if the processor requires doubles to be
   doubleword-aligned.
   Leave CAML_ALIGN_DOUBLE undefined if the processor supports
   word-aligned doubles. */
