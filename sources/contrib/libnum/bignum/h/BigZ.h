/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified on Fri Oct  5 16:45:46 GMT+1:00 1990 by herve */
/*      modified on Thu Mar 22 21:29:09 GMT+1:00 1990 by shand */

/* BigZ.h: Types and structures for clients of BigZ */
 

       			/* BigZ sign */


#define BZ_PLUS				1
#define BZ_ZERO				0
#define BZ_MINUS			-1
#define BzSign	        		BigNumCmp


       			/* BigZ compare result */


#define BZ_LT				BN_LT
#define BZ_EQ				BN_EQ
#define BZ_GT				BN_GT
#define BzCmp				BigNumCmp


       			/* BigZ number */

#ifndef BIGNUM
#include "BigNum.h"
#endif

struct BigZHeader
{
    unsigned long			Size;
    BzSign				Sign;
};


struct BigZStruct
{
    struct BigZHeader			Header;
    BigNumDigit 			Digits [16];
};


typedef struct BigZStruct * 		BigZ;

/**/


		/*********** macros of bz.c **********/


#define BzGetSize(z)			((BigNumLength)(z)->Header.Size)
#define BzGetSign(z)			((z)->Header.Sign)

#define BzSetSize(z,s)			(z)->Header.Size = s
#define BzSetSign(z,s)			(z)->Header.Sign = s

#define BzGetOppositeSign(z)		(-(z)->Header.Sign)


		/*********** functions of bz.c **********/

extern void             BzInit                  ();
extern void             BzClose                  ();

extern BigZ 		BzCreate		();
extern void 		BzFree			();
extern void 		BzFreeString		();

extern unsigned		BzNumDigits		();

extern BigZ 		BzCopy			();
extern BigZ 		BzNegate		();
extern BigZ 		BzAbs			();
extern BzCmp 		BzCompare		();

extern BigZ 		BzAdd			();
extern BigZ 		BzSubtract		();
extern BigZ 		BzMultiply		();
extern BigZ 		BzDivide		();
extern BigZ 		BzDiv			();
extern BigZ 		BzMod			();

extern BigZ 		BzFromString		();
extern char *		BzToString		();

extern BigZ             BzFromInteger           ();
extern int		BzToInteger             ();

extern BigZ		BzFromBigNum		();
extern BigNum		BzToBigNum		();

		/*********** functions of bzf.c **********/

extern BigZ 		BzFactorial		();
