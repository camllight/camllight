/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Adapted by Xavier Leroy on Fri Apr 30 1993 */
/* Last modified on Mon Oct  8 15:12:37 GMT+1:00 1990 by herve */
/*      modified on Fri Aug 17 17:19:01 GMT+2:00 1990 by shand */
/*      modified on Wed Jul  5 10:19:33 GMT+2:00 1989 by bertin */


/* BigN.h - Types and structures for clients of BigNum */



		/******** representation of a bignum ******/
/*
**  <--------------------------- nl ---------------------------->
**  |   Least                                           Most    |
**  |Significant|           |           |           |Significant|
**  |BigNumDigit|           |           |           |BigNumDigit|
**  |___________|___________|___________|___________|___________|
**        ^                                          (sometimes
**        |                                            is zero)
**       nn
*/

/* signals BigNum.h already included */
#define BIGNUM

		/*************** sizes ********************/

#define BN_BYTE_SIZE			8
#ifdef CAML_LIGHT
#define BN_WORD_SIZE			(sizeof (long) * BN_BYTE_SIZE - 2)
#else
#define BN_WORD_SIZE			(sizeof (int) * BN_BYTE_SIZE)
#endif
#define BN_DIGIT_SIZE			(sizeof (BigNumDigit) * BN_BYTE_SIZE)

/* notes: */
/* BN_BYTE_SIZE: number of bits in a byte */
/* BN_WORD_SIZE: number of bits in an "int" in the target language */
/* BN_DIGIT_SIZE: number of bits in a digit of a BigNum */


		/****** results of compare functions ******/

 /* Note: we don't use "enum" to interface with Modula2+, Lisp, ... */
#define BN_LT				-1
#define BN_EQ				0
#define BN_GT				1

		/*************** boolean ******************/

#define TRUE				1
#define FALSE				0


		/* if DIGITon16BITS is defined, a single digit is on 16 bits */
		/* otherwise (by default) a single digit is on 32 bits *****/
		/* Note: on 32 bit machine it makes little sense to mix */
		/* longs and short, so we define Boolean & BigNumCmp to be */
		/* int usually */

#ifdef DIGITon16BITS
typedef unsigned short			BigNumDigit;
typedef short				Boolean;
#else
typedef unsigned int 			BigNumDigit;
typedef int				Boolean;
#endif


		/* bignum types: digits, big numbers, carries ... */

typedef BigNumDigit * 	BigNum;		/* A big number is a digit pointer */
typedef BigNumDigit	BigNumCarry;	/* Either 0 or 1 */
typedef unsigned long 	BigNumProduct;	/* The product of two digits */
/* BigNumLength must be int as nl is in the code, remember int is 16 bits on MSDOS - jch */
typedef unsigned int	BigNumLength;	/* The length of a bignum */
#ifdef DIGITon16BITS
typedef short		BigNumCmp;	/* result of comparison */
#else
typedef int		BigNumCmp;	/* result of comparison */
#endif


/**/


		/************ functions of bn.c ***********/

extern void             BnnInit 			();
extern void             BnnClose 			();

extern Boolean		BnnIsZero 			();
extern BigNumCarry 	BnnMultiply			();
extern void		BnnDivide			();
extern BigNumCmp	BnnCompare			();


		/*********** functions of KerN.c **********/

extern void 		BnnSetToZero			();
extern void 		BnnAssign			();
extern void 		BnnSetDigit			();
extern BigNumDigit 	BnnGetDigit			();
extern BigNumLength	BnnNumDigits			();
extern BigNumDigit	BnnNumLeadingZeroBitsInDigit	();
extern Boolean 		BnnDoesDigitFitInWord 		();
extern Boolean		BnnIsDigitZero 			();
extern Boolean		BnnIsDigitNormalized 		();
extern Boolean 		BnnIsDigitOdd			();
extern BigNumCmp		BnnCompareDigits		();
extern void 		BnnComplement			();
extern void 		BnnAndDigits			();
extern void		BnnOrDigits			();
extern void		BnnXorDigits			();
extern BigNumDigit	BnnShiftLeft			();
extern BigNumDigit	BnnShiftRight			();
extern BigNumCarry 	BnnAddCarry			();
extern BigNumCarry 	BnnAdd				();
extern BigNumCarry 	BnnSubtractBorrow		();
extern BigNumCarry 	BnnSubtract			();
extern BigNumCarry 	BnnMultiplyDigit		();
extern BigNumDigit	BnnDivideDigit			();

/**/

		/* some functions can be written with macro-procedures */


#ifndef BNNMACROS_OFF
/* the functions BnnIsZero and BnnCompareDigits are not macro procedures
since they use parameters twice, and that can produce some bugs if
you pass a parameter like x++, the increment will be executed twice ! */
#define BnnSetDigit(nn,d) 		(*(nn) = (d))
#define BnnGetDigit(nn)			((unsigned)(*(nn)))
#define BnnDoesDigitFitInWord(d)	(BN_DIGIT_SIZE > BN_WORD_SIZE ? ((d) >= 1 << BN_WORD_SIZE ? FALSE : TRUE) : TRUE)
#define BnnIsDigitZero(d)		((d) == 0)
#define BnnIsDigitNormalized(d)		((d) & (1 << (BN_DIGIT_SIZE - 1)) ? TRUE : FALSE)
#define BnnIsDigitOdd(d) 		((d) & 1 ? TRUE : FALSE)
#define BnnAndDigits(nn, d)		(*(nn) &= (d))
#define BnnOrDigits(nn, d)		(*(nn) |= (d))
#define BnnXorDigits(nn, d)		(*(nn) ^= (d))

#endif


#ifdef MSDOS
#define realaddr(p)  ((((long)(p) & (65535 << 16)) >> 12)+((long)(p) & 65535))
#endif
