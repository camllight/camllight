/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified on Fri Oct  5 16:13:31 GMT+1:00 1990 by herve */
/*      modified on Fri Aug 10 17:21:47 GMT+2:00 1990 by shand */


/* bnCmp.c: a piece of the bignum kernel written in C */


		/***************************************/
#define BNNMACROS_OFF
#include "BigNum.h"

                        /*** copyright ***/

static char copyright[]="@(#)bnCmp.c: copyright Digital Equipment Corporation & INRIA 1988, 1989, 1990\n";


Boolean BnnIsZero (nn, nl)

BigNum 		nn;
BigNumLength 	nl;

/* 
 * Returns TRUE iff N = 0
 */

{
    return (BnnNumDigits (nn, nl) == 1 && (nl == 0 || BnnIsDigitZero (*nn)));
}

		/***************************************/
/**/


BigNumCmp BnnCompare (mm, ml, nn, nl)

	 BigNum	 	mm, nn;
register BigNumLength 	ml, nl;

/*
 * return
 *	 	BN_GT 	iff M > N
 *		BN_EQ	iff N = N
 *		BN_LT	iff N < N
*/

{
    register BigNumCmp result = BN_EQ;


    ml = BnnNumDigits (mm, ml);
    nl = BnnNumDigits (nn, nl);

    if (ml != nl)
        return (ml > nl ? BN_GT : BN_LT);

    while (result == BN_EQ && ml-- > 0)
        result = BnnCompareDigits (*(mm+ml), *(nn+ml));

    return (result);

/**** USE memcmp() instead: extern int memcmp ();

    if (ml == nl)
    {
        lex = memcmp (mm, nn, nl*BN_DIGIT_SIZE/BN_BYTE_SIZE);
        return (lex > 0 ? BN_GT: (lex == 0 ? BN_EQ: BN_LT));
    }
    else
        return (ml > nl ? BN_GT : BN_LT);
******/
}
