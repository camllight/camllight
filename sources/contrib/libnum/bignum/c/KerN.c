/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified on Tue Jan 15 19:32:53 GMT+1:00 1991 by herve */
/*      modified on Mon Feb 19 20:22:20 GMT+1:00 1990 by shand */


/* KerN.c: the kernel written in C */
 
/*
 *	Description of types and constants.
 *
 * Several conventions are used in the commentary:  
 *    A "BigNum" is the name for an infinite-precision number.  
 *    Capital letters (e.g., "N") are used to refer to the value of BigNums.
 *    The word "digit" refers to a single BigNum digit.
 *    The notation "Size(N)" refers to the number of digits in N,
 *	 which is typically passed to the subroutine as "nl".
 *    The notation "Length(N)" refers to the number of digits in N,
 *       not including any leading zeros.
 *    The word "Base" is used for the number 2 ** BN_DIGIT_SIZE, where
 *       BN_DIGIT_SIZE is the number of bits in a single BigNum digit.
 *    The expression "BBase(N)" is used for Base ** NumDigits(N).
 *    The term "leading zeros" refers to any zeros before the most 
 *       significant digit of a number.
 *
 *
 * In the code, we have:
 *
 *    "nn" is a pointer to a big number,
 *    "nl" is the number of digits from nn,
 *    "d" is a digit.
 *
 */


/**/

#define BNNMACROS_OFF
#include "BigNum.h"


                /*** copyright ***/

static char copyright[]="@(#)KerN.c: copyright Digital Equipment Corporation & INRIA 1988, 1989\n";


	/******* non arithmetic access to digits ********/


void BnnSetToZero (nn, nl) 

register BigNum 	nn; 
register int 		nl;

/*
 * Sets all the specified digits of the BigNum to 0
 */

{
#ifdef __STDC__
    memset(nn, 0, nl*(BN_DIGIT_SIZE/BN_BYTE_SIZE));
#else
#ifdef NOMEM
    while (--nl >= 0)
	*(nn++) = 0;
#else
    bzero (nn, nl*(BN_DIGIT_SIZE/BN_BYTE_SIZE));
#endif
#endif
}
 
		/***************************************/


void BnnAssign (mm, nn, nl) 

register BigNum	mm, nn;
register int 	nl;

/* 
 * Copies N => M
 */

{
#ifdef __STDC__
    memmove(mm, nn, nl*(BN_DIGIT_SIZE/BN_BYTE_SIZE));
#else 
#ifdef MSDOS
    if (realaddr(mm) < realaddr(nn) || realaddr(mm) > realaddr(nn+nl))
#else
    if ((mm < nn) || ( mm > nn+nl))
#endif
#ifdef NOMEM 
	while (--nl >= 0)
	    *mm++ = *nn++;
#else
        /* be care: bcopy (SRC, DEST, L): SRC-->DEST !!! */
        bcopy (nn, mm, nl*(BN_DIGIT_SIZE/BN_BYTE_SIZE));
#endif
    else
#ifdef MSDOS
    if (realaddr(mm) > realaddr(nn))
#else
    if (mm > nn)
#endif
    {
	nn += nl;
	mm += nl;
	while (--nl >= 0) 
	    *--mm = *--nn;
    }
#endif
}
 
		/***************************************/
/**/


void BnnSetDigit (nn, d) 

BigNum 	nn; 
int 	d;

/*
 * Sets a single digit of N to the passed value
 */

{
    *nn = d;
}

		/***************************************/


BigNumDigit BnnGetDigit (nn)

BigNum 	nn;

/* 
 * Returns the single digit pointed by N
 */

{
    return (*nn);
}

		/***************************************/
/**/


BigNumLength BnnNumDigits (nn, nl) 

register BigNum nn;
register int 	nl;

/* 
 * Returns the number of digits of N, not counting leading zeros
 */

{
    nn += nl;

    while (nl != 0 && *--nn == 0)
        nl--;

    return (nl == 0 ? 1 : nl);
}

		/***************************************/


BigNumDigit BnnNumLeadingZeroBitsInDigit (d) 

BigNumDigit d;

/*
 * Returns the number of leading zero bits in a digit 
 */

{
    register BigNumDigit mask = 1 << (BN_DIGIT_SIZE - 1);
    register int 	p = 0;

    
    if (d == 0) 
        return (BN_DIGIT_SIZE);

    while ((d & mask) == 0)
    {
	p++;
	mask >>= 1;
    }

    return (p);
}

		/***************************************/
/**/

	/************** Predicates on one digit ***************/


Boolean BnnDoesDigitFitInWord (d)

BigNumDigit d;

/*
 * Returns TRUE iff the digit can be represented in just BN_WORD_SIZE bits
 */
{
    /* The C compiler must evaluate the predicate at compile time */
    if (BN_DIGIT_SIZE > BN_WORD_SIZE)
        return (d >= 1 << BN_WORD_SIZE ? FALSE : TRUE);
    else
	return (TRUE);
}

		/***************************************/


Boolean BnnIsDigitZero (d)

BigNumDigit d;

/* Returns TRUE iff digit = 0 */

{
    return (d == 0);
}

		/***************************************/


Boolean BnnIsDigitNormalized (d)

BigNumDigit d;

/*
 * Returns TRUE iff Base/2 <= digit < Base
 * i.e., if digit's leading bit is 1
 */

{
    return (d & (1 << (BN_DIGIT_SIZE - 1)) ? TRUE : FALSE);
}

		/***************************************/


Boolean BnnIsDigitOdd (d) 

BigNumDigit d;

/*
 * Returns TRUE iff digit is odd 
 */

{
    return (d & 1 ? TRUE : FALSE);
}

		/***************************************/


BigNumCmp BnnCompareDigits (d1, d2)

BigNumDigit d1, d2;

/*
 * Returns 	BN_GREATER 	if digit1 > digit2
 *		BN_EQUAL	if digit1 = digit2
 *		BN_LESS	if digit1 < digit2
 */

{
    return (d1 > d2 ? BN_GT : (d1 == d2 ? BN_EQ : BN_LT));
}

	/***************** Logical operations ********************/


void BnnComplement (nn, nl) 

register BigNum nn;
register int 	nl;

/*
 * Performs the computation BBase(N) - N - 1 => N
 */

{
    while (--nl >= 0)
	*(nn++) ^= -1;
}

		/***************************************/
/**/


void BnnAndDigits (n, d)

BigNum		n;
BigNumDigit 	d;

/* 
 * Returns the logical computation n[0] AND d in n[0]
 */

{
    *n &= d;
}

		/***************************************/


void BnnOrDigits (n, d)

BigNum		n;
BigNumDigit 	d;

/*
 * Returns the logical computation n[0] OR d2 in n[0].
 */

{
    *n |= d;
}

		/***************************************/


void BnnXorDigits (n, d)

BigNum		n;
BigNumDigit 	d;

/*
 * Returns the logical computation n[0] XOR d in n[0].
 */

{
    *n ^= d;
}

		/***************************************/
/**/

	/****************** Shift operations *******************/


BigNumDigit BnnShiftLeft (mm, ml, nbits)

register BigNum mm;
register int 	ml;
	 int	nbits;

/* 
 * Shifts M left by "nbits", filling with 0s.  
 * Returns the leftmost "nbits" of M in a digit.
 * Assumes 0 <= nbits < BN_DIGIT_SIZE. 
 */

{
    register BigNumDigit res = 0, save;
    	     int 	 rnbits;


    if (nbits != 0)
    {
	rnbits = BN_DIGIT_SIZE - nbits;

	while (--ml >= 0) 
	{
	    save = *mm;
	    *mm++ = (save << nbits) | res;
	    res = save >> rnbits;
	}
    }

    return (res);
}

		/***************************************/
/**/


BigNumDigit BnnShiftRight (mm, ml, nbits)

register BigNum mm;
register int 	ml;
	 int	nbits;

/*
 * Shifts M right by "nbits", filling with 0s.  
 * Returns the rightmost "nbits" of M in a digit.
 * Assumes 0 <= nbits < BN_DIGIT_SIZE. 
 */

{
    register BigNumDigit res = 0, save;
    	     int 	 lnbits;


    if (nbits != 0)
    {
	mm += ml;
	lnbits = BN_DIGIT_SIZE - nbits;

	while (--ml >= 0)
	{
	    save = *(--mm);
	    *mm = (save >> nbits) | res;
	    res = save << lnbits;
	}
    }

    return (res);
}

		/***************************************/
/**/


	/******************* Additions **************************/


BigNumCarry BnnAddCarry (nn, nl, carryin)

register BigNum 	nn;
register int 		nl;
	 BigNumCarry 	carryin;

/*
 * Performs the sum N + CarryIn => N.  
 * Returns the CarryOut.
 */

{
    if (carryin == 0) 
        return (0);

    if (nl == 0) 
        return (1);

    while (--nl >= 0 && !(++(*nn++)))
        ;

    return (nl >= 0 ? 0 : 1);
}

		/***************************************/
/**/


BigNumCarry BnnAdd (mm, ml, nn, nl, carryin)

register BigNum 	mm, nn;
	 int 		ml;
register int 		nl;
	 BigNumCarry 	carryin; 

/* 
 * Performs the sum M + N + CarryIn => M.
 * Returns the CarryOut. 
 * Assumes Size(M) >= Size(N).
 */

{
    register BigNumProduct c = carryin;


    ml -= nl;
    /* test computed at compile time */
    if (sizeof (BigNumProduct) > sizeof (BigNumDigit))
    {
	while (--nl >= 0)
	{
	    c += ((BigNumProduct)*mm) + *(nn++);
	    *(mm++) = c;
	    c >>= BN_DIGIT_SIZE;
	}
    }
    else 
    {
	register BigNumProduct save;

	while (--nl >= 0)
	{
	    save = *mm;
	    c += save;
	    if (c < save) 
	    {
		*(mm++) = *(nn++);
		c = 1;
	    }
	    else
	    {
		save = *(nn++);
		c += save;
		*(mm++) = c;
		c = (c < save) ? 1 : 0;
	    }
	}
    }

    return (BnnAddCarry (mm, ml, (BigNumCarry) c));
}

		/***************************************/
/**/

	/****************** Subtraction *************************/



BigNumCarry BnnSubtractBorrow (nn, nl, carryin)

register BigNum 	nn;
register int 		nl;
	 BigNumCarry 	carryin;

/*
 * Performs the difference N + CarryIn - 1 => N.
 * Returns the CarryOut.
 */

{
    if (carryin == 1)
        return (1);
    if (nl == 0)
        return (0);

    while (--nl >= 0 && !((*nn++)--)) 
        ;

    return (nl >= 0 ? 1 : 0);
}

		/***************************************/
/**/


BigNumCarry BnnSubtract (mm, ml, nn, nl, carryin)

register BigNum 	mm, nn;
	 int 		ml;
register int 		nl;
	 BigNumCarry 	carryin;

/* 
 * Performs the difference M - N + CarryIn - 1 => M.
 * Returns the CarryOut.
 * Assumes Size(M) >= Size(N).
 */

{
    register BigNumProduct 	c = carryin;
    register BigNumDigit 	invn;


    ml -= nl;
    /* test computed at compile time */
    if (sizeof (BigNumProduct) > sizeof (BigNumDigit))
    {
	while (--nl >= 0) 
	{
	    invn = *(nn++) ^ -1;
	    c += ((BigNumProduct)*mm) + invn;
	    *(mm++) = c;
	    c >>= BN_DIGIT_SIZE;
	}
    }
    else
    {
	register BigNumProduct save;

	while (--nl >= 0) 
	{
	    save = *mm;
	    invn = *(nn++) ^ -1;
	    c += save;

	    if (c < save)
	    {
		*(mm++) = invn;
		c = 1;
	    }
	    else
	    {
		c += invn;
 		*(mm++) = c;
 		c = (c < invn) ? 1 : 0;
	    }
 	}
    }

    return (BnnSubtractBorrow (mm, ml, (BigNumCarry) c)); }


		/***************************************/ 
/* */

	/***************** Multiplication ************************/


BigNumCarry BnnMultiplyDigit (pp, pl, mm, ml, d)

register BigNum 	pp, mm;
         int 		pl, ml; 
	 BigNumDigit	d;

/*
 * Performs the product:
 * Q = P + M * d
 * BB = BBase(P)
 * Q mod BB => P
 * Q div BB => CarryOut
 * Returns the CarryOut. 
 * Assumes Size(P) >= Size(M) + 1. 
 */

{ 
    register BigNumProduct c = 0;

    
    if (d == 0) 
        return (0);

    if (d == 1) 
        return (BnnAdd (pp, pl, mm, ml, (BigNumCarry) 0));

    pl -= ml;
    /* test computed at compile time */
    if (sizeof (BigNumProduct) > sizeof (BigNumDigit)) 
    {
	while (ml != 0) 
	{
	    ml--;
	    c += *pp + (((BigNumProduct)d) * (*(mm++)));
	    *(pp++) = c;
	    c >>= BN_DIGIT_SIZE;
	} 

	while (pl != 0) 
	{
	    pl--;
	    c += *pp;
	    *(pp++) = c;
	    c >>= BN_DIGIT_SIZE;
	}

	return (c);
    }
    else
    {
/* help for stupid compilers--may actually be counter
   productive on pipelined machines with decent register allocation!! */
#define m_digit X0
#define X3 Lm
#define X1 Hm
	register BigNumDigit Lm, Hm, Ld, Hd, X0, X2 /*, X1, X3 */;

	Ld = d & ((1 << (BN_DIGIT_SIZE / 2)) -1);
	Hd = d >> (BN_DIGIT_SIZE / 2);
	while (ml != 0) 
	{
	    ml--;
	    m_digit = *mm++;
	    Lm = m_digit & ((1 << (BN_DIGIT_SIZE / 2)) -1);
	    Hm = m_digit >> (BN_DIGIT_SIZE / 2);
	    X0 = Ld * Lm;
	    X2 = Hd * Lm;
	    X3 = Hd * Hm;
	    X1 = Ld * Hm;

	    if ((c += X0) < X0) X3++;
	    if ((X1 += X2) < X2) X3 += (1<<(BN_DIGIT_SIZE / 2));
	    X3 += (X1 >> (BN_DIGIT_SIZE / 2));
	    X1 <<= (BN_DIGIT_SIZE / 2);
	    if ((c += X1) < X1) X3++;
	    if ((*pp += c) < c) X3++;
	    pp++;

	    c = X3;
#undef m_digit
#undef X1
#undef X3
	}

	X0 = *pp;
	c += X0;
	*(pp++) = c;

	if (c >= X0)
	    return (0);

	pl--;
	while (pl != 0 && !(++(*pp++))) 
	    pl--;

	return (pl != 0 ? 0 : 1);
    }
}

#ifdef mips
BigNumCarry BnnMultiply2Digit (pp, pl, mm, ml, d0, d1)

register BigNum 	pp, mm;
register int 		pl, ml; 
	 BigNumDigit 	d0, d1;

/*
 * Provided for compatibility with mips assembler implementation.
 * Performs the product:
 * Q = P + M * d0_d1
 * BB = BBase(P)
 * Q mod BB => P
 * Q div BB => CarryOut
 * Returns the CarryOut. 
 * Assumes Size(P) >= Size(M) + 1. 
 */

{ 
    return
        BnnMultiplyDigit (pp, pl, mm, ml, d0)
        + BnnMultiplyDigit (pp+1, pl-1, mm, ml, d1);
}
#endif

		/***************************************/
/**/

	/********************** Division *************************/


		/* xh:xl -= yh:yl */
#define SUB(xh,xl,yh,yl)	if (yl > xl) {xl -= yl; xh -= yh + 1;}\
				else         {xl -= yl; xh -= yh;}

#define LOW(x) 			(x & ((1 << (BN_DIGIT_SIZE / 2)) -1)) 
#define HIGH(x) 		(x >> (BN_DIGIT_SIZE / 2)) 
#define L2H(x) 			(x << (BN_DIGIT_SIZE / 2)) 


BigNumDigit BnnDivideDigit (qq, nn, nl, d)

register BigNum 	qq, nn;
register int 		nl;
	 BigNumDigit 	d;

/* Performs the quotient: N div d => Q
 * Returns R = N mod d
 * Assumes leading digit of N < d, and d > 0.
 */

{
    /* test computed at compile time */
    if (sizeof (BigNumProduct) > sizeof (BigNumDigit))
    {
	register BigNumProduct quad;


	nn += nl;
	nl--;
	qq += nl;
	quad = *(--nn);

	while (nl != 0)
	{
	    nl--;
	    quad = (quad << BN_DIGIT_SIZE) | *(--nn);
	    *(--qq) = quad / d;
	    quad = quad % d;
	} 

	return (quad);
    }
    else
    {
	int 		k;
	int 		orig_nl;
	BigNumDigit 	rh;  		/* Two halves of current remainder */
	BigNumDigit 	rl;  		/* Correspond to quad above */
	register BigNumDigit qa;   	/* Current appr. to quotient */
	register BigNumDigit ph, pl;	/* product of c and qa */
	BigNumDigit 	ch, cl, prev_qq;
	

	/* Normalize divisor */
	k = BnnNumLeadingZeroBitsInDigit (d);
	if (k != 0) 
	{
	    prev_qq = qq[-1];
	    orig_nl = nl;
	    d <<= k;
	    BnnShiftLeft (nn, nl, k);    
	}

	nn += nl;
	nl--;
	qq += nl;

	ch = HIGH (d);
	cl = LOW (d);

	rl = *(--nn);

	while (nl != 0)
	{
	    nl--;
	    rh = rl; 
	    rl = *(--nn);
	    qa = rh / ch; 	/* appr. quotient */

	    /* Compute ph, pl */
	    pl = cl * qa;
	    ph = ch * qa;
	    ph += HIGH (pl);
	    pl = L2H (pl);

	    /* While ph:pl > rh:rl, decrement qa, adjust qh:ql */
	    while (ph > rh || ph == rh && pl > rl) 
	    {
		qa--;
		SUB (ph, pl, ch, L2H (cl));
	    }

	    SUB (rh, rl, ph, pl);

	    /* Top half of quotient is correct; save it */
	    *(--qq) = L2H (qa);
	    qa = (L2H (rh) | HIGH (rl)) / ch;

	    /* Approx low half of q */
	    /* Compute ph, pl, again */
	    pl = cl * qa;
	    ph = ch * qa;
	    ph += HIGH (pl);
	    pl = LOW (pl) | L2H (LOW (ph));
	    ph = HIGH (ph);

	    /* While ph:pl > rh:rl, decrement qa, adjust qh:ql */
	    while (ph > rh || ph == rh && pl > rl)
	    {
		qa--;
		SUB (ph, pl, 0, d);
	    }

	    /* Subtract ph:pl from rh:rl; we know rh will be 0 */
	    rl -= pl;
	    *qq |= qa;
	}

	/* Denormalize dividend */
	if (k != 0) {
		if((qq > nn) && (qq < &nn[orig_nl])) {
			/* Overlap between qq and nn. Care of *qq! */
			orig_nl = (qq - nn);
			BnnShiftRight (nn, orig_nl, k);
			nn[orig_nl - 1] = prev_qq;
		} else if(qq == nn) {
			BnnShiftRight(&nn[orig_nl - 1], 1, k);
		} else {
			BnnShiftRight (nn, orig_nl, k);
	}	}
	return (rl >> k);
    }
}

		/***************************************/


