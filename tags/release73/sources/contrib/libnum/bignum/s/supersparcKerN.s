! Copyright     Digital Equipment Corporation     1991
! Last modified on Fri Mar  1 17:21:25 GMT+1:00 1991 by shand
!
!    KerN for SPARC
!    Mark Shand
!
!    Implementation notes:
!
!        Initial implementations of sparc offer very limited support for
!        integer multiplication, so BnnMultiplyDigit is based on
!        double precision floating point multiplies that compute
!        a 16x32->48 bit result without round-off.  Performance is
!        not great, but is about twice as good as using the integer
!        multiply primitives directly.
!
!        BnnDivideDigit uses the unmodified assembly code produced
!        by cc -O2 KerN.c
!
	.seg	"text"			! [internal]
	.proc	16
	.global	_BnnSetToZero
_BnnSetToZero:
	deccc	%o1
	bneg	LBSZ3			! is zero
	andcc	1,%o1,%o2
	be	LBSZ2			! is odd
	nop
	dec	4,%o0	
LBSZ1:					! [internal]
	inc	8,%o0
	st	%g0,[%o0-4]
LBSZ2:
	deccc	2,%o1
	bpos	LBSZ1
	st	%g0,[%o0]
LBSZ3:
	retl
	nop				! [internal]
!
!
	.proc	16
	.global	_BnnAssign
_BnnAssign:
	cmp	%o0,%o1
	bgt,a	LBAG2			! if(mm >= nn) goto LBAG2
	tst	%o2
	be	LBAGX
	tst	%o2
	be	LBAGX			! if(nl==0) return
	nop
LBAG1:
	ld	[%o1],%o3
	inc	4,%o1
	st	%o3,[%o0]
	deccc	%o2
	bgt	LBAG1
	inc	4,%o0
LBAGX:
	retl
	nop
LBAG2:
	be	LBAGX			! if(nl==0) return
	sll	%o2,2,%o3		! nl <<= 2
	add	%o1,%o3,%o1		! nn += nl
	add	%o0,%o3,%o0		! mm += nl
LBAG3:
	dec	4,%o1
	ld	[%o1],%o3		! %o3 = *--nn
	dec	4,%o0
	deccc	%o2
	bgt	LBAG3
	st	%o3,[%o0]		! *--mm = %o3
	retl
	nop
!
!
	.proc	16
	.global	_BnnSetDigit
_BnnSetDigit:
	retl
	st	%o1,[%o0]
!
!
	.proc	14
	.global	_BnnGetDigit
_BnnGetDigit:
	retl
	ld	[%o0],%o0
!
!
	.proc	14
	.global	_BnnNumDigits
_BnnNumDigits:
	tst	%o1
	sll	%o1,2,%o3
	be	LBND2
	add	%o0,%o3,%o4
	dec	4,%o4
LBND1:
	ld	[%o4],%o2
	tst	%o2
	bne	LBND2
	deccc	%o1
	bne,a	LBND1
	dec	4,%o4
LBND2:
	retl
	add	1,%o1,%o0
!
!
	.proc	14
	.global	_BnnNumLeadingZeroBitsInDigit
_BnnNumLeadingZeroBitsInDigit:
	addcc	%o0,%g0,%o5		! %o5 = d
	be	LBLZX			! if(!d) goto BLZX
	sethi	%hi(0xffff0000),%o1	! mask = 0xffff0000
	mov	1,%o0			! p = 1
	andcc	%o1,%o5,%g0		! mask & d
	bne	LBLZ1
	sll	%o1,8,%o1
	 sll	%o5,16,%o5
	 or	16,%o0,%o0
LBLZ1:
	andcc	%o1,%o5,%g0		! mask & d
	bne	LBLZ2
	sll	%o1,4,%o1
	 sll	%o5,8,%o5
	 or	8,%o0,%o0
LBLZ2:
	andcc	%o1,%o5,%g0		! mask & d
	bne	LBLZ3
	sll	%o1,2,%o1
	 sll	%o5,4,%o5
	 or	4,%o0,%o0
LBLZ3:
	andcc	%o1,%o5,%g0		! mask & d
	bne	LBLZ4
	nop
	 sll	%o5,2,%o5
	 or	2,%o0,%o0
LBLZ4:
	srl	%o5,31,%o5		! %o5 = (d & 0x80000000) != 0
	retl
	xor	%o0,%o5,%o0
LBLZX:
	retl
	mov	32,%o0
	.proc	4
	.global	_BnnDoesDigitFitInWord
_BnnDoesDigitFitInWord:
	retl
	mov	1,%o0
	.proc	4
	.global	_BnnIsDigitZero
_BnnIsDigitZero:
	tst	%o0
	bne,a	LBDZ0
	mov	0,%o1
	mov	1,%o1
LBDZ0:
	retl
	add	%g0,%o1,%o0
	.proc	4
	.global	_BnnIsDigitNormalized
_BnnIsDigitNormalized:
	retl
	srl	%o0,31,%o0
	.proc	4
	.global	_BnnIsDigitOdd
_BnnIsDigitOdd:
	retl
	and	%o0,1,%o0
	.proc	4
	.global	_BnnCompareDigits
_BnnCompareDigits:
	cmp	%o0,%o1
	bleu	LBCD1
	mov	-1,%o0
	retl
	mov	1,%o0
LBCD1:					! [internal]
	be,a	LBCD2
	mov	0,%o0
LBCD2:
	retl
	nop				! [internal]
	.proc	16
	.global	_BnnComplement
_BnnComplement:
	deccc	%o1
	bneg	LE129
	nop
LY11:					! [internal]
	ld	[%o0],%o2
	xor	%o2,-1,%o2
	st	%o2,[%o0]
	deccc	%o1
	bpos	LY11
	inc	4,%o0
LE129:
	retl
	nop				! [internal]
	.proc	16
	.global	_BnnAndDigits
_BnnAndDigits:
	ld	[%o0],%o2
	and	%o2,%o1,%o2
	retl
	st	%o2,[%o0]
	.proc	16
	.global	_BnnOrDigits
_BnnOrDigits:
	ld	[%o0],%o2
	or	%o2,%o1,%o2
	retl
	st	%o2,[%o0]
	.proc	16
	.global	_BnnXorDigits
_BnnXorDigits:
	ld	[%o0],%o2
	xor	%o2,%o1,%o2
	retl
	st	%o2,[%o0]
	.proc	14
	.global	_BnnShiftLeft
_BnnShiftLeft:
	tst	%o2
	be	L77105
	mov	0,%o4
	deccc	%o1
	mov	32,%o3
	bneg	L77105
	sub	%o3,%o2,%o3
LY12:					! [internal]
	ld	[%o0],%o5
	sll	%o5,%o2,%g1
	or	%g1,%o4,%g1
	st	%g1,[%o0]
	deccc	%o1
	srl	%o5,%o3,%o4
	bpos	LY12
	inc	4,%o0
L77105:
	retl
	add	%g0,%o4,%o0
	.proc	14
	.global	_BnnShiftRight
_BnnShiftRight:
	tst	%o2
	be	L77114
	mov	0,%o4
	sll	%o1,2,%g1
	deccc	%o1
	mov	32,%o3
	add	%o0,%g1,%o0
	bneg	L77114
	sub	%o3,%o2,%o3
LY13:					! [internal]
	dec	4,%o0
	ld	[%o0],%o5
	srl	%o5,%o2,%g2
	or	%g2,%o4,%g2
	deccc	%o1
	sll	%o5,%o3,%o4
	bpos	LY13
	st	%g2,[%o0]
L77114:
	retl
	add	%g0,%o4,%o0
	.proc	14
	.global	_BnnAddCarry		! (mm, ml, car)
_BnnAddCarry:
	tst	%o2
	be	LBACX0			! if(car == 0) return(0);
	tst	%o1
	be	LBACX1			! if(nl == 0) return(1);
	nop
LBACL:
	ld	[%o0],%o3
	inccc	%o3
	bcc	LBACX0
	st	%o3,[%o0]
	deccc	%o1
	bgt	LBACL
	inc	4,%o0
LBACX1:
	retl
	mov	1,%o0
LBACX0:
	retl
	mov	0,%o0
	.proc	14
	.global	_BnnAdd			! (mm ml nn nl car)
_BnnAdd:
	sub	%o1,%o3,%o1		! ml -= nl
	tst	%o3
	be,a	_BnnAddCarry		! if (nl == 0) %o2 = car; goto AddCarry
	mov	%o4,%o2
LBAD1:
	ld	[%o2],%o5		! o5 = *nn
	addcc	-1,%o4,%g0		! set C = carin
	ld	[%o0],%o4		! o4 = *mm
	inc	4,%o2
	addxcc	%o5,%o4,%o5		! o5 = *mm + *nn, C = carout
	addx	%g0,%g0,%o4		! o4 = carout
	st	%o5,[%o0]
	deccc	%o3
	bne	LBAD1
	inc	4,%o0
	b	_BnnAddCarry
	mov	%o4,%o2
	.proc	14
	.global	_BnnSubtractBorrow	! (mm, ml, car)
_BnnSubtractBorrow:
	tst	%o2
	bne	LSBBX1			! if(car == 1) return(1);
	tst	%o1
	be	LSBBX0			! if(nl == 0) return(0);
	nop
LSBBL:
	ld	[%o0],%o3
	deccc	%o3
	bcc	LSBBX1
	st	%o3,[%o0]
	deccc	%o1
	bgt	LSBBL
	inc	4,%o0
LSBBX0:
	retl
	mov	0,%o0
LSBBX1:
	retl
	mov	1,%o0
	.proc	14
	.global	_BnnSubtract		! (mm ml nn nl car)
_BnnSubtract:
	sub	%o1,%o3,%o1		! ml -= nl
	tst	%o3
	be,a	_BnnSubtractBorrow	! if (nl == 0) %o2 = car; goto SubBorrow
	mov	%o4,%o2
LSUB1:
	ld	[%o2],%o5		! o5 = *nn
	deccc	%o4			! set C = carin
	ld	[%o0],%o4		! o4 = *mm
	inc	4,%o2
	subxcc	%o4,%o5,%o5		! o5 = *mm + *nn, C = carout
	mov	1,%o4
	subx	%o4,%g0,%o4		! o4 = carout
	st	%o5,[%o0]
	deccc	%o3
	bne	LSUB1
	inc	4,%o0
	b	_BnnSubtractBorrow
	mov	%o4,%o2
	.proc	14
	.global	_BnnMultiplyDigit
_BnnMultiplyDigit:
!#PROLOGUE# 0
!#PROLOGUE# 1
	tst	%o4
	bne	LMDnonzero
	cmp	%o4,1
	retl
	mov	0,%o0
LMDnonzero:
	bne	LMD0
	mov	0,%o5
	b	_BnnAdd		! shortcut to BnnAdd
	mov	0,%o4		! carry in = 0
LMD0:
	save	%sp,-96,%sp
	tst	%i3
	be	L77007
	sub	%i1,%i3,%l1
LMD1:
	ld	[%i0],%l7
	ld	[%i2],%l0
	umul	%l0,%i4,%o0
	mov	%y,%o1
	addcc	%o0,%i5,%i1
	inc	4,%i2
	addx	%o1,%g0,%i5
	addcc	%l7,%i1,%l7
	addx	%g0,%i5,%i5
	st	%l7,[%i0]
	deccc	%i3
	bgt	LMD1
	inc	4,%i0
L77007:
	tst	%i5
	be	LMDexit
	deccc	%l1
LY3:					! [internal]
	blt	LMDexit
	inc	4,%i0
	ld	[%i0-4],%i1
	addcc	%i1,%i5,%i1
	addxcc	%g0,%g0,%i5
	st	%i1,[%i0-4]
	bne,a	LY3
	deccc	%l1
LMDexit:
	ret
	restore	%g0,%i5,%o0
	.proc	14
	.global	_BnnDivideDigit
_BnnDivideDigit:
!	BnnDivideDigit(qq, nn, nl, d)

       	save	%sp,-96,%sp
	mov	%i0, %i5
	deccc %i2		! --%i2;
	sll	%i2, 2, %i2
	blt	bnnout
	ld	[%i1+%i2], %i0	! X(hight) = %i1[%i2];
bnndivloop:

	deccc	4, %i2		! --%i2;
! condition code remains unchanged until bgt at loop end
	ld	[%i1+%i2], %i4	! X(%i4) = %i1[%i2];
	mov	%i0, %y
	udiv	%i4, %i3, %l0	! %l0 = %i0,%i4 / %i3;
	umul	%l0, %i3, %l1	! %l1 = %l0 * %i3;
	sub	%i4, %l1, %i0	! %i0 = %i0,%i4 % %i3;
	bgt	bnndivloop	! if (%i2 > 0) goto divloop;
	st	%l0,[%i5+%i2]	! %i5[%i2] = %l0;
bnnout:
	ret
	restore
	
	.seg	"data"			! [internal]
_copyright:
	.half	0x4028
	.half	0x2329
	.half	0x4b65
	.half	0x724e
	.half	0x2e63
	.half	0x3a20
	.half	0x636f
	.half	0x7079
	.half	0x7269
	.half	0x6768
	.half	0x7420
	.half	0x4469
	.half	0x6769
	.half	0x7461
	.half	0x6c20
	.half	0x4571
	.half	0x7569
	.half	0x706d
	.half	0x656e
	.half	0x7420
	.half	0x436f
	.half	0x7270
	.half	0x6f72
	.half	0x6174
	.half	0x696f
	.half	0x6e20
	.half	0x2620
	.half	0x494e
	.half	0x5249
	.half	0x4120
	.half	0x3139
	.half	0x3838
	.half	0x2c20
	.half	0x3139
	.half	0x3839
	.half	0xa00

