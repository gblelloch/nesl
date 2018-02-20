/*
* Copyright (c) 1992 Carnegie Mellon University 
*                    SCAL project: Guy Blelloch, Siddhartha Chatterjee,
*                                  Jonathan Hardwick, Jay Sipelstein,
*                                  Marco Zagha
* All Rights Reserved.
*
* Permission to use, copy, modify and distribute this software and its
* documentation is hereby granted, provided that both the copyright
* notice and this permission notice appear in all copies of the
* software, derivative works or modified versions, and any portions
* thereof, and that both notices appear in supporting documentation.
*
* CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
* CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
* ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
*
* The SCAL project requests users of this software to return to 
*
*  Guy Blelloch				guy.blelloch@cs.cmu.edu
*  School of Computer Science
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3890
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/
/* ----- Miscellaneous ----------------------------------------*/

#define TRUE 1
#define FALSE 0

/* Portable way to glue two strings together in a macro */
#ifdef __STDC__
# 	define GLUE(a,b) a##b
#else
#	define GLUE(a,b) a/**/b
#endif /* __STDC__ */

/* -------------- Scratch Functions ---------------------------*/
/* Every CVL function has an associated scratch function that tells
 * how much scratch space may be needed for it.  This memory must be
 * allocated by the calling function.
 */
#define make_scratch(_name, _body)				\
	int GLUE(_name,_scratch)(len) int len;{_body}

#define NO_SCRATCH return 0;
#define NULL_SCRATCH ((vec_p) NULL)
#define make_no_scratch(_name) make_scratch(_name,NO_SCRATCH)

#define make_no2_scratch(_name)					\
	int GLUE(_name,_scratch)(s_len, d_len)			\
	int s_len, d_len;					\
	{NO_SCRATCH}

#define make_no_seg_scratch(_name) 				\
	int GLUE(_name,_scratch)(vec_len, seg_len)  		\
	int vec_len, seg_len;					\
	{NO_SCRATCH}

#define make_no_seg2_scratch(_name)				\
	int GLUE(_name,_scratch)				\
	        (s_vec_len, s_seg_len, d_vec_len, d_seg_len)	\
	int s_vec_len, s_seg_len, d_vec_len, d_seg_len;		\
	{NO_SCRATCH}

/* --------------Inplace Functions ----------------------------*/
/* Every CVL function has an associated inplace function indicating 
 * whether or not that function can be performed inplace.  This can
 * be used by the calling function to optimize memory usage.
 */

#define make_inplace(_name, _flag)				\
	unsigned int GLUE(_name,_inplace)() {return _flag;}

/* --------------- Small function names ----------------------*/

#define max(i,j)	((i) > (j) ? (i) : (j))
#define min(i,j)	((i) < (j) ? (i) : (j))
#define maxi(i,j)	((*i) > (*j) ? (j++, *i++) : (i++, *j++))
#define mini(i,j)	((*i) < (*j) ? (j++, *i++) : (i++, *j++))

#define lshift(i,j)	((i) << (j))
#define rshift(i,j)	((i) >> (j))
#define plus(i,j)	((i) + (j))
#define minus(i,j)	((i) - (j))
#define times(i,j)	((i) * (j))
#define div(i,j)	((i) / (j))
#define mod(i,j)	((i) % (j))
#define gt(i,j)		((i) > (j))
#define lt(i,j)		((i) < (j))
#define eq(i,j)		((i) == (j))
#define neq(i,j)	((i) != (j))
#define leq(i,j)	((i) <= (j))
#define geq(i,j)	((i) >= (j))

extern double _ranf();
#define cvlrand(i)		((int) (_ranf()*i))

#define or(i,j)		((i) || (j))  		 /* logical or */
#define bor(i,j)	((i) | (j))              /* bitwise or */
#define and(i,j)	((i) && (j))
#define band(i,j)	((i) & (j))
#define not(i)		(! (i))
#define bnot(i)		(~ (i))
#define xor(i,j)	((i) ^ (j))		/* bitwise xor */
#define lxor(i,j)	((!!(i)) ^ (!!(j)))	/* logical xor */

#define neg(i)		(-(i))
#define ident(i)	(i)
#define notnot(i)	(!!(i))


#define select(i,j,k)   ((i) ? (j) : (k))
#define selecti(i,j,k)   (*(i++) ? (k++, *(j++)) : (j++, *(k++)))

#define d_to_z(x) ((int) (x))
#define b_to_z(x) ((int) (x))
#define z_to_d(x) ((double) (x))
#define z_to_b(x) ((cvl_bool) notnot(x))

#ifndef cray  /* cray not IEEE */
#define cvl_round(x) ((int) rint(x))
#else
#define cvl_round(x) ((int) ((x) + 0.5))
#endif

#ifndef cray
#define cvl_floor(x) ((int) floor(x))
#define cvl_ceil(x) ((int) ceil(x))
#else
#define cvl_floor(x) ((int)(x) == (x) ? (int)(x) : ((int)(x) - ((x) < 0)))
#define cvl_ceil(x)  ((int)(x) == (x) ? (int)(x) : ((int)(x) + ((x) > 0)))
#endif

/* ---------------------Unrolling Macros -------------------------*/

/* set UNROLL_FACTOR in Makefile 
 * Default is not to unroll
 */

#if UNROLL_FACTOR == 2
#define unroll(len, st)  unroll2(len,st)
#elif UNROLL_FACTOR == 8
#define unroll(len, st)  unroll8(len,st)
#elif UNROLL_FACTOR == 4
#define unroll(len, st)  unroll4(len,st)
#else
#define unroll(len, st)  unroll1(len,st)
#endif

/* Unroll loop UNROLL_FACTOR times.  Checks for zero length vectors.
 * if st contains variable n, we're hosed! 
 * Is there hygenic way of doing this?
 * Modified old version to make inner loop a single basic block.  This
 * should be better for pipelined RISCs.
 */

/* Another way to write these macros is to take a final value and a test
 * function, instead of a count.  Since all these macros are used to push
 * pointers through vectors, we can save an increment on each iteration of
 * the unrolled loop.  This results in (even) uglier macros and code, so I
 * didn't bother.
 */

#define unroll1(len, st)			\
{						\
	register n = len + 1;			\
	while (--n) {st}			\
}

#define unroll2(len, st) 			\
{						\
	register n = (len + 1) >> 1 ;   	\
	if (n > 0) { 				\
		if (len & 1) {			\
			n--; st			\
		} 				\
		while (n--) {st st} 		\
	}					\
}

#define unroll4(len, st)			\
	{ 					\
	register n = (len + 3) >> 2;		\
	if (n > 0) { 				\
		switch (len & 3) { 		\
			case 3:  st 		\
			case 2:  st 		\
			case 1:  n--; st 	\
			default: ;		\
		} 				\
		while (n--) {st st st st} 	\
	} 					\
}

#define unroll8(len, st) 			\
{	 					\
	register n = (len + 7) >> 3;		\
	if (n > 0) { 				\
		switch (len & 7) { 		\
			case 7:  st 		\
			case 6:  st 		\
			case 5:  st 		\
			case 4:  st 		\
			case 3:  st 		\
			case 2:  st 		\
			case 1:  n--; st	\
			default: ;		\
		} 				\
		while (n--) {st st st st	\
			     st st st st}	\
	} 					\
} 

/* ----------------- Limits -------------------------------------*/
/* ANSI standard requires a limits.h file with this sort of information,
 * but it doesn't seem to exist in our environment.
 */

/* For {MAX,MIN}LONG, replace the 0 with 0L. */
/* Is there a standard .h file with this? */
#define	MAX_INT	((int)(((unsigned) ~0)>>1))
#define	MIN_INT	(~MAX_INT)

/* arbritrarily choosen, MACHINE DEPENDENT.  Works on Suns, Vaxs and
   RTs. HUGE (from math.h) won't work since some compilers complain it is
   too big, and because it returns Infinity or NaN when printed. */
#define MIN_FLOAT   ((float)-1.0e36)
#define MAX_FLOAT   ((float)1.0e36)

#define MAX_DOUBLE (double)MAX_FLOAT
#define MIN_DOUBLE (double)MIN_FLOAT
