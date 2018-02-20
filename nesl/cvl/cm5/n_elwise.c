/*
* Copyright (c) 1992, 1993, 1994, 1995
* Carnegie Mellon University SCAL project:
* Guy Blelloch, Jonathan Hardwick, Jay Sipelstein, Marco Zagha
*
* All Rights Reserved.
*
* Permission to use, copy, modify and distribute this software and its
* documentation is hereby granted, provided that both the copyright
* notice and this permission notice appear in all copies of the
* software, derivative works or modified versions, and any portions
* thereof.
*
* CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
* CONDITION.  CARNEGIE MELLON AND THE SCAL PROJECT DISCLAIMS ANY 
* LIABILITY OF ANY KIND FOR ANY DAMAGES WHATSOEVER RESULTING FROM 
* THE USE OF THIS SOFTWARE.
*
* The SCAL project requests users of this software to return to 
*
*  Guy Blelloch				nesl-contribute@cs.cmu.edu
*  School of Computer Science
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3890
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/

#include <math.h>
#include <cm/cmmd.h>
#include <cvl.h>
#include "cm5cvl.h"
#include "node.h"


/* We count down instead of up to save a single instruction.  This
 * provides almost as much of a speedup as counting up and then
 * unrolling the loops with gcc -funroll.  Counting down and unrolling
 * would presumably be best of all, but gcc 2.4.5 doesn't recognize it.
 */

/* ----------- Macro skeleton for one-argument functions ------------- */

#define Make_One(NAME, FUNCT, SRCTYPE, DSTTYPE)				\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  DSTTYPE *dst = (DSTTYPE *) buf[1];					\
  SRCTYPE *src = (SRCTYPE *) buf[2];					\
  int len = buf[3];							\
  int i;								\
									\
  for (i = Num_Here (len) - 1; i >= 0; i--) {				\
    dst[i] = FUNCT (src[i]);						\
  }									\
}									\


/* ----------- Macro skeleton for two-argument functions ------------- */

#define Make_Two(NAME, FUNCT, _srctype, _dsttype)			\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  _dsttype *dst = (_dsttype *) buf[1];					\
  _srctype *src1 = (_srctype *) buf[2];					\
  _srctype *src2 = (_srctype *) buf[3];					\
  int len = buf[4];							\
  int i;								\
									\
  for (i = Num_Here (len) - 1; i >= 0; i--) {				\
    dst[i] = FUNCT (src1[i], src2[i]);					\
  }									\
}									\


/* ------------ Macro skeleton for selection functions --------------- */

#define Make_Sel(NAME, FUNCT, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  cvl_bool *src1 = (cvl_bool *) buf[2];					\
  TYPE *src2 = (TYPE *) buf[3];						\
  TYPE *src3 = (TYPE *) buf[4];						\
  int len = buf[5];							\
  int i;								\
									\
  for (i = Num_Here (len) - 1; i >= 0; i--) {				\
    dst[i] = FUNCT (src1[i], src2[i], src3[i]);				\
  }									\
}									\


/* Now come the go-faster-with-64-bit-store macros.
 */

/* -- Macro skeleton for two-argument integer-to-integer functions --- */

#define Make_Two_Int_Int(NAME, FUNCT)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  oneword *dst = (oneword *) buf[1];					\
  oneword *src1 = (oneword *) buf[2];					\
  oneword *src2 = (oneword *) buf[3];					\
  int mylen = Num_Here (buf[4]);					\
  int i;								\
									\
  /* If there are an odd number of elements, we operate individually	\
   * on the last one, rather than operating on its word as a whole.	\
   * This avoids e.g. divide by zero errors when we operate on garbage	\
   * with div_wuz.							\
   */									\
  if (mylen & 1) {							\
    int *idst = (int *) buf[1];						\
    int *isrc1 = (int *) buf[2];					\
    int *isrc2 = (int *) buf[3];					\
    idst[mylen - 1] = FUNCT (isrc1[mylen - 1], isrc2[mylen - 1]);	\
  }									\
									\
  /* Get and store two ints at a time; use ts1 and ts2 to make gcc load	\
   * them as a single word.						\
   */									\
  for (i = (mylen >> 1) - 1; i >= 0; i--) {				\
    oneword result, ts1 = src1[i], ts2 = src2[i];			\
    result.ints.lo = FUNCT (ts1.ints.lo, ts2.ints.lo);			\
    result.ints.hi = FUNCT (ts1.ints.hi, ts2.ints.hi);			\
    dst[i] = result;							\
  }									\
}									\


/* ----- Macro skeleton for one-argument word-to-word functions ------ */

#define Make_One_Int_Int(NAME, FUNCT)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  oneword *dst = (oneword *) buf[1];					\
  oneword *src = (oneword *) buf[2];					\
  int mylen = Num_Here (buf[3]);					\
  int i;								\
									\
  if (mylen & 1) {							\
    int *idst = (int *) buf[1];						\
    int *isrc = (int *) buf[2];						\
    idst[mylen - 1] = FUNCT (isrc[mylen - 1]);				\
  }									\
									\
  /* Get and store two ints at a time */				\
  for (i = (mylen >> 1) - 1; i >= 0; i--) {				\
    oneword result, ts = src[i];					\
    result.ints.lo = FUNCT (ts.ints.lo);				\
    result.ints.hi = FUNCT (ts.ints.hi);				\
    dst[i] = result;							\
  }									\
}									\


/* Arithmetic, min, max: defined on integers and doubles */
#define Make_Two_zd(BASENAME, FUNCT)					\
Make_Two_Int_Int (Glue (BASENAME,z_), FUNCT)				\
Make_Two (Glue (BASENAME,d_), FUNCT, double, double)

Make_Two_zd (max_wu, max)
Make_Two_zd (min_wu, min)
Make_Two_zd (add_wu, plus)
Make_Two_zd (sub_wu, minus)
Make_Two_zd (mul_wu, times)
Make_Two_zd (div_wu, divide)

/* XXX we can only get away with using Make_Two_Int_Int and
 * Make_One_Int_Int for cvl_bool stuff because cvl_bool is (currently)
 * an unsigned int.
 */

/* Comparisons: defined on integers and doubles */
#define Make_Two_cmp_zd(BASENAME, FUNCT)				\
Make_Two_Int_Int (Glue (BASENAME,z_), FUNCT)				\
Make_Two (Glue (BASENAME,d_), FUNCT, double, cvl_bool)

Make_Two_cmp_zd (grt_wu, gt)
Make_Two_cmp_zd (les_wu, lt)
Make_Two_cmp_zd (geq_wu, geq)
Make_Two_cmp_zd (leq_wu, leq)

/* Equality: defined on booleans, integers and doubles */
#define Make_Two_eql_bzd(BASENAME, FUNCT)				\
Make_Two_Int_Int (Glue (BASENAME,b_), FUNCT)				\
Make_Two_Int_Int (Glue (BASENAME,z_), FUNCT)				\
Make_Two (Glue (BASENAME,d_), FUNCT, double, cvl_bool)

Make_Two_eql_bzd (eql_wu, eq)
Make_Two_eql_bzd (neq_wu, neq)

/* Shifts, mods and random: defined on integers */
Make_Two_Int_Int (lsh_wuz_, lshift)
Make_Two_Int_Int (rsh_wuz_, rshift)
Make_Two_Int_Int (mod_wuz_, mod)
Make_One_Int_Int (rnd_wuz_, cvlrand)

/* Selection: defined on booleans, integers and doubles */
/* XXX special-case 64-bit selection for ints and cvl_bools? */
Make_Sel (sel_wub_, select, cvl_bool)
Make_Sel (sel_wuz_, select, int)
Make_Sel (sel_wud_, select, double)

/* Logical functions: defined on booleans */
Make_One_Int_Int (not_wub_, not)
Make_Two_Int_Int (xor_wub_, xor)
Make_Two_Int_Int (ior_wub_, bor)
Make_Two_Int_Int (and_wub_, band)

/* Bitwise functions: defined on integers */
Make_One_Int_Int (not_wuz_, bnot)
Make_Two_Int_Int (xor_wuz_, xor)
Make_Two_Int_Int (ior_wuz_, bor)
Make_Two_Int_Int (and_wuz_, band)

/* Math functions: defined on doubles */
Make_One (flr_wud_, cvl_floor, double, int)
Make_One (cei_wud_, cvl_ceil, double, int)
Make_One (trn_wud_, d_to_z, double, int)
Make_One (rou_wud_, cvl_round, double, int)
Make_One (exp_wud_, exp, double, double)
Make_One (log_wud_, log, double, double)
Make_One (sqt_wud_, sqrt, double, double)
Make_One (sin_wud_, sin, double, double)
Make_One (cos_wud_, cos, double, double)
Make_One (tan_wud_, tan, double, double)
Make_One (asn_wud_, asin, double, double)
Make_One (acs_wud_, acos, double, double)
Make_One (atn_wud_, atan, double, double)
Make_One (snh_wud_, sinh, double, double)
Make_One (csh_wud_, cosh, double, double)
Make_One (tnh_wud_, tanh, double, double)

/* Conversion functions */
Make_One (int_wud_, d_to_z, double, int)
Make_One_Int_Int (int_wub_, b_to_z)
Make_One (dbl_wuz_, z_to_d, int, double)
Make_One_Int_Int (boo_wuz_, z_to_b)

/* Copy functions */

/* XXX special-case this to take advantage of word-at-a-time? */

#define Make_Cpy(NAME, ELTSPERWORD, SHIFT)				\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  oneword *dst = (oneword *) buf[1];					\
  oneword *src = (oneword *) buf[2];					\
  int mylen = (Num_Here (buf[3]) + ELTSPERWORD - 1) >> SHIFT;		\
  int i;								\
									\
  for (i = mylen - 1; i >= 0; i--) {					\
    dst[i] = src[i];							\
  }									\
}									\

Make_Cpy (cpy_wuz_, 2, 1)
Make_Cpy (cpy_wub_, 2, 1)
Make_Cpy (cpy_wud_, 1, 0)


void cpy_wus_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  int *dst = (int *) buf[1];
  int *src = (int *) buf[2];
  int nelt = buf[3];
  int nseg = buf[4];
  int i;

  /* XXX copy a word at a time */

  /* Copy segment start positions. */
  for (i = Num_Here (nelt) - 1; i >= 0; i--) {
    dst[i] = src[i];
  }

  dst += Space_For (nseg);
  src += Space_For (nseg);

  /* Copy segment start bits. */
  for (i = Num_Here (nelt) - 1; i >= 0; i--) {
    dst[i] = src[i];
  }

  dst += Space_For (nelt);
  src += Space_For (nelt);

  /* Copy segment number of first element on each processor. */
  *dst++ = *src++;

  /* Copy start address of segment of the first element on processor. */
  *dst = *src;
}
