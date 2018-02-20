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

#include <assert.h>
#include <cm/cmmd.h>
#include <cvl.h>
#include "cm5cvl.h"
#include "host.h"


#define Make_One(NAME)							\
void NAME (dst, src, len, scratch)					\
vec_p dst, src, scratch;						\
int len;								\
{									\
  Func3 (NAME, dst, src, len);						\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_1)


#define Make_Two(NAME)							\
void NAME (dst, src1, src2, len, scratch)				\
vec_p dst, src1, src2, scratch;						\
int len;								\
{									\
  Func4 (NAME, dst, src1, src2, len);					\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_1 | INPLACE_2)


#define Make_Three(NAME)						\
void NAME (dst, src1, src2, src3, len, scratch)				\
vec_p dst, src1, src2, src3, scratch;					\
int len;								\
{									\
  Func5 (NAME, dst, src1, src2, src3, len);				\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_1 | INPLACE_2 | INPLACE_3)


/* Arithmetic, min, max : defined on integers and doubles */
#define Make_Two_zd(BASENAME)						\
Make_Two (Glue (BASENAME,z))						\
Make_Two (Glue (BASENAME,d))

Make_Two_zd (max_wu)
Make_Two_zd (min_wu)
Make_Two_zd (add_wu)
Make_Two_zd (sub_wu)
Make_Two_zd (mul_wu)
Make_Two_zd (div_wu)

/* Comparisons: defined on integers and doubles */
Make_Two_zd (grt_wu)
Make_Two_zd (les_wu)
Make_Two_zd (geq_wu)
Make_Two_zd (leq_wu)

/* Equality: defined on booleans, integers and doubles */
#define Make_Two_bzd(BASENAME)						\
Make_Two (Glue (BASENAME,b))						\
Make_Two (Glue (BASENAME,z))						\
Make_Two (Glue (BASENAME,d))

Make_Two_bzd (eql_wu)
Make_Two_bzd (neq_wu)

/* Shifts, mods and random: defined on integers */
Make_Two (lsh_wuz)
Make_Two (rsh_wuz)
Make_Two (mod_wuz)
Make_One (rnd_wuz)

/* Selection: defined on booleans, integers and doubles */
Make_Three (sel_wub)
Make_Three (sel_wuz)
Make_Three (sel_wud)

/* Logical functions: defined on booleans */
Make_One (not_wub)
Make_Two (ior_wub)
Make_Two (xor_wub)
Make_Two (and_wub)

/* Bitwise functions: defined on integers */
Make_One (not_wuz)
Make_Two (ior_wuz)
Make_Two (xor_wuz)
Make_Two (and_wuz)

/* Math functions: defined on doubles */
Make_One (flr_wud)
Make_One (cei_wud)
Make_One (trn_wud)
Make_One (rou_wud)
Make_One (exp_wud)
Make_One (log_wud)
Make_One (sqt_wud)
Make_One (sin_wud)
Make_One (cos_wud)
Make_One (tan_wud)
Make_One (asn_wud)
Make_One (acs_wud)
Make_One (atn_wud)
Make_One (snh_wud)
Make_One (csh_wud)
Make_One (tnh_wud)

/* Conversion functions */
Make_One (int_wub)
Make_One (int_wud)
Make_One (dbl_wuz)
Make_One (boo_wuz)

/* Copy functions */
Make_One (cpy_wuz)
Make_One (cpy_wub)
Make_One (cpy_wud)

void cpy_wus (dst, src, nelt, nseg, scratch)
vec_p dst, src, scratch;
int nelt, nseg;
{
  Func4 (cpy_wus, dst, src, nelt, nseg);
}
Make_No_Seg_Scratch (cpy_wus)
Make_Inplace (cpy_wus, INPLACE_NONE)
