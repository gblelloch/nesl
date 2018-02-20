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


/* ----------------------- Unsegmented Index ------------------------- */

void ind_luz (dst, init, stride, count, scratch)
vec_p dst, scratch;
int init, stride, count;
{
  Func4 (ind_luz, dst, init, stride, count);
}
Make_No_Scratch (ind_luz)
Make_Inplace (ind_luz, INPLACE_NONE)


/* ------------------------ Segmented Index -------------------------- */

void ind_lez (dst, init, stride, segd, nelt, nseg, scratch)
vec_p dst, init, stride, segd, scratch;
int nelt, nseg;
{
/* XXX extract from dst, init, stride, and count
  if (nseg == 1) {
    Func2 (ind_luz, dst, nelt);
  } else
 */
  {
    Func7 (ind_lez, dst, init, stride, segd, nelt, nseg, scratch);
  }
}
int ind_lez_scratch (nelt, nseg) int nelt, nseg;
{ return (nseg == 0) ? 0 : siz_foz (nelt); }
Make_Inplace (ind_lez, INPLACE_NONE)


/* ----------------------- Unsegmented Pack -------------------------- */

/* Part 1: pk1_luv returns the number of flags set
 */

int pk1_luv (flg, len, scratch)
vec_p flg, scratch;
int len;
{
  Func2 (pk1_luv, flg, len);
  return CMMD_reduce_from_nodes_int (0, CMMD_combiner_add);
}
Make_No_Scratch (pk1_luv)
Make_Inplace (pk1_luv, INPLACE_NONE)


/* Part 2: pk2_lu[zbd] fills a destination vector with elements
 * corresponding to the true elements of the flag vector
 */

#define Make_Pk2(NAME)							\
void NAME (dst, src, flg, src_len, dst_len, scratch)			\
vec_p dst, src, flg, scratch;						\
int src_len, dst_len;							\
{									\
  Func5 (NAME, dst, src, flg, src_len, dst_len);			\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)

Make_Pk2 (pk2_luz)
Make_Pk2 (pk2_lub)
Make_Pk2 (pk2_lud)



/* ----------------------- Segmented Pack ---------------------------- */

/* Part 1: pk1_lev takes a segmented vector of flags, and returns the
 * lengths vector (segment descriptor) describing the result of the
 * final pack.	In other words, it counts the number of flags set in
 * each segment.  So we can use a segmented +_reduce.
 */

void pk1_lev (dst, flg, segd, nelt, nseg, scratch)
vec_p dst, flg, segd, scratch;
int nelt, nseg;
{
  Func5 (add_rez, dst, flg, segd, nelt, nseg);
}
Make_No_Seg_Scratch (pk1_lev)
Make_Inplace (pk1_lev, INPLACE_NONE)


/* Part 2: pk2_le[zbd] are just the same as pk2_lu[zbd].
 */

#define Make_Seg_Pck(NAME, UNSEG)					\
void NAME (dst, src, flg, src_segd, src_nelt, src_nseg, dst_segd,	\
	   dst_nelt, dst_nseg, scratch)					\
vec_p dst, src, flg, src_segd, dst_segd, scratch;			\
int src_nelt, src_nseg, dst_nelt, dst_nseg;				\
{									\
  Func5 (UNSEG, dst, src, flg, src_nelt, dst_nelt);			\
  assert (src_nseg == dst_nseg);					\
}									\
int Glue (NAME,_scratch) (s_n, s_m, d_n, d_m) int s_n, s_m, d_n, d_m;	\
{ return 0; }								\
Make_Inplace (NAME, INPLACE_NONE)

Make_Seg_Pck (pk2_lez, pk2_luz)
Make_Seg_Pck (pk2_leb, pk2_lub)
Make_Seg_Pck (pk2_led, pk2_lud)


/* ----------------------- Unsegmented Rank -------------------------- */

#define Make_Rank(NAME, SPACE)						\
void NAME (dst, src, len, scratch)					\
vec_p dst, src, scratch;						\
int len;								\
{									\
  Func4 (NAME, dst, src, len, scratch);					\
}									\
int Glue (NAME,_scratch) (len) int len;					\
{ return SPACE * siz_foz (len); }					\
Make_Inplace (NAME, INPLACE_1)						\

Make_Rank (rku_luz, 3)
Make_Rank (rkd_luz, 3)
Make_Rank (rku_lud, 6)
Make_Rank (rkd_lud, 6)


/* ------------------------- Segmented Rank ---------------------------- */

#define Make_Seg_Rank(NAME, UNSEG, SEGSPACE, UNSEGSPACE)		\
void NAME (dst, src, segd, nelt, nseg, scratch)				\
vec_p dst, src, segd, scratch;						\
int nelt, nseg;								\
{									\
  if (nseg > 1) {  							\
    Func6 (NAME, dst, src, segd, nelt, nseg, scratch);			\
  } else {								\
    Func4 (UNSEG, dst, src, nelt, scratch);				\
  }									\
}									\
int Glue (NAME,_scratch) (nelt, nseg) int nelt, nseg;			\
{ return ((nseg > 1) ? SEGSPACE : UNSEGSPACE ) * siz_foz (nelt); }	\
Make_Inplace (NAME, INPLACE_1)

Make_Seg_Rank (rku_lez, rku_luz, 6, 3)
Make_Seg_Rank (rkd_lez, rkd_luz, 6, 3)
Make_Seg_Rank (rku_led, rku_lud, 8, 6)
Make_Seg_Rank (rkd_led, rkd_lud, 8, 6)
