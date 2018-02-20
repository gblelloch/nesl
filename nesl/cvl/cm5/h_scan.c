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


/* ---------------------- Unsegmented Reduce ------------------------- */

#define Make_Reduce(TYPE, NAME, INIT, COMBINER)				\
TYPE NAME (src, len, scratch)						\
vec_p src, scratch;							\
int len;								\
{									\
  Func2 (NAME, src, len);						\
  return (Glue (CMMD_reduce_from_nodes_,TYPE) (INIT, COMBINER));	\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)

Make_Reduce (int, add_ruz, 0, CMMD_combiner_add)
Make_Reduce (int, mul_ruz, 0, CMMD_combiner_add) /* see n_scan.c */
Make_Reduce (int, min_ruz, MAXINT, CMMD_combiner_min)
Make_Reduce (int, max_ruz, MININT, CMMD_combiner_max)
Make_Reduce (int, and_ruz, ~0, CMMD_combiner_and)
Make_Reduce (int, ior_ruz, 0, CMMD_combiner_ior)
Make_Reduce (int, xor_ruz, 0, CMMD_combiner_xor)
Make_Reduce (double, add_rud, 0.0, CMMD_combiner_dadd)
Make_Reduce (double, mul_rud, 0.0, CMMD_combiner_dadd) /* see n_scan.c */
Make_Reduce (double, min_rud, MAXDBL, CMMD_combiner_dmin)
Make_Reduce (double, max_rud, MINDBL, CMMD_combiner_dmax)
Make_Reduce (uint, and_rub, TRUE, CMMD_combiner_and)
Make_Reduce (uint, ior_rub, FALSE, CMMD_combiner_ior)
Make_Reduce (uint, xor_rub, FALSE, CMMD_combiner_xor)


/* ----------------------- Segmented Reduce -------------------------- */

#define Make_Seg_Reduce(NAME, UNSEG, REPLACE)				\
void NAME (dst, src, segd, nelt, nseg, scratch)				\
vec_p dst, src, segd, scratch;						\
int nelt, nseg;								\
{									\
  if (nseg == 1) {							\
    /* "len" argument to the REPLACE function is irrelevant because we	\
     * know that we are always replacing the zeroth element on the	\
     * zeroth processor, and "len" won't affect this.  Any non-zero	\
     * value (e.g. 1) will do.						\
     */									\
    REPLACE (dst, 0, UNSEG (src, nelt, scratch), 1, scratch);		\
  } else {								\
    Func5 (NAME, dst, src, segd, nelt, nseg);				\
  }									\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Seg_Reduce (add_rez, add_ruz, rep_vuz)
Make_Seg_Reduce (mul_rez, mul_ruz, rep_vuz)
Make_Seg_Reduce (min_rez, min_ruz, rep_vuz)
Make_Seg_Reduce (max_rez, max_ruz, rep_vuz)
Make_Seg_Reduce (and_rez, and_ruz, rep_vuz)
Make_Seg_Reduce (ior_rez, ior_ruz, rep_vuz)
Make_Seg_Reduce (xor_rez, xor_ruz, rep_vuz)
Make_Seg_Reduce (add_red, add_rud, rep_vud)
Make_Seg_Reduce (mul_red, mul_rud, rep_vud)
Make_Seg_Reduce (min_red, min_rud, rep_vud)
Make_Seg_Reduce (max_red, max_rud, rep_vud)
Make_Seg_Reduce (and_reb, and_rub, rep_vub)
Make_Seg_Reduce (ior_reb, ior_rub, rep_vub)
Make_Seg_Reduce (xor_reb, xor_rub, rep_vub)


/* ----------------------- Unsegmented Scan ------------------------- */

#define Make_Scan(NAME)							\
void NAME (dst, src, len, scratch)					\
vec_p dst, src, scratch;						\
int len;								\
{									\
  Func3 (NAME, dst, src, len);						\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)

Make_Scan (add_suz)
Make_Scan (mul_suz)
Make_Scan (min_suz)
Make_Scan (max_suz)
Make_Scan (and_suz)
Make_Scan (ior_suz)
Make_Scan (xor_suz)
Make_Scan (add_sud)
Make_Scan (mul_sud)
Make_Scan (min_sud)
Make_Scan (max_sud)
Make_Scan (and_sub)
Make_Scan (ior_sub)
Make_Scan (xor_sub)


/* ------------------------ Segmented Scan --------------------------- */

#define Make_Seg_Scan(NAME)						\
void NAME (dst, src, segd, nelt, nseg, scratch)				\
vec_p dst, src, segd, scratch;						\
int nelt, nseg;								\
{									\
  Func5 (NAME, dst, src, segd, nelt, nseg);				\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Seg_Scan (add_sez)
Make_Seg_Scan (mul_sez)
Make_Seg_Scan (max_sez)
Make_Seg_Scan (min_sez)
Make_Seg_Scan (and_sez)
Make_Seg_Scan (ior_sez)
Make_Seg_Scan (xor_sez)
Make_Seg_Scan (add_sed)
Make_Seg_Scan (mul_sed)
Make_Seg_Scan (max_sed)
Make_Seg_Scan (min_sed)
Make_Seg_Scan (and_seb)
Make_Seg_Scan (ior_seb)
Make_Seg_Scan (xor_seb)
