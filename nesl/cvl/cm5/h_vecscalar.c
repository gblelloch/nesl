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


/* -------------------- Unsegmented Distribute ----------------------- */

#define Make_Distribute(NAME, TYPE)					\
void NAME (dst, value, len, scratch)					\
vec_p dst, scratch;							\
TYPE value;								\
int len;								\
{									\
  Func3 (NAME, dst, value, len);					\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)

Make_Distribute (dis_vuz, int)
Make_Distribute (dis_vub, cvl_bool)


void dis_vud (dst, value, len, scratch)
vec_p dst, scratch;
double value;
int len;
{
  /* Func3 turns everything into ints -- not good for passing doubles.
   * This replaces Func3 (NAME, dst, value, len);
   */
  bcbuf buf;
  buf.d[0] = dis_vud_tag;
  buf.d[1] = (int) dst;
  *((double *) &buf.d[2]) = value;
  buf.d[4] = len;
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));
}
Make_No_Scratch (dis_vud)
Make_Inplace (dis_vud, INPLACE_NONE)


/* --------------------- Segmented Distribute ------------------------ */

#define Make_Seg_Dist(NAME, UNSEG, EXTRACT)				\
void NAME (dst, value, segd, nelt, nseg, scratch)			\
vec_p dst, value, segd, scratch;					\
int nelt, nseg;								\
{									\
  if (nseg == 1) {							\
    /* "len" argument to the EXTRACT function is irrelevant because we	\
     * know that we are always getting the zeroth element on the	\
     * zeroth processor, and "len" won't affect this.  Any non-zero	\
     * value (e.g. 1) will do.						\
     */									\
    UNSEG (dst, EXTRACT (value, 0, 1, scratch), nelt, scratch);		\
  } else {								\
    Func5 (NAME, dst, value, segd, nelt, nseg);				\
  }									\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Seg_Dist (dis_vez, dis_vuz, ext_vuz)
Make_Seg_Dist (dis_ved, dis_vud, ext_vud)
Make_Seg_Dist (dis_veb, dis_vub, ext_vub)


/* ---------------------- Unsegmented Replace ------------------------ */

#define Make_Rep(NAME, TYPE)						\
void NAME (dst, elt, val, len, scratch)					\
vec_p dst, scratch;							\
int elt, len;								\
TYPE val;								\
{									\
  Func4 (NAME, dst, elt, val, len);					\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)

Make_Rep (rep_vuz, int)
Make_Rep (rep_vub, cvl_bool)


void rep_vud (dst, elt, val, len, scratch)
vec_p dst, scratch;
int elt, len;
double val;
{
  /* This replaces Func4 (NAME, dst, elt, val, len);
   */
  bcbuf buf;
  buf.d[0] = rep_vud_tag;
  buf.d[1] = (int) dst;
  buf.d[2] = elt;
  buf.d[3] = len;
  *((double *) &buf.d[4]) = val;
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));
}
Make_No_Scratch (rep_vud)
Make_Inplace (rep_vud, INPLACE_NONE)


/* ----------------------- Segmented Replace ------------------------- */

#define Make_Seg_Rep(NAME)						\
void NAME (dst, index, value, segd, nelt, nseg, scratch)		\
vec_p dst, index, value, segd, scratch;					\
int nelt, nseg;								\
{									\
  Func6 (NAME, dst, index, value, segd, nelt, nseg);			\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Seg_Rep (rep_vez)
Make_Seg_Rep (rep_ved)
Make_Seg_Rep (rep_veb)


/* --------------------- Unsegmented Extract ------------------------- */

#define Make_Ext(TYPE, NAME, IDENT, COMBINER)				\
TYPE NAME (src, elt, len, scratch)					\
vec_p src, scratch;							\
int elt, len;								\
{									\
  Func3 (NAME, src, elt, len);						\
  return Glue (CMMD_reduce_from_nodes_,TYPE) (IDENT, COMBINER);		\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)

Make_Ext (int, ext_vuz, 0, CMMD_combiner_add)
Make_Ext (uint, ext_vub, 0, CMMD_combiner_uadd)
Make_Ext (double, ext_vud, 0.0, CMMD_combiner_dadd)


/* ----------------------- Segmented Extract ------------------------- */

#define Make_Seg_Ext(NAME)						\
void NAME (dst, src, index, segd, nelt, nseg, scratch)			\
vec_p dst, src, index, segd, scratch;					\
int nelt, nseg;								\
{									\
  Func6 (NAME, dst, src, index, segd, nelt, nseg);			\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Seg_Ext (ext_vez)
Make_Seg_Ext (ext_ved)
Make_Seg_Ext (ext_veb)
