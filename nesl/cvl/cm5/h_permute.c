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


/* ------------------- Unsegmented Simple Permute -------------------- */

#define Make_Smp(NAME, TYPE)						\
void NAME (dst, src, index, len, scratch)				\
vec_p dst, src, index, scratch;						\
int len;								\
{									\
  if (len) {								\
    Func4 (NAME, dst, src, index, len);					\
  }									\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)

Make_Smp (smp_puz, int)
Make_Smp (smp_pub, cvl_bool)
Make_Smp (smp_pud, double)


/* -------------------- Segmented Simple Permute --------------------- */

#define Make_Smp_Seg(NAME, UNSEG, TYPE)					\
void NAME (dst, src, index, segd, nelt, nseg, scratch)			\
vec_p dst, src, index, segd, scratch;					\
int nelt, nseg;								\
{									\
  if (nelt) {								\
    if (nseg == 1) {							\
      Func4 (UNSEG, dst, src, index, nelt);				\
    } else {								\
      Func6 (NAME, dst, src, index, segd, nelt, nseg);			\
    }									\
  }									\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Smp_Seg (smp_pez, smp_puz, int)
Make_Smp_Seg (smp_peb, smp_pub, cvl_bool)
Make_Smp_Seg (smp_ped, smp_pud, double)


/* ---------------- Unsegmented Simple Flag Permute ------------------ */

#define Make_Fpm(NAME, TYPE)						\
void NAME (dst, src, index, flag, src_len, dst_len, scratch)		\
vec_p dst, src, index, flag, scratch;					\
int src_len, dst_len;							\
{									\
  if (dst_len) {							\
    Func6 (NAME, dst, src, index, flag, src_len, dst_len);		\
  }									\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Fpm (fpm_puz, int)
Make_Fpm (fpm_pub, cvl_bool)
Make_Fpm (fpm_pud, double)


/* ----------------- Segmented Simple Flag Permute ------------------- */

#define Make_Fpm_Seg(NAME, UNSEG, TYPE)					\
void NAME (dst, src, index, flag, src_segd, src_nelt, src_nseg,		\
	   dst_segd, dst_nelt, dst_nseg, scratch)			\
vec_p dst, src, index, flag, src_segd, dst_segd, scratch;		\
int src_nelt, src_nseg, dst_nelt, dst_nseg;				\
{									\
  if (src_nelt) {							\
    assert (src_nseg == dst_nseg);					\
    if (src_nseg == 1) {						\
      Func6 (UNSEG, dst, src, index, flag, src_nelt, dst_nelt);		\
    } else {								\
      Func10 (NAME, dst, src, index, flag, src_segd, src_nelt,		\
	      src_nseg, dst_segd, dst_nelt, scratch);			\
    }									\
  }									\
}									\
int Glue(NAME,_scratch) (s_n, s_m, d_n, d_m) int s_n, s_m, d_n, d_m;	\
{ return (s_m == 1) ? 0 : siz_foz(s_n); }				\
Make_Inplace (NAME, INPLACE_NONE)

Make_Fpm_Seg (fpm_pez, fpm_puz, int)
Make_Fpm_Seg (fpm_peb, fpm_pub, cvl_bool)
Make_Fpm_Seg (fpm_ped, fpm_pud, double)


/* ----------------- Unsegmented Backwards Permute ------------------- */

#define Make_Bck(NAME, TYPE)						\
void NAME (dst, src, index, src_len, dst_len, scratch)			\
vec_p dst, src, index, scratch;						\
int src_len, dst_len;							\
{									\
  if (dst_len) {							\
    Func5 (NAME, dst, src, index, src_len, dst_len);			\
  }									\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Bck (bck_puz, int)
Make_Bck (bck_pub, cvl_bool)
Make_Bck (bck_pud, double)


/* ------------------- Segmented Backwards Permute ------------------- */

#define Make_Bck_Seg(NAME, UNSEG, TYPE)					\
void NAME (dst, src, index, src_segd, src_nelt, src_nseg, dst_segd,	\
	   dst_nelt, dst_nseg, scratch)					\
vec_p dst, src, index, src_segd, dst_segd, scratch;			\
int src_nelt, src_nseg, dst_nelt, dst_nseg;				\
{									\
  if (dst_nelt) {							\
    assert (src_nseg == dst_nseg);					\
    if (src_nseg == 1) {						\
      Func5 (UNSEG, dst, src, index, src_nelt, dst_nelt);		\
    } else {								\
      Func9 (NAME, dst, src, index, src_segd, src_nelt, src_nseg,	\
	     dst_segd, dst_nelt, scratch);				\
    }									\
  }									\
}									\
int Glue(NAME,_scratch) (s_n, s_m, d_n, d_m) int s_n, s_m, d_n, d_m;	\
{ return (s_m == 1) ? 0 : siz_foz(d_n); }				\
Make_Inplace (NAME, INPLACE_NONE)

Make_Bck_Seg (bck_pez, bck_puz, int)
Make_Bck_Seg (bck_peb, bck_pub, cvl_bool)
Make_Bck_Seg (bck_ped, bck_pud, double)


/* --------------- Unsegmented Backwards Flag Permute ---------------- */

#define Make_Bfp(NAME, TYPE)						\
void NAME (dst, src, index, flag, src_len, dst_len, scratch)		\
vec_p dst, src, index, flag, scratch;					\
int src_len, dst_len;							\
{									\
  if (dst_len) {							\
    Func6 (NAME, dst, src, index, flag, src_len, dst_len);		\
  }									\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Bfp (bfp_puz, int)
Make_Bfp (bfp_pub, cvl_bool)
Make_Bfp (bfp_pud, double)


/* ---------------- Segmented Backwards Flag Permute ----------------- */

#define Make_Bfp_Seg(NAME, UNSEG, TYPE)					\
void NAME (dst, src, index, flag, src_segd, src_nelt, src_nseg,		\
	   dst_segd, dst_nelt, dst_nseg, scratch)			\
vec_p dst, src, index, flag, src_segd, dst_segd, scratch;		\
int src_nelt, src_nseg, dst_nelt, dst_nseg;				\
{									\
  if (dst_nelt) {							\
    assert (src_nseg == dst_nseg);					\
    if (src_nseg == 1) {						\
      Func6 (UNSEG, dst, src, index, flag, src_nelt, dst_nelt);		\
    } else {								\
      Func10 (NAME, dst, src, index, flag, src_segd, src_nelt,		\
	      src_nseg, dst_segd, dst_nelt, scratch);			\
    }									\
  }									\
}									\
int Glue(NAME,_scratch) (s_n, s_m, d_n, d_m) int s_n, s_m, d_n, d_m;	\
{ return (s_m == 1) ? 0 : siz_foz(d_n); }				\
Make_Inplace (NAME, INPLACE_NONE)

Make_Bfp_Seg (bfp_pez, bfp_puz, int)
Make_Bfp_Seg (bfp_peb, bfp_pub, cvl_bool)
Make_Bfp_Seg (bfp_ped, bfp_pud, double)


/* ------------------ Unsegmented Default Permute -------------------- */

#define Make_Dpe(NAME, TYPE)						\
void NAME (dst, src, index, defaultv, src_len, dst_len, scratch)	\
vec_p dst, src, index, defaultv, scratch;				\
int src_len, dst_len;							\
{									\
  Func6 (NAME, dst, src, index, defaultv, src_len, dst_len);		\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Dpe (dpe_puz, int);
Make_Dpe (dpe_pub, cvl_bool);
Make_Dpe (dpe_pud, double);


/* ------------------- Segmented Default Permute --------------------- */

#define Make_Dpe_Seg(NAME, UNSEG, TYPE)					\
void NAME (dst, src, index, defaultv, src_segd, src_nelt, src_nseg,	\
	   dst_segd, dst_nelt, dst_nseg, scratch)			\
vec_p dst, src, index, defaultv, src_segd, dst_segd, scratch;		\
int src_nelt, src_nseg, dst_nelt, dst_nseg;				\
{									\
  if (src_nseg == 1) {							\
    Func6 (UNSEG, dst, src, index, defaultv, src_nelt, dst_nelt);	\
  } else {								\
    Func10 (NAME, dst, src, index, defaultv, src_segd, src_nelt,	\
	    src_nseg, dst_segd, dst_nelt, scratch);			\
  }									\
}									\
int Glue (NAME,_scratch) (s_n, s_m, d_n, d_m) int s_n, s_m, d_n, d_m;	\
{ return (s_m == 1) ? 0 : siz_foz(s_n); }				\
Make_Inplace (NAME, INPLACE_NONE)

Make_Dpe_Seg (dpe_pez, dpe_puz, int)
Make_Dpe_Seg (dpe_peb, dpe_pub, cvl_bool)
Make_Dpe_Seg (dpe_ped, dpe_pud, double)


/* ---------------- Unsegmented Default Flag Permute ----------------- */

#define Make_Dfp(NAME, TYPE)						\
void NAME (dst, src, index, flag, defaultv, src_len, dst_len, scratch)	\
vec_p dst, src, index, flag, defaultv, scratch;				\
int src_len, dst_len;							\
{									\
  Func7 (NAME, dst, src, index, flag, defaultv, src_len, dst_len);	\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)

Make_Dfp (dfp_puz, int);
Make_Dfp (dfp_pub, cvl_bool);
Make_Dfp (dfp_pud, double);


/* ----------------- Segmented Default Flag Permute ------------------ */

#define Make_Dfp_Seg(NAME, UNSEG, TYPE)					\
void NAME (dst, src, index, flag, defaultv, src_segd, src_nelt,		\
	   src_nseg, dst_segd, dst_nelt, dst_nseg, scratch)		\
vec_p dst, src, index, flag, defaultv, src_segd, dst_segd, scratch;	\
int src_nelt, src_nseg, dst_nelt, dst_nseg;				\
{									\
  if (src_nseg == 1) {							\
    Func7 (UNSEG, dst, src, index, flag, defaultv, src_nelt, dst_nelt);	\
  } else {								\
    Func11 (NAME, dst, src, index, flag, defaultv, src_segd, src_nelt,	\
	    src_nseg, dst_segd, dst_nelt, scratch);			\
  }									\
}									\
int Glue (NAME,_scratch) (s_n, s_m, d_n, d_m) int s_n, s_m, d_n, d_m;	\
{ return (s_m == 1) ? 0 : siz_foz(s_n); }				\
Make_Inplace (NAME, INPLACE_NONE)

Make_Dfp_Seg (dfp_pez, dfp_puz, int)
Make_Dfp_Seg (dfp_peb, dfp_pub, cvl_bool)
Make_Dfp_Seg (dfp_ped, dfp_pud, double)
