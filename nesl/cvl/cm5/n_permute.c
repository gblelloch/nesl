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

#include <cm/cmmd.h>
#include <cvl.h>
#include "cm5cvl.h"
#include "node.h"


/* ------------------- Unsegmented Simple Permute -------------------- */

#define Make_Smp(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  int nelt = buf[4];							\
  int i;								\
									\
  Prep (nelt);								\
  for (i = Num_Here (nelt) - 1; i >= 0; i--) {				\
    Map_To_Proc_And_Posn (index[i]);					\
    Send (TYPE, proc, &dst[posn], src[i]);				\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Smp (smp_puz_, int)
Make_Smp (smp_pub_, cvl_bool)
Make_Smp (smp_pud_, double)


/* -------------------- Segmented Simple Permute --------------------- */

#define Make_Smp_Seg(NAME, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  int nelt = buf[5];							\
  int nseg = buf[6];							\
  int *segcount = (int *) buf[4] + Space_For (nseg);			\
  int start = *(segcount + Space_For (nelt) + 1);			\
  int mylen = Num_Here (nelt);						\
  int first = First_Elt_Here (nelt);					\
  int i;								\
									\
  Prep (nelt);								\
  for (i = 0; i < mylen; i++) {						\
    if (segcount[i]) {							\
      start = first + i;						\
    }									\
    Map_To_Proc_And_Posn (start + index[i]);				\
    Send (TYPE, proc, &dst[posn], src[i]);				\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Smp_Seg (smp_pez_, int)
Make_Smp_Seg (smp_peb_, cvl_bool)
Make_Smp_Seg (smp_ped_, double)


/* ---------------- Unsegmented Simple Flag Permute ------------------ */

#define Make_Fpm(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  cvl_bool *flag = (cvl_bool *) buf[4];					\
  int s_nelt = buf[5];							\
  int d_nelt = buf[6];							\
  int i;								\
									\
  Prep (d_nelt);							\
  for (i = Num_Here (s_nelt) - 1; i >= 0; i--) {			\
    if (flag[i]) {							\
      Map_To_Proc_And_Posn (index[i]);					\
      Send (TYPE, proc, &dst[posn], src[i]);				\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Fpm (fpm_puz_, int)
Make_Fpm (fpm_pub_, cvl_bool)
Make_Fpm (fpm_pud_, double)


/* ----------------- Segmented Simple Flag Permute ------------------- */

/* Records the maximum value received so far.
 */
static void int_send_max (posn, value)
int *posn;
int value;
{
  *posn = value;
  if (value > max_rcvd)
    max_rcvd = value;
  num_rcvd++;
}


#define Max_Send(PROC, DST, SRC)					\
    {									\
      int *addr = DST;							\
      int data = Actual_Value (SRC);					\
      if (proc == thisproc) {						\
	*addr = data;							\
	if (data > max_rcvd) max_rcvd = data;				\
      } else {								\
	CMAML_rpc (PROC, int_send_max, addr, data);			\
	num_sent++;							\
      }									\
    }


#define Make_Fpm_Seg(NAME, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  cvl_bool *flag = (cvl_bool *) buf[4];					\
  unsigned *s_segstart = (unsigned *) buf[5];				\
  int s_nelt = buf[6];							\
  int nseg = buf[7];							\
  unsigned *d_segstart = (unsigned *) buf[8];				\
  int d_nelt = buf[9];							\
  int *scratch = (int *) buf[10];					\
  int *s_segcount = (int *) s_segstart + Space_For (nseg);		\
  int mylen, i, start;							\
									\
  /* Send start-of-segment indices of dst segment descriptor to		\
   * start-of-segment positions of scratch vector, which is the same	\
   * size as the src vector.						\
   */									\
  Prep (s_nelt);							\
  max_rcvd = 0;								\
  for (i = Num_Here (nseg) - 1; i >= 0; i--) {				\
    if (! Empty_Seg (d_segstart[i])) {					\
      Map_To_Proc_And_Posn (Actual_Value (s_segstart[i]));		\
      Max_Send (proc, &scratch[posn], d_segstart[i]);			\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
									\
  /* We've used int_send_max so that on each processor we know the	\
   * biggest integer sent; scan across this value to give the start	\
   * address in the dst vector of the segment containing the first	\
   * element on this processor from the src vector.			\
   */									\
  start = CMMD_scan_int (max_rcvd, CMMD_combiner_max, CMMD_upward,	\
			 CMMD_none, 0, CMMD_exclusive);			\
  mylen = Num_Here (s_nelt);						\
  Prep (d_nelt);							\
  for (i = 0; i < mylen; i++) {						\
    if (s_segcount[i]) {						\
      start = scratch[i];						\
    }									\
    if (flag[i]) {							\
      Map_To_Proc_And_Posn (start + index[i]);				\
      Send (TYPE, proc, &dst[posn], src[i]);				\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Fpm_Seg (fpm_pez_, int)
Make_Fpm_Seg (fpm_peb_, cvl_bool)
Make_Fpm_Seg (fpm_ped_, double)


/* ------------------ Unsegmented Backwards Permute ------------------ */

#define Make_Bck(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  int s_nelt = buf[4];							\
  int d_nelt = buf[5];							\
  int i;								\
									\
  if (s_nelt == 0) return;						\
									\
  Prep (s_nelt);							\
  for (i = Num_Here (d_nelt) - 1; i >= 0; i--) {			\
    Map_To_Proc_And_Posn (index[i]);					\
    Get (TYPE, proc, &dst[i], &src[posn]);				\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Bck (bck_puz_, int)
Make_Bck (bck_pub_, cvl_bool)
Make_Bck (bck_pud_, double)


/* ------------------ Segmented Backwards Permute -------------------- */

#define Make_Bck_Seg(NAME, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  unsigned *s_segstart = (unsigned *) buf[4];				\
  int s_nelt = buf[5];							\
  int nseg = buf[6];							\
  unsigned *d_segstart = (unsigned *) buf[7];				\
  int d_nelt = buf[8];							\
  int *scratch = (int *) buf[9];					\
  int *d_segcount = (int *) d_segstart + Space_For (nseg);		\
  int mylen, i, start;							\
									\
  /* Send the start-of-segment indices of the src segment descriptor	\
   * to the start-of-segment positions of the scratch vector, which	\
   * is the same size as the dst vector.				\
   */									\
  Prep (d_nelt);							\
  max_rcvd = 0;								\
  for (i = Num_Here (nseg) - 1; i >= 0; i--) {				\
    if (! Empty_Seg (d_segstart[i])) {					\
      Map_To_Proc_And_Posn (Actual_Value (d_segstart[i]));		\
      Max_Send (proc, &scratch[posn], s_segstart[i]);			\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
									\
  /* We've used int_send_max so that on each processor we know the	\
   * biggest integer sent; scan across this value to give the start	\
   * address in the src vector of the segment containing the first	\
   * element on this processor from the dst vector.			\
   */									\
  start = CMMD_scan_int (max_rcvd, CMMD_combiner_max, CMMD_upward,	\
			 CMMD_none, 0, CMMD_exclusive);			\
  mylen = Num_Here (d_nelt);						\
  Prep (s_nelt);							\
  for (i = 0; i < mylen; i++) {						\
    if (d_segcount[i]) {						\
      start = scratch[i];						\
    }									\
    Map_To_Proc_And_Posn (start + index[i]);				\
    Get (TYPE, proc, &dst[i], &src[posn]);				\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Bck_Seg (bck_pez_, int)
Make_Bck_Seg (bck_peb_, cvl_bool)
Make_Bck_Seg (bck_ped_, double)


/* --------------- Unsegmented Backwards Flag Permute ---------------- */

#define Make_Bfp(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  cvl_bool *flag = (cvl_bool *) buf[4];					\
  int s_nelt = buf[5];							\
  int d_nelt = buf[6];							\
  int i;								\
									\
  Prep (s_nelt);							\
  for (i = Num_Here (d_nelt) - 1; i >= 0; i--) {			\
    if (flag[i]) {							\
      Map_To_Proc_And_Posn (index[i]);					\
      Get (TYPE, proc, &dst[i], &src[posn]);				\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Bfp (bfp_puz_, int)
Make_Bfp (bfp_pub_, cvl_bool)
Make_Bfp (bfp_pud_, double)


/* ---------------- Segmented Backwards Flag Permute ----------------- */

#define Make_Bfp_Seg(NAME, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  cvl_bool *flag = (cvl_bool *) buf[4];					\
  unsigned *s_segstart = (unsigned *) buf[5];				\
  int s_nelt = buf[6];							\
  int nseg = buf[7];							\
  unsigned *d_segstart = (unsigned *) buf[8];				\
  int d_nelt = buf[9];							\
  int *scratch = (int *) buf[10];					\
  int *d_segcount = (int *) d_segstart + Space_For (nseg);		\
  int mylen, i, start;							\
									\
  Prep (d_nelt);							\
  max_rcvd = 0;								\
  for (i = Num_Here (nseg) - 1; i >= 0; i--) {				\
    if (! Empty_Seg (s_segstart[i])) {					\
      Map_To_Proc_And_Posn (Actual_Value (d_segstart[i]));		\
      Max_Send (proc, &scratch[posn], s_segstart[i]);			\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
									\
  start = CMMD_scan_int (max_rcvd, CMMD_combiner_max, CMMD_upward,	\
			 CMMD_none, 0, CMMD_exclusive);			\
  mylen = Num_Here (d_nelt);						\
  Prep (s_nelt);							\
  for (i = 0; i < mylen; i++) {						\
    if (d_segcount[i]) {						\
      start = scratch[i];						\
    }									\
    if (flag[i]) {							\
      Map_To_Proc_And_Posn (start + index[i]);				\
      Get (TYPE, proc, &dst[i], &src[posn]);				\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Bfp_Seg (bfp_pez_, int)
Make_Bfp_Seg (bfp_peb_, cvl_bool)
Make_Bfp_Seg (bfp_ped_, double)


/* ------------------ Unsegmented Default Permute -------------------- */

#define Make_Dpe(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  TYPE *def = (TYPE *) buf[4];						\
  int s_nelt = buf[5];							\
  int d_nelt = buf[6];							\
  int i;								\
									\
  /* Copy the default vector to the destination vector. */		\
  for (i = Num_Here (d_nelt) - 1; i >= 0; i--) {			\
    dst[i] = def[i];							\
  }									\
									\
  if (d_nelt == 0) return;						\
									\
  Prep (d_nelt);							\
  for (i = Num_Here (s_nelt) - 1; i >= 0; i--) {			\
    Map_To_Proc_And_Posn (index[i]);					\
    Send (TYPE, proc, &dst[posn], src[i]);				\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Dpe (dpe_puz_, int)
Make_Dpe (dpe_pub_, cvl_bool)
Make_Dpe (dpe_pud_, double)


/* ------------------- Segmented Default Permute --------------------- */

#define Make_Dpe_Seg(NAME, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  TYPE *def = (TYPE *) buf[4];						\
  unsigned *s_segstart = (unsigned *) buf[5];				\
  int s_nelt = buf[6];							\
  int nseg = buf[7];							\
  unsigned *d_segstart = (unsigned *) buf[8];				\
  int d_nelt = buf[9];							\
  int *scratch = (int *) buf[10];					\
  int *s_segcount = (int *) s_segstart + Space_For (nseg);		\
  int mylen, i, start;							\
									\
  for (i = Num_Here (d_nelt) - 1; i >= 0; i--) {			\
    dst[i] = def[i];							\
  }									\
									\
  if (s_nelt == 0) return;						\
									\
  Prep (s_nelt);							\
  max_rcvd = 0;								\
  for (i = Num_Here (nseg) - 1; i >= 0; i--) {				\
    if (! Empty_Seg (d_segstart[i])) {					\
      Map_To_Proc_And_Posn (Actual_Value (s_segstart[i]));		\
      Max_Send (proc, &scratch[posn], d_segstart[i]);			\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
									\
  start = CMMD_scan_int (max_rcvd, CMMD_combiner_max, CMMD_upward,	\
			 CMMD_none, 0, CMMD_exclusive);			\
  mylen = Num_Here (s_nelt);						\
  Prep (d_nelt);							\
  for (i = 0; i < mylen; i++) {						\
    if (s_segcount[i]) {						\
      start = scratch[i];						\
    }									\
    Map_To_Proc_And_Posn (start + index[i]);				\
    Send (TYPE, proc, &dst[posn], src[i]);				\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Dpe_Seg (dpe_pez_, int)
Make_Dpe_Seg (dpe_peb_, cvl_bool)
Make_Dpe_Seg (dpe_ped_, double)


/* ---------------- Unsegmented Default Flag Permute ----------------- */

#define Make_Dfp(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  cvl_bool *flag = (cvl_bool *) buf[4];					\
  TYPE *def = (TYPE *) buf[5];						\
  int s_nelt = buf[6];							\
  int d_nelt = buf[7];							\
  int i;								\
									\
  for (i = Num_Here (d_nelt) - 1; i >= 0; i--) {			\
    dst[i] = def[i];							\
  }									\
									\
  if (d_nelt == 0) return;						\
									\
  Prep (d_nelt);							\
  for (i = Num_Here (s_nelt) - 1; i >= 0; i--) {			\
    if (flag[i]) {							\
      Map_To_Proc_And_Posn (index[i]);					\
      Send (TYPE, proc, &dst[posn], src[i]);				\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Dfp (dfp_puz_, int)
Make_Dfp (dfp_pub_, cvl_bool)
Make_Dfp (dfp_pud_, double)


/* ----------------- Segmented Default Flag Permute ------------------ */

#define Make_Dfp_Seg(NAME, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *index = (int *) buf[3];						\
  cvl_bool *flag = (cvl_bool *) buf[4];					\
  TYPE *def = (TYPE *) buf[5];						\
  unsigned *s_segstart = (unsigned *) buf[6];				\
  int s_nelt = buf[7];							\
  int nseg = buf[8];							\
  unsigned *d_segstart = (unsigned *) buf[9];				\
  int d_nelt = buf[10];							\
  int *scratch = (int *) buf[11];					\
  int *s_segcount = (int *) s_segstart + Space_For (nseg);		\
  int mylen, i, start;							\
									\
  for (i = Num_Here (d_nelt) - 1; i >= 0; i--) {			\
    dst[i] = def[i];							\
  }									\
									\
  if (s_nelt == 0) return;						\
									\
  mylen = Num_Here (nseg);						\
  Prep (s_nelt);							\
  max_rcvd = 0;								\
  for (i = 0; i < mylen; i++) {						\
    if (! Empty_Seg (d_segstart[i])) {					\
      Map_To_Proc_And_Posn (Actual_Value (s_segstart[i]));		\
      Max_Send (proc, &scratch[posn], d_segstart[i]);			\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
									\
  start = CMMD_scan_int (max_rcvd, CMMD_combiner_max, CMMD_upward,	\
			 CMMD_none, 0, CMMD_exclusive);			\
  mylen = Num_Here (s_nelt);						\
  Prep (d_nelt);							\
  for (i = 0; i < mylen; i++) {						\
    if (s_segcount[i]) {						\
      start = scratch[i];						\
    }									\
    if (flag[i]) {							\
      Map_To_Proc_And_Posn (start + index[i]);				\
      Send (TYPE, proc, &dst[posn], src[i]);				\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Dfp_Seg (dfp_pez_, int)
Make_Dfp_Seg (dfp_peb_, cvl_bool)
Make_Dfp_Seg (dfp_ped_, double)
