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


/* -------------------- Unsegmented Distribute ----------------------- */

/* Store two ints at a time.
 */
void dis_vuz_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  oneword *dst = (oneword *) buf[1];
  int len = buf[3];
  oneword val;
  int i;

  val.ints.lo = val.ints.hi = buf[2];
  for (i = ((Num_Here (len) + 1) >> 1) - 1; i >= 0; i--) {
    dst[i] = val;
  }
}


/* Store two cvl_bools (ints) at a time.
 */
void dis_vub_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  oneword *dst = (oneword *) buf[1];
  int len = buf[3];
  oneword val;
  int i;

  val.ints.lo = val.ints.hi = (cvl_bool) buf[2];
  for (i = ((Num_Here (len) + 1) >> 1) - 1; i >= 0; i--) {
    dst[i] = val;
  }
}


/* Store one double at a time.
 */
void dis_vud_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  double *dst = (double *) buf[1];
  double value = *((double *) &buf[2]);
  int len = buf[4];
  int i;

  for (i = (Num_Here (len) - 1); i >= 0; i--) {
    dst[i] = value;
  }
}


/* --------------------- Segmented Distribute ------------------------ */

/* XXX worth unrolling integer and character distributes? */

#ifdef __STDC__
static volatile int dist_posn;
static volatile int dist_val_int;
static volatile double dist_val_double;
static volatile uint dist_val_uint;
#else
static int dist_posn;
static int dist_val_int;
static double dist_val_double;
static uint dist_val_uint;
#endif


#define Make_Send_Dist(TYPE)						\
static void Glue (TYPE,_send_dist) (posn, data)				\
TYPE *posn, data;							\
{									\
  *posn = data;								\
  if ((int) posn > dist_posn) {						\
    dist_posn = (int) posn;						\
    Glue (dist_val_,TYPE) = data;					\
  }									\
  num_rcvd++;								\
}									\

Make_Send_Dist (int)
Make_Send_Dist (double)
Make_Send_Dist (uint)


#define Make_Seg_Dist(TYPE, NAME, INIT, COMBINER)			\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  unsigned *segstart = (unsigned *) buf[3];				\
  int nelt = buf[4];							\
  int nseg = buf[5];							\
  TYPE tmp;								\
  int *segcount = (int *) segstart + Space_For (nseg);			\
  int mylen = Num_Here (nseg);						\
  int i;								\
									\
  /* Send out values to start-of-segment positions.			\
   */									\
  Prep (nelt);								\
  dist_posn = 0;							\
  Glue (dist_val_,TYPE) = INIT;						\
  for (i = 0; i < mylen; i++) {						\
    if (! Empty_Seg (segstart[i])) {					\
      Map_To_Proc_And_Posn (Actual_Value (segstart[i]));		\
      if (proc == thisproc) {						\
	dst[posn] = src[i];						\
	if ((int) &dst[posn] > dist_posn) {				\
	  dist_posn = (int) &dst[posn];					\
	  Glue (dist_val_,TYPE) = src[i];				\
	}								\
      } else {								\
	CMAML_rpc (proc, Glue (TYPE,_send_dist), &dst[posn], src[i]);	\
	num_sent++;							\
      }									\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
									\
  /* Copy-scan across the values from the last segment on each node,	\
   * resetting each time there's a new last segment.  Note that we use	\
   * CMMD_start_bit and CMMD_exclusive, which puts the result of segment\
   * n-1 into the first element of segment n.				\
   */									\
  if (dist_posn) dist_posn = 1;						\
  tmp = Glue (CMMD_scan_,TYPE) (Glue (dist_val_,TYPE), COMBINER,	\
				CMMD_upward, CMMD_start_bit,		\
				dist_posn, CMMD_exclusive);		\
									\
  /* Now do a serial segmented copy-scan on each node, starting out with\
   * the result of the scan.						\
   */									\
  mylen = Num_Here (nelt);						\
  for (i = 0; i < mylen; i++) {						\
    if (segcount[i]) {							\
      tmp = dst[i];							\
    } else {								\
      dst[i] = tmp;							\
    }									\
  }									\
}									\

Make_Seg_Dist (int, dis_vez_, MININT, CMMD_combiner_max)
Make_Seg_Dist (double, dis_ved_, MINDBL, CMMD_combiner_max)
Make_Seg_Dist (uint, dis_veb_, 0, CMMD_combiner_ior)


/* ---------------------- Unsegmented Replace ------------------------ */

#define Make_Rep(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  int len = buf[4];							\
  Simple_Prep (len);							\
  Simple_Map_To_Proc_And_Posn (buf[2]);					\
									\
  if (proc == thisproc) {						\
    dst[posn] = buf[3];							\
  }									\
}									\

Make_Rep (rep_vuz_, int)
Make_Rep (rep_vub_, cvl_bool)


void rep_vud_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  double *dst = (double *) buf[1];
  Simple_Prep (buf[3]);
  Simple_Map_To_Proc_And_Posn (buf[2]);

  if (proc == thisproc) {
    dst[posn] = *((double *) &buf[4]);
  }
}


/* ----------------------- Segmented Replace ------------------------- */

#define Make_Seg_Rep(NAME, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  int *indx = (int *) buf[2];						\
  TYPE *src = (TYPE *) buf[3];						\
  unsigned *segstart = (unsigned *) buf[4];				\
  int nelt = buf[5];							\
  int nseg = buf[6];							\
  int mylen = Num_Here (nseg);						\
  int i;								\
									\
  /* Take each segment start position in turn, add the offset from	\
   * the index vector, map the result to a processor, and send		\
   * the value to that position.					\
   */									\
  Prep (nelt);								\
  for (i = 0; i < mylen; i++) {						\
    if (! Empty_Seg (segstart[i])) {					\
      Map_To_Proc_And_Posn (Actual_Value (segstart[i]) + indx[i]);	\
      Send (TYPE, proc, &dst[posn], src[i]);				\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Seg_Rep (rep_vez_,int)
Make_Seg_Rep (rep_veb_,cvl_bool)
Make_Seg_Rep (rep_ved_,double)


/* ---------------------- Unsegmented Extract ------------------------ */

#define Make_Ext(TYPE, NAME, IDENT, COMBINER)				\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *src = (TYPE *) buf[1];						\
  Simple_Prep (buf[3]);							\
  Simple_Map_To_Proc_And_Posn (buf[2]);					\
  Glue (CMMD_reduce_to_host_,TYPE) ((proc == thisproc) ? src[posn] :	\
				    IDENT, COMBINER);			\
}									\

Make_Ext (int, ext_vuz_, 0, CMMD_combiner_add)
Make_Ext (uint, ext_vub_, 0, CMMD_combiner_uadd)
Make_Ext (double, ext_vud_, 0.0, CMMD_combiner_dadd)


/* ----------------------- Segmented Extract ------------------------- */

#define Make_Seg_Ext(NAME, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int *indx = (int *) buf[3];						\
  unsigned *segstart = (unsigned *) buf[4];				\
  int nelt = buf[5];							\
  int nseg = buf[6];							\
  int mylen = Num_Here (nseg);						\
  int i;								\
									\
  /* Take each segment start position in turn, add the offset from	\
   * the index vector, map the result to a processor, and request	\
   * the value at that position.					\
   */									\
  Prep (nelt);								\
  for (i = 0; i < mylen; i++) {						\
    if (! Empty_Seg (segstart[i])) {					\
      Map_To_Proc_And_Posn (Actual_Value (segstart[i]) + indx[i]);	\
      Get (TYPE, proc, &dst[i], &src[posn]);				\
    }									\
    Poll ();								\
  }									\
  Wait ();								\
}									\

Make_Seg_Ext (ext_vez_,int)
Make_Seg_Ext (ext_veb_,cvl_bool)
Make_Seg_Ext (ext_ved_,double)
