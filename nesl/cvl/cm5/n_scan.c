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


/* ---------------------- Unsegmented Reduce ------------------------- */

#define Make_Reduce(TYPE, NAME, IDENT, FUNCT, COMBINER)			\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE result = IDENT;							\
  TYPE *src = (TYPE *) buf[1];						\
  int len = buf[2];							\
  int i;								\
									\
  for (i = Num_Here (len) - 1; i >= 0; i--) {				\
    result = FUNCT (result, src[i]);					\
  }									\
  Glue (CMMD_reduce_to_host_,TYPE) (result, COMBINER);			\
}									\

Make_Reduce (int, add_ruz_, 0, plus, CMMD_combiner_add)
Make_Reduce (int, min_ruz_, MAXINT, min, CMMD_combiner_min)
Make_Reduce (int, max_ruz_, MININT, max, CMMD_combiner_max)
Make_Reduce (int, and_ruz_, ~0, band, CMMD_combiner_and)
Make_Reduce (int, ior_ruz_, 0, bor, CMMD_combiner_ior)
Make_Reduce (int, xor_ruz_, 0, xor, CMMD_combiner_xor)
Make_Reduce (double, add_rud_, 0.0, plus, CMMD_combiner_dadd)
Make_Reduce (double, min_rud_, MAXDBL, min, CMMD_combiner_dmin)
Make_Reduce (double, max_rud_, MINDBL, max, CMMD_combiner_dmax)
Make_Reduce (uint, and_rub_,  TRUE, and, CMMD_combiner_and)
Make_Reduce (uint, ior_rub_, FALSE, or, CMMD_combiner_ior)
Make_Reduce (uint, xor_rub_, FALSE, xor, CMMD_combiner_xor)


/* We have to write our own multiply scan/reduce functions.  Use a
   standard binary tree, with the root of the tree being node 0.
   XXX rewrite using active messages for more speed */

#define Make_Mult_Reduce(TYPE, NAME, IDENT, FUNCT, COMIDENT, COMBINER)	\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE recvd, result = IDENT;						\
  TYPE *src = (TYPE *) buf[1];						\
  int len = buf[2];							\
  int gap;								\
  int i;								\
  int droppedout = FALSE;						\
  									\
  for (i = Num_Here (len) - 1; i >= 0; i--) {				\
    result = FUNCT (result, src[i]);					\
  }									\
  									\
  /* The code from here on replaces CMMD_reduce_to_host with a message-	\
     passing implementation -- note that we end with a *real*		\
     CMMD_reduce_to_host, since that's faster than sending an active	\
     message containing the final result to the host */			\
  									\
  for (gap = 1; (gap < nprocs) && (!droppedout); gap *= 2) {		\
    if (thisproc % (gap * 2)) {						\
      CMMD_send_block (thisproc - gap, Glue (NAME,tag), &result,	\
		       sizeof (TYPE));					\
      droppedout = TRUE;						\
    } else {								\
      CMMD_receive_block (thisproc + gap, Glue (NAME,tag), &recvd,	\
			  sizeof (TYPE));				\
      result = FUNCT (result, recvd);					\
    }									\
  }									\
  Glue (CMMD_reduce_to_host_,TYPE) ((thisproc == 0) ? result : COMIDENT,\
				    COMBINER);				\
}									\

Make_Mult_Reduce (int, mul_ruz_, 1, times, 0, CMMD_combiner_add)
Make_Mult_Reduce (double, mul_rud_, 1.0, times, 0.0, CMMD_combiner_dadd)


/* ----------------------- Segmented Reduce -------------------------- */

/* Combines the data with the destination using reduction function.
 */
#define Make_Send_Combine(NAME, TYPE, CMAMTYPE, FUNCT)			\
static void Glue (NAME,_combine) (posn, data)				\
TYPE *posn;								\
CMAMTYPE data;								\
{									\
  *posn = FUNCT (*posn, (TYPE) data);					\
  num_rcvd++;								\
}									\

Make_Send_Combine (add_rez,int, int, plus)
Make_Send_Combine (mul_rez,int, int, times)
Make_Send_Combine (min_rez,int, int, min)
Make_Send_Combine (max_rez,int, int, max)
Make_Send_Combine (and_rez,int, int, band)
Make_Send_Combine (ior_rez,int, int, bor)
Make_Send_Combine (xor_rez,int, int, xor)
Make_Send_Combine (add_red,double, double, plus)
Make_Send_Combine (mul_red,double, double, times)
Make_Send_Combine (min_red,double, double, min)
Make_Send_Combine (max_red,double, double, max)
Make_Send_Combine (and_reb,cvl_bool, cvl_bool, and)
Make_Send_Combine (ior_reb,cvl_bool, cvl_bool, or)
Make_Send_Combine (xor_reb,cvl_bool, cvl_bool, xor)


#define Combine(TYPE, FUNCT, NAME, PROC, DST, SUM)			\
    {									\
      TYPE *addr = DST;							\
      if (proc == thisproc) {						\
	*addr = FUNCT (*addr, SUM);					\
      } else {								\
	CMAML_rpc (PROC, Glue (NAME,combine), addr, SUM);		\
	num_sent++;							\
      }									\
    }


#define Make_Seg_Reduce(NAME, TYPE, IDENT, FUNCT)			\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE sum = IDENT;							\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int nelt = buf[4];							\
  int nseg = buf[5];							\
  int *segcount = (int *) buf[3] + Space_For (nseg);			\
  int firstseg = (thisproc == 0) ? -1 : *(segcount + Space_For (nelt));	\
  int mylen, i;								\
									\
  /* Clear the destination vector.					\
   */									\
  for (i = Num_Here (nseg) - 1; i >= 0; i--) {				\
    dst[i] = IDENT;							\
  }									\
									\
  /* Do a serial segmented exclusive reduce on each processor.		\
   * Whenever we enter a new segment, send the (partial) reduction of	\
   * the old segment to the processor responsible for that segment.	\
   */									\
  Prep (nseg);								\
  mylen = Num_Here (nelt);						\
  for (i = 0; i < mylen; i++) {						\
    if (segcount[i]) {		/* new segment */			\
      if (sum != IDENT) {	/* only send if needed */		\
	Map_To_Proc_And_Posn (firstseg);				\
	Combine (TYPE, FUNCT, NAME, proc, &dst[posn], sum);		\
      }									\
      firstseg += segcount[i];						\
      sum = src[i];							\
    } else {			/* else just keep accumulating	*/	\
      sum = FUNCT (sum, src[i]);					\
    }									\
    Poll ();								\
  }									\
									\
  /* Send out the final reduction from this processor, if needed.	\
   */									\
  if (sum != IDENT) {							\
    Map_To_Proc_And_Posn (firstseg);					\
    Combine (TYPE, FUNCT, NAME, proc, &dst[posn], sum);			\
  }									\
  Wait ();								\
}									\

Make_Seg_Reduce (add_rez_, int, 0, plus)
Make_Seg_Reduce (mul_rez_, int, 1, times)
Make_Seg_Reduce (min_rez_, int, MAXINT, min)
Make_Seg_Reduce (max_rez_, int, MININT, max)
Make_Seg_Reduce (and_rez_, int, ~0, band)
Make_Seg_Reduce (ior_rez_, int, 0, bor)
Make_Seg_Reduce (xor_rez_, int, 0, xor)
Make_Seg_Reduce (add_red_, double, 0.0, plus)
Make_Seg_Reduce (mul_red_, double, 1.0, times)
Make_Seg_Reduce (min_red_, double, MAXDBL, min)
Make_Seg_Reduce (max_red_, double, MINDBL, max)
Make_Seg_Reduce (and_reb_, cvl_bool, TRUE, and)
Make_Seg_Reduce (ior_reb_, cvl_bool, FALSE, or)
Make_Seg_Reduce (xor_reb_, cvl_bool, FALSE, xor)


/* ----------------------- Unsegmented Scan -------------------------- */

#define Make_Scan(TYPE, NAME, IDENT, FUNCT, COMBINER)			\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE tmp, sum = IDENT;						\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int len = buf[3];							\
  int mylen = Num_Here (len);						\
  int i;								\
									\
  /* Do a serial exclusive scan on each processor,			\
   */									\
  for (i = 0; i < mylen; i++) {						\
    tmp = sum;								\
    sum = FUNCT (sum, src[i]);						\
    dst[i] = tmp;							\
  }									\
									\
  /* Scan across the processors.					\
   */									\
  sum = Glue (CMMD_scan_,TYPE) (sum, COMBINER, CMMD_upward, CMMD_none,	\
				0, CMMD_exclusive);			\
									\
  /* And combine the two.						\
   */									\
  for (i = mylen - 1; i >= 0; i--) {					\
    dst[i] = FUNCT (dst[i], sum);					\
  }									\
}									\

Make_Scan (int, add_suz_, 0, plus, CMMD_combiner_add)
Make_Scan (int, min_suz_, MAXINT, min, CMMD_combiner_min)
Make_Scan (int, max_suz_, MININT, max, CMMD_combiner_max)
Make_Scan (int, and_suz_, ~0, band, CMMD_combiner_and)
Make_Scan (int, ior_suz_, 0, bor, CMMD_combiner_ior)
Make_Scan (int, xor_suz_, 0, xor, CMMD_combiner_xor)
Make_Scan (double, add_sud_, 0.0, plus, CMMD_combiner_dadd)
Make_Scan (double, min_sud_, MAXDBL, min, CMMD_combiner_dmin)
Make_Scan (double, max_sud_, MINDBL, max, CMMD_combiner_dmax)
Make_Scan (uint, and_sub_, TRUE, and, CMMD_combiner_and)
Make_Scan (uint, ior_sub_, FALSE, or, CMMD_combiner_ior)
Make_Scan (uint, xor_sub_, FALSE, xor, CMMD_combiner_xor)

/* Multiply scans are implemented using a binary-tree algorithm,
 * with a butterfly mapping to the actual processors, i.e.
 *
 * 0______
 * |      \
 * 0__     4__
 * |  \    |  \
 * 0   2   4   6
 * |\  |\  |\  |\
 * 0 1 2 3 4 5 6 7
 *
 * Standard node-level algorithms for an unsegmented scan are
 *
 * Up-sweep		Down-sweep
 * --------		----------
 * u = l OP r		l = u
 * m = l		r = u OP m	[NOT "m OP u" as in some papers]
 *
 * For a segmented scan, each value is a (flag, value) pair, and OP
 * is replaced by FLAGOP:
 *
 * FLAGOP ((fa, va), (fb, vb) = if fb then (fb, vb)
 *  				else (fa, va OP vb)
 *
 * or equivalently		f <- fa or fb
 *				v <- if fb then vb else va OP vb
 */


#define Make_Mult_Scan(TYPE, NAME, IDENT)				\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE tmp, sum = IDENT;						\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int len = buf[3];							\
  int mylen = Num_Here (len);						\
  int i;								\
  									\
  /* Do a serial exclusive scan on each processor,			\
  */									\
  for (i = 0; i < mylen; i++) {						\
    tmp = sum;								\
    sum = times (sum, src[i]);						\
    dst[i] = tmp;							\
  }									\
  									\
  /* This bit of code implements an unsegmented exclusive multiply-scan	\
   * on sum across the processors					\
   */									\
  {									\
    TYPE recvd;								\
    TYPE sumsaved[16];		/* can handle 2**16 procs */		\
    int i, gap;								\
    int droppedout = FALSE;						\
    									\
    for (i =0, gap =1; (gap < nprocs) && (!droppedout); i++, gap *= 2) {\
      if (thisproc % (gap * 2)) {					\
        CMMD_send_block (thisproc - gap, Glue (NAME,tag), &sum,		\
			 sizeof (TYPE));				\
	droppedout = TRUE;						\
      } else {								\
        sumsaved [i] = sum;						\
        CMMD_receive_block (thisproc + gap, Glue (NAME,tag), &recvd,	\
			    sizeof (TYPE));				\
	sum = times (sum, recvd);					\
      }									\
    }									\
    									\
    sum = IDENT;							\
    for (i--, gap /= 2; gap >= 1; i--, gap /= 2) {			\
      /* Are we sending? */						\
      if (!droppedout) {						\
	recvd = times (sum, sumsaved[i]);				\
        CMMD_send_block (thisproc + gap, Glue (NAME,tag)+1, &recvd,	\
			 sizeof (TYPE));				\
      } else {								\
        CMMD_receive_block (thisproc - gap, Glue (NAME,tag)+1, &sum,	\
			    sizeof (TYPE));				\
        droppedout = FALSE;						\
      }									\
    }									\
  }									\
  									\
  /* And combine the two.						\
    */									\
  for (i = mylen - 1; i >= 0; i--) {					\
    dst[i] = times (dst[i], sum);					\
  }									\
}

Make_Mult_Scan (int, mul_suz_, 1)
Make_Mult_Scan (double, mul_sud_, 1.0)


/* ------------------------ Segmented Scan --------------------------- */

#define Make_Seg_Scan(TYPE, NAME, IDENT, FUNCT, COMBINER)               \
void NAME (thisproc, logprocs, nprocs, buf)                             \
int thisproc, logprocs, nprocs, *buf;                                   \
{                                                                       \
  TYPE tmp, sum = IDENT;                                                \
  TYPE *dst = (TYPE *) buf[1];                                          \
  TYPE *src = (TYPE *) buf[2];                                          \
  int nelt = buf[4];                                                    \
  int nseg = buf[5];                                                    \
  int *segcount = (int *) buf[3] + Space_For (nseg);                    \
  int mylen = Num_Here (nelt);                                          \
  int i, start = FALSE;                                                 \
                                                                        \
  /* Do a serial segmented exclusive scan on each processor.            \
   * Set "start" if any segment bits are set.                           \
   */                                                                   \
  for (i = 0; i < mylen; i++) {                                         \
    if (segcount[i]) {                                                  \
      start = TRUE;                                                     \
      sum = IDENT;                                                      \
    }                                                                   \
    tmp = sum;                                                          \
    sum = FUNCT (sum, src[i]);                                          \
    dst[i] = tmp;                                                       \
  }                                                                     \
                                                                        \
  /* Do a segmented CMMD scan across "sum" on each processor,           \
   * using "start" as the segment bit.                                  \
   */                                                                   \
  sum = Glue (CMMD_scan_,TYPE) (sum, COMBINER, CMMD_upward,             \
                                CMMD_start_bit, start, CMMD_exclusive); \
                                                                        \
  /* Combine this value with all the elements in the first segment      \
   * on each processor.                                                 \
   */                                                                   \
  for (i = 0; i < mylen; i++) {                                         \
    if (segcount[i]) break;                                             \
    dst[i] = FUNCT (dst[i], sum);                                       \
  }                                                                     \
}                                                                       \

Make_Seg_Scan (int, add_sez_, 0, plus, CMMD_combiner_add)
Make_Seg_Scan (int, min_sez_, MAXINT, min, CMMD_combiner_min)
Make_Seg_Scan (int, max_sez_, MININT, max, CMMD_combiner_max)
Make_Seg_Scan (int, and_sez_, ~0, band, CMMD_combiner_and)
Make_Seg_Scan (int, ior_sez_, 0, bor, CMMD_combiner_ior)
Make_Seg_Scan (int, xor_sez_, 0, xor, CMMD_combiner_xor)
Make_Seg_Scan (double, add_sed_, 0.0, plus, CMMD_combiner_dadd)
Make_Seg_Scan (double, min_sed_, MAXDBL, min, CMMD_combiner_dmin)
Make_Seg_Scan (double, max_sed_, MINDBL, max, CMMD_combiner_dmax)
Make_Seg_Scan (uint, and_seb_, TRUE, and, CMMD_combiner_and)
Make_Seg_Scan (uint, ior_seb_, FALSE, or, CMMD_combiner_ior)
Make_Seg_Scan (uint, xor_seb_, FALSE, xor, CMMD_combiner_xor)


/* Types and subsidiary functions used by the segmented multiply scans
 */

typedef struct int_flag_pair_t {
  int flag;
  int value;
} int_flag_pair;

typedef struct double_flag_pair_t {
  int flag;
  double value;
} double_flag_pair;

static void int_flag_mult (pa, pb, pout)
int_flag_pair pa, pb, *pout;
{
  /* pout can be either pa or pb without causing problems */
  pout->value = (pb.flag) ? pb.value : pa.value * pb.value;
  pout->flag = pa.flag | pb.flag;
}

static void double_flag_mult (pa, pb, pout)
double_flag_pair pa, pb, *pout;
{
  pout->flag = pa.flag | pb.flag;
  pout->value = (pb.flag) ? pb.value : pa.value * pb.value;
}


#define Make_Seg_Mult_Scan(TYPE, NAME, IDENT)				\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE tmp, sum = IDENT;						\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  int nelt = buf[4];							\
  int nseg = buf[5];							\
  int *segcount = (int *) buf[3] + Space_For (nseg);			\
  int mylen = Num_Here (nelt);						\
  int i, start = FALSE;							\
									\
  /* Do a serial segmented exclusive scan on each processor.		\
   * Set "start" if any segment bits are set.				\
   */									\
  for (i = 0; i < mylen; i++) {						\
    if (segcount[i]) {							\
      start = TRUE;							\
      sum = IDENT;							\
    }									\
    tmp = sum;								\
    sum = times (sum, src[i]);						\
    dst[i] = tmp;							\
  }									\
									\
  /* This bit of code implements a segmented exclusive multiply-scan	\
   * on sum across the processors, using start as the segment bit	\
   */									\
  {									\
    Glue (TYPE,_flag_pair) recvd;					\
    Glue (TYPE,_flag_pair) current;					\
    Glue (TYPE,_flag_pair) saved[16];	/* can handle 2**16 procs */	\
    int i, gap;								\
    int droppedout = FALSE;						\
									\
    current.value = sum;						\
    current.flag = start;						\
    									\
    for (i =0, gap =1; (gap < nprocs) && (!droppedout); i++, gap *= 2) {\
      if (thisproc % (gap * 2)) {					\
        CMMD_send_block (thisproc - gap, Glue (NAME,tag), &current,	\
			 sizeof (Glue (TYPE,_flag_pair)));		\
	droppedout = TRUE;						\
      } else {								\
        saved [i] = current;						\
        CMMD_receive_block (thisproc + gap, Glue (NAME,tag), &recvd,	\
			    sizeof (Glue (TYPE,_flag_pair)));		\
									\
	Glue (TYPE,_flag_mult) (current, recvd, &current);		\
      }									\
    }									\
    									\
    current.value = IDENT;						\
    for (i--, gap /= 2; gap >= 1; i--, gap /= 2) {			\
      /* Are we sending? */						\
      if (!droppedout) {						\
	Glue (TYPE,_flag_mult) (current, saved [i], &recvd);		\
        CMMD_send_block (thisproc + gap, Glue (NAME,tag)+1, &recvd,	\
			 sizeof (Glue (TYPE,_flag_pair)));		\
      } else {								\
        CMMD_receive_block (thisproc - gap, Glue (NAME,tag)+1, &current,\
			    sizeof (Glue (TYPE,_flag_pair)));		\
        droppedout = FALSE;						\
      }									\
    }									\
    sum = current.value;						\
  }									\
									\
  /* Combine this value with all the elements in the first segment	\
   * on each processor.							\
   */									\
  for (i = 0; i < mylen; i++) {						\
    if (segcount[i]) break;						\
    dst[i] = times (dst[i], sum);					\
  }									\
}									\

Make_Seg_Mult_Scan (int, mul_sez_, 1)
Make_Seg_Mult_Scan (double, mul_sed_, 1.0)
