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


/* This file has been machine-generated */


#include <limits.h>
#include <float.h>
#include <mpi.h>
#include <cvl.h>
#include "mpicvl.h"
#include "messages.h"

/* ---------------------- Unsegmented Reduce ------------------------- */


int add_ruz (vec_p src_v, int nelts, vec_p scratch_v)
{
  int *src;
  int result, value;
  int n_here, i;

  LOG_START (add_ruz, nelts);
  n_here = _num_here (nelts);
  src = (int *) (Mem + src_v);
  value = 0;

  for (i = 0; i < n_here; i++) {
    value = add (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  LOG_STOP (add_ruz, nelts);
  return (int) result;
}

unsigned int add_ruz_inplace (void)
{
  return INPLACE_NONE;
}

int add_ruz_scratch (int nelts)
{
  return 0;
}

int mul_ruz (vec_p src_v, int nelts, vec_p scratch_v)
{
  int *src;
  int result, value;
  int n_here, i;

  LOG_START (mul_ruz, nelts);
  n_here = _num_here (nelts);
  src = (int *) (Mem + src_v);
  value = 1;

  for (i = 0; i < n_here; i++) {
    value = mul (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_INT, MPI_PROD, MPI_COMM_WORLD);
  LOG_STOP (mul_ruz, nelts);
  return (int) result;
}

unsigned int mul_ruz_inplace (void)
{
  return INPLACE_NONE;
}

int mul_ruz_scratch (int nelts)
{
  return 0;
}

int max_ruz (vec_p src_v, int nelts, vec_p scratch_v)
{
  int *src;
  int result, value;
  int n_here, i;

  LOG_START (max_ruz, nelts);
  n_here = _num_here (nelts);
  src = (int *) (Mem + src_v);
  value = INT_MIN;

  for (i = 0; i < n_here; i++) {
    value = max (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
  LOG_STOP (max_ruz, nelts);
  return (int) result;
}

unsigned int max_ruz_inplace (void)
{
  return INPLACE_NONE;
}

int max_ruz_scratch (int nelts)
{
  return 0;
}

int min_ruz (vec_p src_v, int nelts, vec_p scratch_v)
{
  int *src;
  int result, value;
  int n_here, i;

  LOG_START (min_ruz, nelts);
  n_here = _num_here (nelts);
  src = (int *) (Mem + src_v);
  value = INT_MAX;

  for (i = 0; i < n_here; i++) {
    value = min (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
  LOG_STOP (min_ruz, nelts);
  return (int) result;
}

unsigned int min_ruz_inplace (void)
{
  return INPLACE_NONE;
}

int min_ruz_scratch (int nelts)
{
  return 0;
}

int and_ruz (vec_p src_v, int nelts, vec_p scratch_v)
{
  int *src;
  int result, value;
  int n_here, i;

  LOG_START (and_ruz, nelts);
  n_here = _num_here (nelts);
  src = (int *) (Mem + src_v);
  value = ~0;

  for (i = 0; i < n_here; i++) {
    value = bnd (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_INT, MPI_BAND, MPI_COMM_WORLD);
  LOG_STOP (and_ruz, nelts);
  return (int) result;
}

unsigned int and_ruz_inplace (void)
{
  return INPLACE_NONE;
}

int and_ruz_scratch (int nelts)
{
  return 0;
}

int ior_ruz (vec_p src_v, int nelts, vec_p scratch_v)
{
  int *src;
  int result, value;
  int n_here, i;

  LOG_START (ior_ruz, nelts);
  n_here = _num_here (nelts);
  src = (int *) (Mem + src_v);
  value = 0;

  for (i = 0; i < n_here; i++) {
    value = bor (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_INT, MPI_BOR, MPI_COMM_WORLD);
  LOG_STOP (ior_ruz, nelts);
  return (int) result;
}

unsigned int ior_ruz_inplace (void)
{
  return INPLACE_NONE;
}

int ior_ruz_scratch (int nelts)
{
  return 0;
}

int xor_ruz (vec_p src_v, int nelts, vec_p scratch_v)
{
  int *src;
  int result, value;
  int n_here, i;

  LOG_START (xor_ruz, nelts);
  n_here = _num_here (nelts);
  src = (int *) (Mem + src_v);
  value = 0;

  for (i = 0; i < n_here; i++) {
    value = xor (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_INT, MPI_BXOR, MPI_COMM_WORLD);
  LOG_STOP (xor_ruz, nelts);
  return (int) result;
}

unsigned int xor_ruz_inplace (void)
{
  return INPLACE_NONE;
}

int xor_ruz_scratch (int nelts)
{
  return 0;
}

double add_rud (vec_p src_v, int nelts, vec_p scratch_v)
{
  double *src;
  double result, value;
  int n_here, i;

  LOG_START (add_rud, nelts);
  n_here = _num_here (nelts);
  src = (double *) (Mem + src_v);
  value = 0.0;

  for (i = 0; i < n_here; i++) {
    value = add (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
  LOG_STOP (add_rud, nelts);
  return (double) result;
}

unsigned int add_rud_inplace (void)
{
  return INPLACE_NONE;
}

int add_rud_scratch (int nelts)
{
  return 0;
}

double mul_rud (vec_p src_v, int nelts, vec_p scratch_v)
{
  double *src;
  double result, value;
  int n_here, i;

  LOG_START (mul_rud, nelts);
  n_here = _num_here (nelts);
  src = (double *) (Mem + src_v);
  value = 1.0;

  for (i = 0; i < n_here; i++) {
    value = mul (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_DOUBLE, MPI_PROD, MPI_COMM_WORLD);
  LOG_STOP (mul_rud, nelts);
  return (double) result;
}

unsigned int mul_rud_inplace (void)
{
  return INPLACE_NONE;
}

int mul_rud_scratch (int nelts)
{
  return 0;
}

double max_rud (vec_p src_v, int nelts, vec_p scratch_v)
{
  double *src;
  double result, value;
  int n_here, i;

  LOG_START (max_rud, nelts);
  n_here = _num_here (nelts);
  src = (double *) (Mem + src_v);
  value = -DBL_MAX;

  for (i = 0; i < n_here; i++) {
    value = max (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
  LOG_STOP (max_rud, nelts);
  return (double) result;
}

unsigned int max_rud_inplace (void)
{
  return INPLACE_NONE;
}

int max_rud_scratch (int nelts)
{
  return 0;
}

double min_rud (vec_p src_v, int nelts, vec_p scratch_v)
{
  double *src;
  double result, value;
  int n_here, i;

  LOG_START (min_rud, nelts);
  n_here = _num_here (nelts);
  src = (double *) (Mem + src_v);
  value = DBL_MAX;

  for (i = 0; i < n_here; i++) {
    value = min (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_DOUBLE, MPI_MIN, MPI_COMM_WORLD);
  LOG_STOP (min_rud, nelts);
  return (double) result;
}

unsigned int min_rud_inplace (void)
{
  return INPLACE_NONE;
}

int min_rud_scratch (int nelts)
{
  return 0;
}

cvl_bool and_rub (vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *src;
  cvl_bool result, value;
  int n_here, i;

  LOG_START (and_rub, nelts);
  n_here = _num_here (nelts);
  src = (cvl_bool *) (Mem + src_v);
  value = TRUE;

  for (i = 0; i < n_here; i++) {
    value = and (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_BOOL, MPI_BAND, MPI_COMM_WORLD);
  LOG_STOP (and_rub, nelts);
  return (cvl_bool) result;
}

unsigned int and_rub_inplace (void)
{
  return INPLACE_NONE;
}

int and_rub_scratch (int nelts)
{
  return 0;
}

cvl_bool ior_rub (vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *src;
  cvl_bool result, value;
  int n_here, i;

  LOG_START (ior_rub, nelts);
  n_here = _num_here (nelts);
  src = (cvl_bool *) (Mem + src_v);
  value = FALSE;

  for (i = 0; i < n_here; i++) {
    value = ior (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_BOOL, MPI_BOR, MPI_COMM_WORLD);
  LOG_STOP (ior_rub, nelts);
  return (cvl_bool) result;
}

unsigned int ior_rub_inplace (void)
{
  return INPLACE_NONE;
}

int ior_rub_scratch (int nelts)
{
  return 0;
}

cvl_bool xor_rub (vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *src;
  cvl_bool result, value;
  int n_here, i;

  LOG_START (xor_rub, nelts);
  n_here = _num_here (nelts);
  src = (cvl_bool *) (Mem + src_v);
  value = FALSE;

  for (i = 0; i < n_here; i++) {
    value = xor (value, src[i]);
  }
  MPI_Allreduce (&value, &result, 1, MPI_BOOL, MPI_BXOR, MPI_COMM_WORLD);
  LOG_STOP (xor_rub, nelts);
  return (cvl_bool) result;
}

unsigned int xor_rub_inplace (void)
{
  return INPLACE_NONE;
}

int xor_rub_scratch (int nelts)
{
  return 0;
}



/* ----------------------- Segmented Reduce -------------------------- */


void add_rez (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  int *dst, *src;
  int *segd, *segment;
  int accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (add_rez, nelts);
  nelts_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  if (nsegs == 1) {
    accum = 0;
    for (i = 0; i < nelts_here; i++) {
      accum = add (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = 0;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = 0;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = add (accum, src[x]);
	  } else {
	    if (accum != 0) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = add (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		intx *buf = (intx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, add_rez_tag, _unpack_add_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, add_rez_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != 0) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = add (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  intx *buf = (intx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, add_rez_tag, n_sent, n_rcvd,
		      sizeof (intx), _unpack_add_intx);
    }
  }
  LOG_STOP (add_rez, nelts);
}

unsigned int add_rez_inplace (void)
{
  return INPLACE_NONE;
}

int add_rez_scratch (int nelts, int nsegs)
{
  return 0;
}

void mul_rez (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  int *dst, *src;
  int *segd, *segment;
  int accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (mul_rez, nelts);
  nelts_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  if (nsegs == 1) {
    accum = 1;
    for (i = 0; i < nelts_here; i++) {
      accum = mul (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_INT, MPI_PROD, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = 1;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = 1;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = mul (accum, src[x]);
	  } else {
	    if (accum != 1) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = mul (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		intx *buf = (intx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, mul_rez_tag, _unpack_mul_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, mul_rez_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != 1) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = mul (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  intx *buf = (intx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, mul_rez_tag, n_sent, n_rcvd,
		      sizeof (intx), _unpack_mul_intx);
    }
  }
  LOG_STOP (mul_rez, nelts);
}

unsigned int mul_rez_inplace (void)
{
  return INPLACE_NONE;
}

int mul_rez_scratch (int nelts, int nsegs)
{
  return 0;
}

void max_rez (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  int *dst, *src;
  int *segd, *segment;
  int accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (max_rez, nelts);
  nelts_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  if (nsegs == 1) {
    accum = INT_MIN;
    for (i = 0; i < nelts_here; i++) {
      accum = max (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = INT_MIN;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = INT_MIN;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = max (accum, src[x]);
	  } else {
	    if (accum != INT_MIN) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = max (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		intx *buf = (intx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, max_rez_tag, _unpack_max_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, max_rez_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != INT_MIN) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = max (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  intx *buf = (intx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, max_rez_tag, n_sent, n_rcvd,
		      sizeof (intx), _unpack_max_intx);
    }
  }
  LOG_STOP (max_rez, nelts);
}

unsigned int max_rez_inplace (void)
{
  return INPLACE_NONE;
}

int max_rez_scratch (int nelts, int nsegs)
{
  return 0;
}

void min_rez (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  int *dst, *src;
  int *segd, *segment;
  int accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (min_rez, nelts);
  nelts_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  if (nsegs == 1) {
    accum = INT_MAX;
    for (i = 0; i < nelts_here; i++) {
      accum = min (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_INT, MPI_MIN, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = INT_MAX;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = INT_MAX;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = min (accum, src[x]);
	  } else {
	    if (accum != INT_MAX) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = min (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		intx *buf = (intx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, min_rez_tag, _unpack_min_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, min_rez_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != INT_MAX) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = min (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  intx *buf = (intx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, min_rez_tag, n_sent, n_rcvd,
		      sizeof (intx), _unpack_min_intx);
    }
  }
  LOG_STOP (min_rez, nelts);
}

unsigned int min_rez_inplace (void)
{
  return INPLACE_NONE;
}

int min_rez_scratch (int nelts, int nsegs)
{
  return 0;
}

void and_rez (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  int *dst, *src;
  int *segd, *segment;
  int accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (and_rez, nelts);
  nelts_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  if (nsegs == 1) {
    accum = ~0;
    for (i = 0; i < nelts_here; i++) {
      accum = bnd (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_INT, MPI_BAND, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = ~0;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = ~0;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = bnd (accum, src[x]);
	  } else {
	    if (accum != ~0) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = bnd (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		intx *buf = (intx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, and_rez_tag, _unpack_bnd_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, and_rez_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != ~0) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = bnd (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  intx *buf = (intx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, and_rez_tag, n_sent, n_rcvd,
		      sizeof (intx), _unpack_bnd_intx);
    }
  }
  LOG_STOP (and_rez, nelts);
}

unsigned int and_rez_inplace (void)
{
  return INPLACE_NONE;
}

int and_rez_scratch (int nelts, int nsegs)
{
  return 0;
}

void ior_rez (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  int *dst, *src;
  int *segd, *segment;
  int accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (ior_rez, nelts);
  nelts_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  if (nsegs == 1) {
    accum = 0;
    for (i = 0; i < nelts_here; i++) {
      accum = bor (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_INT, MPI_BOR, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = 0;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = 0;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = bor (accum, src[x]);
	  } else {
	    if (accum != 0) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = bor (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		intx *buf = (intx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, ior_rez_tag, _unpack_bor_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, ior_rez_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != 0) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = bor (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  intx *buf = (intx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, ior_rez_tag, n_sent, n_rcvd,
		      sizeof (intx), _unpack_bor_intx);
    }
  }
  LOG_STOP (ior_rez, nelts);
}

unsigned int ior_rez_inplace (void)
{
  return INPLACE_NONE;
}

int ior_rez_scratch (int nelts, int nsegs)
{
  return 0;
}

void xor_rez (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  int *dst, *src;
  int *segd, *segment;
  int accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (xor_rez, nelts);
  nelts_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  if (nsegs == 1) {
    accum = 0;
    for (i = 0; i < nelts_here; i++) {
      accum = xor (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_INT, MPI_BXOR, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = 0;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = 0;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = xor (accum, src[x]);
	  } else {
	    if (accum != 0) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = xor (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		intx *buf = (intx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, xor_rez_tag, _unpack_xor_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, xor_rez_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != 0) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = xor (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  intx *buf = (intx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, xor_rez_tag, n_sent, n_rcvd,
		      sizeof (intx), _unpack_xor_intx);
    }
  }
  LOG_STOP (xor_rez, nelts);
}

unsigned int xor_rez_inplace (void)
{
  return INPLACE_NONE;
}

int xor_rez_scratch (int nelts, int nsegs)
{
  return 0;
}

void add_red (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  double *dst, *src;
  int *segd, *segment;
  double accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (add_red, nelts);
  nelts_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  if (nsegs == 1) {
    accum = 0.0;
    for (i = 0; i < nelts_here; i++) {
      accum = add (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = 0.0;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = 0.0;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = add (accum, src[x]);
	  } else {
	    if (accum != 0.0) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = add (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		doublex *buf = (doublex *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, add_red_tag, _unpack_add_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, add_red_tag, (Send_ctr[x] * sizeof (doublex)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != 0.0) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = add (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  doublex *buf = (doublex *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, add_red_tag, n_sent, n_rcvd,
		      sizeof (doublex), _unpack_add_doublex);
    }
  }
  LOG_STOP (add_red, nelts);
}

unsigned int add_red_inplace (void)
{
  return INPLACE_NONE;
}

int add_red_scratch (int nelts, int nsegs)
{
  return 0;
}

void mul_red (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  double *dst, *src;
  int *segd, *segment;
  double accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (mul_red, nelts);
  nelts_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  if (nsegs == 1) {
    accum = 1.0;
    for (i = 0; i < nelts_here; i++) {
      accum = mul (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_DOUBLE, MPI_PROD, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = 1.0;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = 1.0;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = mul (accum, src[x]);
	  } else {
	    if (accum != 1.0) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = mul (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		doublex *buf = (doublex *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, mul_red_tag, _unpack_mul_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, mul_red_tag, (Send_ctr[x] * sizeof (doublex)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != 1.0) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = mul (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  doublex *buf = (doublex *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, mul_red_tag, n_sent, n_rcvd,
		      sizeof (doublex), _unpack_mul_doublex);
    }
  }
  LOG_STOP (mul_red, nelts);
}

unsigned int mul_red_inplace (void)
{
  return INPLACE_NONE;
}

int mul_red_scratch (int nelts, int nsegs)
{
  return 0;
}

void max_red (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  double *dst, *src;
  int *segd, *segment;
  double accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (max_red, nelts);
  nelts_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  if (nsegs == 1) {
    accum = -DBL_MAX;
    for (i = 0; i < nelts_here; i++) {
      accum = max (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = -DBL_MAX;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = -DBL_MAX;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = max (accum, src[x]);
	  } else {
	    if (accum != -DBL_MAX) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = max (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		doublex *buf = (doublex *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, max_red_tag, _unpack_max_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, max_red_tag, (Send_ctr[x] * sizeof (doublex)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != -DBL_MAX) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = max (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  doublex *buf = (doublex *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, max_red_tag, n_sent, n_rcvd,
		      sizeof (doublex), _unpack_max_doublex);
    }
  }
  LOG_STOP (max_red, nelts);
}

unsigned int max_red_inplace (void)
{
  return INPLACE_NONE;
}

int max_red_scratch (int nelts, int nsegs)
{
  return 0;
}

void min_red (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  double *dst, *src;
  int *segd, *segment;
  double accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (min_red, nelts);
  nelts_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  if (nsegs == 1) {
    accum = DBL_MAX;
    for (i = 0; i < nelts_here; i++) {
      accum = min (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = DBL_MAX;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = DBL_MAX;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = min (accum, src[x]);
	  } else {
	    if (accum != DBL_MAX) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = min (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		doublex *buf = (doublex *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, min_red_tag, _unpack_min_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, min_red_tag, (Send_ctr[x] * sizeof (doublex)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != DBL_MAX) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = min (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  doublex *buf = (doublex *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, min_red_tag, n_sent, n_rcvd,
		      sizeof (doublex), _unpack_min_doublex);
    }
  }
  LOG_STOP (min_red, nelts);
}

unsigned int min_red_inplace (void)
{
  return INPLACE_NONE;
}

int min_red_scratch (int nelts, int nsegs)
{
  return 0;
}

void and_reb (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  int *segd, *segment;
  cvl_bool accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (and_reb, nelts);
  nelts_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);

  if (nsegs == 1) {
    accum = TRUE;
    for (i = 0; i < nelts_here; i++) {
      accum = and (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_BOOL, MPI_BAND, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = TRUE;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = TRUE;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = and (accum, src[x]);
	  } else {
	    if (accum != TRUE) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = and (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, and_reb_tag, _unpack_and_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, and_reb_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != TRUE) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = and (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, and_reb_tag, n_sent, n_rcvd,
		      sizeof (cvl_boolx), _unpack_and_cvl_boolx);
    }
  }
  LOG_STOP (and_reb, nelts);
}

unsigned int and_reb_inplace (void)
{
  return INPLACE_NONE;
}

int and_reb_scratch (int nelts, int nsegs)
{
  return 0;
}

void ior_reb (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  int *segd, *segment;
  cvl_bool accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (ior_reb, nelts);
  nelts_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);

  if (nsegs == 1) {
    accum = FALSE;
    for (i = 0; i < nelts_here; i++) {
      accum = ior (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_BOOL, MPI_BOR, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = FALSE;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = FALSE;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = ior (accum, src[x]);
	  } else {
	    if (accum != FALSE) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = ior (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, ior_reb_tag, _unpack_ior_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, ior_reb_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != FALSE) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = ior (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, ior_reb_tag, n_sent, n_rcvd,
		      sizeof (cvl_boolx), _unpack_ior_cvl_boolx);
    }
  }
  LOG_STOP (ior_reb, nelts);
}

unsigned int ior_reb_inplace (void)
{
  return INPLACE_NONE;
}

int ior_reb_scratch (int nelts, int nsegs)
{
  return 0;
}

void xor_reb (vec_p dst_v, vec_p src_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  int *segd, *segment;
  cvl_bool accum;
  int nelts_here, nsegs_here, current_seg, i;

  LOG_START (xor_reb, nelts);
  nelts_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);

  if (nsegs == 1) {
    accum = FALSE;
    for (i = 0; i < nelts_here; i++) {
      accum = xor (accum, src[i]);
    }
    MPI_Reduce (&accum, &dst[0], 1, MPI_BOOL, MPI_BXOR, 0, MPI_COMM_WORLD);
  } else {
    /* Initialize destination vector (segment reductions) to identity */
    nsegs_here = _num_here (nsegs);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = FALSE;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = FALSE;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[x].  If we hit a segment
       * boundary, send accum to the processor/offset pair that should
       * hold the reduction of the current segment, and combine it with
       * whatever is there using OP.  If accum is the identity, do not
       * bother: this saves time, and handles the special case of the
       * first element on each processor. */
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nsegs);
      int x, y, loopbound;

      for (y = 0; y < nelts_here; y += RCV_EVERY) {
	loopbound = min (nelts_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] == current_seg) {
	    accum = xor (accum, src[x]);
	  } else {
	    if (accum != FALSE) {
	      int proc = current_seg / space;
	      int offset = current_seg - (proc * space);

	      if (proc == Self) {
		dst[offset] = xor (dst[offset], accum);
	      } else {
		int posn = Send_ctr[proc]++;
		cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

		buf[posn].value = accum;
		buf[posn].index = offset;
	      }
	    }
	    accum = src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, xor_reb_tag, _unpack_xor_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, xor_reb_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	    n_sent++;
	  }
	}
      }

      /* Send out the final result from each processor */
      if (accum != FALSE) {
	int proc = current_seg / space;
	int offset = current_seg - (proc * space);

	if (proc == Self) {
	  dst[offset] = xor (dst[offset], accum);
	} else {
	  int posn = Send_ctr[proc]++;
	  cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

	  buf[posn].value = accum;
	  buf[posn].index = offset;
	}
      }
      _finish_simple (dst, xor_reb_tag, n_sent, n_rcvd,
		      sizeof (cvl_boolx), _unpack_xor_cvl_boolx);
    }
  }
  LOG_STOP (xor_reb, nelts);
}

unsigned int xor_reb_inplace (void)
{
  return INPLACE_NONE;
}

int xor_reb_scratch (int nelts, int nsegs)
{
  return 0;
}
