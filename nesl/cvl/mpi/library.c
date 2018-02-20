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


#include <mpi.h>
#include <cvl.h>
#include "mpicvl.h"
#include "messages.h"


/* ----------------------- Unsegmented Pack -------------------------- */

/* XXX cache state from pk1_luv to pk2_lu[zbd] */

/* Part 1.  Count the flags.
 */
int pk1_luv (vec_p flag_v, int nelts, vec_p scratch_v)
{
  cvl_bool *flags;
  int n_here, value, result, i;

  LOG_START (pk1_luv, nelts);
  n_here = _num_here (nelts);
  flags = (cvl_bool *) (Mem + flag_v);
  value = 0;

  for (i = 0; i < n_here; i++) {
    value += flags[i];
  }
  MPI_Allreduce (&value, &result, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  LOG_STOP (pk1_luv, nelts);
  return result;
}

unsigned int pk1_luv_inplace (void)
{
  return INPLACE_NONE;
}

int pk1_luv_scratch (int nelts)
{
  return 0;
}




void pk2_luz (vec_p dst_v, vec_p src_v, vec_p flags_v, int src_nelts,
	       int dst_nelts, vec_p scratch_v)
{
  int *dst, *src;
  cvl_bool *flag;
  int src_here, accum, base, i;

  LOG_START (pk2_luz, src_nelts);
  src_here = _num_here (src_nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);
  flag = (cvl_bool *) (Mem + flags_v);
  accum = 0;

  /* Do an exclusive scan across the number of flags to get the address
   * to start sending elements to. */
  for (i = 0; i < src_here; i++) {
    if (flag[i])
      accum++;
  }
  MPI_Scan (&accum, &base, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  base -= accum;		/* convert inclusive to exclusive */

  /* XXX sends n indices, when one would do. */
  /* Send out the elements. */
  {
    int n_sent = 0, n_rcvd = 0;
    int space = SpaceFor (dst_nelts);
    int x, y, loopbound;

    for (y = 0; y < src_here; y += RCV_EVERY) {
      loopbound = min (src_here, y + RCV_EVERY);
      for (x = y; x < loopbound; x++) {
	/* if (flag[x]) dst[base++] = src[x] */
	if (flag[x]) {
	  int proc = base / space;
	  int offset = base - (proc * space);

	  if (proc == Self) {
	    dst[offset] = src[x];
	  } else {
	    int posn = Send_ctr[proc]++;
	    intx *buf = (intx *) Usr_send[proc];

	    buf[posn].value = src[x];
	    buf[posn].index = offset;
	  }
	  base++;
	}
      }
      n_rcvd += _recv_simple (dst, pk2_luz_tag, _unpack_simple_intx);
      for (x = 0; x < NumProcs; x++) {
	if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	  _send_buffer (x, pk2_luz_tag, (Send_ctr[x] * sizeof (intx)));
	  n_sent++;
	}
      }
    }

    _finish_simple (dst, pk2_luz_tag, n_sent, n_rcvd, sizeof (intx),
		    _unpack_simple_intx);
  }
  LOG_STOP (pk2_luz, src_nelts);
}

unsigned int pk2_luz_inplace (void)
{
  return INPLACE_NONE;
}

int pk2_luz_scratch (int nelts)
{
  return 0;
}

void pk2_lud (vec_p dst_v, vec_p src_v, vec_p flags_v, int src_nelts,
	       int dst_nelts, vec_p scratch_v)
{
  double *dst, *src;
  cvl_bool *flag;
  int src_here, accum, base, i;

  LOG_START (pk2_lud, src_nelts);
  src_here = _num_here (src_nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);
  flag = (cvl_bool *) (Mem + flags_v);
  accum = 0;

  /* Do an exclusive scan across the number of flags to get the address
   * to start sending elements to. */
  for (i = 0; i < src_here; i++) {
    if (flag[i])
      accum++;
  }
  MPI_Scan (&accum, &base, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  base -= accum;		/* convert inclusive to exclusive */

  /* XXX sends n indices, when one would do. */
  /* Send out the elements. */
  {
    int n_sent = 0, n_rcvd = 0;
    int space = SpaceFor (dst_nelts);
    int x, y, loopbound;

    for (y = 0; y < src_here; y += RCV_EVERY) {
      loopbound = min (src_here, y + RCV_EVERY);
      for (x = y; x < loopbound; x++) {
	/* if (flag[x]) dst[base++] = src[x] */
	if (flag[x]) {
	  int proc = base / space;
	  int offset = base - (proc * space);

	  if (proc == Self) {
	    dst[offset] = src[x];
	  } else {
	    int posn = Send_ctr[proc]++;
	    doublex *buf = (doublex *) Usr_send[proc];

	    buf[posn].value = src[x];
	    buf[posn].index = offset;
	  }
	  base++;
	}
      }
      n_rcvd += _recv_simple (dst, pk2_lud_tag, _unpack_simple_doublex);
      for (x = 0; x < NumProcs; x++) {
	if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	  _send_buffer (x, pk2_lud_tag, (Send_ctr[x] * sizeof (doublex)));
	  n_sent++;
	}
      }
    }

    _finish_simple (dst, pk2_lud_tag, n_sent, n_rcvd, sizeof (doublex),
		    _unpack_simple_doublex);
  }
  LOG_STOP (pk2_lud, src_nelts);
}

unsigned int pk2_lud_inplace (void)
{
  return INPLACE_NONE;
}

int pk2_lud_scratch (int nelts)
{
  return 0;
}

void pk2_lub (vec_p dst_v, vec_p src_v, vec_p flags_v, int src_nelts,
	       int dst_nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  cvl_bool *flag;
  int src_here, accum, base, i;

  LOG_START (pk2_lub, src_nelts);
  src_here = _num_here (src_nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);
  flag = (cvl_bool *) (Mem + flags_v);
  accum = 0;

  /* Do an exclusive scan across the number of flags to get the address
   * to start sending elements to. */
  for (i = 0; i < src_here; i++) {
    if (flag[i])
      accum++;
  }
  MPI_Scan (&accum, &base, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  base -= accum;		/* convert inclusive to exclusive */

  /* XXX sends n indices, when one would do. */
  /* Send out the elements. */
  {
    int n_sent = 0, n_rcvd = 0;
    int space = SpaceFor (dst_nelts);
    int x, y, loopbound;

    for (y = 0; y < src_here; y += RCV_EVERY) {
      loopbound = min (src_here, y + RCV_EVERY);
      for (x = y; x < loopbound; x++) {
	/* if (flag[x]) dst[base++] = src[x] */
	if (flag[x]) {
	  int proc = base / space;
	  int offset = base - (proc * space);

	  if (proc == Self) {
	    dst[offset] = src[x];
	  } else {
	    int posn = Send_ctr[proc]++;
	    cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

	    buf[posn].value = src[x];
	    buf[posn].index = offset;
	  }
	  base++;
	}
      }
      n_rcvd += _recv_simple (dst, pk2_lub_tag, _unpack_simple_cvl_boolx);
      for (x = 0; x < NumProcs; x++) {
	if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	  _send_buffer (x, pk2_lub_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	  n_sent++;
	}
      }
    }

    _finish_simple (dst, pk2_lub_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
		    _unpack_simple_cvl_boolx);
  }
  LOG_STOP (pk2_lub, src_nelts);
}

unsigned int pk2_lub_inplace (void)
{
  return INPLACE_NONE;
}

int pk2_lub_scratch (int nelts)
{
  return 0;
}



/* ------------------------ Segmented Pack --------------------------- */

/* Part 1: pk1_lev takes a segmented vector of flags, and returns the
 * lengths vector (segment descriptor) describing the result of the
 * final pack.	In other words, it counts the number of flags set in
 * each segment.  So we can use a specialized segmented +_reduce.
 */
void pk1_lev (vec_p dst_v, vec_p flg_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch_v)
{
  int *dst, *segd, *segment;
  cvl_bool *src;
  int nelts_here, nsegs_here, current_seg, accum, i;

  LOG_START (pk1_lev, nelts);
  if (nsegs == 1) {
    /* "len" argument to the REPLACE function is irrelevant because we
     * know that we are always replacing the zeroth element on the
     * zeroth processor, and "len" will not affect this.  Any non-zero
     * value (e.g. 1) will do. */
    rep_vuz (dst_v, 0, pk1_luv (flg_v, nelts, scratch_v), 1, scratch_v);
  } else {
    nsegs_here = _num_here (nsegs);
    nelts_here = _num_here (nelts);
    dst = (int *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + flg_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);

    /* Initialize destination vector (segment reductions) to identity */
    for (i = 0; i < nsegs_here; i++) {
      dst[i] = 0;
    }

    /* Initialize an accumulator for the section of segment we have
     * seen so far, and the number of that segment */
    accum = 0;
    current_seg = 0;
    /* Iterate over the segmented source vector */
    {
      /* Normally, update accum with src[i].  If we hit a segment
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
	    accum = accum + (int) src[x];
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
	    accum = (int) src[x];
	    current_seg = segment[x];
	  }
	}
	n_rcvd += _recv_simple (dst, pk1_lev_tag, _unpack_add_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, pk1_lev_tag, (Send_ctr[x] * sizeof (intx)));
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
      _finish_simple (dst, pk1_lev_tag, n_sent, n_rcvd,
		      sizeof (intx), _unpack_add_intx);
    }
  }
  LOG_STOP (pk1_lev, nelts);
}

unsigned int pk1_lev_inplace (void)
{
  return INPLACE_NONE;
}

int pk1_lev_scratch (int nelts, int nsegs)
{
  return 0;
}





void pk2_lez (vec_p dst_v, vec_p src_v, vec_p flg_v,
	       vec_p src_segd_v, int src_nelts, int src_nsegs,
      vec_p dst_segd_v, int dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  LOG_START (pk2_lez, src_nelts);
  pk2_luz (dst_v, src_v, flg_v, src_nelts, dst_nelts, scratch_v);
  LOG_STOP (pk2_lez, src_nelts);
}

unsigned int pk2_lez_inplace (void)
{
  return INPLACE_NONE;
}

int pk2_lez_scratch (int src_nelts, int src_nsegs, int dst_nelts, int dst_nsegs)
{
  return 0;
}

void pk2_leb (vec_p dst_v, vec_p src_v, vec_p flg_v,
	       vec_p src_segd_v, int src_nelts, int src_nsegs,
      vec_p dst_segd_v, int dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  LOG_START (pk2_leb, src_nelts);
  pk2_lub (dst_v, src_v, flg_v, src_nelts, dst_nelts, scratch_v);
  LOG_STOP (pk2_leb, src_nelts);
}

unsigned int pk2_leb_inplace (void)
{
  return INPLACE_NONE;
}

int pk2_leb_scratch (int src_nelts, int src_nsegs, int dst_nelts, int dst_nsegs)
{
  return 0;
}

void pk2_led (vec_p dst_v, vec_p src_v, vec_p flg_v,
	       vec_p src_segd_v, int src_nelts, int src_nsegs,
      vec_p dst_segd_v, int dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  LOG_START (pk2_led, src_nelts);
  pk2_lud (dst_v, src_v, flg_v, src_nelts, dst_nelts, scratch_v);
  LOG_STOP (pk2_led, src_nelts);
}

unsigned int pk2_led_inplace (void)
{
  return INPLACE_NONE;
}

int pk2_led_scratch (int src_nelts, int src_nsegs, int dst_nelts, int dst_nsegs)
{
  return 0;
}



/* ---------------------- Unsegmented Index -------------------------- */

void ind_luz (vec_p dst_v, int init, int stride, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int val, n_here, i;

  LOG_START (ind_luz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  val = (FirstHere (nelts) * stride) + init;

  for (i = 0; i < n_here; i++) {
    dst[i] = val;
    val += stride;
  }
  LOG_STOP (ind_luz, nelts);
}

unsigned int ind_luz_inplace (void)
{
  return INPLACE_NONE;
}

int ind_luz_scratch (int nelts)
{
  return 0;
}



/* ----------------------- Segmented Index --------------------------- */

/* Send the initial value and stride out to the start of each segment,
 * then do a segmented copy-scan across their values to spread them out.
 */
void ind_lez (vec_p dst_v, vec_p init_v, vec_p stride_v, vec_p
	       segd_v, int nelts, int nsegs, vec_p scratch_v)
{
  int *init, *stride, *dst, *segd, *scratch, *start, *length, *segment;
  int nsegs_here, current_seg, start_seg, nelts_here, stride_value;
  int index_value, max_index, i;

  LOG_START (ind_lez, nelts);
  init = (int *) (Mem + init_v);
  stride = (int *) (Mem + stride_v);

  if (nsegs == 1) {
    int buf[2];

    buf[0] = init[0];
    buf[1] = stride[0];
    MPI_Bcast (buf, 2, MPI_INT, 0, MPI_COMM_WORLD);
    ind_luz (dst_v, buf[0], buf[1], nelts, scratch_v);
  } else if (nelts != 0) {

    nelts_here = _num_here (nelts);
    nsegs_here = _num_here (nsegs);
    dst = (int *) (Mem + dst_v);
    segd = (int *) (Mem + segd_v);
    scratch = (int *) (Mem + scratch_v);
    start = SegdStart (segd, nelts, nsegs);
    length = SegdLength (segd, nelts, nsegs);
    segment = SegdSegment (segd, nelts, nsegs);
    current_seg = *(SegdBefore (segd, nelts, nsegs));
    start_seg = *(SegdFirst (segd, nelts, nsegs));
    max_index = SENTINEL;

    /* Iterate over the init and stride vectors, sending out the values
     * to the start-of-segment positions and keeping track of the
     * maximum position (== last segment start) on each processor. */
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	loopbound = min (nsegs_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* dst[start[x]] <- init[x] scratch[start[x]] <- stride[x] */
	  if (length[x] != 0) {
	    int proc = start[x] / space;
	    int offset = start[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = init[x];
	      scratch[offset] = stride[x];
	      if (offset > max_index)
		max_index = offset;
	    } else {
	      int posn = Send_ctr[proc]++;
	      two_intx *buf = (two_intx *) Usr_send[proc];

	      buf[posn].value1 = init[x];
	      buf[posn].value2 = stride[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_twointx_maxindx (dst, scratch, ind_lez_tag,
					 &max_index);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (two_intx)) - RCV_EVERY)) {
	    _send_buffer (x, ind_lez_tag, (Send_ctr[x] * sizeof (two_intx)));
	    n_sent++;
	  }
	}
      }
      _finish_twointx_maxindx (dst, scratch, ind_lez_tag, n_sent,
			       n_rcvd, &max_index);
    }

    /* If max_index != SENTINEL, at least one segment starts on this
     * processor; get the index and stride values to give to the
     * copy-scan (the values are always those of the last segment on a
     * processor). */
    if (max_index != SENTINEL) {
      index_value = dst[max_index];
      stride_value = scratch[max_index];
      Segment_Here = TRUE;
    } else {
      index_value = 0;
      stride_value = 0;
      Segment_Here = FALSE;
    }

    /* Do segmented copy-scans across the values from the last segment
     * on each processor. */
    _copy_scan_two_int (&stride_value, &index_value);

    /* First processor always has a segment beginning at offset 0. */
    if (Self == 0) {
      index_value = dst[0];
      stride_value = scratch[0];
    }
    /* Now we can actually do the index. */
    index_value += (FirstHere (nelts) - start_seg) * stride_value;

    for (i = 0; i < nelts_here; i++) {
      if (segment[i] == current_seg) {
	dst[i] = index_value;
      } else {
	index_value = dst[i];
	stride_value = scratch[i];
	current_seg = segment[i];
	Segment_Here = TRUE;
      }
      index_value += stride_value;
    }
  }
  LOG_STOP (ind_lez, nelts);
}

unsigned int ind_lez_inplace (void)
{
  return INPLACE_NONE;
}

int ind_lez_scratch (int nelts, int nsegs)
{
  return ((nelts == 1) || (nsegs == 1)) ? 0 : siz_foz (nelts);
}
