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


/* -------------------- Unsegmented Distribute ----------------------- */


void dis_vuz (vec_p dst_v, int value, int nelts, vec_p scratch)
{
  int *dst;
  int n_here, i;

  LOG_START (dis_vuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = value;
  }
  LOG_STOP (dis_vuz, nelts);
}

unsigned int dis_vuz_inplace (void)
{
  return INPLACE_NONE;
}

int dis_vuz_scratch (int nelts)
{
  return 0;
}

void dis_vud (vec_p dst_v, double value, int nelts, vec_p scratch)
{
  double *dst;
  int n_here, i;

  LOG_START (dis_vud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = value;
  }
  LOG_STOP (dis_vud, nelts);
}

unsigned int dis_vud_inplace (void)
{
  return INPLACE_NONE;
}

int dis_vud_scratch (int nelts)
{
  return 0;
}

void dis_vub (vec_p dst_v, cvl_bool value, int nelts, vec_p scratch)
{
  cvl_bool *dst;
  int n_here, i;

  LOG_START (dis_vub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = value;
  }
  LOG_STOP (dis_vub, nelts);
}

unsigned int dis_vub_inplace (void)
{
  return INPLACE_NONE;
}

int dis_vub_scratch (int nelts)
{
  return 0;
}



/* ---------------------- Unsegmented Replace ------------------------ */


void rep_vuz (vec_p dst_v, int index, int value, int nelts, vec_p scratch)
{
  int *dst;
  int proc, offset;

  LOG_START (rep_vuz, 1);
  proc = MapToProc (index, nelts);
  offset = MapToOffset (index, nelts);
  if (proc == Self) {
    dst = (int *) (Mem + dst_v);
    dst[offset] = value;
  }
  LOG_STOP (rep_vuz, 1);
}

unsigned int rep_vuz_inplace (void)
{
  return INPLACE_NONE;
}

int rep_vuz_scratch (int nelts)
{
  return 0;
}

void rep_vud (vec_p dst_v, int index, double value, int nelts, vec_p scratch)
{
  double *dst;
  int proc, offset;

  LOG_START (rep_vud, 1);
  proc = MapToProc (index, nelts);
  offset = MapToOffset (index, nelts);
  if (proc == Self) {
    dst = (double *) (Mem + dst_v);
    dst[offset] = value;
  }
  LOG_STOP (rep_vud, 1);
}

unsigned int rep_vud_inplace (void)
{
  return INPLACE_NONE;
}

int rep_vud_scratch (int nelts)
{
  return 0;
}

void rep_vub (vec_p dst_v, int index, cvl_bool value, int nelts, vec_p scratch)
{
  cvl_bool *dst;
  int proc, offset;

  LOG_START (rep_vub, 1);
  proc = MapToProc (index, nelts);
  offset = MapToOffset (index, nelts);
  if (proc == Self) {
    dst = (cvl_bool *) (Mem + dst_v);
    dst[offset] = value;
  }
  LOG_STOP (rep_vub, 1);
}

unsigned int rep_vub_inplace (void)
{
  return INPLACE_NONE;
}

int rep_vub_scratch (int nelts)
{
  return 0;
}



/* ---------------------- Unsegmented Extract ------------------------ */


int ext_vuz (vec_p src_v, int index, int nelts, vec_p scratch)
{
  int *src;
  int result;
  int proc, offset;

  LOG_START (ext_vuz, 1);
  proc = MapToProc (index, nelts);
  offset = MapToOffset (index, nelts);
  if (proc == Self) {
    src = (int *) (Mem + src_v);
    result = src[offset];
  }
  MPI_Bcast (&result, 1, MPI_INT, proc, MPI_COMM_WORLD);
  LOG_STOP (ext_vuz, 1);
  return result;
}

unsigned int ext_vuz_inplace (void)
{
  return INPLACE_NONE;
}

int ext_vuz_scratch (int nelts)
{
  return 0;
}

double ext_vud (vec_p src_v, int index, int nelts, vec_p scratch)
{
  double *src;
  double result;
  int proc, offset;

  LOG_START (ext_vud, 1);
  proc = MapToProc (index, nelts);
  offset = MapToOffset (index, nelts);
  if (proc == Self) {
    src = (double *) (Mem + src_v);
    result = src[offset];
  }
  MPI_Bcast (&result, 1, MPI_DOUBLE, proc, MPI_COMM_WORLD);
  LOG_STOP (ext_vud, 1);
  return result;
}

unsigned int ext_vud_inplace (void)
{
  return INPLACE_NONE;
}

int ext_vud_scratch (int nelts)
{
  return 0;
}

cvl_bool ext_vub (vec_p src_v, int index, int nelts, vec_p scratch)
{
  cvl_bool *src;
  cvl_bool result;
  int proc, offset;

  LOG_START (ext_vub, 1);
  proc = MapToProc (index, nelts);
  offset = MapToOffset (index, nelts);
  if (proc == Self) {
    src = (cvl_bool *) (Mem + src_v);
    result = src[offset];
  }
  MPI_Bcast (&result, 1, MPI_BOOL, proc, MPI_COMM_WORLD);
  LOG_STOP (ext_vub, 1);
  return result;
}

unsigned int ext_vub_inplace (void)
{
  return INPLACE_NONE;
}

int ext_vub_scratch (int nelts)
{
  return 0;
}



/* --------------------- Segmented Distribute ------------------------ */


void dis_vez (vec_p dst_v, vec_p values_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch)
{
  int *dst, *values;
  int *segd, *start, *length, *segment;
  int value;
  int first_seg, nelts_here, nsegs_here, max_index, current_seg,
   i;

  LOG_START (dis_vez, nelts);
  if (nelts != 0) {
    values = (int *) (Mem + values_v);
    if (nsegs == 1) {
      /* If we have only one segment, then broadcast the 0th value
       * element, which must be in position 0 on processor 0, and use
       * this to do an unsegmented distribute. */
      MPI_Bcast (&values[0], 1, MPI_INT, 0, MPI_COMM_WORLD);
      dis_vuz (dst_v, values[0], nelts, scratch);
    } else {
      nsegs_here = _num_here (nsegs);
      dst = (int *) (Mem + dst_v);
      segd = (int *) (Mem + segd_v);
      start = SegdStart (segd, nelts, nsegs);
      length = SegdLength (segd, nelts, nsegs);
      segment = SegdSegment (segd, nelts, nsegs);
      first_seg = *(SegdBefore (segd, nelts, nsegs));
      max_index = SENTINEL;

      /* Send values to start-of-segment positions, keeping track of
       * the highest position sent to on each processor. */
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (nelts);
	int x, y, loopbound;

	for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	  loopbound = min (nsegs_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    /* if (length[x] != 0) dst[start[x]] <- values[x] */
	    if (length[x] != 0) {
	      int proc = start[x] / space;
	      int offset = start[x] - (proc * space);

	      if (proc == Self) {
		dst[offset] = values[x];
		if (offset > max_index)
		  max_index = offset;
	      } else {
		intx *buf = (intx *) Usr_send[proc];
		int posn = Send_ctr[proc]++;

		buf[posn].value = values[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_maxindx (dst, &max_index, dis_vez_tag,
				   _unpack_intx_maxindx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	      _send_buffer (x, dis_vez_tag, (Send_ctr[x] * sizeof (intx)));
	      n_sent++;
	    }
	  }
	}
	_finish_maxindx (dst, dis_vez_tag, n_sent, n_rcvd, sizeof (intx),
			 &max_index, _unpack_intx_maxindx);
      }

      /* If max_index is still SENTINEL, no segments are on this proc.
       * Otherwise, the value at dst[max_index] is the value for the
       * last segment beginning on this processor -- use it for the
       * copy-scan. */
      if (max_index == SENTINEL) {
	Segment_Here = FALSE;
      } else {
	Segment_Here = TRUE;
	value = dst[max_index];
      }
      _copy_scan_int (&value);

      /* Copy-scan over the destination vector, reloading the element
       * to be copied each time we hit a segment boundary. */
      nelts_here = _num_here (nelts);
      current_seg = first_seg;
      for (i = 0; i < nelts_here; i++) {
	if (segment[i] == current_seg) {
	  dst[i] = value;
	} else {
	  value = dst[i];
	  current_seg = segment[i];
	}
      }
    }
  }
  LOG_STOP (dis_vez, nelts);
}

unsigned int dis_vez_inplace (void)
{
  return INPLACE_NONE;
}

int dis_vez_scratch (int nelts, int nsegs)
{
  return 0;
}

void dis_ved (vec_p dst_v, vec_p values_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch)
{
  double *dst, *values;
  int *segd, *start, *length, *segment;
  double value;
  int first_seg, nelts_here, nsegs_here, max_index, current_seg,
   i;

  LOG_START (dis_ved, nelts);
  if (nelts != 0) {
    values = (double *) (Mem + values_v);
    if (nsegs == 1) {
      /* If we have only one segment, then broadcast the 0th value
       * element, which must be in position 0 on processor 0, and use
       * this to do an unsegmented distribute. */
      MPI_Bcast (&values[0], 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);
      dis_vud (dst_v, values[0], nelts, scratch);
    } else {
      nsegs_here = _num_here (nsegs);
      dst = (double *) (Mem + dst_v);
      segd = (int *) (Mem + segd_v);
      start = SegdStart (segd, nelts, nsegs);
      length = SegdLength (segd, nelts, nsegs);
      segment = SegdSegment (segd, nelts, nsegs);
      first_seg = *(SegdBefore (segd, nelts, nsegs));
      max_index = SENTINEL;

      /* Send values to start-of-segment positions, keeping track of
       * the highest position sent to on each processor. */
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (nelts);
	int x, y, loopbound;

	for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	  loopbound = min (nsegs_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    /* if (length[x] != 0) dst[start[x]] <- values[x] */
	    if (length[x] != 0) {
	      int proc = start[x] / space;
	      int offset = start[x] - (proc * space);

	      if (proc == Self) {
		dst[offset] = values[x];
		if (offset > max_index)
		  max_index = offset;
	      } else {
		doublex *buf = (doublex *) Usr_send[proc];
		int posn = Send_ctr[proc]++;

		buf[posn].value = values[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_maxindx (dst, &max_index, dis_ved_tag,
				   _unpack_doublex_maxindx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	      _send_buffer (x, dis_ved_tag, (Send_ctr[x] * sizeof (doublex)));
	      n_sent++;
	    }
	  }
	}
	_finish_maxindx (dst, dis_ved_tag, n_sent, n_rcvd, sizeof (doublex),
			 &max_index, _unpack_doublex_maxindx);
      }

      /* If max_index is still SENTINEL, no segments are on this proc.
       * Otherwise, the value at dst[max_index] is the value for the
       * last segment beginning on this processor -- use it for the
       * copy-scan. */
      if (max_index == SENTINEL) {
	Segment_Here = FALSE;
      } else {
	Segment_Here = TRUE;
	value = dst[max_index];
      }
      _copy_scan_double (&value);

      /* Copy-scan over the destination vector, reloading the element
       * to be copied each time we hit a segment boundary. */
      nelts_here = _num_here (nelts);
      current_seg = first_seg;
      for (i = 0; i < nelts_here; i++) {
	if (segment[i] == current_seg) {
	  dst[i] = value;
	} else {
	  value = dst[i];
	  current_seg = segment[i];
	}
      }
    }
  }
  LOG_STOP (dis_ved, nelts);
}

unsigned int dis_ved_inplace (void)
{
  return INPLACE_NONE;
}

int dis_ved_scratch (int nelts, int nsegs)
{
  return 0;
}

void dis_veb (vec_p dst_v, vec_p values_v, vec_p segd_v, int nelts,
	       int nsegs, vec_p scratch)
{
  cvl_bool *dst, *values;
  int *segd, *start, *length, *segment;
  cvl_bool value;
  int first_seg, nelts_here, nsegs_here, max_index, current_seg,
   i;

  LOG_START (dis_veb, nelts);
  if (nelts != 0) {
    values = (cvl_bool *) (Mem + values_v);
    if (nsegs == 1) {
      /* If we have only one segment, then broadcast the 0th value
       * element, which must be in position 0 on processor 0, and use
       * this to do an unsegmented distribute. */
      MPI_Bcast (&values[0], 1, MPI_BOOL, 0, MPI_COMM_WORLD);
      dis_vub (dst_v, values[0], nelts, scratch);
    } else {
      nsegs_here = _num_here (nsegs);
      dst = (cvl_bool *) (Mem + dst_v);
      segd = (int *) (Mem + segd_v);
      start = SegdStart (segd, nelts, nsegs);
      length = SegdLength (segd, nelts, nsegs);
      segment = SegdSegment (segd, nelts, nsegs);
      first_seg = *(SegdBefore (segd, nelts, nsegs));
      max_index = SENTINEL;

      /* Send values to start-of-segment positions, keeping track of
       * the highest position sent to on each processor. */
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (nelts);
	int x, y, loopbound;

	for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	  loopbound = min (nsegs_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    /* if (length[x] != 0) dst[start[x]] <- values[x] */
	    if (length[x] != 0) {
	      int proc = start[x] / space;
	      int offset = start[x] - (proc * space);

	      if (proc == Self) {
		dst[offset] = values[x];
		if (offset > max_index)
		  max_index = offset;
	      } else {
		cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];
		int posn = Send_ctr[proc]++;

		buf[posn].value = values[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_maxindx (dst, &max_index, dis_veb_tag,
				   _unpack_cvl_boolx_maxindx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	      _send_buffer (x, dis_veb_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	      n_sent++;
	    }
	  }
	}
	_finish_maxindx (dst, dis_veb_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
			 &max_index, _unpack_cvl_boolx_maxindx);
      }

      /* If max_index is still SENTINEL, no segments are on this proc.
       * Otherwise, the value at dst[max_index] is the value for the
       * last segment beginning on this processor -- use it for the
       * copy-scan. */
      if (max_index == SENTINEL) {
	Segment_Here = FALSE;
      } else {
	Segment_Here = TRUE;
	value = dst[max_index];
      }
      _copy_scan_cvl_bool (&value);

      /* Copy-scan over the destination vector, reloading the element
       * to be copied each time we hit a segment boundary. */
      nelts_here = _num_here (nelts);
      current_seg = first_seg;
      for (i = 0; i < nelts_here; i++) {
	if (segment[i] == current_seg) {
	  dst[i] = value;
	} else {
	  value = dst[i];
	  current_seg = segment[i];
	}
      }
    }
  }
  LOG_STOP (dis_veb, nelts);
}

unsigned int dis_veb_inplace (void)
{
  return INPLACE_NONE;
}

int dis_veb_scratch (int nelts, int nsegs)
{
  return 0;
}



/* ----------------------- Segmented Replace ------------------------- */


void rep_vez (vec_p dst_v, vec_p index_v, vec_p values_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch)
{
  int *dst, *values;
  int *index, *segd, *length, *start;
  int nsegs_here;

  LOG_START (rep_vez, nelts);
  dst = (int *) (Mem + dst_v);
  index = (int *) (Mem + index_v);
  values = (int *) (Mem + values_v);
  segd = (int *) (Mem + segd_v);
  start = SegdStart (segd, nelts, nsegs);

  if (nsegs == 1) {
    /* If we only have one segment, then we are only interested in
     * sending values[0] to (start[0]+index[0]), i.e. the 0th processor
     * sends to an arbitrary processor.	 Broadcast {element, index},
     * and then let each processor work out whether it is the
     * destination. */
    intx buf;
    int proc, offset;

    if (Self == 0) {
      buf.value = values[0];
      buf.index = start[0] + index[0];
    }
    MPI_Bcast (&buf, 2, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    proc = MapToProc (buf.index, nelts);
    offset = MapToOffset (buf.index, nelts);

    if (proc == Self) {
      dst[offset] = buf.value;
    }
  } else {
    nsegs_here = _num_here (nsegs);
    length = SegdLength (segd, nelts, nsegs);

    /* Iterate over the segments stored on this processor.  Add the
     * offset from the index vector to the position of the start of the
     * segment, and send the value to that position in the destination
     * vector. */
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	loopbound = min (nsegs_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* Ignore empty segments */
	  if (length[x] != 0) {
	    /* dst[start[x] + index[x]] <- values[x] */
	    int proc = (start[x] + index[x]) / space;
	    int offset = (start[x] + index[x]) - (proc * space);

	    if (proc == Self) {
	      dst[offset] = values[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      intx *buf = (intx *) Usr_send[proc];

	      buf[posn].value = values[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, rep_vez_tag, _unpack_simple_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, rep_vez_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, rep_vez_tag, n_sent, n_rcvd, sizeof (intx),
		      _unpack_simple_intx);
    }
  }
  LOG_STOP (rep_vez, nelts);
}

unsigned int rep_vez_inplace (void)
{
  return INPLACE_NONE;
}

int rep_vez_scratch (int nelts, int nsegs)
{
  return 0;
}

void rep_ved (vec_p dst_v, vec_p index_v, vec_p values_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch)
{
  double *dst, *values;
  int *index, *segd, *length, *start;
  int nsegs_here;

  LOG_START (rep_ved, nelts);
  dst = (double *) (Mem + dst_v);
  index = (int *) (Mem + index_v);
  values = (double *) (Mem + values_v);
  segd = (int *) (Mem + segd_v);
  start = SegdStart (segd, nelts, nsegs);

  if (nsegs == 1) {
    /* If we only have one segment, then we are only interested in
     * sending values[0] to (start[0]+index[0]), i.e. the 0th processor
     * sends to an arbitrary processor.	 Broadcast {element, index},
     * and then let each processor work out whether it is the
     * destination. */
    doublex buf;
    int proc, offset;

    if (Self == 0) {
      buf.value = values[0];
      buf.index = start[0] + index[0];
    }
    MPI_Bcast (&buf, 2, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    proc = MapToProc (buf.index, nelts);
    offset = MapToOffset (buf.index, nelts);

    if (proc == Self) {
      dst[offset] = buf.value;
    }
  } else {
    nsegs_here = _num_here (nsegs);
    length = SegdLength (segd, nelts, nsegs);

    /* Iterate over the segments stored on this processor.  Add the
     * offset from the index vector to the position of the start of the
     * segment, and send the value to that position in the destination
     * vector. */
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	loopbound = min (nsegs_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* Ignore empty segments */
	  if (length[x] != 0) {
	    /* dst[start[x] + index[x]] <- values[x] */
	    int proc = (start[x] + index[x]) / space;
	    int offset = (start[x] + index[x]) - (proc * space);

	    if (proc == Self) {
	      dst[offset] = values[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      doublex *buf = (doublex *) Usr_send[proc];

	      buf[posn].value = values[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, rep_ved_tag, _unpack_simple_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, rep_ved_tag, (Send_ctr[x] * sizeof (doublex)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, rep_ved_tag, n_sent, n_rcvd, sizeof (doublex),
		      _unpack_simple_doublex);
    }
  }
  LOG_STOP (rep_ved, nelts);
}

unsigned int rep_ved_inplace (void)
{
  return INPLACE_NONE;
}

int rep_ved_scratch (int nelts, int nsegs)
{
  return 0;
}

void rep_veb (vec_p dst_v, vec_p index_v, vec_p values_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch)
{
  cvl_bool *dst, *values;
  int *index, *segd, *length, *start;
  int nsegs_here;

  LOG_START (rep_veb, nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  index = (int *) (Mem + index_v);
  values = (cvl_bool *) (Mem + values_v);
  segd = (int *) (Mem + segd_v);
  start = SegdStart (segd, nelts, nsegs);

  if (nsegs == 1) {
    /* If we only have one segment, then we are only interested in
     * sending values[0] to (start[0]+index[0]), i.e. the 0th processor
     * sends to an arbitrary processor.	 Broadcast {element, index},
     * and then let each processor work out whether it is the
     * destination. */
    cvl_boolx buf;
    int proc, offset;

    if (Self == 0) {
      buf.value = values[0];
      buf.index = start[0] + index[0];
    }
    MPI_Bcast (&buf, 2, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    proc = MapToProc (buf.index, nelts);
    offset = MapToOffset (buf.index, nelts);

    if (proc == Self) {
      dst[offset] = buf.value;
    }
  } else {
    nsegs_here = _num_here (nsegs);
    length = SegdLength (segd, nelts, nsegs);

    /* Iterate over the segments stored on this processor.  Add the
     * offset from the index vector to the position of the start of the
     * segment, and send the value to that position in the destination
     * vector. */
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	loopbound = min (nsegs_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* Ignore empty segments */
	  if (length[x] != 0) {
	    /* dst[start[x] + index[x]] <- values[x] */
	    int proc = (start[x] + index[x]) / space;
	    int offset = (start[x] + index[x]) - (proc * space);

	    if (proc == Self) {
	      dst[offset] = values[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

	      buf[posn].value = values[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, rep_veb_tag, _unpack_simple_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, rep_veb_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, rep_veb_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
		      _unpack_simple_cvl_boolx);
    }
  }
  LOG_STOP (rep_veb, nelts);
}

unsigned int rep_veb_inplace (void)
{
  return INPLACE_NONE;
}

int rep_veb_scratch (int nelts, int nsegs)
{
  return 0;
}



/* ----------------------- Segmented Extract ------------------------- */


void ext_vez (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch)
{
  int *dst, *src;
  int *index, *segd, *length, *start;
  int nsegs_here;

  LOG_START (ext_vez, nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);
  index = (int *) (Mem + index_v);
  segd = (int *) (Mem + segd_v);
  start = SegdStart (segd, nelts, nsegs);

  if (nsegs == 1) {
    /* If we only have one segment, then we are only interested in
     * getting the contents of src[start[0]+index[0]] into dst[0].  0th
     * processor broadcasts start[0]+index[0], then waits for a send
     * from whichever processor holds the element at that index. */
    MPI_Status status;
    int proc, offset, req_index;

    req_index = start[0] + index[0];
    MPI_Bcast (&req_index, 1, MPI_INT, 0, MPI_COMM_WORLD);
    proc = MapToProc (req_index, nelts);
    offset = MapToOffset (req_index, nelts);

    if (proc == 0) {
      if (Self == 0) {
	dst[0] = src[offset];
      }
    } else {
      if (Self == 0) {
	MPI_Recv (&dst[0], 1, MPI_INT, MPI_ANY_SOURCE, ext_vez_tag, CommShift,
		  &status);
      } else if (proc == Self) {
	MPI_Send (&src[offset], 1, MPI_INT, 0, ext_vez_tag, CommShift);
      }
    }
  } else {
    /* Iterate over the segments stored on this processor.  Add offset
     * from index vector to the position of the start of the segment,
     * and send a request for the value at that position in src vector. */
    nsegs_here = _num_here (nsegs);
    length = SegdLength (segd, nelts, nsegs);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	loopbound = min (nsegs_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* Ignore empty segments */
	  if (length[x] != 0) {
	    /* Request: dst[x] <- src[start[x] + index[x]] */
	    int proc = (start[x] + index[x]) / space;
	    int offset = (start[x] + index[x]) - (proc * space);

	    if (proc == Self) {
	      dst[x] = src[offset];
	    } else {
	      int posn = Send_ctr[proc]++;
	      fetch *buf = (fetch *) Usr_send[proc];

	      buf[posn].src_index = offset;	/* where to get from */
	      buf[posn].dst_index = x;	/* where to send to */
	    }
	  }
	}
	n_rcvd += _recv_fetch (dst, src, ext_vez_tag, _unpack_fetch_intx,
			       _unpack_simple_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, ext_vez_tag, (Send_ctr[x] * sizeof (fetch)));
	    n_sent++;
	  }
	}
      }
      _finish_fetch (dst, src, ext_vez_tag, n_sent, n_rcvd,
		     _unpack_fetch_intx, _unpack_simple_intx);
    }
    LOG_STOP (ext_vez, nelts);
  }
}

int ext_vez_scratch (int nelts, int nsegs)
{
  return 0;
}

unsigned int ext_vez_inplace (void)
{
  return INPLACE_NONE;
}

void ext_ved (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch)
{
  double *dst, *src;
  int *index, *segd, *length, *start;
  int nsegs_here;

  LOG_START (ext_ved, nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);
  index = (int *) (Mem + index_v);
  segd = (int *) (Mem + segd_v);
  start = SegdStart (segd, nelts, nsegs);

  if (nsegs == 1) {
    /* If we only have one segment, then we are only interested in
     * getting the contents of src[start[0]+index[0]] into dst[0].  0th
     * processor broadcasts start[0]+index[0], then waits for a send
     * from whichever processor holds the element at that index. */
    MPI_Status status;
    int proc, offset, req_index;

    req_index = start[0] + index[0];
    MPI_Bcast (&req_index, 1, MPI_INT, 0, MPI_COMM_WORLD);
    proc = MapToProc (req_index, nelts);
    offset = MapToOffset (req_index, nelts);

    if (proc == 0) {
      if (Self == 0) {
	dst[0] = src[offset];
      }
    } else {
      if (Self == 0) {
	MPI_Recv (&dst[0], 1, MPI_DOUBLE, MPI_ANY_SOURCE, ext_ved_tag, CommShift,
		  &status);
      } else if (proc == Self) {
	MPI_Send (&src[offset], 1, MPI_DOUBLE, 0, ext_ved_tag, CommShift);
      }
    }
  } else {
    /* Iterate over the segments stored on this processor.  Add offset
     * from index vector to the position of the start of the segment,
     * and send a request for the value at that position in src vector. */
    nsegs_here = _num_here (nsegs);
    length = SegdLength (segd, nelts, nsegs);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	loopbound = min (nsegs_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* Ignore empty segments */
	  if (length[x] != 0) {
	    /* Request: dst[x] <- src[start[x] + index[x]] */
	    int proc = (start[x] + index[x]) / space;
	    int offset = (start[x] + index[x]) - (proc * space);

	    if (proc == Self) {
	      dst[x] = src[offset];
	    } else {
	      int posn = Send_ctr[proc]++;
	      fetch *buf = (fetch *) Usr_send[proc];

	      buf[posn].src_index = offset;	/* where to get from */
	      buf[posn].dst_index = x;	/* where to send to */
	    }
	  }
	}
	n_rcvd += _recv_fetch (dst, src, ext_ved_tag, _unpack_fetch_doublex,
			       _unpack_simple_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, ext_ved_tag, (Send_ctr[x] * sizeof (fetch)));
	    n_sent++;
	  }
	}
      }
      _finish_fetch (dst, src, ext_ved_tag, n_sent, n_rcvd,
		     _unpack_fetch_doublex, _unpack_simple_doublex);
    }
    LOG_STOP (ext_ved, nelts);
  }
}

int ext_ved_scratch (int nelts, int nsegs)
{
  return 0;
}

unsigned int ext_ved_inplace (void)
{
  return INPLACE_NONE;
}

void ext_veb (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch)
{
  cvl_bool *dst, *src;
  int *index, *segd, *length, *start;
  int nsegs_here;

  LOG_START (ext_veb, nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);
  index = (int *) (Mem + index_v);
  segd = (int *) (Mem + segd_v);
  start = SegdStart (segd, nelts, nsegs);

  if (nsegs == 1) {
    /* If we only have one segment, then we are only interested in
     * getting the contents of src[start[0]+index[0]] into dst[0].  0th
     * processor broadcasts start[0]+index[0], then waits for a send
     * from whichever processor holds the element at that index. */
    MPI_Status status;
    int proc, offset, req_index;

    req_index = start[0] + index[0];
    MPI_Bcast (&req_index, 1, MPI_INT, 0, MPI_COMM_WORLD);
    proc = MapToProc (req_index, nelts);
    offset = MapToOffset (req_index, nelts);

    if (proc == 0) {
      if (Self == 0) {
	dst[0] = src[offset];
      }
    } else {
      if (Self == 0) {
	MPI_Recv (&dst[0], 1, MPI_BOOL, MPI_ANY_SOURCE, ext_veb_tag, CommShift,
		  &status);
      } else if (proc == Self) {
	MPI_Send (&src[offset], 1, MPI_BOOL, 0, ext_veb_tag, CommShift);
      }
    }
  } else {
    /* Iterate over the segments stored on this processor.  Add offset
     * from index vector to the position of the start of the segment,
     * and send a request for the value at that position in src vector. */
    nsegs_here = _num_here (nsegs);
    length = SegdLength (segd, nelts, nsegs);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	loopbound = min (nsegs_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* Ignore empty segments */
	  if (length[x] != 0) {
	    /* Request: dst[x] <- src[start[x] + index[x]] */
	    int proc = (start[x] + index[x]) / space;
	    int offset = (start[x] + index[x]) - (proc * space);

	    if (proc == Self) {
	      dst[x] = src[offset];
	    } else {
	      int posn = Send_ctr[proc]++;
	      fetch *buf = (fetch *) Usr_send[proc];

	      buf[posn].src_index = offset;	/* where to get from */
	      buf[posn].dst_index = x;	/* where to send to */
	    }
	  }
	}
	n_rcvd += _recv_fetch (dst, src, ext_veb_tag, _unpack_fetch_cvl_boolx,
			       _unpack_simple_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, ext_veb_tag, (Send_ctr[x] * sizeof (fetch)));
	    n_sent++;
	  }
	}
      }
      _finish_fetch (dst, src, ext_veb_tag, n_sent, n_rcvd,
		   _unpack_fetch_cvl_boolx, _unpack_simple_cvl_boolx);
    }
    LOG_STOP (ext_veb, nelts);
  }
}

int ext_veb_scratch (int nelts, int nsegs)
{
  return 0;
}

unsigned int ext_veb_inplace (void)
{
  return INPLACE_NONE;
}
