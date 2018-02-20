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


/* ------------------- Unsegmented Simple Permute -------------------- */

/* XXX Rewrite to count the number of bytes received and terminate when
 * sent = received.
 */

void smp_puz (vec_p dst_v, vec_p src_v, vec_p index_v, int nelts,
	       vec_p scratch_v)
{
  int *dst, *src;
  int *index;
  int n_here;

  LOG_START (smp_puz, nelts);
  if (nelts != 0) {
    n_here = _num_here (nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  {
	    /* dst[index[x]] <- src[x] */
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      intx *buf = (intx *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, smp_puz_tag, _unpack_simple_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, smp_puz_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, smp_puz_tag, n_sent, n_rcvd, sizeof (intx),
		      _unpack_simple_intx);
    }
  }
  LOG_STOP (smp_puz, nelts);
}

unsigned int smp_puz_inplace (void)
{
  return INPLACE_NONE;
}

int smp_puz_scratch (int nelts)
{
  return 0;
}

void smp_pud (vec_p dst_v, vec_p src_v, vec_p index_v, int nelts,
	       vec_p scratch_v)
{
  double *dst, *src;
  int *index;
  int n_here;

  LOG_START (smp_pud, nelts);
  if (nelts != 0) {
    n_here = _num_here (nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  {
	    /* dst[index[x]] <- src[x] */
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      doublex *buf = (doublex *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, smp_pud_tag, _unpack_simple_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, smp_pud_tag, (Send_ctr[x] * sizeof (doublex)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, smp_pud_tag, n_sent, n_rcvd, sizeof (doublex),
		      _unpack_simple_doublex);
    }
  }
  LOG_STOP (smp_pud, nelts);
}

unsigned int smp_pud_inplace (void)
{
  return INPLACE_NONE;
}

int smp_pud_scratch (int nelts)
{
  return 0;
}

void smp_pub (vec_p dst_v, vec_p src_v, vec_p index_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst, *src;
  int *index;
  int n_here;

  LOG_START (smp_pub, nelts);
  if (nelts != 0) {
    n_here = _num_here (nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  {
	    /* dst[index[x]] <- src[x] */
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, smp_pub_tag, _unpack_simple_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, smp_pub_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, smp_pub_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
		      _unpack_simple_cvl_boolx);
    }
  }
  LOG_STOP (smp_pub, nelts);
}

unsigned int smp_pub_inplace (void)
{
  return INPLACE_NONE;
}

int smp_pub_scratch (int nelts)
{
  return 0;
}



/* ------------------- Unsegmented Flag Permute ---------------------- */


void fpm_puz (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       int src_nelts, int dst_nelts, vec_p scratch_v)
{
  int *dst, *src;
  int *index;
  cvl_bool *flag;
  int n_here;

  LOG_START (fpm_puz, src_nelts);
  if (src_nelts != 0) {
    n_here = _num_here (src_nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    flag = (cvl_bool *) (Mem + flag_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (dst_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* if (flag[x]) dst[index[x]] <- src[x] */
	  if (flag[x]) {
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      intx *buf = (intx *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, fpm_puz_tag, _unpack_simple_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, fpm_puz_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, fpm_puz_tag, n_sent, n_rcvd, sizeof (intx),
		      _unpack_simple_intx);
    }
  }
  LOG_STOP (fpm_puz, src_nelts);
}

unsigned int fpm_puz_inplace (void)
{
  return INPLACE_NONE;
}

int fpm_puz_scratch (int nelts, int nsegs)
{
  return 0;
}

void fpm_pud (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       int src_nelts, int dst_nelts, vec_p scratch_v)
{
  double *dst, *src;
  int *index;
  cvl_bool *flag;
  int n_here;

  LOG_START (fpm_pud, src_nelts);
  if (src_nelts != 0) {
    n_here = _num_here (src_nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    flag = (cvl_bool *) (Mem + flag_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (dst_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* if (flag[x]) dst[index[x]] <- src[x] */
	  if (flag[x]) {
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      doublex *buf = (doublex *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, fpm_pud_tag, _unpack_simple_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, fpm_pud_tag, (Send_ctr[x] * sizeof (doublex)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, fpm_pud_tag, n_sent, n_rcvd, sizeof (doublex),
		      _unpack_simple_doublex);
    }
  }
  LOG_STOP (fpm_pud, src_nelts);
}

unsigned int fpm_pud_inplace (void)
{
  return INPLACE_NONE;
}

int fpm_pud_scratch (int nelts, int nsegs)
{
  return 0;
}

void fpm_pub (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       int src_nelts, int dst_nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  int *index;
  cvl_bool *flag;
  int n_here;

  LOG_START (fpm_pub, src_nelts);
  if (src_nelts != 0) {
    n_here = _num_here (src_nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    flag = (cvl_bool *) (Mem + flag_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (dst_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* if (flag[x]) dst[index[x]] <- src[x] */
	  if (flag[x]) {
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, fpm_pub_tag, _unpack_simple_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, fpm_pub_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, fpm_pub_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
		      _unpack_simple_cvl_boolx);
    }
  }
  LOG_STOP (fpm_pub, src_nelts);
}

unsigned int fpm_pub_inplace (void)
{
  return INPLACE_NONE;
}

int fpm_pub_scratch (int nelts, int nsegs)
{
  return 0;
}



/* ------------------ Unsegmented Default Permute -------------------- */


void dpe_puz (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p default_v,
	       int src_nelts, int dst_nelts, vec_p scratch_v)
{
  int *dst, *src, *def;
  int *index;
  int n_here_src, n_here_dst, i;

  LOG_START (dpe_puz, src_nelts);
  if (dst_nelts != 0) {
    n_here_src = _num_here (src_nelts);
    n_here_dst = _num_here (dst_nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    def = (int *) (Mem + default_v);

    for (i = 0; i < n_here_dst; i++) {
      dst[i] = def[i];
    }

    if (src_nelts != 0) {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (dst_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here_src; y += RCV_EVERY) {
	loopbound = min (n_here_src, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  {
	    /* dst[index[x]] <- src[x] */
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      intx *buf = (intx *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, dpe_puz_tag, _unpack_simple_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, dpe_puz_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, dpe_puz_tag, n_sent, n_rcvd, sizeof (intx),
		      _unpack_simple_intx);
    }
  }
  LOG_STOP (dpe_puz, src_nelts);
}

unsigned int dpe_puz_inplace (void)
{
  return INPLACE_NONE;
}

int dpe_puz_scratch (int nelts, int nsegs)
{
  return 0;
}

void dpe_pud (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p default_v,
	       int src_nelts, int dst_nelts, vec_p scratch_v)
{
  double *dst, *src, *def;
  int *index;
  int n_here_src, n_here_dst, i;

  LOG_START (dpe_pud, src_nelts);
  if (dst_nelts != 0) {
    n_here_src = _num_here (src_nelts);
    n_here_dst = _num_here (dst_nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    def = (double *) (Mem + default_v);

    for (i = 0; i < n_here_dst; i++) {
      dst[i] = def[i];
    }

    if (src_nelts != 0) {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (dst_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here_src; y += RCV_EVERY) {
	loopbound = min (n_here_src, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  {
	    /* dst[index[x]] <- src[x] */
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      doublex *buf = (doublex *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, dpe_pud_tag, _unpack_simple_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, dpe_pud_tag, (Send_ctr[x] * sizeof (doublex)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, dpe_pud_tag, n_sent, n_rcvd, sizeof (doublex),
		      _unpack_simple_doublex);
    }
  }
  LOG_STOP (dpe_pud, src_nelts);
}

unsigned int dpe_pud_inplace (void)
{
  return INPLACE_NONE;
}

int dpe_pud_scratch (int nelts, int nsegs)
{
  return 0;
}

void dpe_pub (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p default_v,
	       int src_nelts, int dst_nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src, *def;
  int *index;
  int n_here_src, n_here_dst, i;

  LOG_START (dpe_pub, src_nelts);
  if (dst_nelts != 0) {
    n_here_src = _num_here (src_nelts);
    n_here_dst = _num_here (dst_nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    def = (cvl_bool *) (Mem + default_v);

    for (i = 0; i < n_here_dst; i++) {
      dst[i] = def[i];
    }

    if (src_nelts != 0) {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (dst_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here_src; y += RCV_EVERY) {
	loopbound = min (n_here_src, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  {
	    /* dst[index[x]] <- src[x] */
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, dpe_pub_tag, _unpack_simple_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, dpe_pub_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, dpe_pub_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
		      _unpack_simple_cvl_boolx);
    }
  }
  LOG_STOP (dpe_pub, src_nelts);
}

unsigned int dpe_pub_inplace (void)
{
  return INPLACE_NONE;
}

int dpe_pub_scratch (int nelts, int nsegs)
{
  return 0;
}



/* ---------------- Unsegmented Default Flag Permute ----------------- */


void dfp_puz (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v, vec_p
	     default_v, int src_nelts, int dst_nelts, vec_p scratch_v)
{
  int *dst, *src, *def;
  int *index;
  cvl_bool *flag;
  int n_here_dst, n_here_src, i;

  LOG_START (dfp_puz, src_nelts);
  if (dst_nelts != 0) {
    n_here_dst = _num_here (dst_nelts);
    dst = (int *) (Mem + dst_v);
    def = (int *) (Mem + default_v);

    for (i = 0; i < n_here_dst; i++) {
      dst[i] = def[i];
    }

    if (src_nelts != 0) {
      n_here_src = _num_here (src_nelts);
      src = (int *) (Mem + src_v);
      index = (int *) (Mem + index_v);
      flag = (cvl_bool *) (Mem + flag_v);
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (dst_nelts);
	int x, y, loopbound;

	for (y = 0; y < n_here_src; y += RCV_EVERY) {
	  loopbound = min (n_here_src, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    /* if (flag[x]) dst[index[x]] <- src[x] */
	    if (flag[x]) {
	      int proc = index[x] / space;
	      int offset = index[x] - (proc * space);

	      if (proc == Self) {
		dst[offset] = src[x];
	      } else {
		int posn = Send_ctr[proc]++;
		intx *buf = (intx *) Usr_send[proc];

		buf[posn].value = src[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_simple (dst, dfp_puz_tag, _unpack_simple_intx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	      _send_buffer (x, dfp_puz_tag, (Send_ctr[x] * sizeof (intx)));
	      n_sent++;
	    }
	  }
	}
	_finish_simple (dst, dfp_puz_tag, n_sent, n_rcvd, sizeof (intx),
			_unpack_simple_intx);
      }
    }
  }
  LOG_STOP (dfp_puz, src_nelts);
}

unsigned int dfp_puz_inplace (void)
{
  return INPLACE_NONE;
}

int dfp_puz_scratch (int nelts, int nsegs)
{
  return 0;
}

void dfp_pud (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v, vec_p
	     default_v, int src_nelts, int dst_nelts, vec_p scratch_v)
{
  double *dst, *src, *def;
  int *index;
  cvl_bool *flag;
  int n_here_dst, n_here_src, i;

  LOG_START (dfp_pud, src_nelts);
  if (dst_nelts != 0) {
    n_here_dst = _num_here (dst_nelts);
    dst = (double *) (Mem + dst_v);
    def = (double *) (Mem + default_v);

    for (i = 0; i < n_here_dst; i++) {
      dst[i] = def[i];
    }

    if (src_nelts != 0) {
      n_here_src = _num_here (src_nelts);
      src = (double *) (Mem + src_v);
      index = (int *) (Mem + index_v);
      flag = (cvl_bool *) (Mem + flag_v);
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (dst_nelts);
	int x, y, loopbound;

	for (y = 0; y < n_here_src; y += RCV_EVERY) {
	  loopbound = min (n_here_src, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    /* if (flag[x]) dst[index[x]] <- src[x] */
	    if (flag[x]) {
	      int proc = index[x] / space;
	      int offset = index[x] - (proc * space);

	      if (proc == Self) {
		dst[offset] = src[x];
	      } else {
		int posn = Send_ctr[proc]++;
		doublex *buf = (doublex *) Usr_send[proc];

		buf[posn].value = src[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_simple (dst, dfp_pud_tag, _unpack_simple_doublex);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	      _send_buffer (x, dfp_pud_tag, (Send_ctr[x] * sizeof (doublex)));
	      n_sent++;
	    }
	  }
	}
	_finish_simple (dst, dfp_pud_tag, n_sent, n_rcvd, sizeof (doublex),
			_unpack_simple_doublex);
      }
    }
  }
  LOG_STOP (dfp_pud, src_nelts);
}

unsigned int dfp_pud_inplace (void)
{
  return INPLACE_NONE;
}

int dfp_pud_scratch (int nelts, int nsegs)
{
  return 0;
}

void dfp_pub (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v, vec_p
	     default_v, int src_nelts, int dst_nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src, *def;
  int *index;
  cvl_bool *flag;
  int n_here_dst, n_here_src, i;

  LOG_START (dfp_pub, src_nelts);
  if (dst_nelts != 0) {
    n_here_dst = _num_here (dst_nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    def = (cvl_bool *) (Mem + default_v);

    for (i = 0; i < n_here_dst; i++) {
      dst[i] = def[i];
    }

    if (src_nelts != 0) {
      n_here_src = _num_here (src_nelts);
      src = (cvl_bool *) (Mem + src_v);
      index = (int *) (Mem + index_v);
      flag = (cvl_bool *) (Mem + flag_v);
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (dst_nelts);
	int x, y, loopbound;

	for (y = 0; y < n_here_src; y += RCV_EVERY) {
	  loopbound = min (n_here_src, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    /* if (flag[x]) dst[index[x]] <- src[x] */
	    if (flag[x]) {
	      int proc = index[x] / space;
	      int offset = index[x] - (proc * space);

	      if (proc == Self) {
		dst[offset] = src[x];
	      } else {
		int posn = Send_ctr[proc]++;
		cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

		buf[posn].value = src[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_simple (dst, dfp_pub_tag, _unpack_simple_cvl_boolx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	      _send_buffer (x, dfp_pub_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	      n_sent++;
	    }
	  }
	}
	_finish_simple (dst, dfp_pub_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
			_unpack_simple_cvl_boolx);
      }
    }
  }
  LOG_STOP (dfp_pub, src_nelts);
}

unsigned int dfp_pub_inplace (void)
{
  return INPLACE_NONE;
}

int dfp_pub_scratch (int nelts, int nsegs)
{
  return 0;
}



/* ------------------ Unsegmented Backwards Permute ------------------ */


void bck_puz (vec_p dst_v, vec_p src_v, vec_p index_v, int src_nelts,
	       int dst_nelts, vec_p scratch_v)
{
  int *dst, *src;
  int *index;
  int n_here;

  LOG_START (bck_puz, src_nelts);
  if (dst_nelts != 0) {
    n_here = _num_here (dst_nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (src_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  {
	    /* dst[x] <- src[index[x]] */
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

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
	n_rcvd += _recv_fetch (dst, src, bck_puz_tag, _unpack_fetch_intx,
			       _unpack_simple_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, bck_puz_tag, (Send_ctr[x] * sizeof (fetch)));
	    n_sent++;
	  }
	}
      }
      _finish_fetch (dst, src, bck_puz_tag, n_sent, n_rcvd,
		     _unpack_fetch_intx, _unpack_simple_intx);
    }
  }
  LOG_STOP (bck_puz, src_nelts);
}

unsigned int bck_puz_inplace (void)
{
  return INPLACE_NONE;
}

int bck_puz_scratch (int nelts, int nsegs)
{
  return 0;
}

void bck_pud (vec_p dst_v, vec_p src_v, vec_p index_v, int src_nelts,
	       int dst_nelts, vec_p scratch_v)
{
  double *dst, *src;
  int *index;
  int n_here;

  LOG_START (bck_pud, src_nelts);
  if (dst_nelts != 0) {
    n_here = _num_here (dst_nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (src_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  {
	    /* dst[x] <- src[index[x]] */
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

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
	n_rcvd += _recv_fetch (dst, src, bck_pud_tag, _unpack_fetch_doublex,
			       _unpack_simple_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, bck_pud_tag, (Send_ctr[x] * sizeof (fetch)));
	    n_sent++;
	  }
	}
      }
      _finish_fetch (dst, src, bck_pud_tag, n_sent, n_rcvd,
		     _unpack_fetch_doublex, _unpack_simple_doublex);
    }
  }
  LOG_STOP (bck_pud, src_nelts);
}

unsigned int bck_pud_inplace (void)
{
  return INPLACE_NONE;
}

int bck_pud_scratch (int nelts, int nsegs)
{
  return 0;
}

void bck_pub (vec_p dst_v, vec_p src_v, vec_p index_v, int src_nelts,
	       int dst_nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  int *index;
  int n_here;

  LOG_START (bck_pub, src_nelts);
  if (dst_nelts != 0) {
    n_here = _num_here (dst_nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (src_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  {
	    /* dst[x] <- src[index[x]] */
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

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
	n_rcvd += _recv_fetch (dst, src, bck_pub_tag, _unpack_fetch_cvl_boolx,
			       _unpack_simple_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, bck_pub_tag, (Send_ctr[x] * sizeof (fetch)));
	    n_sent++;
	  }
	}
      }
      _finish_fetch (dst, src, bck_pub_tag, n_sent, n_rcvd,
		   _unpack_fetch_cvl_boolx, _unpack_simple_cvl_boolx);
    }
  }
  LOG_STOP (bck_pub, src_nelts);
}

unsigned int bck_pub_inplace (void)
{
  return INPLACE_NONE;
}

int bck_pub_scratch (int nelts, int nsegs)
{
  return 0;
}



/* --------------- Unsegmented Backwards Flag Permute ---------------- */


void bfp_puz (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       int src_nelts, int dst_nelts, vec_p scratch_v)
{
  int *dst, *src;
  int *index;
  cvl_bool *flag;
  int n_here;

  LOG_START (bfp_puz, src_nelts);
  if (dst_nelts != 0) {
    n_here = _num_here (dst_nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    flag = (cvl_bool *) (Mem + flag_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (src_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* if (flag[x]) dst[x] <- src[index[x]] */
	  if (flag[x]) {
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

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
	n_rcvd += _recv_fetch (dst, src, bfp_puz_tag, _unpack_fetch_intx,
			       _unpack_simple_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, bfp_puz_tag, (Send_ctr[x] * sizeof (fetch)));
	    n_sent++;
	  }
	}
      }
      _finish_fetch (dst, src, bfp_puz_tag, n_sent, n_rcvd,
		     _unpack_fetch_intx, _unpack_simple_intx);
    }
  }
  LOG_STOP (bfp_puz, src_nelts);
}

unsigned int bfp_puz_inplace (void)
{
  return INPLACE_NONE;
}

int bfp_puz_scratch (int nelts, int nsegs)
{
  return 0;
}

void bfp_pud (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       int src_nelts, int dst_nelts, vec_p scratch_v)
{
  double *dst, *src;
  int *index;
  cvl_bool *flag;
  int n_here;

  LOG_START (bfp_pud, src_nelts);
  if (dst_nelts != 0) {
    n_here = _num_here (dst_nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    flag = (cvl_bool *) (Mem + flag_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (src_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* if (flag[x]) dst[x] <- src[index[x]] */
	  if (flag[x]) {
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

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
	n_rcvd += _recv_fetch (dst, src, bfp_pud_tag, _unpack_fetch_doublex,
			       _unpack_simple_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, bfp_pud_tag, (Send_ctr[x] * sizeof (fetch)));
	    n_sent++;
	  }
	}
      }
      _finish_fetch (dst, src, bfp_pud_tag, n_sent, n_rcvd,
		     _unpack_fetch_doublex, _unpack_simple_doublex);
    }
  }
  LOG_STOP (bfp_pud, src_nelts);
}

unsigned int bfp_pud_inplace (void)
{
  return INPLACE_NONE;
}

int bfp_pud_scratch (int nelts, int nsegs)
{
  return 0;
}

void bfp_pub (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       int src_nelts, int dst_nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  int *index;
  cvl_bool *flag;
  int n_here;

  LOG_START (bfp_pub, src_nelts);
  if (dst_nelts != 0) {
    n_here = _num_here (dst_nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    flag = (cvl_bool *) (Mem + flag_v);
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (src_nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  /* if (flag[x]) dst[x] <- src[index[x]] */
	  if (flag[x]) {
	    int proc = index[x] / space;
	    int offset = index[x] - (proc * space);

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
	n_rcvd += _recv_fetch (dst, src, bfp_pub_tag, _unpack_fetch_cvl_boolx,
			       _unpack_simple_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, bfp_pub_tag, (Send_ctr[x] * sizeof (fetch)));
	    n_sent++;
	  }
	}
      }
      _finish_fetch (dst, src, bfp_pub_tag, n_sent, n_rcvd,
		   _unpack_fetch_cvl_boolx, _unpack_simple_cvl_boolx);
    }
  }
  LOG_STOP (bfp_pub, src_nelts);
}

unsigned int bfp_pub_inplace (void)
{
  return INPLACE_NONE;
}

int bfp_pub_scratch (int nelts, int nsegs)
{
  return 0;
}



/* -------------------- Segmented Simple Permute --------------------- */

/* XXX Rewrite to count the number of bytes received and terminate when
 * sent = received.
 */

void smp_pez (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch_v)
{
  int *dst, *src;
  int *index, *segd, *segment;
  int current_seg, start, n_here, base;

  LOG_START (smp_pez, nelts);
  if (nelts != 0) {
    n_here = _num_here (nelts);
    base = FirstHere (nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    current_seg = *(SegdBefore (segd, nelts, nsegs));
    start = *(SegdFirst (segd, nelts, nsegs));
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] != current_seg) {
	    current_seg = segment[x];
	    start = base + x;
	  }
	  /* dst[start+index[x]] <- src[x] */
	  {
	    int proc = (start + index[x]) / space;
	    int offset = (start + index[x]) - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      intx *buf = (intx *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, smp_pez_tag, _unpack_simple_intx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	    _send_buffer (x, smp_pez_tag, (Send_ctr[x] * sizeof (intx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, smp_pez_tag, n_sent, n_rcvd, sizeof (intx),
		      _unpack_simple_intx);
    }
  }
  LOG_STOP (smp_pez, nelts);
}
unsigned int smp_pez_inplace (void)
{
  return INPLACE_NONE;
}

int smp_pez_scratch (int nelts, int nsegs)
{
  return 0;
}

void smp_ped (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch_v)
{
  double *dst, *src;
  int *index, *segd, *segment;
  int current_seg, start, n_here, base;

  LOG_START (smp_ped, nelts);
  if (nelts != 0) {
    n_here = _num_here (nelts);
    base = FirstHere (nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    current_seg = *(SegdBefore (segd, nelts, nsegs));
    start = *(SegdFirst (segd, nelts, nsegs));
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] != current_seg) {
	    current_seg = segment[x];
	    start = base + x;
	  }
	  /* dst[start+index[x]] <- src[x] */
	  {
	    int proc = (start + index[x]) / space;
	    int offset = (start + index[x]) - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      doublex *buf = (doublex *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, smp_ped_tag, _unpack_simple_doublex);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	    _send_buffer (x, smp_ped_tag, (Send_ctr[x] * sizeof (doublex)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, smp_ped_tag, n_sent, n_rcvd, sizeof (doublex),
		      _unpack_simple_doublex);
    }
  }
  LOG_STOP (smp_ped, nelts);
}
unsigned int smp_ped_inplace (void)
{
  return INPLACE_NONE;
}

int smp_ped_scratch (int nelts, int nsegs)
{
  return 0;
}

void smp_peb (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p segd_v,
	       int nelts, int nsegs, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  int *index, *segd, *segment;
  int current_seg, start, n_here, base;

  LOG_START (smp_peb, nelts);
  if (nelts != 0) {
    n_here = _num_here (nelts);
    base = FirstHere (nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + src_v);
    index = (int *) (Mem + index_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    current_seg = *(SegdBefore (segd, nelts, nsegs));
    start = *(SegdFirst (segd, nelts, nsegs));
    {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < n_here; y += RCV_EVERY) {
	loopbound = min (n_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (segment[x] != current_seg) {
	    current_seg = segment[x];
	    start = base + x;
	  }
	  /* dst[start+index[x]] <- src[x] */
	  {
	    int proc = (start + index[x]) / space;
	    int offset = (start + index[x]) - (proc * space);

	    if (proc == Self) {
	      dst[offset] = src[x];
	    } else {
	      int posn = Send_ctr[proc]++;
	      cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

	      buf[posn].value = src[x];
	      buf[posn].index = offset;
	    }
	  }
	}
	n_rcvd += _recv_simple (dst, smp_peb_tag, _unpack_simple_cvl_boolx);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	    _send_buffer (x, smp_peb_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	    n_sent++;
	  }
	}
      }
      _finish_simple (dst, smp_peb_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
		      _unpack_simple_cvl_boolx);
    }
  }
  LOG_STOP (smp_peb, nelts);
}
unsigned int smp_peb_inplace (void)
{
  return INPLACE_NONE;
}

int smp_peb_scratch (int nelts, int nsegs)
{
  return 0;
}



/* Send start-of-segment positions of dst vector as integers to the
 * start-of-segment positions of the scratch vector, which is the same
 * length as the src vector.  Return the maximum value received (==
 * position of last dst segment on each processor).
 */
static int _init_seg_starts (int *scratch, int *src_start, int *dst_start,
			  int *src_length, int *dst_length, int nsegs,
			      int nelts, int tag)
{
  int nsegs_here = _num_here (nsegs);
  int max_value = 0;

  int n_sent = 0, n_rcvd = 0;
  int space = SpaceFor (nelts);
  int x, y, loopbound;

  for (y = 0; y < nsegs_here; y += RCV_EVERY) {
    loopbound = min (nsegs_here, y + RCV_EVERY);
    for (x = y; x < loopbound; x++) {
      /* if ... scratch[src_start[x]] <- dst_start[x] */
      if ((dst_length[x] != 0) && (src_length[x] != 0)
	  && (src_start[x] != nelts)) {
	int proc = src_start[x] / space;
	int offset = src_start[x] - (proc * space);

	if (proc == Self) {
	  scratch[offset] = dst_start[x];
	  if (dst_start[x] > max_value)
	    max_value = dst_start[x];
	} else {
	  int posn = Send_ctr[proc]++;
	  intx *buf = (intx *) Usr_send[proc];

	  buf[posn].value = dst_start[x];
	  buf[posn].index = offset;
	}
      }
    }
    n_rcvd += _recv_intx_maxval (scratch, tag, &max_value);
    for (x = 0; x < NumProcs; x++) {
      if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	_send_buffer (x, tag, (Send_ctr[x] * sizeof (intx)));
	n_sent++;
      }
    }
  }
  _finish_intx_maxval (scratch, &max_value, tag, n_sent, n_rcvd);

  /* max_value contains start position of last destination segment on
   * each processor; do exclusive max_scan to give the position of last
   * destination segment on the _previous_ processor. */
  _exclusive_max_scan (&max_value);
  return max_value;
}


/* -------------------- Segmented Flag Permute ----------------------- */


void fpm_pez (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v, vec_p
       src_segd_v, int src_nelts, int src_nsegs, vec_p dst_segd_v, int
	       dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  int *dst, *src;
  cvl_bool *flag;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *src_segment;
  int current_seg, base, src_nelts_here;

  LOG_START (fpm_pez, src_nelts);
  if ((src_nelts != 0) && (dst_nelts != 0)) {
    if (src_nsegs == 1) {
      fpm_puz (dst_v, src_v, index_v, flag_v, src_nelts, dst_nelts,
	       scratch_v);
    } else {
      scratch = (int *) (Mem + scratch_v);
      src_segd = (int *) (Mem + src_segd_v);
      dst_segd = (int *) (Mem + dst_segd_v);
      src_start = SegdStart (src_segd, src_nelts, src_nsegs);
      src_length = SegdLength (src_segd, src_nelts, src_nsegs);
      dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
      dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

      base = _init_seg_starts (scratch, src_start, dst_start, src_length,
		       dst_length, src_nsegs, src_nelts, fpm_pez_tag);

      /* Now we can do the actual permute, using base as the initial
       * base address of the segment to send to, and resetting it to
       * the current element of scratch whenever we hit a segment
       * start. */
      src_nelts_here = _num_here (src_nelts);
      dst = (int *) (Mem + dst_v);
      src = (int *) (Mem + src_v);
      index = (int *) (Mem + index_v);
      flag = (cvl_bool *) (Mem + flag_v);
      src_segment = SegdSegment (src_segd, src_nelts, src_nsegs);
      current_seg = *(SegdBefore (src_segd, src_nelts, src_nsegs));
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (dst_nelts);
	int x, y, loopbound;

	for (y = 0; y < src_nelts_here; y += RCV_EVERY) {
	  loopbound = min (src_nelts_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    if (src_segment[x] != current_seg) {
	      current_seg = src_segment[x];
	      base = scratch[x];
	    }
	    /* if (flag[x]) dst[base+index[x]] <- src[x] */
	    if (flag[x]) {
	      int proc = (base + index[x]) / space;
	      int offset = (base + index[x]) - (proc * space);

	      if (proc == Self) {
		dst[offset] = src[x];
	      } else {
		int posn = Send_ctr[proc]++;
		intx *buf = (intx *) Usr_send[proc];

		buf[posn].value = src[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_simple (dst, fpm_pez_tag, _unpack_simple_intx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	      _send_buffer (x, fpm_pez_tag, (Send_ctr[x] * sizeof (intx)));
	      n_sent++;
	    }
	  }
	}
	_finish_simple (dst, fpm_pez_tag, n_sent, n_rcvd, sizeof (intx),
			_unpack_simple_intx);
      }
    }
  }
  LOG_STOP (fpm_pez, src_nelts);
}

unsigned int fpm_pez_inplace (void)
{
  return INPLACE_NONE;
}

int fpm_pez_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (src_nelts);
  }
  return result;
}
void fpm_ped (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v, vec_p
       src_segd_v, int src_nelts, int src_nsegs, vec_p dst_segd_v, int
	       dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  double *dst, *src;
  cvl_bool *flag;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *src_segment;
  int current_seg, base, src_nelts_here;

  LOG_START (fpm_ped, src_nelts);
  if ((src_nelts != 0) && (dst_nelts != 0)) {
    if (src_nsegs == 1) {
      fpm_pud (dst_v, src_v, index_v, flag_v, src_nelts, dst_nelts,
	       scratch_v);
    } else {
      scratch = (int *) (Mem + scratch_v);
      src_segd = (int *) (Mem + src_segd_v);
      dst_segd = (int *) (Mem + dst_segd_v);
      src_start = SegdStart (src_segd, src_nelts, src_nsegs);
      src_length = SegdLength (src_segd, src_nelts, src_nsegs);
      dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
      dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

      base = _init_seg_starts (scratch, src_start, dst_start, src_length,
		       dst_length, src_nsegs, src_nelts, fpm_ped_tag);

      /* Now we can do the actual permute, using base as the initial
       * base address of the segment to send to, and resetting it to
       * the current element of scratch whenever we hit a segment
       * start. */
      src_nelts_here = _num_here (src_nelts);
      dst = (double *) (Mem + dst_v);
      src = (double *) (Mem + src_v);
      index = (int *) (Mem + index_v);
      flag = (cvl_bool *) (Mem + flag_v);
      src_segment = SegdSegment (src_segd, src_nelts, src_nsegs);
      current_seg = *(SegdBefore (src_segd, src_nelts, src_nsegs));
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (dst_nelts);
	int x, y, loopbound;

	for (y = 0; y < src_nelts_here; y += RCV_EVERY) {
	  loopbound = min (src_nelts_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    if (src_segment[x] != current_seg) {
	      current_seg = src_segment[x];
	      base = scratch[x];
	    }
	    /* if (flag[x]) dst[base+index[x]] <- src[x] */
	    if (flag[x]) {
	      int proc = (base + index[x]) / space;
	      int offset = (base + index[x]) - (proc * space);

	      if (proc == Self) {
		dst[offset] = src[x];
	      } else {
		int posn = Send_ctr[proc]++;
		doublex *buf = (doublex *) Usr_send[proc];

		buf[posn].value = src[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_simple (dst, fpm_ped_tag, _unpack_simple_doublex);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	      _send_buffer (x, fpm_ped_tag, (Send_ctr[x] * sizeof (doublex)));
	      n_sent++;
	    }
	  }
	}
	_finish_simple (dst, fpm_ped_tag, n_sent, n_rcvd, sizeof (doublex),
			_unpack_simple_doublex);
      }
    }
  }
  LOG_STOP (fpm_ped, src_nelts);
}

unsigned int fpm_ped_inplace (void)
{
  return INPLACE_NONE;
}

int fpm_ped_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (src_nelts);
  }
  return result;
}
void fpm_peb (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v, vec_p
       src_segd_v, int src_nelts, int src_nsegs, vec_p dst_segd_v, int
	       dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  cvl_bool *flag;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *src_segment;
  int current_seg, base, src_nelts_here;

  LOG_START (fpm_peb, src_nelts);
  if ((src_nelts != 0) && (dst_nelts != 0)) {
    if (src_nsegs == 1) {
      fpm_pub (dst_v, src_v, index_v, flag_v, src_nelts, dst_nelts,
	       scratch_v);
    } else {
      scratch = (int *) (Mem + scratch_v);
      src_segd = (int *) (Mem + src_segd_v);
      dst_segd = (int *) (Mem + dst_segd_v);
      src_start = SegdStart (src_segd, src_nelts, src_nsegs);
      src_length = SegdLength (src_segd, src_nelts, src_nsegs);
      dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
      dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

      base = _init_seg_starts (scratch, src_start, dst_start, src_length,
		       dst_length, src_nsegs, src_nelts, fpm_peb_tag);

      /* Now we can do the actual permute, using base as the initial
       * base address of the segment to send to, and resetting it to
       * the current element of scratch whenever we hit a segment
       * start. */
      src_nelts_here = _num_here (src_nelts);
      dst = (cvl_bool *) (Mem + dst_v);
      src = (cvl_bool *) (Mem + src_v);
      index = (int *) (Mem + index_v);
      flag = (cvl_bool *) (Mem + flag_v);
      src_segment = SegdSegment (src_segd, src_nelts, src_nsegs);
      current_seg = *(SegdBefore (src_segd, src_nelts, src_nsegs));
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (dst_nelts);
	int x, y, loopbound;

	for (y = 0; y < src_nelts_here; y += RCV_EVERY) {
	  loopbound = min (src_nelts_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    if (src_segment[x] != current_seg) {
	      current_seg = src_segment[x];
	      base = scratch[x];
	    }
	    /* if (flag[x]) dst[base+index[x]] <- src[x] */
	    if (flag[x]) {
	      int proc = (base + index[x]) / space;
	      int offset = (base + index[x]) - (proc * space);

	      if (proc == Self) {
		dst[offset] = src[x];
	      } else {
		int posn = Send_ctr[proc]++;
		cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

		buf[posn].value = src[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_simple (dst, fpm_peb_tag, _unpack_simple_cvl_boolx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	      _send_buffer (x, fpm_peb_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
	      n_sent++;
	    }
	  }
	}
	_finish_simple (dst, fpm_peb_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
			_unpack_simple_cvl_boolx);
      }
    }
  }
  LOG_STOP (fpm_peb, src_nelts);
}

unsigned int fpm_peb_inplace (void)
{
  return INPLACE_NONE;
}

int fpm_peb_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (src_nelts);
  }
  return result;
}


/* ------------------- Segmented Default Permute --------------------- */


void dpe_pez (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p default_v, vec_p
       src_segd_v, int src_nelts, int src_nsegs, vec_p dst_segd_v, int
	       dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  int *dst, *def, *src;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *src_segment;
  int current_seg, base, src_nelts_here, dst_nelts_here, i;

  LOG_START (dpe_pez, src_nelts);
  if (dst_nelts != 0) {
    if (src_nsegs == 1) {
      dpe_puz (dst_v, src_v, index_v, default_v, src_nelts, dst_nelts,
	       scratch_v);
    } else {
      dst_nelts_here = _num_here (dst_nelts);
      dst = (int *) (Mem + dst_v);
      def = (int *) (Mem + default_v);

      for (i = 0; i < dst_nelts_here; i++) {
	dst[i] = def[i];
      }

      if (src_nelts != 0) {
	scratch = (int *) (Mem + scratch_v);
	src_segd = (int *) (Mem + src_segd_v);
	dst_segd = (int *) (Mem + dst_segd_v);
	src_start = SegdStart (src_segd, src_nelts, src_nsegs);
	src_length = SegdLength (src_segd, src_nelts, src_nsegs);
	dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
	dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

	base = _init_seg_starts (scratch, src_start, dst_start, src_length,
				 dst_length, src_nsegs, src_nelts,
				 dpe_pez_tag);

	src_nelts_here = _num_here (src_nelts);
	src = (int *) (Mem + src_v);
	index = (int *) (Mem + index_v);
	src_segment = SegdSegment (src_segd, src_nelts, src_nsegs);
	current_seg = *(SegdBefore (src_segd, src_nelts, src_nsegs));
	{
	  int n_sent = 0, n_rcvd = 0;
	  int space = SpaceFor (dst_nelts);
	  int x, y, loopbound;

	  for (y = 0; y < src_nelts_here; y += RCV_EVERY) {
	    loopbound = min (src_nelts_here, y + RCV_EVERY);
	    for (x = y; x < loopbound; x++) {
	      if (src_segment[x] != current_seg) {
		current_seg = src_segment[x];
		base = scratch[x];
	      }
	      /* dst[base+index[x]] <- src[x] */
	      {
		int proc = (base + index[x]) / space;
		int offset = (base + index[x]) - (proc * space);

		if (proc == Self) {
		  dst[offset] = src[x];
		} else {
		  int posn = Send_ctr[proc]++;
		  intx *buf = (intx *) Usr_send[proc];

		  buf[posn].value = src[x];
		  buf[posn].index = offset;
		}
	      }
	    }
	    n_rcvd += _recv_simple (dst, dpe_pez_tag, _unpack_simple_intx);
	    for (x = 0; x < NumProcs; x++) {
	      if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
		_send_buffer (x, dpe_pez_tag, (Send_ctr[x] * sizeof (intx)));
		n_sent++;
	      }
	    }
	  }
	  _finish_simple (dst, dpe_pez_tag, n_sent, n_rcvd, sizeof (intx),
			  _unpack_simple_intx);
	}
      }
    }
  }
  LOG_STOP (dpe_pez, src_nelts);
}

unsigned int dpe_pez_inplace (void)
{
  return INPLACE_NONE;
}

int dpe_pez_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (src_nelts);
  }
  return result;
}
void dpe_ped (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p default_v, vec_p
       src_segd_v, int src_nelts, int src_nsegs, vec_p dst_segd_v, int
	       dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  double *dst, *def, *src;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *src_segment;
  int current_seg, base, src_nelts_here, dst_nelts_here, i;

  LOG_START (dpe_ped, src_nelts);
  if (dst_nelts != 0) {
    if (src_nsegs == 1) {
      dpe_pud (dst_v, src_v, index_v, default_v, src_nelts, dst_nelts,
	       scratch_v);
    } else {
      dst_nelts_here = _num_here (dst_nelts);
      dst = (double *) (Mem + dst_v);
      def = (double *) (Mem + default_v);

      for (i = 0; i < dst_nelts_here; i++) {
	dst[i] = def[i];
      }

      if (src_nelts != 0) {
	scratch = (int *) (Mem + scratch_v);
	src_segd = (int *) (Mem + src_segd_v);
	dst_segd = (int *) (Mem + dst_segd_v);
	src_start = SegdStart (src_segd, src_nelts, src_nsegs);
	src_length = SegdLength (src_segd, src_nelts, src_nsegs);
	dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
	dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

	base = _init_seg_starts (scratch, src_start, dst_start, src_length,
				 dst_length, src_nsegs, src_nelts,
				 dpe_ped_tag);

	src_nelts_here = _num_here (src_nelts);
	src = (double *) (Mem + src_v);
	index = (int *) (Mem + index_v);
	src_segment = SegdSegment (src_segd, src_nelts, src_nsegs);
	current_seg = *(SegdBefore (src_segd, src_nelts, src_nsegs));
	{
	  int n_sent = 0, n_rcvd = 0;
	  int space = SpaceFor (dst_nelts);
	  int x, y, loopbound;

	  for (y = 0; y < src_nelts_here; y += RCV_EVERY) {
	    loopbound = min (src_nelts_here, y + RCV_EVERY);
	    for (x = y; x < loopbound; x++) {
	      if (src_segment[x] != current_seg) {
		current_seg = src_segment[x];
		base = scratch[x];
	      }
	      /* dst[base+index[x]] <- src[x] */
	      {
		int proc = (base + index[x]) / space;
		int offset = (base + index[x]) - (proc * space);

		if (proc == Self) {
		  dst[offset] = src[x];
		} else {
		  int posn = Send_ctr[proc]++;
		  doublex *buf = (doublex *) Usr_send[proc];

		  buf[posn].value = src[x];
		  buf[posn].index = offset;
		}
	      }
	    }
	    n_rcvd += _recv_simple (dst, dpe_ped_tag, _unpack_simple_doublex);
	    for (x = 0; x < NumProcs; x++) {
	      if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
		_send_buffer (x, dpe_ped_tag, (Send_ctr[x] * sizeof (doublex)));
		n_sent++;
	      }
	    }
	  }
	  _finish_simple (dst, dpe_ped_tag, n_sent, n_rcvd, sizeof (doublex),
			  _unpack_simple_doublex);
	}
      }
    }
  }
  LOG_STOP (dpe_ped, src_nelts);
}

unsigned int dpe_ped_inplace (void)
{
  return INPLACE_NONE;
}

int dpe_ped_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (src_nelts);
  }
  return result;
}
void dpe_peb (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p default_v, vec_p
       src_segd_v, int src_nelts, int src_nsegs, vec_p dst_segd_v, int
	       dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  cvl_bool *dst, *def, *src;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *src_segment;
  int current_seg, base, src_nelts_here, dst_nelts_here, i;

  LOG_START (dpe_peb, src_nelts);
  if (dst_nelts != 0) {
    if (src_nsegs == 1) {
      dpe_pub (dst_v, src_v, index_v, default_v, src_nelts, dst_nelts,
	       scratch_v);
    } else {
      dst_nelts_here = _num_here (dst_nelts);
      dst = (cvl_bool *) (Mem + dst_v);
      def = (cvl_bool *) (Mem + default_v);

      for (i = 0; i < dst_nelts_here; i++) {
	dst[i] = def[i];
      }

      if (src_nelts != 0) {
	scratch = (int *) (Mem + scratch_v);
	src_segd = (int *) (Mem + src_segd_v);
	dst_segd = (int *) (Mem + dst_segd_v);
	src_start = SegdStart (src_segd, src_nelts, src_nsegs);
	src_length = SegdLength (src_segd, src_nelts, src_nsegs);
	dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
	dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

	base = _init_seg_starts (scratch, src_start, dst_start, src_length,
				 dst_length, src_nsegs, src_nelts,
				 dpe_peb_tag);

	src_nelts_here = _num_here (src_nelts);
	src = (cvl_bool *) (Mem + src_v);
	index = (int *) (Mem + index_v);
	src_segment = SegdSegment (src_segd, src_nelts, src_nsegs);
	current_seg = *(SegdBefore (src_segd, src_nelts, src_nsegs));
	{
	  int n_sent = 0, n_rcvd = 0;
	  int space = SpaceFor (dst_nelts);
	  int x, y, loopbound;

	  for (y = 0; y < src_nelts_here; y += RCV_EVERY) {
	    loopbound = min (src_nelts_here, y + RCV_EVERY);
	    for (x = y; x < loopbound; x++) {
	      if (src_segment[x] != current_seg) {
		current_seg = src_segment[x];
		base = scratch[x];
	      }
	      /* dst[base+index[x]] <- src[x] */
	      {
		int proc = (base + index[x]) / space;
		int offset = (base + index[x]) - (proc * space);

		if (proc == Self) {
		  dst[offset] = src[x];
		} else {
		  int posn = Send_ctr[proc]++;
		  cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

		  buf[posn].value = src[x];
		  buf[posn].index = offset;
		}
	      }
	    }
	    n_rcvd += _recv_simple (dst, dpe_peb_tag, _unpack_simple_cvl_boolx);
	    for (x = 0; x < NumProcs; x++) {
	      if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
		_send_buffer (x, dpe_peb_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
		n_sent++;
	      }
	    }
	  }
	  _finish_simple (dst, dpe_peb_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
			  _unpack_simple_cvl_boolx);
	}
      }
    }
  }
  LOG_STOP (dpe_peb, src_nelts);
}

unsigned int dpe_peb_inplace (void)
{
  return INPLACE_NONE;
}

int dpe_peb_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (src_nelts);
  }
  return result;
}


/* ------------------- Segmented Backwards Permute ------------------- */


void bck_pez (vec_p dst_v, vec_p src_v, vec_p index_v,
	       vec_p src_segd_v, int src_nelts, int src_nsegs,
	       vec_p dst_segd_v, int dst_nelts, int dst_nsegs,
	       vec_p scratch_v)
{
  int *dst, *src;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *dst_segment;
  int dst_nelts_here, current_seg, base;

  LOG_START (bck_pez, src_nelts);
  if ((dst_nelts != 0) && (src_nelts != 0)) {
    if (src_nsegs == 1) {
      bck_puz (dst_v, src_v, index_v, src_nelts, dst_nelts, scratch_v);
    } else {
      scratch = (int *) (Mem + scratch_v);
      src_segd = (int *) (Mem + src_segd_v);
      dst_segd = (int *) (Mem + dst_segd_v);
      src_start = SegdStart (src_segd, src_nelts, src_nsegs);
      src_length = SegdLength (src_segd, src_nelts, src_nsegs);
      dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
      dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

      /* Send start-of-seg positions of _src_ vector to start-of-seg
       * positions of scratch vector, which is same size as _dst_
       * vector. */
      base = _init_seg_starts (scratch, dst_start, src_start, dst_length,
		       src_length, src_nsegs, dst_nelts, bck_pez_tag);

      dst_nelts_here = _num_here (dst_nelts);
      dst = (int *) (Mem + dst_v);
      src = (int *) (Mem + src_v);
      dst_segment = SegdSegment (dst_segd, dst_nelts, dst_nsegs);
      current_seg = *(SegdBefore (dst_segd, dst_nelts, dst_nsegs));
      index = (int *) (Mem + index_v);

      /* Now we can do the actual permute, using base as the initial
       * base address of the segment to get from, and resetting it to
       * the current element of scratch whenever we hit a segment
       * start. */
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (src_nelts);
	int x, y, loopbound;

	for (y = 0; y < dst_nelts_here; y += RCV_EVERY) {
	  loopbound = min (dst_nelts_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    if (dst_segment[x] != current_seg) {
	      current_seg = dst_segment[x];
	      base = scratch[x];
	    }
	    /* dst[x] <- src[base+index[x]] */
	    {
	      int proc = (base + index[x]) / space;
	      int offset = (base + index[x]) - (proc * space);

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
	  n_rcvd += _recv_fetch (dst, src, bck_pez_tag, _unpack_fetch_intx,
				 _unpack_simple_intx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	      _send_buffer (x, bck_pez_tag, (Send_ctr[x] * sizeof (fetch)));
	      n_sent++;
	    }
	  }
	}
	_finish_fetch (dst, src, bck_pez_tag, n_sent, n_rcvd,
		       _unpack_fetch_intx, _unpack_simple_intx);
      }
    }
  }
  LOG_STOP (bck_pez, src_nelts);
}

unsigned int bck_pez_inplace (void)
{
  return INPLACE_NONE;
}

int bck_pez_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (dst_nelts);
  }
  return result;
}
void bck_ped (vec_p dst_v, vec_p src_v, vec_p index_v,
	       vec_p src_segd_v, int src_nelts, int src_nsegs,
	       vec_p dst_segd_v, int dst_nelts, int dst_nsegs,
	       vec_p scratch_v)
{
  double *dst, *src;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *dst_segment;
  int dst_nelts_here, current_seg, base;

  LOG_START (bck_ped, src_nelts);
  if ((dst_nelts != 0) && (src_nelts != 0)) {
    if (src_nsegs == 1) {
      bck_pud (dst_v, src_v, index_v, src_nelts, dst_nelts, scratch_v);
    } else {
      scratch = (int *) (Mem + scratch_v);
      src_segd = (int *) (Mem + src_segd_v);
      dst_segd = (int *) (Mem + dst_segd_v);
      src_start = SegdStart (src_segd, src_nelts, src_nsegs);
      src_length = SegdLength (src_segd, src_nelts, src_nsegs);
      dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
      dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

      /* Send start-of-seg positions of _src_ vector to start-of-seg
       * positions of scratch vector, which is same size as _dst_
       * vector. */
      base = _init_seg_starts (scratch, dst_start, src_start, dst_length,
		       src_length, src_nsegs, dst_nelts, bck_ped_tag);

      dst_nelts_here = _num_here (dst_nelts);
      dst = (double *) (Mem + dst_v);
      src = (double *) (Mem + src_v);
      dst_segment = SegdSegment (dst_segd, dst_nelts, dst_nsegs);
      current_seg = *(SegdBefore (dst_segd, dst_nelts, dst_nsegs));
      index = (int *) (Mem + index_v);

      /* Now we can do the actual permute, using base as the initial
       * base address of the segment to get from, and resetting it to
       * the current element of scratch whenever we hit a segment
       * start. */
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (src_nelts);
	int x, y, loopbound;

	for (y = 0; y < dst_nelts_here; y += RCV_EVERY) {
	  loopbound = min (dst_nelts_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    if (dst_segment[x] != current_seg) {
	      current_seg = dst_segment[x];
	      base = scratch[x];
	    }
	    /* dst[x] <- src[base+index[x]] */
	    {
	      int proc = (base + index[x]) / space;
	      int offset = (base + index[x]) - (proc * space);

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
	  n_rcvd += _recv_fetch (dst, src, bck_ped_tag, _unpack_fetch_doublex,
				 _unpack_simple_doublex);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	      _send_buffer (x, bck_ped_tag, (Send_ctr[x] * sizeof (fetch)));
	      n_sent++;
	    }
	  }
	}
	_finish_fetch (dst, src, bck_ped_tag, n_sent, n_rcvd,
		       _unpack_fetch_doublex, _unpack_simple_doublex);
      }
    }
  }
  LOG_STOP (bck_ped, src_nelts);
}

unsigned int bck_ped_inplace (void)
{
  return INPLACE_NONE;
}

int bck_ped_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (dst_nelts);
  }
  return result;
}
void bck_peb (vec_p dst_v, vec_p src_v, vec_p index_v,
	       vec_p src_segd_v, int src_nelts, int src_nsegs,
	       vec_p dst_segd_v, int dst_nelts, int dst_nsegs,
	       vec_p scratch_v)
{
  cvl_bool *dst, *src;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *dst_segment;
  int dst_nelts_here, current_seg, base;

  LOG_START (bck_peb, src_nelts);
  if ((dst_nelts != 0) && (src_nelts != 0)) {
    if (src_nsegs == 1) {
      bck_pub (dst_v, src_v, index_v, src_nelts, dst_nelts, scratch_v);
    } else {
      scratch = (int *) (Mem + scratch_v);
      src_segd = (int *) (Mem + src_segd_v);
      dst_segd = (int *) (Mem + dst_segd_v);
      src_start = SegdStart (src_segd, src_nelts, src_nsegs);
      src_length = SegdLength (src_segd, src_nelts, src_nsegs);
      dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
      dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

      /* Send start-of-seg positions of _src_ vector to start-of-seg
       * positions of scratch vector, which is same size as _dst_
       * vector. */
      base = _init_seg_starts (scratch, dst_start, src_start, dst_length,
		       src_length, src_nsegs, dst_nelts, bck_peb_tag);

      dst_nelts_here = _num_here (dst_nelts);
      dst = (cvl_bool *) (Mem + dst_v);
      src = (cvl_bool *) (Mem + src_v);
      dst_segment = SegdSegment (dst_segd, dst_nelts, dst_nsegs);
      current_seg = *(SegdBefore (dst_segd, dst_nelts, dst_nsegs));
      index = (int *) (Mem + index_v);

      /* Now we can do the actual permute, using base as the initial
       * base address of the segment to get from, and resetting it to
       * the current element of scratch whenever we hit a segment
       * start. */
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (src_nelts);
	int x, y, loopbound;

	for (y = 0; y < dst_nelts_here; y += RCV_EVERY) {
	  loopbound = min (dst_nelts_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    if (dst_segment[x] != current_seg) {
	      current_seg = dst_segment[x];
	      base = scratch[x];
	    }
	    /* dst[x] <- src[base+index[x]] */
	    {
	      int proc = (base + index[x]) / space;
	      int offset = (base + index[x]) - (proc * space);

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
	  n_rcvd += _recv_fetch (dst, src, bck_peb_tag, _unpack_fetch_cvl_boolx,
				 _unpack_simple_cvl_boolx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	      _send_buffer (x, bck_peb_tag, (Send_ctr[x] * sizeof (fetch)));
	      n_sent++;
	    }
	  }
	}
	_finish_fetch (dst, src, bck_peb_tag, n_sent, n_rcvd,
		   _unpack_fetch_cvl_boolx, _unpack_simple_cvl_boolx);
      }
    }
  }
  LOG_STOP (bck_peb, src_nelts);
}

unsigned int bck_peb_inplace (void)
{
  return INPLACE_NONE;
}

int bck_peb_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (dst_nelts);
  }
  return result;
}


/* ---------------- Segmented Backwards Flag Permute ----------------- */


void bfp_pez (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       vec_p src_segd_v, int src_nelts, int src_nsegs, vec_p
	    dst_segd_v, int dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  int *dst, *src;
  cvl_bool *flag;
  int *index, *scratch, *dst_segd, *src_segd;
  int *dst_length, *src_length, *dst_start, *src_start, *dst_segment;
  int current_seg, base, dst_nelts_here;

  LOG_START (bfp_pez, src_nelts);
  if ((dst_nelts != 0) && (src_nelts != 0)) {
    if (src_nsegs == 1) {
      bfp_puz (dst_v, src_v, index_v, flag_v, src_nelts, dst_nelts, scratch_v);
    } else {
      scratch = (int *) (Mem + scratch_v);
      src_segd = (int *) (Mem + src_segd_v);
      dst_segd = (int *) (Mem + dst_segd_v);
      dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
      dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);
      src_start = SegdStart (src_segd, src_nelts, src_nsegs);
      src_length = SegdLength (src_segd, src_nelts, src_nsegs);

      /* Send start-of-seg positions of _src_ vector to start-of-seg
       * positions of scratch vector, which is same size as _dst_
       * vector. */
      base = _init_seg_starts (scratch, dst_start, src_start, dst_length,
		       src_length, src_nsegs, dst_nelts, bfp_pez_tag);

      /* Now we can do the actual permute, using base as the initial
       * base address of the segment to get from, and resetting it to
       * the current element of scratch whenever we hit a segment
       * start. */
      dst_nelts_here = _num_here (dst_nelts);
      dst = (int *) (Mem + dst_v);
      src = (int *) (Mem + src_v);
      index = (int *) (Mem + index_v);
      flag = (cvl_bool *) (Mem + flag_v);
      dst_segment = SegdSegment (dst_segd, dst_nelts, dst_nsegs);
      current_seg = *(SegdBefore (dst_segd, dst_nelts, dst_nsegs));
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (src_nelts);
	int x, y, loopbound;

	for (y = 0; y < dst_nelts_here; y += RCV_EVERY) {
	  loopbound = min (dst_nelts_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    if (dst_segment[x] != current_seg) {
	      current_seg = dst_segment[x];
	      base = scratch[x];
	    }
	    /* if (flag[x] dst[x] <- src[base+index[x]] */
	    if (flag[x]) {
	      int proc = (base + index[x]) / space;
	      int offset = (base + index[x]) - (proc * space);

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
	  n_rcvd += _recv_fetch (dst, src, bfp_pez_tag, _unpack_fetch_intx,
				 _unpack_simple_intx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	      _send_buffer (x, bfp_pez_tag, (Send_ctr[x] * sizeof (fetch)));
	      n_sent++;
	    }
	  }
	}
	_finish_fetch (dst, src, bfp_pez_tag, n_sent, n_rcvd,
		       _unpack_fetch_intx, _unpack_simple_intx);
      }
    }
  }
  LOG_STOP (bfp_pez, src_nelts);
}

unsigned int bfp_pez_inplace (void)
{
  return INPLACE_NONE;
}

int bfp_pez_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (dst_nelts);
  }
  return result;
}
void bfp_ped (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       vec_p src_segd_v, int src_nelts, int src_nsegs, vec_p
	    dst_segd_v, int dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  double *dst, *src;
  cvl_bool *flag;
  int *index, *scratch, *dst_segd, *src_segd;
  int *dst_length, *src_length, *dst_start, *src_start, *dst_segment;
  int current_seg, base, dst_nelts_here;

  LOG_START (bfp_ped, src_nelts);
  if ((dst_nelts != 0) && (src_nelts != 0)) {
    if (src_nsegs == 1) {
      bfp_pud (dst_v, src_v, index_v, flag_v, src_nelts, dst_nelts, scratch_v);
    } else {
      scratch = (int *) (Mem + scratch_v);
      src_segd = (int *) (Mem + src_segd_v);
      dst_segd = (int *) (Mem + dst_segd_v);
      dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
      dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);
      src_start = SegdStart (src_segd, src_nelts, src_nsegs);
      src_length = SegdLength (src_segd, src_nelts, src_nsegs);

      /* Send start-of-seg positions of _src_ vector to start-of-seg
       * positions of scratch vector, which is same size as _dst_
       * vector. */
      base = _init_seg_starts (scratch, dst_start, src_start, dst_length,
		       src_length, src_nsegs, dst_nelts, bfp_ped_tag);

      /* Now we can do the actual permute, using base as the initial
       * base address of the segment to get from, and resetting it to
       * the current element of scratch whenever we hit a segment
       * start. */
      dst_nelts_here = _num_here (dst_nelts);
      dst = (double *) (Mem + dst_v);
      src = (double *) (Mem + src_v);
      index = (int *) (Mem + index_v);
      flag = (cvl_bool *) (Mem + flag_v);
      dst_segment = SegdSegment (dst_segd, dst_nelts, dst_nsegs);
      current_seg = *(SegdBefore (dst_segd, dst_nelts, dst_nsegs));
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (src_nelts);
	int x, y, loopbound;

	for (y = 0; y < dst_nelts_here; y += RCV_EVERY) {
	  loopbound = min (dst_nelts_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    if (dst_segment[x] != current_seg) {
	      current_seg = dst_segment[x];
	      base = scratch[x];
	    }
	    /* if (flag[x] dst[x] <- src[base+index[x]] */
	    if (flag[x]) {
	      int proc = (base + index[x]) / space;
	      int offset = (base + index[x]) - (proc * space);

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
	  n_rcvd += _recv_fetch (dst, src, bfp_ped_tag, _unpack_fetch_doublex,
				 _unpack_simple_doublex);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
	      _send_buffer (x, bfp_ped_tag, (Send_ctr[x] * sizeof (fetch)));
	      n_sent++;
	    }
	  }
	}
	_finish_fetch (dst, src, bfp_ped_tag, n_sent, n_rcvd,
		       _unpack_fetch_doublex, _unpack_simple_doublex);
      }
    }
  }
  LOG_STOP (bfp_ped, src_nelts);
}

unsigned int bfp_ped_inplace (void)
{
  return INPLACE_NONE;
}

int bfp_ped_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (dst_nelts);
  }
  return result;
}
void bfp_peb (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       vec_p src_segd_v, int src_nelts, int src_nsegs, vec_p
	    dst_segd_v, int dst_nelts, int dst_nsegs, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  cvl_bool *flag;
  int *index, *scratch, *dst_segd, *src_segd;
  int *dst_length, *src_length, *dst_start, *src_start, *dst_segment;
  int current_seg, base, dst_nelts_here;

  LOG_START (bfp_peb, src_nelts);
  if ((dst_nelts != 0) && (src_nelts != 0)) {
    if (src_nsegs == 1) {
      bfp_pub (dst_v, src_v, index_v, flag_v, src_nelts, dst_nelts, scratch_v);
    } else {
      scratch = (int *) (Mem + scratch_v);
      src_segd = (int *) (Mem + src_segd_v);
      dst_segd = (int *) (Mem + dst_segd_v);
      dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
      dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);
      src_start = SegdStart (src_segd, src_nelts, src_nsegs);
      src_length = SegdLength (src_segd, src_nelts, src_nsegs);

      /* Send start-of-seg positions of _src_ vector to start-of-seg
       * positions of scratch vector, which is same size as _dst_
       * vector. */
      base = _init_seg_starts (scratch, dst_start, src_start, dst_length,
		       src_length, src_nsegs, dst_nelts, bfp_peb_tag);

      /* Now we can do the actual permute, using base as the initial
       * base address of the segment to get from, and resetting it to
       * the current element of scratch whenever we hit a segment
       * start. */
      dst_nelts_here = _num_here (dst_nelts);
      dst = (cvl_bool *) (Mem + dst_v);
      src = (cvl_bool *) (Mem + src_v);
      index = (int *) (Mem + index_v);
      flag = (cvl_bool *) (Mem + flag_v);
      dst_segment = SegdSegment (dst_segd, dst_nelts, dst_nsegs);
      current_seg = *(SegdBefore (dst_segd, dst_nelts, dst_nsegs));
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (src_nelts);
	int x, y, loopbound;

	for (y = 0; y < dst_nelts_here; y += RCV_EVERY) {
	  loopbound = min (dst_nelts_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    if (dst_segment[x] != current_seg) {
	      current_seg = dst_segment[x];
	      base = scratch[x];
	    }
	    /* if (flag[x] dst[x] <- src[base+index[x]] */
	    if (flag[x]) {
	      int proc = (base + index[x]) / space;
	      int offset = (base + index[x]) - (proc * space);

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
	  n_rcvd += _recv_fetch (dst, src, bfp_peb_tag, _unpack_fetch_cvl_boolx,
				 _unpack_simple_cvl_boolx);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
	      _send_buffer (x, bfp_peb_tag, (Send_ctr[x] * sizeof (fetch)));
	      n_sent++;
	    }
	  }
	}
	_finish_fetch (dst, src, bfp_peb_tag, n_sent, n_rcvd,
		   _unpack_fetch_cvl_boolx, _unpack_simple_cvl_boolx);
      }
    }
  }
  LOG_STOP (bfp_peb, src_nelts);
}

unsigned int bfp_peb_inplace (void)
{
  return INPLACE_NONE;
}

int bfp_peb_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (dst_nelts);
  }
  return result;
}


/* ----------------- Segmented Default Flag Permute ------------------ */


void dfp_pez (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       vec_p default_v, vec_p src_segd_v, int src_nelts,
        int src_nsegs, vec_p dst_segd_v, int dst_nelts, int dst_nsegs,
	       vec_p scratch_v)
{
  int *dst, *src, *def;
  cvl_bool *flag;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *src_segment;
  int current_seg, base, dst_nelts_here, src_nelts_here, i;

  LOG_START (dfp_pez, src_nelts);
  if (dst_nelts != 0) {
    if (src_nsegs == 1) {
      dfp_puz (dst_v, src_v, index_v, flag_v, default_v, src_nelts, dst_nelts,
	       scratch_v);
    } else {
      dst_nelts_here = _num_here (dst_nelts);
      dst = (int *) (Mem + dst_v);
      def = (int *) (Mem + default_v);

      for (i = 0; i < dst_nelts_here; i++) {
	dst[i] = def[i];
      }

      if (src_nelts != 0) {
	scratch = (int *) (Mem + scratch_v);
	src_segd = (int *) (Mem + src_segd_v);
	dst_segd = (int *) (Mem + dst_segd_v);
	src_start = SegdStart (src_segd, src_nelts, src_nsegs);
	src_length = SegdStart (src_segd, src_nelts, src_nsegs);
	dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
	dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

	base = _init_seg_starts (scratch, src_start, dst_start, src_length,
				 dst_length, src_nsegs, src_nelts,
				 dfp_pez_tag);

	src_nelts_here = _num_here (src_nelts);
	src = (int *) (Mem + src_v);
	index = (int *) (Mem + index_v);
	flag = (cvl_bool *) (Mem + flag_v);
	src_segment = SegdSegment (src_segd, src_nelts, src_nsegs);
	current_seg = *(SegdBefore (src_segd, src_nelts, src_nsegs));

	/* Now we can do the actual permute, using base as the initial
	 * base address of the segment to send to, and resetting it to
	 * the current element of scratch whenever we hit a segment
	 * start. */
	{
	  int n_sent = 0, n_rcvd = 0;
	  int space = SpaceFor (dst_nelts);
	  int x, y, loopbound;

	  for (y = 0; y < src_nelts_here; y += RCV_EVERY) {
	    loopbound = min (src_nelts_here, y + RCV_EVERY);
	    for (x = y; x < loopbound; x++) {
	      if (src_segment[x] != current_seg) {
		current_seg = src_segment[x];
		base = scratch[x];
	      }
	      /* if (flag[x] dst[base+index[x]] <- src[x] */
	      if (flag[x]) {
		int proc = (base + index[x]) / space;
		int offset = (base + index[x]) - (proc * space);

		if (proc == Self) {
		  dst[offset] = src[x];
		} else {
		  int posn = Send_ctr[proc]++;
		  intx *buf = (intx *) Usr_send[proc];

		  buf[posn].value = src[x];
		  buf[posn].index = offset;
		}
	      }
	    }
	    n_rcvd += _recv_simple (dst, dfp_pez_tag, _unpack_simple_intx);
	    for (x = 0; x < NumProcs; x++) {
	      if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
		_send_buffer (x, dfp_pez_tag, (Send_ctr[x] * sizeof (intx)));
		n_sent++;
	      }
	    }
	  }
	  _finish_simple (dst, dfp_pez_tag, n_sent, n_rcvd, sizeof (intx),
			  _unpack_simple_intx);
	}
      }
    }
  }
  LOG_STOP (dfp_pez, src_nelts);
}

unsigned int dfp_pez_inplace (void)
{
  return INPLACE_NONE;
}

int dfp_pez_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (src_nelts);
  }
  return result;
}
void dfp_ped (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       vec_p default_v, vec_p src_segd_v, int src_nelts,
        int src_nsegs, vec_p dst_segd_v, int dst_nelts, int dst_nsegs,
	       vec_p scratch_v)
{
  double *dst, *src, *def;
  cvl_bool *flag;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *src_segment;
  int current_seg, base, dst_nelts_here, src_nelts_here, i;

  LOG_START (dfp_ped, src_nelts);
  if (dst_nelts != 0) {
    if (src_nsegs == 1) {
      dfp_pud (dst_v, src_v, index_v, flag_v, default_v, src_nelts, dst_nelts,
	       scratch_v);
    } else {
      dst_nelts_here = _num_here (dst_nelts);
      dst = (double *) (Mem + dst_v);
      def = (double *) (Mem + default_v);

      for (i = 0; i < dst_nelts_here; i++) {
	dst[i] = def[i];
      }

      if (src_nelts != 0) {
	scratch = (int *) (Mem + scratch_v);
	src_segd = (int *) (Mem + src_segd_v);
	dst_segd = (int *) (Mem + dst_segd_v);
	src_start = SegdStart (src_segd, src_nelts, src_nsegs);
	src_length = SegdStart (src_segd, src_nelts, src_nsegs);
	dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
	dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

	base = _init_seg_starts (scratch, src_start, dst_start, src_length,
				 dst_length, src_nsegs, src_nelts,
				 dfp_ped_tag);

	src_nelts_here = _num_here (src_nelts);
	src = (double *) (Mem + src_v);
	index = (int *) (Mem + index_v);
	flag = (cvl_bool *) (Mem + flag_v);
	src_segment = SegdSegment (src_segd, src_nelts, src_nsegs);
	current_seg = *(SegdBefore (src_segd, src_nelts, src_nsegs));

	/* Now we can do the actual permute, using base as the initial
	 * base address of the segment to send to, and resetting it to
	 * the current element of scratch whenever we hit a segment
	 * start. */
	{
	  int n_sent = 0, n_rcvd = 0;
	  int space = SpaceFor (dst_nelts);
	  int x, y, loopbound;

	  for (y = 0; y < src_nelts_here; y += RCV_EVERY) {
	    loopbound = min (src_nelts_here, y + RCV_EVERY);
	    for (x = y; x < loopbound; x++) {
	      if (src_segment[x] != current_seg) {
		current_seg = src_segment[x];
		base = scratch[x];
	      }
	      /* if (flag[x] dst[base+index[x]] <- src[x] */
	      if (flag[x]) {
		int proc = (base + index[x]) / space;
		int offset = (base + index[x]) - (proc * space);

		if (proc == Self) {
		  dst[offset] = src[x];
		} else {
		  int posn = Send_ctr[proc]++;
		  doublex *buf = (doublex *) Usr_send[proc];

		  buf[posn].value = src[x];
		  buf[posn].index = offset;
		}
	      }
	    }
	    n_rcvd += _recv_simple (dst, dfp_ped_tag, _unpack_simple_doublex);
	    for (x = 0; x < NumProcs; x++) {
	      if (Send_ctr[x] >= ((BUF_SIZE / sizeof (doublex)) - RCV_EVERY)) {
		_send_buffer (x, dfp_ped_tag, (Send_ctr[x] * sizeof (doublex)));
		n_sent++;
	      }
	    }
	  }
	  _finish_simple (dst, dfp_ped_tag, n_sent, n_rcvd, sizeof (doublex),
			  _unpack_simple_doublex);
	}
      }
    }
  }
  LOG_STOP (dfp_ped, src_nelts);
}

unsigned int dfp_ped_inplace (void)
{
  return INPLACE_NONE;
}

int dfp_ped_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (src_nelts);
  }
  return result;
}
void dfp_peb (vec_p dst_v, vec_p src_v, vec_p index_v, vec_p flag_v,
	       vec_p default_v, vec_p src_segd_v, int src_nelts,
        int src_nsegs, vec_p dst_segd_v, int dst_nelts, int dst_nsegs,
	       vec_p scratch_v)
{
  cvl_bool *dst, *src, *def;
  cvl_bool *flag;
  int *index, *scratch, *dst_segd, *src_segd;
  int *src_length, *dst_length, *dst_start, *src_start, *src_segment;
  int current_seg, base, dst_nelts_here, src_nelts_here, i;

  LOG_START (dfp_peb, src_nelts);
  if (dst_nelts != 0) {
    if (src_nsegs == 1) {
      dfp_pub (dst_v, src_v, index_v, flag_v, default_v, src_nelts, dst_nelts,
	       scratch_v);
    } else {
      dst_nelts_here = _num_here (dst_nelts);
      dst = (cvl_bool *) (Mem + dst_v);
      def = (cvl_bool *) (Mem + default_v);

      for (i = 0; i < dst_nelts_here; i++) {
	dst[i] = def[i];
      }

      if (src_nelts != 0) {
	scratch = (int *) (Mem + scratch_v);
	src_segd = (int *) (Mem + src_segd_v);
	dst_segd = (int *) (Mem + dst_segd_v);
	src_start = SegdStart (src_segd, src_nelts, src_nsegs);
	src_length = SegdStart (src_segd, src_nelts, src_nsegs);
	dst_start = SegdStart (dst_segd, dst_nelts, dst_nsegs);
	dst_length = SegdLength (dst_segd, dst_nelts, dst_nsegs);

	base = _init_seg_starts (scratch, src_start, dst_start, src_length,
				 dst_length, src_nsegs, src_nelts,
				 dfp_peb_tag);

	src_nelts_here = _num_here (src_nelts);
	src = (cvl_bool *) (Mem + src_v);
	index = (int *) (Mem + index_v);
	flag = (cvl_bool *) (Mem + flag_v);
	src_segment = SegdSegment (src_segd, src_nelts, src_nsegs);
	current_seg = *(SegdBefore (src_segd, src_nelts, src_nsegs));

	/* Now we can do the actual permute, using base as the initial
	 * base address of the segment to send to, and resetting it to
	 * the current element of scratch whenever we hit a segment
	 * start. */
	{
	  int n_sent = 0, n_rcvd = 0;
	  int space = SpaceFor (dst_nelts);
	  int x, y, loopbound;

	  for (y = 0; y < src_nelts_here; y += RCV_EVERY) {
	    loopbound = min (src_nelts_here, y + RCV_EVERY);
	    for (x = y; x < loopbound; x++) {
	      if (src_segment[x] != current_seg) {
		current_seg = src_segment[x];
		base = scratch[x];
	      }
	      /* if (flag[x] dst[base+index[x]] <- src[x] */
	      if (flag[x]) {
		int proc = (base + index[x]) / space;
		int offset = (base + index[x]) - (proc * space);

		if (proc == Self) {
		  dst[offset] = src[x];
		} else {
		  int posn = Send_ctr[proc]++;
		  cvl_boolx *buf = (cvl_boolx *) Usr_send[proc];

		  buf[posn].value = src[x];
		  buf[posn].index = offset;
		}
	      }
	    }
	    n_rcvd += _recv_simple (dst, dfp_peb_tag, _unpack_simple_cvl_boolx);
	    for (x = 0; x < NumProcs; x++) {
	      if (Send_ctr[x] >= ((BUF_SIZE / sizeof (cvl_boolx)) - RCV_EVERY)) {
		_send_buffer (x, dfp_peb_tag, (Send_ctr[x] * sizeof (cvl_boolx)));
		n_sent++;
	      }
	    }
	  }
	  _finish_simple (dst, dfp_peb_tag, n_sent, n_rcvd, sizeof (cvl_boolx),
			  _unpack_simple_cvl_boolx);
	}
      }
    }
  }
  LOG_STOP (dfp_peb, src_nelts);
}

unsigned int dfp_peb_inplace (void)
{
  return INPLACE_NONE;
}

int dfp_peb_scratch (int src_nelts, int src_nsegs,
		      int dst_nelts, int dst_nsegs)
{
  int result = 0;

  if ((src_nelts != 0) && (dst_nelts != 0) && (src_nsegs != 1)) {
    result = siz_foz (src_nelts);
  }
  return result;
}
