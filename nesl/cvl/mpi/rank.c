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


/* --------------------------- Ranking ------------------------------- */

/* Use 8 bits' worth of buckets for simplicity.	 Optimal number of bits
 * will actually depend on hardware's computation/communication speeds,
 * and hence is platform dependent.  See Marco's papers on radix sorts.
 */

#define BitsPerWord	32
#define BitsPerPass	8
#define NumBuckets	(1<<BitsPerPass)
#define BitsForMask	~(~0 << BitsPerPass)
#define bits(x,y)	((((unsigned) x) >> y) & BitsForMask)
#define Signbit		(1<<31)


/* Transpose the array of buckets (NumProcs processors, each with
 * NumBuckets on).  Conceptually, each processor sends their first
 * bucket to the first processor, second bucket to the second
 * processor, etc, wrapping around as necessary.  In reality, this would
 * lead to bottlenecks, as one processor would have to receive NumProcs
 * messages on each step.  So the loop is broken up into two subloops,
 * spreading the reception load evenly across processors.
 */

static void transpose_buckets (int *dst, int *src)
{
  /* See also defines */
  int n_sent = 0, n_rcvd = 0;
  int space = SpaceFor (NumProcs * NumBuckets);
  int i, j, loopbound;

  for (j = Self; j < NumBuckets; j += RCV_EVERY) {
    loopbound = min (NumBuckets, j + RCV_EVERY);
    for (i = j; i < loopbound; i++) {
      int proc = ((i * NumProcs) + Self) / space;
      int offset = ((i * NumProcs) + Self) - (proc * space);

      if (proc == Self) {
	dst[offset] = src[i];
      } else {
	int posn = Send_ctr[proc]++;
	intx *buf = (intx *) Usr_send[proc];

	buf[posn].value = src[i];
	buf[posn].index = offset;
      }
    }
    n_rcvd += _recv_simple (dst, CVL_trn_tag, _unpack_simple_intx);
    for (i = 0; i < NumProcs; i++) {
      if (Send_ctr[i] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	_send_buffer (i, CVL_trn_tag, (Send_ctr[i] * sizeof (intx)));
	n_sent++;
      }
    }
  }
  for (j = 0; j < Self; j += RCV_EVERY) {
    loopbound = min (Self, j + RCV_EVERY);
    for (i = j; i < loopbound; i++) {
      int proc = ((i * NumProcs) + Self) / space;
      int offset = ((i * NumProcs) + Self) - (proc * space);

      if (proc == Self) {
	dst[offset] = src[i];
      } else {
	int posn = Send_ctr[proc]++;
	intx *buf = (intx *) Usr_send[proc];

	buf[posn].value = src[i];
	buf[posn].index = offset;
      }
    }
    n_rcvd += _recv_simple (dst, CVL_trn_tag, _unpack_simple_intx);
    for (i = 0; i < NumProcs; i++) {
      if (Send_ctr[i] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	_send_buffer (i, CVL_trn_tag, (Send_ctr[i] * sizeof (intx)));
	n_sent++;
      }
    }
  }
  _finish_simple (dst, CVL_trn_tag, n_sent, n_rcvd, sizeof (intx),
		  _unpack_simple_intx);
}


static void untranspose_buckets (int *dst, int *src)
{
  /* See also defines */
  int n_sent = 0, n_rcvd = 0;
  int i, j, loopbound;
  int indx, proc, offset;

  for (j = Self; j < NumBuckets; j += RCV_EVERY) {
    loopbound = min (NumBuckets, j + RCV_EVERY);
    for (i = j; i < loopbound; i++) {
      indx = (Self * NumBuckets) + i;
      proc = indx % NumProcs;
      offset = indx / NumProcs;
      if (proc == Self) {
	dst[offset] = src[i];
      } else {
	int posn = Send_ctr[proc]++;
	intx *buf = (intx *) Usr_send[proc];

	buf[posn].value = src[i];
	buf[posn].index = offset;
      };
    }
    n_rcvd += _recv_simple (dst, CVL_unt_tag, _unpack_simple_intx);
    for (i = 0; i < NumProcs; i++) {
      if (Send_ctr[i] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	_send_buffer (i, CVL_unt_tag, (Send_ctr[i] * sizeof (intx)));
	n_sent++;
      }
    }
  }
  for (j = 0; j < Self; j += RCV_EVERY) {
    loopbound = min (NumBuckets, j + RCV_EVERY);
    for (i = j; i < loopbound; i++) {
      indx = (Self * NumBuckets) + i;
      proc = indx % NumProcs;
      offset = indx / NumProcs;
      if (proc == Self) {
	dst[offset] = src[i];
      } else {
	int posn = Send_ctr[proc]++;
	intx *buf = (intx *) Usr_send[proc];

	buf[posn].value = src[i];
	buf[posn].index = offset;
      };
    }
    n_rcvd += _recv_simple (dst, CVL_unt_tag, _unpack_simple_intx);
    for (i = 0; i < NumProcs; i++) {
      if (Send_ctr[i] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	_send_buffer (i, CVL_unt_tag, (Send_ctr[i] * sizeof (intx)));
	n_sent++;
      }
    }
  }
  _finish_simple (dst, CVL_unt_tag, n_sent, n_rcvd, sizeof (intx),
		  _unpack_simple_intx);
}


static void scan_buckets (int *buckets)
{
  int transbuckets[NumBuckets];
  int sum, swap, incl_sum, i;

  /* Transpose buckets[] into transbuckets[]. */
  transpose_buckets (transbuckets, buckets);

  /* Do an exclusive plus-scan on transbuckets[]. */
  sum = 0;
  for (i = 0; i < NumBuckets; i++) {
    swap = sum;
    sum += transbuckets[i];
    transbuckets[i] = swap;
  }

  /* Inclusive add-scan across sum into incl_sum. */
  MPI_Scan (&sum, &incl_sum, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

  /* Convert inclusive result into exclusive result. */
  sum = incl_sum - sum;

  for (i = 0; i < NumBuckets; i++) {
    transbuckets[i] += sum;
  }

  /* Untranspose transbuckets[] back into buckets[]. */
  untranspose_buckets (buckets, transbuckets);
}


/* Radix rank on 32, 64 or 96-bit bitfields.  Permutes perm according to
 * the rank of the numbers in src.  Initialize perm to the index set for
 * a simple integer sort, or to the result of a previous rank for a sort
 * on e.g. doubles.  Will destructively overwrite src.	Needs a scratch
 * area big enough for one vector of perms (ints) and one vector of the
 * key size being sorted (1, 2, or 3 ints), i.e. total of 2, 3 or 4 int
 * vectors.
 */



static void rank_t32bits (int *perm, t32bits * src, int *scratch,
			   int len, int isUp, int tag)
{
  int *perm_src, *perm_dst, *perm_swap;
  t32bits *key_src, *key_dst, *key_swap;
  int n_here, i, j, k;
  unsigned val;
  int buckets[NumBuckets];

  n_here = _num_here (len);
  perm_src = (int *) perm;
  key_src = (t32bits *) src;
  perm_dst = (int *) scratch;
  key_dst = (t32bits *) (scratch + SpaceFor (len));

  /* Rank from key_src to key_dst on each iteration, and send the
   * permutation from perm_src to perm_dst the same way.  To avoid
   * copying _dst back to _src after each iteration, we swap pointers. */
  /* Iterate over the key space. */
  for (k = 1 - 1; k >= 0; k--) {/* k = word number */

    for (j = 0; j < BitsPerWord; j += BitsPerPass) {	/* j = bit number */

      /* Histogram keys locally. */
      for (i = 0; i < NumBuckets; i++) {
	buckets[i] = 0;
      }
      for (i = 0; i < n_here; i++) {
	val = bits (key_src[i].word[k], j);
	buckets[val]++;
      }

      /* Scan the buckets. */
      scan_buckets (buckets);

      /* Rank and permute globally. */
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (len);
	int x, y, loopbound;

	for (y = 0; y < n_here; y += RCV_EVERY) {
	  loopbound = min (n_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    {
	      unsigned newval = bits (key_src[x].word[k], j);
	      int indx = buckets[newval]++;
	      int proc = indx / space;
	      int offset = indx - (proc * space);

	      if (proc == Self) {
		key_dst[offset].word[0] = key_src[x].word[0];
		perm_dst[offset] = perm_src[x];
	      } else {
		int posn = Send_ctr[proc]++;
		two_intx *buf = (two_intx *) Usr_send[proc];

		buf[posn].value1 = key_src[x].word[0];
		buf[posn].value2 = perm_src[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_t32bits (key_dst, tag, perm_dst);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (two_intx)) - RCV_EVERY)) {
	      _send_buffer (x, tag, (Send_ctr[x] * sizeof (two_intx)));
	      n_sent++;
	    }
	  }
	}
	_finish_t32bits (key_dst, perm_dst, tag, n_sent, n_rcvd);
      }

      /* Flip _src and _dst pointers. */
      key_swap = key_src;
      key_src = key_dst;
      key_dst = key_swap;
      perm_swap = perm_src;
      perm_src = perm_dst;
      perm_dst = perm_swap;
    }
  }
}

static void rank_t64bits (int *perm, t64bits * src, int *scratch,
			   int len, int isUp, int tag)
{
  int *perm_src, *perm_dst, *perm_swap;
  t64bits *key_src, *key_dst, *key_swap;
  int n_here, i, j, k;
  unsigned val;
  int buckets[NumBuckets];

  n_here = _num_here (len);
  perm_src = (int *) perm;
  key_src = (t64bits *) src;
  perm_dst = (int *) scratch;
  key_dst = (t64bits *) (scratch + SpaceFor (len));

  /* Rank from key_src to key_dst on each iteration, and send the
   * permutation from perm_src to perm_dst the same way.  To avoid
   * copying _dst back to _src after each iteration, we swap pointers. */
  /* Iterate over the key space. */
  for (k = 2 - 1; k >= 0; k--) {/* k = word number */

    for (j = 0; j < BitsPerWord; j += BitsPerPass) {	/* j = bit number */

      /* Histogram keys locally. */
      for (i = 0; i < NumBuckets; i++) {
	buckets[i] = 0;
      }
      for (i = 0; i < n_here; i++) {
	val = bits (key_src[i].word[k], j);
	buckets[val]++;
      }

      /* Scan the buckets. */
      scan_buckets (buckets);

      /* Rank and permute globally. */
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (len);
	int x, y, loopbound;

	for (y = 0; y < n_here; y += RCV_EVERY) {
	  loopbound = min (n_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    {
	      unsigned newval = bits (key_src[x].word[k], j);
	      int indx = buckets[newval]++;
	      int proc = indx / space;
	      int offset = indx - (proc * space);

	      if (proc == Self) {
		key_dst[offset].word[0] = key_src[x].word[0];
		key_dst[offset].word[1] = key_src[x].word[1];
		perm_dst[offset] = perm_src[x];
	      } else {
		three_intx *buf = (three_intx *) Usr_send[proc];
		int posn = Send_ctr[proc]++;

		buf[posn].value1 = key_src[x].word[0];
		buf[posn].value2 = key_src[x].word[1];
		buf[posn].value3 = perm_src[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_t64bits (key_dst, tag, perm_dst);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (three_intx)) - RCV_EVERY)) {
	      _send_buffer (x, tag, (Send_ctr[x] * sizeof (three_intx)));
	      n_sent++;
	    }
	  }
	}
	_finish_t64bits (key_dst, perm_dst, tag, n_sent, n_rcvd);
      }

      /* Flip _src and _dst pointers. */
      key_swap = key_src;
      key_src = key_dst;
      key_dst = key_swap;
      perm_swap = perm_src;
      perm_src = perm_dst;
      perm_dst = perm_swap;
    }
  }
}

static void rank_t96bits (int *perm, t96bits * src, int *scratch,
			   int len, int isUp, int tag)
{
  int *perm_src, *perm_dst, *perm_swap;
  t96bits *key_src, *key_dst, *key_swap;
  int n_here, i, j, k;
  unsigned val;
  int buckets[NumBuckets];

  n_here = _num_here (len);
  perm_src = (int *) perm;
  key_src = (t96bits *) src;
  perm_dst = (int *) scratch;
  key_dst = (t96bits *) (scratch + SpaceFor (len));

  /* Rank from key_src to key_dst on each iteration, and send the
   * permutation from perm_src to perm_dst the same way.  To avoid
   * copying _dst back to _src after each iteration, we swap pointers. */
  /* Iterate over the key space. */
  for (k = 3 - 1; k >= 0; k--) {/* k = word number */

    for (j = 0; j < BitsPerWord; j += BitsPerPass) {	/* j = bit number */

      /* Histogram keys locally. */
      for (i = 0; i < NumBuckets; i++) {
	buckets[i] = 0;
      }
      for (i = 0; i < n_here; i++) {
	val = bits (key_src[i].word[k], j);
	buckets[val]++;
      }

      /* Scan the buckets. */
      scan_buckets (buckets);

      /* Rank and permute globally. */
      {
	int n_sent = 0, n_rcvd = 0;
	int space = SpaceFor (len);
	int x, y, loopbound;

	for (y = 0; y < n_here; y += RCV_EVERY) {
	  loopbound = min (n_here, y + RCV_EVERY);
	  for (x = y; x < loopbound; x++) {
	    {
	      unsigned newval = bits (key_src[x].word[k], j);
	      int indx = buckets[newval]++;
	      int proc = indx / space;
	      int offset = indx - (proc * space);

	      if (proc == Self) {
		key_dst[offset].word[0] = key_src[x].word[0];
		key_dst[offset].word[1] = key_src[x].word[1];
		key_dst[offset].word[2] = key_src[x].word[2];
		perm_dst[offset] = perm_src[x];
	      } else {
		int posn = Send_ctr[proc]++;
		four_intx *buf = (four_intx *) Usr_send[proc];

		buf[posn].value1 = key_src[x].word[0];
		buf[posn].value2 = key_src[x].word[1];
		buf[posn].value3 = key_src[x].word[2];
		buf[posn].value4 = perm_src[x];
		buf[posn].index = offset;
	      }
	    }
	  }
	  n_rcvd += _recv_t96bits (key_dst, tag, perm_dst);
	  for (x = 0; x < NumProcs; x++) {
	    if (Send_ctr[x] >= ((BUF_SIZE / sizeof (four_intx)) - RCV_EVERY)) {
	      _send_buffer (x, tag, (Send_ctr[x] * sizeof (four_intx)));
	      n_sent++;
	    }
	  }
	}
	_finish_t96bits (key_dst, perm_dst, tag, n_sent, n_rcvd);
      }

      /* Flip _src and _dst pointers. */
      key_swap = key_src;
      key_src = key_dst;
      key_dst = key_swap;
      perm_swap = perm_src;
      perm_src = perm_dst;
      perm_dst = perm_swap;
    }
  }
}



/* ------------------- Unsegmented Integer Rank ---------------------- */

/* Needs scratch area to hold one vector of ints, plus the scratch area
 * required by rank_t32bits (two more vectors of ints), for a total of 3
 * vectors of ints.
 */

static void int_rank_unseg (int *dst, int *src, int len, int *scratch,
			     int is_up, int tag)
{
  int *perm, *newscratch;
  int n_here, first_idx, xor_mask, i, j;

  n_here = _num_here (len);
  first_idx = FirstHere (len);
  perm = scratch;
  newscratch = scratch + SpaceFor (len);

  /* xor sign bits of src into dst to handle signed numbers correctly:
   * - if upward rank, flip the sign bit, - else downward rank, so flip
   * every bit except the sign bit Also, initialize perm to contain the
   * identity permutation. */
  xor_mask = is_up ? Signbit : ~Signbit;
  for (i = 0, j = first_idx; i < n_here; i++, j++) {
    dst[i] = src[i] ^ xor_mask;
    perm[i] = j;
  }

  rank_t32bits (perm, (t32bits *) dst, newscratch, len, is_up, tag);

  {
    int counter = FirstHere (len);
    int n_sent = 0, n_rcvd = 0;
    int space = SpaceFor (len);
    int x, y, loopbound;

    for (y = 0; y < n_here; y += RCV_EVERY) {
      loopbound = min (n_here, y + RCV_EVERY);
      for (x = y; x < loopbound; x++) {
	/* send index vector (identity perm) to dst */
	{
	  int proc = perm[x] / space;
	  int offset = perm[x] - (proc * space);

	  if (proc == Self) {
	    dst[offset] = counter++;
	  } else {
	    int posn = Send_ctr[proc]++;
	    intx *buf = (intx *) Usr_send[proc];

	    buf[posn].value = counter++;
	    buf[posn].index = offset;
	  }
	}
      }
      n_rcvd += _recv_simple (dst, tag, _unpack_simple_intx);
      for (x = 0; x < NumProcs; x++) {
	if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	  _send_buffer (x, tag, (Send_ctr[x] * sizeof (intx)));
	  n_sent++;
	}
      }
    }
    _finish_simple (dst, tag, n_sent, n_rcvd, sizeof (intx),
		    _unpack_simple_intx);
  }
}




void rku_luz (vec_p dst, vec_p src, int len, vec_p scratch)
{
  LOG_START (rku_luz, len);
  int_rank_unseg ((int *) (Mem + dst), (int *) (Mem + src), len,
		  (int *) (Mem + scratch), 1, rku_luz_tag);
  LOG_STOP (rku_luz, len);
}

unsigned int rku_luz_inplace (void)
{
  return INPLACE_1;
}

int rku_luz_scratch (int len) {
  return 3 * siz_foz (len);
}

void rkd_luz (vec_p dst, vec_p src, int len, vec_p scratch)
{
  LOG_START (rkd_luz, len);
  int_rank_unseg ((int *) (Mem + dst), (int *) (Mem + src), len,
		  (int *) (Mem + scratch), 0, rku_luz_tag);
  LOG_STOP (rkd_luz, len);
}

unsigned int rkd_luz_inplace (void)
{
  return INPLACE_1;
}

int rkd_luz_scratch (int len) {
  return 3 * siz_foz (len);
}



/* -------------------- Segmented Integer Rank ----------------------- */

/* Add segment number to the beginning of each key to form 64-bit keys,
 * then rank normally.	Radix sort is stable, and the segment number is
 * in the high-order bits of each 64-bit key, so we end up with a
 * permutation within segments.	 Begin with a segmented index vector, so
 * the final permutation is suitable for feeding to e.g. smp_pez.
 */

static void int_rank_seg (int *dst, int *src, int *segd, int nelts, int
			   nsegs, int *scratch, int is_up, int tag)
{
  int *segment, *perm, *newscratch;
  t64bits *tmpsrc;
  int n_here, first_idx, xor_mask, i, j;

  n_here = _num_here (nelts);
  first_idx = FirstHere (nelts);
  segment = SegdSegment (segd, nelts, nsegs);
  perm = scratch;
  tmpsrc = (t64bits *) (scratch + SpaceFor (nelts));
  xor_mask = is_up ? Signbit : ~Signbit;

  /* xor sign bits of src into tmpsrc, as per int_rank_unseg, and put
   * segment number into high-order word as we go.  At the same time,
   * initialize perm to contain the unsegmented identity permutation. */
  for (i = 0, j = first_idx; i < n_here; i++, j++) {
    tmpsrc[i].word[0] = segment[i];
    tmpsrc[i].word[1] = src[i] ^ xor_mask;
    perm[i] = j;
  }

  /* Rank perm according to tmpsrc. */
  newscratch = (int *) (tmpsrc + 2 * SpaceFor (nelts));
  rank_t64bits (perm, tmpsrc, newscratch, nelts, is_up, tag);

  /* Send a segmented index (segmented identity permutation) to perm. */
  {
    int currentseg = *(SegdBefore (segd, nelts, nsegs));
    int segfirst = *(SegdFirst (segd, nelts, nsegs));
    int indx = (Self * SpaceFor (nelts)) - segfirst;

    /* segfirst is global offset of start of first segment on this
     * processor.  Subtract it from the global offset of the first
     * element on this processor to find the index of that element
     * WITHIN the first segment on this processor.  Reset to 0 when we
     * hit a segment boundary. */
    int n_sent = 0, n_rcvd = 0;
    int space = SpaceFor (nelts);
    int x, y, loopbound;

    for (y = 0; y < n_here; y += RCV_EVERY) {
      loopbound = min (n_here, y + RCV_EVERY);
      for (x = y; x < loopbound; x++) {
	if (currentseg != segment[x]) {
	  currentseg = segment[x];
	  indx = 0;
	} {
	  int proc = perm[x] / space;
	  int offset = perm[x] - (proc * space);

	  if (proc == Self) {
	    dst[offset] = indx;
	  } else {
	    int posn = Send_ctr[proc]++;
	    intx *buf = (intx *) Usr_send[proc];

	    buf[posn].value = indx;
	    buf[posn].index = offset;
	  }
	}
	indx++;
      }
      n_rcvd += _recv_simple (dst, tag, _unpack_simple_intx);
      for (x = 0; x < NumProcs; x++) {
	if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	  _send_buffer (x, tag, (Send_ctr[x] * sizeof (intx)));
	  n_sent++;
	}
      }
    }
    _finish_simple (dst, tag, n_sent, n_rcvd, sizeof (intx),
		    _unpack_simple_intx);
  }
}



void rku_lez (vec_p dst, vec_p src, vec_p segd, int nelts, int nsegs,
	       vec_p scratch)
{
  LOG_START (rku_lez, nelts);
  int_rank_seg ((int *) (Mem + dst), (int *) (Mem + src), (int *)
		(Mem + segd), nelts, nsegs, (int *) (Mem + scratch),
		1, rku_lez_tag);
  LOG_STOP (rku_lez, nelts);
}

unsigned int rku_lez_inplace (void)
{
  return INPLACE_1;
}

int rku_lez_scratch (int nelts, int nsegs) {
  return (6 * siz_foz (nelts));
}

void rkd_lez (vec_p dst, vec_p src, vec_p segd, int nelts, int nsegs,
	       vec_p scratch)
{
  LOG_START (rkd_lez, nelts);
  int_rank_seg ((int *) (Mem + dst), (int *) (Mem + src), (int *)
		(Mem + segd), nelts, nsegs, (int *) (Mem + scratch),
		0, rku_lez_tag);
  LOG_STOP (rkd_lez, nelts);
}

unsigned int rkd_lez_inplace (void)
{
  return INPLACE_1;
}

int rkd_lez_scratch (int nelts, int nsegs) {
  return (6 * siz_foz (nelts));
}



/* -------------------- Unsegmented Double Rank ---------------------- */

/* Need to know which words of a double are most significant
 *
 * We assume that doubles are stored in IEEE format:
 *	s, e, m
 * where s is a sign bit with 1 = negative
 *	 e is the exponent stored in an excess form
 *	 m is the mantissa
 */
#if defined(__mips) || defined(__alpha) || defined(__i860)
#if defined(__sgi)
#undef FP_LITTLE_ENDIAN
#else
#define FP_LITTLE_ENDIAN
#endif
#else
#undef FP_LITTLE_ENDIAN
#endif

/* Needs scratch area to hold one vector of ints (perm), plus one vector
 * of 64bit structs (tmpsrc), plus the scratch area required by
 * rank_t64bits (three more vectors of ints), for a total of 6 vectors
 * of ints.
 */

static void double_rank_unseg (int *dst, t64bits * src, int len,
			        int *scratch, int is_up, int tag)
{
  int n_here, first_index, indx, i;
  int *perm, *newscratch;
  t64bits *tmpsrc;

  /* Bit-twiddle src into tmpsrc.  At the same time, initialize perm to
   * contain the identity permutation. */
  n_here = _num_here (len);
  first_index = FirstHere (len);
  perm = scratch;
  tmpsrc = (t64bits *) (scratch + SpaceFor (len));
  indx = first_index;

  for (i = 0; i < n_here; i++) {
    unsigned int sign, word0, word1, field0, field1;

#ifdef FP_LITTLE_ENDIAN
    word0 = src[i].word[1];
    word1 = src[i].word[0];
#else
    word0 = src[i].word[0];
    word1 = src[i].word[1];
#endif
    /* Get the sign bit. */
    sign = word0 & Signbit;
    if (sign) {
      /* Flip all bits to make big negative numbers small unsigneds */
      field0 = ~word0;
      field1 = ~word1;
    } else {
      /* Add signbit into positive numbers to make them big unsigneds */
      field0 = word0 ^ Signbit;
      field1 = word1;
    }

    if (is_up) {
      tmpsrc[i].word[0] = field0;
      tmpsrc[i].word[1] = field1;
    } else {
      tmpsrc[i].word[0] = ~field0;
      tmpsrc[i].word[1] = ~field1;
    }
    perm[i] = indx++;
  }

  /* Rank. */
  newscratch = ((int *) tmpsrc) + (2 * SpaceFor (len));
  rank_t64bits (perm, tmpsrc, newscratch, len, is_up, tag);

  /* Send index vector to dst according to perm. */
  indx = first_index;
  {
    int n_sent = 0, n_rcvd = 0;
    int space = SpaceFor (len);
    int x, y, loopbound;

    for (y = 0; y < n_here; y += RCV_EVERY) {
      loopbound = min (n_here, y + RCV_EVERY);
      for (x = y; x < loopbound; x++) {
	{
	  int proc = perm[x] / space;
	  int offset = perm[x] - (proc * space);

	  if (proc == Self) {
	    dst[offset] = indx;
	  } else {
	    int posn = Send_ctr[proc]++;
	    intx *buf = (intx *) Usr_send[proc];

	    buf[posn].value = indx;
	    buf[posn].index = offset;
	  }
	  indx++;
	}
      }
      n_rcvd += _recv_simple (dst, tag, _unpack_simple_intx);
      for (x = 0; x < NumProcs; x++) {
	if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	  _send_buffer (x, tag, (Send_ctr[x] * sizeof (intx)));
	  n_sent++;
	}
      }
    }
    _finish_simple (dst, tag, n_sent, n_rcvd, sizeof (intx),
		    _unpack_simple_intx);
  }
}



void rku_lud (vec_p dst, vec_p src, int len, vec_p scratch)
{
  LOG_START (rku_lud, len);
  double_rank_unseg ((int *) (Mem + dst), (t64bits *) (Mem + src), len,
		     (int *) (Mem + scratch), 1, rku_lud_tag);
  LOG_STOP (rku_lud, len);
}

unsigned int rku_lud_inplace (void)
{
  return INPLACE_1;
}

int rku_lud_scratch (int len) {
  return 6 * siz_foz (len);
}

void rkd_lud (vec_p dst, vec_p src, int len, vec_p scratch)
{
  LOG_START (rkd_lud, len);
  double_rank_unseg ((int *) (Mem + dst), (t64bits *) (Mem + src), len,
		     (int *) (Mem + scratch), 0, rku_lud_tag);
  LOG_STOP (rkd_lud, len);
}

unsigned int rkd_lud_inplace (void)
{
  return INPLACE_1;
}

int rkd_lud_scratch (int len) {
  return 6 * siz_foz (len);
}



/* --------------------- Segmented Double Rank ----------------------- */

static void double_rank_seg (int *dst, t64bits * src, int *segd, int
			    nelts, int nsegs, int *scratch, int is_up,
			      int tag)
{
  int *segment, *perm, *newscratch;
  int n_here, first_index, index, i;
  t96bits *tmpsrc;

  /* Bit-twiddle src into tmpsrc and put identity permutation into
   * perm. */
  n_here = _num_here (nelts);
  first_index = FirstHere (nelts);
  segment = SegdSegment (segd, nelts, nsegs);
  perm = scratch;
  tmpsrc = (t96bits *) (scratch + SpaceFor (nelts));
  index = first_index;

  for (i = 0; i < n_here; i++) {
    unsigned int sign, word0, word1, field0, field1;

    /* High word gets segment number */
    tmpsrc[i].word[0] = segment[i];

#ifdef FP_LITTLE_ENDIAN
    word0 = src[i].word[1];
    word1 = src[i].word[0];
#else
    word0 = src[i].word[0];
    word1 = src[i].word[1];
#endif

    sign = word0 & Signbit;
    if (sign) {
      field0 = ~word0;
      field1 = ~word1;
    } else {
      field0 = word0 ^ Signbit;
      field1 = word1;
    }

    if (is_up) {
      tmpsrc[i].word[1] = field0;
      tmpsrc[i].word[2] = field1;
    } else {
      tmpsrc[i].word[1] = ~field0;
      tmpsrc[i].word[2] = ~field1;
    }
    perm[i] = index++;
  }

  /* Calculate rank of tmpsrc, put it in perm. */
  newscratch = (int *) (tmpsrc + 3 * SpaceFor (nelts));
  rank_t96bits (perm, tmpsrc, newscratch, nelts, is_up, tag);

  /* Send a segmented index (segmented identity permutation) to dst,
   * according to perm. */
  {
    int currentseg = *(SegdBefore (segd, nelts, nsegs));
    int segfirst = *(SegdFirst (segd, nelts, nsegs));
    int indx = (Self * SpaceFor (nelts)) - segfirst;

    int n_sent = 0, n_rcvd = 0;
    int space = SpaceFor (nelts);
    int x, y, loopbound;

    for (y = 0; y < n_here; y += RCV_EVERY) {
      loopbound = min (n_here, y + RCV_EVERY);
      for (x = y; x < loopbound; x++) {
	if (currentseg != segment[x]) {
	  currentseg = segment[x];
	  indx = 0;
	} {
	  int proc = perm[x] / space;
	  int offset = perm[x] - (proc * space);

	  if (proc == Self) {
	    dst[offset] = indx;
	  } else {
	    int posn = Send_ctr[proc]++;
	    intx *buf = (intx *) Usr_send[proc];

	    buf[posn].value = indx;
	    buf[posn].index = offset;
	  }
	  indx++;
	}
      }
      n_rcvd += _recv_simple (dst, tag, _unpack_simple_intx);
      for (x = 0; x < NumProcs; x++) {
	if (Send_ctr[x] >= ((BUF_SIZE / sizeof (intx)) - RCV_EVERY)) {
	  _send_buffer (x, tag, (Send_ctr[x] * sizeof (intx)));
	  n_sent++;
	}
      }
    }
    _finish_simple (dst, tag, n_sent, n_rcvd, sizeof (intx),
		    _unpack_simple_intx);
  }
}




void rku_led (vec_p dst, vec_p src, vec_p segd, int nelts, int nsegs,
	       vec_p scratch)
{
  LOG_START (rku_led, nelts);
  double_rank_seg ((int *) (Mem + dst), (t64bits *) (Mem + src), (int *)
		   (Mem + segd), nelts, nsegs, (int *) (Mem + scratch),
		   1, rku_led_tag);
  LOG_STOP (rku_led, nelts);
}

unsigned int rku_led_inplace (void)
{
  return INPLACE_1;
}

int rku_led_scratch (int nelts, int nsegs) {
  return (8 * siz_foz (nelts));
}

void rkd_led (vec_p dst, vec_p src, vec_p segd, int nelts, int nsegs,
	       vec_p scratch)
{
  LOG_START (rkd_led, nelts);
  double_rank_seg ((int *) (Mem + dst), (t64bits *) (Mem + src), (int *)
		   (Mem + segd), nelts, nsegs, (int *) (Mem + scratch),
		   0, rku_led_tag);
  LOG_STOP (rkd_led, nelts);
}

unsigned int rkd_led_inplace (void)
{
  return INPLACE_1;
}

int rkd_led_scratch (int nelts, int nsegs) {
  return (8 * siz_foz (nelts));
}


/* XXX Rewrite up/down rank as two separate functions. */
