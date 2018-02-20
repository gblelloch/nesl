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
#include <assert.h>


/* ----------------------- Unsegmented Index ------------------------- */

void ind_luz_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  oneword *dst = (oneword *) buf[1];
  int init = buf[2];
  int stride = buf[3];
  int count = buf[4];
  int mylen = (Num_Here (count) + 2 - 1) >> 1;
  int first = First_Elt_Here (count);
  oneword val;
  int i;

  val.ints.lo = (first*stride) + init; val.ints.hi = val.ints.lo + stride;
  stride += stride;
  for (i = 0; i < mylen; i++) {
    dst[i] = val;
    val.ints.lo += stride; val.ints.hi += stride;
  }
}


/* ------------------------ Segmented Index -------------------------- */

#ifdef __STDC__
static volatile int ind_posn;
static volatile int last_init;
static volatile int last_stride;
#else
static int ind_posn;
static int last_init;
static int last_stride;
#endif

static void ind_lez_handler (init_p, init, stride_p, stride)
int *init_p, init, *stride_p, stride;
{
  *stride_p = stride;
  *init_p = init;
  if ((int) stride_p > ind_posn) {	/* biggest so far? */
    ind_posn = (int) stride_p;
    last_init = init;
    last_stride = stride;
  }
  num_rcvd++;
}


void ind_lez_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  int *dst = (int *) buf[1];
  int *init = (int *) buf[2];
  int *stride = (int *) buf[3];
  unsigned *segstart = (unsigned *) buf[4];
  int nelt = buf[5];
  int nseg = buf[6];
  int *scr = (int *) buf[7];
  int *segcount = (int *) segstart + Space_For (nseg);
  int start_of_first_seg = * ((int *) (segcount + Space_For (nelt) + 1));
  int i, first_init, first_stride, mylen = Num_Here (nseg);

  /* Send init, stride to start of every segment (single packet).
   * Count is redundant, so we don't need to send it.
   */

  ind_posn = 0;
  last_init = MININT;			/* negative infinity */
  last_stride = MININT;			/* negative infinity */
  Prep (nelt);
  for (i = 0; i < mylen; i++) {
    if (! Empty_Seg (segstart[i])) {
      Map_To_Proc_And_Posn (Actual_Value (segstart[i]));
      if (proc == thisproc) {
	dst[posn] = init[i];		/* put init at start of vector */
	scr[posn] = stride[i];		/* put stride in matching vector */
	if ((int) &dst[posn] > ind_posn) {
	  ind_posn = (int) &dst[posn];
	  last_init = init[i];
	  last_stride = stride[i];
	}
      } else {
	CMAML_rpc (proc, ind_lez_handler, &dst[posn], init[i],
		   &scr[posn], stride[i]);
	num_sent++;
      }
    }
    Poll ();
  }
  Wait ();

  /* Copy-scan across the values from the last segment on each node,
   * resetting each time there's a new last segment.  Note that we use
   * CMMD_start_bit and CMMD_exclusive, which puts the result of segment
   * n-1 into the first element of segment n.  Just like segmented
   * distribute.  Effectively, we're using ind_posn as a segment *end*
   * bit, rather than a segment start bit.
   */
  if (ind_posn) ind_posn = 1;
  /* XXX combine the two scans into one vector scan? */
  first_stride = CMMD_scan_int (last_stride, CMMD_combiner_max,
				CMMD_upward, CMMD_start_bit, ind_posn,
				CMMD_exclusive);
  first_init = CMMD_scan_int (last_init, CMMD_combiner_max, CMMD_upward,
			      CMMD_start_bit, ind_posn, CMMD_exclusive);

  /* Every processor should now have the correct init and stride for the
   * first segment that it contains.  Processor 0 will have -inf, -inf,
   * because we used the exclusive_scan/start_bit combination, so get
   * the correct values.
   */
  if (thisproc == 0) {
    first_init = dst[0];
    first_stride = scr[0];
  }

  /* Work out where in its segment the 0th element on this processor
   * lies, and hence how much to multiply first_stride by before adding
   * it to first_init.
   */
  first_init += first_stride * (First_Elt_Here (nelt) - start_of_first_seg);

  /* Now do the actual indexing, starting with the results of the scan,
   * and restarting every time we hit a new segment.
   */
  mylen = Num_Here (nelt);
  for (i = 0; i < mylen; i++) {
    if (segcount[i]) {
      first_stride = scr[i];
      first_init   = dst[i] + first_stride;
    } else {
      dst[i]	   = first_init;
      first_init  += first_stride;
    }
  }
}


/* ----------------------- Unsegmented Pack -------------------------- */

/* Part 1.  Rather than just doing a reduce_to_host, we use a scan so we
 * can store some state.  This saves pk2 from having to recompute it (as
 * long as pk1's are generally followed by their associated pk2's).
 */

#ifdef NESLHACK
static int pack_state;			/* state stored from pk1 to pk2 */
static cvl_bool *pack_vecp;		/* matches the pk1 and pk2 */
#endif

void pk1_luv_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  cvl_bool *flag = (cvl_bool *) buf[1];
  int len = buf[2];
  int mylen = Num_Here (len);
  int i, sum = 0;

  /* Count the number of flag bits set on each processor,
   */
  for (i = mylen - 1; i >= 0; i--) {
    if (flag[i]) sum++;
  }

#ifdef NESLHACK
  /* Store it away as state away for pk2 to use.
   */
  pack_state = CMMD_scan_int (sum, CMMD_combiner_add, CMMD_upward,
			      CMMD_none, 0, CMMD_exclusive);
  pack_vecp = flag;

  /* Last processor sends total number of flag bits to host.
   * It's faster to use reduce_to_host than an active message.
   */
  sum = (thisproc == nprocs-1) ? sum + pack_state : 0;
#endif

  CMMD_reduce_to_host_int (sum, CMMD_combiner_add);
}


/* Part 2 (pk2_lud).  This sends one double per packet.
 */

#define Make_Pk2(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  cvl_bool *flag = (cvl_bool *) buf[3];					\
  int srclen = buf[4];							\
  int dstlen = buf[5];							\
  int mylen = Num_Here (srclen);					\
  int i, tmp;								\
									\
  /* If we're using the same flag vector as the one that was used for	\
   * the last pk1, use the state that it stored, otherwise recompute.	\
   * CONSIDER THIS INSIDE AN IFDEF NESLHACK				\
  if (flag == pack_vecp) {						\
    tmp = pack_state;							\
  } else 								\
   */									\
  {									\
    int sum = 0;							\
    for (i = mylen - 1; i >= 0; i--) {					\
      if (flag[i]) sum++;						\
    }									\
    tmp = CMMD_scan_int (sum, CMMD_combiner_add, CMMD_upward,		\
			 CMMD_none, 0, CMMD_exclusive);			\
  }									\
									\
  Prep (dstlen);							\
  for (i = 0; i < mylen; i++) {						\
    if (flag[i]) {							\
      Map_To_Proc_And_Posn (tmp);					\
      Send (TYPE, proc, &dst[posn], src[i]);				\
      tmp++;								\
    } else {								\
      Poll ();								\
    }									\
  }									\
  Wait ();								\
}									\

Make_Pk2 (pk2_lud_, double)


/* Part 2 (pk2_lu[zb]).	 This sends up to three ints or booleans in each
 * packet.  Time per elt is 0.0676 (vs ~0.1100 for one double per packet).
 */

static void pck_send1 (posn, data1)
int *posn;
int data1;
{
  *posn = data1;
  num_rcvd ++;
}


static void pck_send2 (posn, data1, data2)
int *posn;
int data1, data2;
{
  *posn = data1;
  *(posn+1) = data2;
  num_rcvd ++;
}


static void pck_send3 (posn, data1, data2, data3)
int *posn;
int data1, data2, data3;
{
  *posn = data1;
  *(posn+1) = data2;
  *(posn+2) = data3;
  num_rcvd ++;
}


#define Make_Fast_Pk2(NAME, TYPE)					\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  TYPE *dst = (TYPE *) buf[1];						\
  TYPE *src = (TYPE *) buf[2];						\
  cvl_bool *flag = (cvl_bool *) buf[3];					\
  int srclen = buf[4];							\
  int dstlen = buf[5];							\
  int mylen = Num_Here (srclen);					\
  int i, tmp, currentproc = -1, currentposn = 0, npck = -1;		\
  int packet[4];							\
									\
  /* If we're using the same flag vector as the one that was used for	\
   * the last pk1, use the state that it stored, otherwise recompute	\
   * CONSIDER THIS INSIDE AN IFDEF NESLHACK				\
  if (flag == pack_vecp) {						\
    tmp = pack_state;							\
  } else								\
   */									\
  {									\
    int sum = 0;							\
    for (i = mylen - 1; i >= 0; i--) {					\
      if (flag[i]) sum++;						\
    }									\
    tmp = CMMD_scan_int (sum, CMMD_combiner_add, CMMD_upward,		\
			 CMMD_none, 0, CMMD_exclusive);			\
  }									\
									\
  Prep (dstlen);							\
  for (i = 0; i < mylen; i++) {						\
    if (flag[i]) {							\
      proc = tmp * recip;						\
      if (proc == thisproc) {						\
	/* Maps to this processor.					\
	 */								\
	posn = tmp - (proc * eltsper);					\
	dst[posn] = src[i];						\
      } else if ((proc != currentproc) || (npck == 2)) {		\
	/* Send out partial packet to currentproc.			\
	 */								\
	switch (npck) {							\
	case 0:								\
	  CMAML_rpc (currentproc, pck_send1, &dst[currentposn],		\
		     packet[0]);					\
	  num_sent++;							\
	  break;							\
	case 1:								\
	  CMAML_rpc (currentproc, pck_send2, &dst[currentposn],		\
		     packet[0], packet[1]);				\
	  num_sent++;							\
	  break;							\
	case 2:								\
	  CMAML_rpc (currentproc, pck_send3, &dst[currentposn],		\
		     packet[0], packet[1], packet[2]);			\
	  num_sent++;							\
	  break;							\
	}								\
	/* Start a new packet.						\
	 */								\
	packet[0] = src[i];						\
	currentproc = proc;						\
	currentposn = tmp - (proc * eltsper);				\
	npck = 0;							\
      } else {								\
	/* Add to the current packet.					\
	 */								\
	npck++;								\
	packet[npck] = src[i];						\
      }									\
      tmp++;								\
    }									\
    Poll ();								\
  }									\
									\
  /* Send out any remaining incomplete packet.				\
   */									\
  switch (npck) {							\
  case 0:								\
    CMAML_rpc (currentproc, pck_send1, &dst[currentposn],		\
	       packet[0]);						\
    num_sent++;								\
    break;								\
  case 1:								\
    CMAML_rpc (currentproc, pck_send2, &dst[currentposn],		\
	       packet[0], packet[1]);					\
    num_sent++;								\
    break;								\
  case 2:								\
    CMAML_rpc (currentproc, pck_send3, &dst[currentposn],		\
	       packet[0], packet[1], packet[2]);			\
    num_sent++;								\
    break;								\
  }									\
  Wait ();								\
}									\

Make_Fast_Pk2 (pk2_luz_, int)
Make_Fast_Pk2 (pk2_lub_, cvl_bool)


/* ------------------------ Segmented Pack --------------------------- */

/* Part 1.  pk1_lev does exactly the same thing as add_rez, so use
 * add_rez instead.  See h_library.c.
 */

/* Part 2.  This is exactly the same as the unsegmented case, so use
 * that instead.  See h_library.c
 */


/* -------------------------- Rank Stuff ----------------------------- */

#define BytesPerWord	4		/* hardcoding is fun */
#define NumBuckets     	256		/* hardcoding is profitable */
#define Signbit		(1 << 31)	/* hardcoding lets you meet people */


/* Types and active message handlers for ranking */

typedef union t32bits_t {
  unsigned char byte[4];
  int filler;
} t32bits;

static void send_two_words (keyposn, key, indxposn, indx)
int *keyposn, *indxposn;
int key, indx;
{
  *keyposn = key;
  *indxposn = indx;
  num_rcvd++;
}

#define t32bits_Send(PROC, KEYPOSN, KEY, INDXPOSN, INDX)		\
  if (PROC == thisproc) {						\
    *(KEYPOSN.filler) = KEY.filler;					\
    *(INDXPOSN) = INDX;							\
  } else {								\
    CMAML_rpc (PROC, send_two_words, KEYPOSN.filler, KEY.filler, 	\
	       INDXPOSN, INDX);						\
    num_sent++;								\
  }


typedef union t64bits_t {
  unsigned char byte[8];
  unsigned int word[2];
} t64bits;


#define t64bits_Send(PROC, KEYPOSN, KEY, INDXPOSN, INDX)		\
  if (PROC == thisproc) {						\
    *(KEYPOSN.word[0]) = KEY.word[0];					\
    *(KEYPOSN.word[1]) = KEY.word[1];					\
    *(INDXPOSN) = INDX;							\
  } else {								\
    CMAML_rpc (PROC, send_two_words, KEYPOSN.word[0], KEY.word[0],	\
	       KEYPOSN.word[1], KEY.word[1]);				\
    CMAML_rpc (PROC, int_send, INDXPOSN, INDX);				\
    num_sent += 2;							\
  }


typedef union t96bits_t {
  unsigned char byte[12];
  unsigned int word[3];
} t96bits;

#define t96bits_Send(PROC, KEYPOSN, KEY, INDXPOSN, INDX)		\
  if (PROC == thisproc) {						\
    *(KEYPOSN.word[0]) = KEY.word[0];					\
    *(KEYPOSN.word[1]) = KEY.word[1];					\
    *(KEYPOSN.word[2]) = KEY.word[2];					\
    *(INDXPOSN) = INDX;							\
  } else {								\
    CMAML_rpc (PROC, send_two_words, KEYPOSN.word[0], KEY.word[0],	\
	       KEYPOSN.word[1], KEY.word[1]);				\
    CMAML_rpc (PROC, send_two_words, KEYPOSN.word[2], KEY.word[2],	\
	       INDXPOSN, INDX);						\
    num_sent += 2;							\
  }


/* Everyone sends their first bucket to the first processor, their
 * second bucket to the second, etc., wrapping around processors as
 * necessary.  This would hose the net if we coded it like that, so
 * instead on each iteration everyone sends to a different processor.  
 */
static void transpose_buckets (to, from, thisproc, nprocs, logprocs)
int *to, *from, thisproc, nprocs, logprocs;
{
  int i;

  /* Use two loops to break up the starting point amongst processors.
   * This avoids flooding one processor with sends on every iteration.
   * XXX It assumes we have more buckets than processors. 
   */
  Prep (nprocs * NumBuckets);
  for (i = thisproc; i < NumBuckets; i++) {
    Map_To_Proc_And_Posn ((i * nprocs) + thisproc);
    Send (int, proc, &to[posn], from[i]);
    Poll ();
  }
  for (i = 0; i < thisproc; i++) {
    Map_To_Proc_And_Posn ((i * nprocs) + thisproc);
    Send (int, proc, &to[posn], from[i]);
    Poll ();
  }
  Wait ();
}


/* Again, two loops to balance the network load.
 */
static void untranspose_buckets (to, from, thisproc, nprocs, logprocs)
int *to, *from, thisproc, nprocs, logprocs;
{
  int indx, i;

  /* Dummy arg: we're not using eltsperproc, which depends on it */
  Prep (0); 
  for (i = thisproc; i < NumBuckets; i++) {
    indx = ((thisproc * NumBuckets) + i);
    proc = indx % nprocs;
    posn = indx / nprocs;
    Send (int, proc, &to[posn], from[i]);
    Poll ();
  }
  for (i = 0; i < thisproc; i++) {
    indx = ((thisproc * NumBuckets) + i);
    proc = indx % nprocs;
    posn = indx / nprocs;
    Send (int, proc, &to[posn], from[i]);
    Poll ();
  }
  Wait ();
}


static void scan_buckets (bucket, thisproc, nprocs, logprocs)
int *bucket, thisproc, nprocs, logprocs;
{
  int transbucket [NumBuckets];
  int sum, swap, i;

  /* Transpose bucket[] into transbucket[] */
  transpose_buckets (transbucket, bucket, thisproc, nprocs, logprocs);

  /* Do an exclusive plus-scan on transbucket[] */
  sum = 0;
  for (i = 0; i < NumBuckets; i++) {
    swap = sum;
    sum += transbucket[i];
    transbucket[i] = swap;
  }

  sum = CMMD_scan_int (sum, CMMD_combiner_add, CMMD_upward,
		       CMMD_none, 0, CMMD_exclusive);

  for (i = 0; i < NumBuckets; i++) {
    transbucket[i] += sum;
  }

  /* Transpose transbucket[] back into bucket[] */
  untranspose_buckets (bucket, transbucket, thisproc, nprocs, logprocs);

  /* Sync to avoid race condition between successive Wait ()'s */
  CMMD_sync_with_nodes ();
}


/* Radix rank on 32, 64, or 96-bit bitfields.  Permutes perm according
 * to the rank of the numbers in src.  Initialize dst to the index set
 * for a simple integer sort, or to the result of a previous rank for a
 * sort on e.g.  doubles.  Will destructively overwrite src.  Needs a
 * scratch area to hold one vector of perms and one vector of the key
 * size being sorted (1, 2 or 3 ints), i.e. total of 2, 3, or 4 int vecs
 */

#define Make_Rank_Field(TYPE, ITERATIONS)				\
static void Glue(rank_,TYPE) (perm, src, scratch, len, isUp, 		\
			      thisproc, logprocs, nprocs)		\
unsigned int *perm, *src, *scratch;					\
int len, isUp, thisproc, logprocs, nprocs;				\
{									\
  int bucket [NumBuckets];						\
  int mylen = Num_Here (len);						\
  TYPE *key_src = (TYPE *) src;						\
  TYPE *key_dst = (TYPE *) (scratch + Space_For (len));			\
  unsigned int *perm_src = perm;					\
  unsigned int *perm_dst = scratch;		 			\
  void *swap;								\
  int i, j, k;								\
									\
  /* We rank from key_src to key_dst on each iteration, and send the	\
   * permutation from perm_src to perm_dst the same way.  To avoid	\
   * copying _dst back to _src at the end of each iteration, we swap	\
   * pointers. 								\
   */									\
									\
  /* Iterate over the key space */					\
  for (k = ITERATIONS - 1; k >= 0; k--) {				\
    									\
    /* Histogram keys (locally) */					\
    for (i = 0; i < NumBuckets; i++) {					\
      bucket [i] = 0;							\
    }									\
    for (i = 0; i < mylen; i++) {					\
      bucket [key_src[i].byte[k]] += 1;					\
    }									\
									\
    /* Scan the buckets */						\
    scan_buckets (bucket, thisproc, nprocs, logprocs);			\
    									\
    /* Rank and permute (globally) */					\
    Prep (len);								\
    for (i = 0; i < mylen; i++) {					\
      j = bucket [key_src[i].byte[k]]++;				\
      Map_To_Proc_And_Posn (j);						\
      Glue (TYPE,_Send) (proc, &key_dst[posn], key_src[i], 		\
			 &perm_dst [posn], perm_src [i]);		\
      Poll ();								\
    }									\
    Wait ();								\
									\
    /* Flip the _src and _dst pointers */				\
    swap = key_src; key_src = key_dst; key_dst = swap;			\
    swap = perm_src; perm_src = perm_dst; perm_dst = swap;		\
									\
    /* Sync to avoid race condition between successive Wait ()'s */	\
    CMMD_sync_with_nodes ();						\
  }									\
									\
  /* XXX should be able to eliminate a send in the last pass */		\
									\
  /* After an even number of passes, perm is back in perm_src */	\
}

Make_Rank_Field (t32bits, 4)
Make_Rank_Field (t64bits, 8)
Make_Rank_Field (t96bits, 12)


/* -------------------- Unsegmented Integer Rank ----------------------- */

/* Needs scratch area to hold one vector of ints, plus the scratch area
 * required by rank_t32bits (two more vectors of ints).  Total == 3.
 */

static void int_rank_unseg (thisproc, logprocs, nprocs, buf, isUp)
int thisproc, logprocs, nprocs, *buf, isUp;
{
  unsigned int *dst = (unsigned int *) buf[1];
  unsigned int *src = (unsigned int *) buf[2];
  int len = buf[3];
  unsigned int *scratch = (unsigned int *) buf[4];
  int mylen = Num_Here (len);
  int firstindex = First_Elt_Here (len);
  int i, j;
  unsigned int *perm = scratch;
  unsigned int *newscratch = scratch + Space_For (len); 
  unsigned xorMask = isUp ? Signbit : ~Signbit;

  /* xor sign bits of src to handle signed numbers correctly:
   * - if upward rank, then flip the sign bit
   * - if downward rank, then flip every bit except the sign bit
   * xor from src to dst, which we'll then rank.  At the same time,
   * initialize perm to contain the identity permutation.
   */
  for (i = 0, j = firstindex; i < mylen; i++, j++) {
    dst [i] = src[i] ^ xorMask;
    perm [i] = j;
  }

  rank_t32bits (perm, dst, newscratch, len, isUp, thisproc, logprocs, nprocs);

  /* Send the identity permutation (index vector) to perm */
  Prep (len);
  for (i = 0, j = firstindex; i < mylen; i++, j++) {
    Map_To_Proc_And_Posn (perm[i]);
    Send (int, proc, &dst[posn], j);
    Poll ();
  }
  Wait ();
}


void rku_luz_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  int_rank_unseg (thisproc, logprocs, nprocs, buf, 1); /* isUp */
}


void rkd_luz_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  int_rank_unseg (thisproc, logprocs, nprocs, buf, 0); /* !isUp */
}


/* --------------------- Segmented Integer Rank ------------------------ */

/* Add the segment number to the beginning of each key (so we have
 * 64-bit keys).  Rank normally.  Since radix sort is stable, and the
 * segment number is in the high-order bits of the key, we'll end up
 * with a correct permutation; all sorting takes place within segments.
 * We also start off with the identity permutation for our segmented
 * vectors (i.e. a segmented index vector), and therefore end up with a
 * permutation suitable to feed to e.g. smp_pez.
 *
 * Needs scratch area to hold one vector of ints (perm), plus one vector
 * of 64bits, plus the scratch area required by rank_t64bits (vector of
 * ints plus vector of 64bits).  Total == 6.
 */

static void int_rank_seg (thisproc, logprocs, nprocs, buf, isUp)
int thisproc, logprocs, nprocs, *buf, isUp;
{
  unsigned int *dst = (unsigned int *) buf[1];
  unsigned int *src = (unsigned int *) buf[2];
  int nelt = buf[4];
  int nseg = buf[5];
  int *segcount = (int *) buf[3] + Space_For (nseg);
  /* Number of first segment */
  int currentsegnum = *(segcount + Space_For (nelt));
  /* Global index of start of first segment */
  int firstsegindx = *(segcount + Space_For (nelt) + 1);
  /* Global index of first element */
  int firstindx = First_Elt_Here (nelt);
  int mylen = Num_Here (nelt);
  unsigned int *perm = (unsigned int *) buf[6];
  t64bits *tmpsrc = (t64bits *) (perm + Space_For (nelt));
  unsigned int *newscratch = (unsigned int *) (tmpsrc + 2 * Space_For (nelt));
  unsigned xorMask = isUp ? Signbit : ~Signbit;
  int i, j;

  /* Bit-twiddle src into tmpsrc, putting the segment number in each
   * high-order word as we go.  At the same time, initialize perm to
   * contain the global (unsegmented) identity permutation
   */

  /* kludge to compensate for immediate addition when we start the loop*/
  if (thisproc == 0) currentsegnum -= segcount[0];

  j = firstindx;			/* Index for global permutation */
  for (i = 0; i < mylen; i++, j++) {
    if (segcount[i]) {    		/* If a segment(s) start here */
      currentsegnum += segcount[i];	/* ...increment segment number */
    }

    tmpsrc[i].word[0] = currentsegnum;	/* Segment number in high word   */
    tmpsrc[i].word[1] = src[i] ^ xorMask; /* Twiddled source in low word */

    perm [i] = j;
  }

  /* Rank */
  rank_t64bits (perm, tmpsrc, newscratch, nelt, isUp, 
		thisproc, logprocs, nprocs);

  /* Send a segmented identity permutation (segmented index) to perm */
  Prep (nelt);

  /* firstindx is the (global) index of the first element on this
   * vector.  firstsegindx is the (global) start index of segment it
   * belongs to.  So, the difference between the two is the relative
   * index within the segment of the first element on this vector */

  j = firstindx - firstsegindx;		/* Index for relative permutation */
  for (i = 0; i < mylen; i++, j++) {
    if (segcount[i]) {			/* If a segment(s) start here */
      j = 0;				/* ...reset index counter */
    }
    Map_To_Proc_And_Posn (perm[i]);
    Send (int, proc, &dst[posn], j);
    Poll ();
  }
  Wait ();
}


void rku_lez_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  int_rank_seg (thisproc, logprocs, nprocs, buf, 1); /* isUp */
}


void rkd_lez_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  int_rank_seg (thisproc, logprocs, nprocs, buf, 0); /* !isUp */
}


/* -------------------- Unsegmented Double Rank ------------------------ */

/* Doubles are stored in the format
 *    s, e, m
 * where s is sign bit with 1=negative
 *       e is exponent stored in excess form
 *       m is mantissa
 */

/* Needs a scratch area to hold one vector of ints (perm), plus one
 * vector of 64bit structs (tmpsrc), plus the space required by 
 * t64bits_rank (96 bits).  Total == 6.
 */
static void double_rank_unseg (thisproc, logprocs, nprocs, buf, isUp)
int thisproc, logprocs, nprocs, *buf, isUp;
{
  unsigned int *dst = (unsigned int *) buf[1];
  t64bits *src = (t64bits *) buf[2];
  int len = buf[3];
  unsigned int *scratch = (unsigned int *) buf[4];
  int mylen = Num_Here (len);
  int firstindex = First_Elt_Here (len);
  int i, j;
  unsigned int *perm = scratch;
  t64bits *tmpsrc = (t64bits *) (scratch + Space_For (len));
  unsigned int *newscratch = (unsigned int *) (tmpsrc + 2 * Space_For (len));

  /* Bit-twiddle src into tmpsrc.  At the same time,
   * initialize perm to contain the identity permutation.
   */

  for (i = 0, j = firstindex; i < mylen; i++, j++) {
    unsigned int t, sign, fld;

    /* XXX compress all of this into one word-operation */

    /* Convert upper half of double */
    t = src[i].word[0];
    sign = t & Signbit;
    fld = sign ? ~t : t^Signbit;
    tmpsrc[i].word[0] = isUp ? fld : ~fld;

    /* Convert lower half of double */
    t = src[i].word[1];
    fld = sign ? ~t : t;
    tmpsrc[i].word[1] = isUp ? fld : ~fld;

    perm [i] = j;
  }

  /* Rank */
  rank_t64bits (perm, tmpsrc, newscratch, len, isUp, 
		thisproc, logprocs, nprocs);

  /* Send the identity permutation (index vector) to perm */
  Prep (len);
  for (i = 0, j = firstindex; i < mylen; i++, j++) {
    Map_To_Proc_And_Posn (perm[i]);
    Send (int, proc, &dst[posn], j);
    Poll ();
  }
  Wait ();
}


void rku_lud_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  double_rank_unseg (thisproc, logprocs, nprocs, buf, 1); /* isUp */
}


void rkd_lud_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  double_rank_unseg (thisproc, logprocs, nprocs, buf, 0); /* !isUp */
}


/* --------------------- Segmented Double Rank ------------------------- */

/* Needs a scratch area to hold one vector of ints (perm), plus one
 * vector of 96bit structs (tmpsrc), plus the space required by 
 * t96bits_rank (128 bits).  Total == 8.
 */
static void double_rank_seg (thisproc, logprocs, nprocs, buf, isUp)
int thisproc, logprocs, nprocs, *buf, isUp;
{
  unsigned int *dst = (unsigned int *) buf[1];
  t64bits *src = (t64bits *) buf[2];
  int nelt = buf[4];
  int nseg = buf[5];
  int *segcount = (int *) buf[3] + Space_For (nseg);
  /* Number of first segment */
  int currentsegnum = *(segcount + Space_For (nelt));
  /* Global index of start of first segment */
  int firstsegindx = *(segcount + Space_For (nelt) + 1);
  /* Global index of first element */
  int firstindx = First_Elt_Here (nelt);
  int mylen = Num_Here (nelt);
  unsigned int *perm = (unsigned int *) buf[6];
  t96bits *tmpsrc = (t96bits *) (perm + Space_For (nelt));
  unsigned int *newscratch = (unsigned int *) (tmpsrc + 3 * Space_For (nelt));
  int i, j;

  /* Bit-twiddle src into tmpsrc.  At the same time,
   * initialize perm to contain the identity permutation.
   */
  /* kludge to compensate for immediate addition when we start the loop*/
  if (thisproc == 0) currentsegnum -= segcount[0];

  for (i = 0, j = firstindx; i < mylen; i++, j++) {
    unsigned int t, sign, fld;

    if (segcount[i]) {    		/* If a segment(s) start here */
      currentsegnum += segcount[i];	/* ...increment segment number */
    }

    tmpsrc[i].word[0] = currentsegnum;	/* Segment number in high word   */

    /* XXX compress all of this into one word-operation */

    /* Convert upper half of double */
    t = src[i].word[0];
    sign = t & Signbit;
    fld = sign ? ~t : t^Signbit;
    tmpsrc[i].word[1] = isUp ? fld : ~fld;

    /* Convert lower half of double */
    t = src[i].word[1];
    fld = sign ? ~t : t;
    tmpsrc[i].word[2] = isUp ? fld : ~fld;

    perm [i] = j;
  }

  /* Rank */
  rank_t96bits (perm, tmpsrc, newscratch, nelt, isUp, 
		thisproc, logprocs, nprocs);

  /* Send the identity permutation (index vector) to perm */
  Prep (nelt);

  j = firstindx - firstsegindx;
  for (i = 0; i < mylen; i++, j++) {
    if (segcount[i]) {			/* If a segment(s) start here */
      j = 0;				/* ...reset index counter */
    }
    Map_To_Proc_And_Posn (perm[i]);
    Send (int, proc, &dst[posn], j);
    Poll ();
  }
  Wait ();
}


void rku_led_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  double_rank_seg (thisproc, logprocs, nprocs, buf, 1); /* isUp */
}


void rkd_led_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  double_rank_seg (thisproc, logprocs, nprocs, buf, 0); /* !isUp */
}
