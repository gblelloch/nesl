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


#include <stdlib.h>
#include <sys/time.h>
#include <float.h>
#include <mpi.h>
#include <cvl.h>
#include "mpicvl.h"
#include "messages.h"

#if !defined(BUF_SIZE)
#include <stdio.h>
int BUF_SIZE = SENTINEL;

#endif
#if !defined(RCV_EVERY)
int RCV_EVERY = SENTINEL;

#endif


/* ------------------------ Layout Functions ------------------------- */

/* Number of elements stored on this processor in a vector of length
 * nelts.
 */
int _num_here (int nelts)
{
  int space, remaining, result;

  space = SpaceFor (nelts);
  remaining = nelts - FirstHere (nelts);
  result = 0;

  if (remaining > space) {
    result = space;
  } else if (remaining > 0) {
    result = remaining;
  }
  return result;
}


/* ----------------------- Timing Functions -------------------------- */

/* Returns the local time on a processor.
 */
void tgt_fos (cvl_timer_t * t1)
{
  LOG_START (tgt_fos, 0);
  *t1 = MPI_Wtime ();		/* MPI_Wtime returns time in sec */
  LOG_STOP (tgt_fos, 0);
}


#if DUMBTIMING
double tdf_fos (cvl_timer_t * t1, cvl_timer_t * t2)
{
  cvl_timer_t diff;

  LOG_START (tdf_fos, 0);
  diff = *t1 - *t2;
  LOG_STOP (tdf_fos, 0);
  return diff;
}

#else
/* Every processor computes the local difference in times, and then we
 * compute the global average of these differences.
 */
double tdf_fos (cvl_timer_t * t1, cvl_timer_t * t2)
{
  double local, global, result;

  LOG_START (tdf_fos, 0);
  local = *t1 - *t2;
  MPI_Allreduce (&local, &global, 1, MPI_DOUBLE, MPI_SUM,
		 MPI_COMM_WORLD);
  result = global /NumProcs;

  LOG_STOP (tdf_fos, 0);
  return result;
}

#endif


/* ------------------------- Size Functions -------------------------- */


int siz_foz (int length)
{
  int space_per_proc, n_per_space, result;

  LOG_START (siz_foz, 0);
  space_per_proc = SpaceFor (length);
  n_per_space = sizeof (maxalign) / sizeof (int);
  result = (space_per_proc + n_per_space - 1) / n_per_space;
  LOG_STOP (siz_foz, 0);
  return result;
}

int siz_fod (int length)
{
  int space_per_proc, n_per_space, result;

  LOG_START (siz_fod, 0);
  space_per_proc = SpaceFor (length);
  n_per_space = sizeof (maxalign) / sizeof (double);
  result = (space_per_proc + n_per_space - 1) / n_per_space;
  LOG_STOP (siz_fod, 0);
  return result;
}

int siz_fob (int length)
{
  int space_per_proc, n_per_space, result;

  LOG_START (siz_fob, 0);
  space_per_proc = SpaceFor (length);
  n_per_space = sizeof (maxalign) / sizeof (cvl_bool);
  result = (space_per_proc + n_per_space - 1) / n_per_space;
  LOG_STOP (siz_fob, 0);
  return result;
}



int siz_fos (int nelts, int nsegs)
{
  int result;

  LOG_START (siz_fos, 0);
  result = (siz_foz (nsegs) * 2)/* SegdStart, SegdLength */
    +siz_foz (nelts)		/* SegdSegment */
    +(2 * sizeof (int) / sizeof (maxalign));	/* scalars */
  LOG_STOP (siz_fos, 0);
  return result;
}


/* ------------------------ Memory Functions -------------------------*/

/* Return a handle (of type vec_p) to a block of vector memory of size
 * size, otherwise return NULL.	 Size is defined as the number of
 * maxalign-sized units needed per processor.  A vec_p is defined as a
 * maxalign-sized offset into the block of memory on each processor.
 */
vec_p alo_foz (int size)
{
  int in, out;

  LOG_START (alo_foz, size);
  Mem = (maxalign *) malloc (size * sizeof (maxalign));

  /* Did any of the processors return NULL? */
  in = (Mem != NULL);		/* Hopefully this should be 1 on all
				 * processors */
  MPI_Allreduce (&in, &out, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
  LOG_STOP (alo_foz, size);
  return ((out) ? (vec_p) 1 : (vec_p) NULL);
}


/* Free allocated chunk of memory.  Only one alo_foz should ever be
 * performed, so "handle" is redundant.
 */
void fre_fov (vec_p handle)
{
  LOG_START (fre_fov, 0);
  free ((char *) Mem);
  LOG_STOP (fre_fov, 0);
}


/* Copy chunk of memory, where dst may overlap src.
 */
void mov_fov (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  maxalign *dst, *src;
  int i;

  LOG_START (mov_fov, nelts);
  dst = (maxalign *) (Mem + dst_v);
  src = (maxalign *) (Mem + src_v);

  if (dst < src) {
    for (i = 0; i < nelts; i++) {
      dst[i] = src[i];
    }
  } else {
    for (i = nelts - 1; i >= 0; i--) {
      dst[i] = src[i];
    }
  }
  LOG_STOP (mov_fov, nelts);
}

int mov_fov_scratch (int nelts)
{
  return 0;
}



/* ----------------------- Memory arithmetic ------------------------- */

/* A vec_p is just an int, so these are trivial functions.
 */
vec_p add_fov (vec_p v, int size)
{
  int result;

  LOG_START (add_fov, 0);
  result = v + size;
  LOG_STOP (add_fov, 0);
  return result;
}


int sub_fov (vec_p v1, vec_p v2)
{
  int result;

  LOG_START (sub_fov, 0);
  result = abs (v1 - v2);
  LOG_STOP (sub_fov, 0);
  return result;
}


cvl_bool eql_fov (vec_p v1, vec_p v2)
{
  cvl_bool result;

  LOG_START (eql_fov, 0);
  result = (v1 == v2);
  LOG_STOP (eql_fov, 0);
  return result;
}


int cmp_fov (vec_p v1, vec_p v2)
{
  int result;

  LOG_START (cmp_fov, 0);
  result = v1 - v2;
  LOG_STOP (cmp_fov, 0);
  return result;
}


/* ----------------------- Vector Conversion ------------------------- */

/* Given an array distrib of length MAX_PROCS, and a length nelts,
 * calculates how a vector of length nelts would be distributed across
 * the current number of processors (NumProcs), and puts the appropriate
 * element count in each element of the array.
 */
static void calc_distrib (int *distrib, int *displs, int nelts)
{
  int n_per_proc, full_procs, left_over, base, i;

  n_per_proc = SpaceFor (nelts);
  full_procs = nelts / n_per_proc;	/* # procs with n_per_proc */
  left_over = nelts % n_per_proc;	/* what's left over */

  i = 0;
  while (i < full_procs) {
    distrib[i++] = n_per_proc;
  }
  if (left_over != 0) {
    distrib[i++] = left_over;
  }
  while (i < NumProcs) {
    distrib[i++] = 0;
  }
  base = 0;
  for (i = 0; i < NumProcs; i++) {
    displs[i] = base;
    base += distrib[i];
  }
}



void v2c_fuz (int *dst, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *src;
  int n_here;
  int displs[MAX_PROC], distrib[MAX_PROC];

  LOG_START (v2c_fuz, nelts);
  n_here = _num_here (nelts);
  src = (int *) (Mem + src_v);

  if (nelts == 1) {
    if (Self == 0)
      *dst = *src;
    MPI_Bcast (dst, 1, MPI_INT, 0, MPI_COMM_WORLD);
  } else if (nelts != 0) {
    calc_distrib (distrib, displs, nelts);
    MPI_Allgatherv (src, n_here, MPI_INT, dst, distrib, displs,
		    MPI_INT, MPI_COMM_WORLD);
  }
  LOG_STOP (v2c_fuz, nelts);
}

unsigned int v2c_fuz_inplace (void)
{
  return INPLACE_NONE;
}

int v2c_fuz_scratch (int nelts)
{
  return 0;
}

void v2c_fud (double *dst, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *src;
  int n_here;
  int displs[MAX_PROC], distrib[MAX_PROC];

  LOG_START (v2c_fud, nelts);
  n_here = _num_here (nelts);
  src = (double *) (Mem + src_v);

  if (nelts == 1) {
    if (Self == 0)
      *dst = *src;
    MPI_Bcast (dst, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);
  } else if (nelts != 0) {
    calc_distrib (distrib, displs, nelts);
    MPI_Allgatherv (src, n_here, MPI_DOUBLE, dst, distrib, displs,
		    MPI_DOUBLE, MPI_COMM_WORLD);
  }
  LOG_STOP (v2c_fud, nelts);
}

unsigned int v2c_fud_inplace (void)
{
  return INPLACE_NONE;
}

int v2c_fud_scratch (int nelts)
{
  return 0;
}

void v2c_fub (cvl_bool * dst, vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *src;
  int n_here;
  int displs[MAX_PROC], distrib[MAX_PROC];

  LOG_START (v2c_fub, nelts);
  n_here = _num_here (nelts);
  src = (cvl_bool *) (Mem + src_v);

  if (nelts == 1) {
    if (Self == 0)
      *dst = *src;
    MPI_Bcast (dst, 1, MPI_BOOL, 0, MPI_COMM_WORLD);
  } else if (nelts != 0) {
    calc_distrib (distrib, displs, nelts);
    MPI_Allgatherv (src, n_here, MPI_BOOL, dst, distrib, displs,
		    MPI_BOOL, MPI_COMM_WORLD);
  }
  LOG_STOP (v2c_fub, nelts);
}

unsigned int v2c_fub_inplace (void)
{
  return INPLACE_NONE;
}

int v2c_fub_scratch (int nelts)
{
  return 0;
}




void c2v_fuz (vec_p dst_v, int *src, int nelts, vec_p scratch_v)
{
  int *dst;
  int n_here, offset, i;

  LOG_START (c2v_fuz, nelts);
  n_here = _num_here (nelts);
  offset = FirstHere (nelts);
  dst = (int *) (Mem + dst_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = src[offset + i];
  }
  LOG_STOP (c2v_fuz, nelts);
}

unsigned int c2v_fuz_inplace (void)
{
  return INPLACE_NONE;
}

int c2v_fuz_scratch (int nelts)
{
  return 0;
}

void c2v_fud (vec_p dst_v, double *src, int nelts, vec_p scratch_v)
{
  double *dst;
  int n_here, offset, i;

  LOG_START (c2v_fud, nelts);
  n_here = _num_here (nelts);
  offset = FirstHere (nelts);
  dst = (double *) (Mem + dst_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = src[offset + i];
  }
  LOG_STOP (c2v_fud, nelts);
}

unsigned int c2v_fud_inplace (void)
{
  return INPLACE_NONE;
}

int c2v_fud_scratch (int nelts)
{
  return 0;
}

void c2v_fub (vec_p dst_v, cvl_bool * src, int nelts, vec_p scratch_v)
{
  cvl_bool *dst;
  int n_here, offset, i;

  LOG_START (c2v_fub, nelts);
  n_here = _num_here (nelts);
  offset = FirstHere (nelts);
  dst = (cvl_bool *) (Mem + dst_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = src[offset + i];
  }
  LOG_STOP (c2v_fub, nelts);
}

unsigned int c2v_fub_inplace (void)
{
  return INPLACE_NONE;
}

int c2v_fub_scratch (int nelts)
{
  return 0;
}



/* ---------------------- Segment Descriptors ------------------------ */

/* A segment descriptor contains the following vectors:
 *  1) length (nseg ints): Length of each segment.
 *  2) start (nseg ints): Start address of each segment.
 *  3) segment (nelt ints): Segment each element is in.
 * and the following integers:
 *  4) before : Segment just before this processor; used to initialize
 *     loops that compare segment[i] to current_seg to detect
 *     the start of a segment.
 *  5) first : (Global) start offset of first segment on this proc;
 *     used in e.g. segmented permutes (add index to offset of
 *     start of segment to find offset to send to).
 *
 * There's a macro (SegdLength, SegdStart, SegdSegment, SegdBefore,
 * SegdFirst  - see mpicvl.h) to return a pointer to each of these.
 */
void mke_fov (vec_p segd_v, vec_p lengths_v, int nelts, int nsegs,
	       vec_p scratch_v)
{
  int *segd, *src, *length, *start, *segment, *before_p, *first_p;
  int nelts_here, nsegs_here, max_index, result, accum, i;

  LOG_START (mke_fov, nelts);
  nsegs_here = _num_here (nsegs);
  nelts_here = _num_here (nelts);
  segd = (int *) (Mem + segd_v);
  src = (int *) (Mem + lengths_v);
  length = SegdLength (segd, nelts, nsegs);
  start = SegdStart (segd, nelts, nsegs);
  segment = SegdSegment (segd, nelts, nsegs);
  before_p = SegdBefore (segd, nelts, nsegs);
  first_p = SegdFirst (segd, nelts, nsegs);

  /* Copy the segment lengths over. */
  for (i = 0; i < nsegs_here; i++) {
    length[i] = src[i];
  }

  /* Initialize segment-this-element-is-in vector. */
  for (i = 0; i < nelts_here; i++) {
    segment[i] = 0;
  }

  /* Things can go faster if we only have 1 segment. */
  if (nsegs == 1) {
    start[0] = 0;
    *before_p = (Self == 0) ? -1 : 0;
    *first_p = 0;
  } else {
    /* Add-scan across the segment lengths to give segment starts. */
    accum = 0;
    for (i = 0; i < nsegs_here; i++) {
      int swap = accum;

      accum += length[i];
      start[i] = swap;
    }

    MPI_Scan (&accum, &result, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    result -= accum;		/* convert inclusive to exclusive */

    for (i = 0; i < nsegs_here; i++) {
      start[i] += result;
    }

    max_index = SENTINEL;

    /* Increment start-of-segment positions. */
    if (nelts != 0) {
      int n_sent = 0, n_rcvd = 0;
      int space = SpaceFor (nelts);
      int x, y, loopbound;

      for (y = 0; y < nsegs_here; y += RCV_EVERY) {
	loopbound = min (nsegs_here, y + RCV_EVERY);
	for (x = y; x < loopbound; x++) {
	  if (start[x] != nelts) {
	    int proc = start[x] / space;
	    int offset = start[x] - (proc * space);

	    if (proc == Self) {
	      segment[offset]++;
	      if (offset > max_index)
		max_index = offset;
	    } else {
	      int posn = Send_ctr[proc]++;
	      int *buf = (int *) Usr_send[proc];

	      buf[posn] = offset;
	    }
	  }
	}
	n_rcvd += _recv_increment (segment, mke_fov_tag, &max_index);
	for (x = 0; x < NumProcs; x++) {
	  if (Send_ctr[x] >= ((BUF_SIZE / sizeof (int)) - RCV_EVERY)) {
	    _send_buffer (x, mke_fov_tag, (Send_ctr[x] * sizeof (int)));
	    n_sent++;
	  }
	}
      }
      _finish_increment (segment, mke_fov_tag, n_sent, n_rcvd,
			 &max_index);
    }
    /* If we received anything, we have the index of the last segment
     * start on this processor; do a max_scan across them (effectively
     * a copy-scan) to set SegdStart correctly. */
    if (max_index != SENTINEL) {
      *first_p = max_index + FirstHere (nelts);	/* convert to global */
    } else {
      *first_p = 0;
    }

    _exclusive_max_scan (first_p);

    /* Do an inclusive add-scan across segment. */
    accum = 0;
    for (i = 0; i < nelts_here; i++) {
      accum += segment[i];
      segment[i] = accum;
    }

    /* accum now contains the number of segment starts on each proc,
     * and segment[] contains the local segment numbers plus one.  We
     * need to convert the local segment numbers into global segment
     * numbers. */

    /* Do an exclusive add-scan across accum. */
    MPI_Scan (&accum, &result, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    result -= accum;		/* turn inclusive into exclusive */

    /* Add (result-1) to all the entries in segment, to give correct
     * global segment numbers. */
    result--;
    for (i = 0; i < nelts_here; i++) {
      segment[i] += result;
    }
    *before_p = result;
  }
  LOG_STOP (mke_fov, nelts);
}
int mke_fov_scratch (int nelts, int nsegs)
{
  return 0;
}

unsigned int mke_fov_inplace (void)
{
  return INPLACE_NONE;
}



void len_fos (vec_p dst_v, vec_p segd_v, int nelts, int nsegs,
	       vec_p scratch_v)
{
  int *dst, *segd, *length;
  int nsegs_here, i;

  LOG_START (len_fos, nelts);
  nsegs_here = _num_here (nsegs);
  dst = (int *) (Mem + dst_v);
  segd = (int *) (Mem + segd_v);
  length = (int *) SegdLength (segd, nelts, nsegs);

  /* Just copy the segment lengths directly from segd. */
  for (i = 0; i < nsegs_here; i++) {
    dst[i] = length[i];
  }
  LOG_STOP (len_fos, nelts);
}

int len_fos_scratch (int nelts, int nsegs)
{
  return 0;
}

unsigned int len_fos_inplace (void)
{
  return INPLACE_1;
}



#if defined(FIBRNG)
int *genptr = NULL;
extern int *init_rng_d_int (int gennum, int length, int seed, int total_gen);
extern void free_rng_int (int *genptr);

#elif defined(__hpux)
extern void srandom srand48 (long seed);

#define SRANDOM(x) srand48((long) x)
#elif defined(__alpha) && defined(__osf__)
extern int srandom (unsigned seed);

#define SRANDOM(x) srandom((unsigned) x)
#elif defined(MPI_t3d)
extern int srandom (int seed);

#define SRANDOM(x) srandom((int) x)
#else
extern void srandom (int seed);

#define SRANDOM(x) srandom((int) x)
#endif

void rnd_foz (int seed)
{
  LOG_START (rnd_foz, 1);
#ifdef FIBRNG
  /* init_rng_d_int has parameters (gennum, length, start, totalgen), */
  /* but we can't change length or start on subsequent calls. */
  if (genptr == NULL) {
    /* The first call -- use default values. */
    genptr = init_rng_d_int (0, 17, 0, NumProcs);
  } else {
    /* Throw away old generator, pretend we're initializing (nprocs *
     * (seed+1)) generators, and use the (nprocs * seed) + self'th. */
    free_rng_int (genptr);
    genptr = init_rng_d_int ((NumProcs * seed) + Self, 0, 0, (NumProcs * (seed + 1)));
  }
#else
  SRANDOM ((seed * NumProcs) + Self);
#endif
  LOG_STOP (rnd_foz, 1);
}


/* --------------------- Startup and Shutdown ------------------------ */

/* Needs extra indirection through command-line arguments so they're in
 * the correct format for MPI_Init.
 */
void CVL_init (int *p_argc, char ***p_argv)
{
  /* Initialize MPI, stripping MPI-specific flags from command line */
  MPI_Init (p_argc, p_argv);

#ifdef CVLPROFILE
  MPE_Init_log ();
#endif

  _set_self_and_numprocs ();

#ifdef CVLPROFILE
  MPE_Describe_state (max_wuz_START, max_wuz_STOP, "max_wuz", "MidnightBlue");
  MPE_Describe_state (min_wuz_START, min_wuz_STOP, "min_wuz", "NavyBlue");
  MPE_Describe_state (add_wuz_START, add_wuz_STOP, "add_wuz", "CornflowerBlue");
  MPE_Describe_state (sub_wuz_START, sub_wuz_STOP, "sub_wuz", "DarkSlateBlue");
  MPE_Describe_state (mul_wuz_START, mul_wuz_STOP, "mul_wuz", "SlateBlue");
  MPE_Describe_state (div_wuz_START, div_wuz_STOP, "div_wuz", "MediumSlateBlue");
  MPE_Describe_state (max_wud_START, max_wud_STOP, "max_wud", "LightSlateBlue");
  MPE_Describe_state (min_wud_START, min_wud_STOP, "min_wud", "MediumBlue");
  MPE_Describe_state (add_wud_START, add_wud_STOP, "add_wud", "RoyalBlue");
  MPE_Describe_state (sub_wud_START, sub_wud_STOP, "sub_wud", "blue");
  MPE_Describe_state (mul_wud_START, mul_wud_STOP, "mul_wud", "DodgerBlue");
  MPE_Describe_state (div_wud_START, div_wud_STOP, "div_wud", "DeepSkyBlue");
  MPE_Describe_state (grt_wuz_START, grt_wuz_STOP, "grt_wuz", "SkyBlue");
  MPE_Describe_state (les_wuz_START, les_wuz_STOP, "les_wuz", "LightSkyBlue");
  MPE_Describe_state (geq_wuz_START, geq_wuz_STOP, "geq_wuz", "LightSteelBlue");
  MPE_Describe_state (leq_wuz_START, leq_wuz_STOP, "leq_wuz", "LightBlue");
  MPE_Describe_state (grt_wud_START, grt_wud_STOP, "grt_wud", "PowderBlue");
  MPE_Describe_state (les_wud_START, les_wud_STOP, "les_wud", "PaleTurquoise");
  MPE_Describe_state (geq_wud_START, geq_wud_STOP, "geq_wud", "DarkTurquoise");
  MPE_Describe_state (leq_wud_START, leq_wud_STOP, "leq_wud", "MediumTurquoise");
  MPE_Describe_state (eql_wub_START, eql_wub_STOP, "eql_wub", "turquoise");
  MPE_Describe_state (neq_wub_START, neq_wub_STOP, "neq_wub", "cyan");
  MPE_Describe_state (eql_wuz_START, eql_wuz_STOP, "eql_wuz", "LightGreen");
  MPE_Describe_state (neq_wuz_START, neq_wuz_STOP, "neq_wuz", "LightCyan");
  MPE_Describe_state (eql_wud_START, eql_wud_STOP, "eql_wud", "CadetBlue");
  MPE_Describe_state (neq_wud_START, neq_wud_STOP, "neq_wud", "MediumAquamarine");
  MPE_Describe_state (lsh_wuz_START, lsh_wuz_STOP, "lsh_wuz", "aquamarine");
  MPE_Describe_state (rsh_wuz_START, rsh_wuz_STOP, "rsh_wuz", "DarkGreen");
  MPE_Describe_state (mod_wuz_START, mod_wuz_STOP, "mod_wuz", "DarkOliveGreen");
  MPE_Describe_state (rnd_wuz_START, rnd_wuz_STOP, "rnd_wuz", "DarkSeaGreen");
  MPE_Describe_state (sel_wuz_START, sel_wuz_STOP, "sel_wuz", "SeaGreen");
  MPE_Describe_state (sel_wud_START, sel_wud_STOP, "sel_wud", "MediumSeaGreen");
  MPE_Describe_state (sel_wub_START, sel_wub_STOP, "sel_wub", "LightSeaGreen");
  MPE_Describe_state (not_wub_START, not_wub_STOP, "not_wub", "PaleGreen");
  MPE_Describe_state (xor_wub_START, xor_wub_STOP, "xor_wub", "SpringGreen");
  MPE_Describe_state (ior_wub_START, ior_wub_STOP, "ior_wub", "LawnGreen");
  MPE_Describe_state (and_wub_START, and_wub_STOP, "and_wub", "green");
  MPE_Describe_state (not_wuz_START, not_wuz_STOP, "not_wuz", "chartreuse");
  MPE_Describe_state (xor_wuz_START, xor_wuz_STOP, "xor_wuz", "MediumSpringGreen");
  MPE_Describe_state (ior_wuz_START, ior_wuz_STOP, "ior_wuz", "GreenYellow");
  MPE_Describe_state (and_wuz_START, and_wuz_STOP, "and_wuz", "LimeGreen");
  MPE_Describe_state (flr_wud_START, flr_wud_STOP, "flr_wud", "YellowGreen");
  MPE_Describe_state (cei_wud_START, cei_wud_STOP, "cei_wud", "ForestGreen");
  MPE_Describe_state (trn_wud_START, trn_wud_STOP, "trn_wud", "OliveDrab");
  MPE_Describe_state (rou_wud_START, rou_wud_STOP, "rou_wud", "DarkKhaki");
  MPE_Describe_state (exp_wud_START, exp_wud_STOP, "exp_wud", "khaki");
  MPE_Describe_state (log_wud_START, log_wud_STOP, "log_wud", "PaleGoldenrod");
  MPE_Describe_state (sqt_wud_START, sqt_wud_STOP, "sqt_wud", "LightGoldenrodYellow");
  MPE_Describe_state (sin_wud_START, sin_wud_STOP, "sin_wud", "LightYellow");
  MPE_Describe_state (cos_wud_START, cos_wud_STOP, "cos_wud", "yellow");
  MPE_Describe_state (tan_wud_START, tan_wud_STOP, "tan_wud", "gold");
  MPE_Describe_state (asn_wud_START, asn_wud_STOP, "asn_wud", "LightGoldenrod");
  MPE_Describe_state (acs_wud_START, acs_wud_STOP, "acs_wud", "goldenrod");
  MPE_Describe_state (atn_wud_START, atn_wud_STOP, "atn_wud", "DarkGoldenrod");
  MPE_Describe_state (snh_wud_START, snh_wud_STOP, "snh_wud", "RosyBrown");
  MPE_Describe_state (csh_wud_START, csh_wud_STOP, "csh_wud", "IndianRed");
  MPE_Describe_state (tnh_wud_START, tnh_wud_STOP, "tnh_wud", "SaddleBrown");
  MPE_Describe_state (int_wud_START, int_wud_STOP, "int_wud", "sienna");
  MPE_Describe_state (int_wub_START, int_wub_STOP, "int_wub", "peru");
  MPE_Describe_state (dbl_wuz_START, dbl_wuz_STOP, "dbl_wuz", "burlywood");
  MPE_Describe_state (boo_wuz_START, boo_wuz_STOP, "boo_wuz", "beige");
  MPE_Describe_state (cpy_wuz_START, cpy_wuz_STOP, "cpy_wuz", "wheat");
  MPE_Describe_state (cpy_wud_START, cpy_wud_STOP, "cpy_wud", "SandyBrown");
  MPE_Describe_state (cpy_wub_START, cpy_wub_STOP, "cpy_wub", "tan");
  MPE_Describe_state (cpy_wus_START, cpy_wus_STOP, "cpy_wus", "chocolate");
  MPE_Describe_state (alo_foz_START, alo_foz_STOP, "alo_foz", "DarkOrange");
  MPE_Describe_state (fre_fov_START, fre_fov_STOP, "fre_fov", "coral");
  MPE_Describe_state (mov_fov_START, mov_fov_STOP, "mov_fov", "LightCoral");
  MPE_Describe_state (tgt_fos_START, tgt_fos_STOP, "tgt_fos", "firebrick");
  MPE_Describe_state (tdf_fos_START, tdf_fos_STOP, "tdf_fos", "brown");
  MPE_Describe_state (siz_foz_START, siz_foz_STOP, "siz_foz", "DarkSalmon");
  MPE_Describe_state (siz_fod_START, siz_fod_STOP, "siz_fod", "salmon");
  MPE_Describe_state (siz_fob_START, siz_fob_STOP, "siz_fob", "LightSalmon");
  MPE_Describe_state (siz_fos_START, siz_fos_STOP, "siz_fos", "orange");
  MPE_Describe_state (add_fov_START, add_fov_STOP, "add_fov", "tomato");
  MPE_Describe_state (sub_fov_START, sub_fov_STOP, "sub_fov", "OrangeRed");
  MPE_Describe_state (eql_fov_START, eql_fov_STOP, "eql_fov", "red");
  MPE_Describe_state (cmp_fov_START, cmp_fov_STOP, "cmp_fov", "HotPink");
  MPE_Describe_state (v2c_fuz_START, v2c_fuz_STOP, "v2c_fuz", "DeepPink");
  MPE_Describe_state (v2c_fud_START, v2c_fud_STOP, "v2c_fud", "pink");
  MPE_Describe_state (v2c_fub_START, v2c_fub_STOP, "v2c_fub", "LightPink");
  MPE_Describe_state (c2v_fuz_START, c2v_fuz_STOP, "c2v_fuz", "PaleVioletRed");
  MPE_Describe_state (c2v_fud_START, c2v_fud_STOP, "c2v_fud", "maroon");
  MPE_Describe_state (c2v_fub_START, c2v_fub_STOP, "c2v_fub", "MediumVioletRed");
  MPE_Describe_state (mke_fov_START, mke_fov_STOP, "mke_fov", "VioletRed");
  MPE_Describe_state (len_fos_START, len_fos_STOP, "len_fos", "magenta");
  MPE_Describe_state (pk1_luv_START, pk1_luv_STOP, "pk1_luv", "violet");
  MPE_Describe_state (pk2_luz_START, pk2_luz_STOP, "pk2_luz", "plum");
  MPE_Describe_state (pk2_lud_START, pk2_lud_STOP, "pk2_lud", "orchid");
  MPE_Describe_state (pk2_lub_START, pk2_lub_STOP, "pk2_lub", "MediumOrchid");
  MPE_Describe_state (pk1_lev_START, pk1_lev_STOP, "pk1_lev", "DarkOrchid");
  MPE_Describe_state (pk2_lez_START, pk2_lez_STOP, "pk2_lez", "DarkViolet");
  MPE_Describe_state (pk2_leb_START, pk2_leb_STOP, "pk2_leb", "BlueViolet");
  MPE_Describe_state (pk2_led_START, pk2_led_STOP, "pk2_led", "purple");
  MPE_Describe_state (ind_luz_START, ind_luz_STOP, "ind_luz", "MediumPurple");
  MPE_Describe_state (ind_lez_START, ind_lez_STOP, "ind_lez", "thistle");
  MPE_Describe_state (rku_luz_START, rku_luz_STOP, "rku_luz", "snow1");
  MPE_Describe_state (rkd_luz_START, rkd_luz_STOP, "rkd_luz", "snow4");
  MPE_Describe_state (rku_lez_START, rku_lez_STOP, "rku_lez", "seashell1");
  MPE_Describe_state (rkd_lez_START, rkd_lez_STOP, "rkd_lez", "seashell4");
  MPE_Describe_state (rku_lud_START, rku_lud_STOP, "rku_lud", "AntiqueWhite1");
  MPE_Describe_state (rkd_lud_START, rkd_lud_STOP, "rkd_lud", "AntiqueWhite4");
  MPE_Describe_state (rku_led_START, rku_led_STOP, "rku_led", "bisque1");
  MPE_Describe_state (rkd_led_START, rkd_led_STOP, "rkd_led", "bisque4");
  MPE_Describe_state (smp_puz_START, smp_puz_STOP, "smp_puz", "PeachPuff1");
  MPE_Describe_state (smp_pud_START, smp_pud_STOP, "smp_pud", "PeachPuff4");
  MPE_Describe_state (smp_pub_START, smp_pub_STOP, "smp_pub", "NavajoWhite1");
  MPE_Describe_state (fpm_puz_START, fpm_puz_STOP, "fpm_puz", "NavajoWhite4");
  MPE_Describe_state (fpm_pud_START, fpm_pud_STOP, "fpm_pud", "LemonChiffon1");
  MPE_Describe_state (fpm_pub_START, fpm_pub_STOP, "fpm_pub", "LemonChiffon4");
  MPE_Describe_state (dpe_puz_START, dpe_puz_STOP, "dpe_puz", "cornsilk1");
  MPE_Describe_state (dpe_pud_START, dpe_pud_STOP, "dpe_pud", "cornsilk4");
  MPE_Describe_state (dpe_pub_START, dpe_pub_STOP, "dpe_pub", "ivory1");
  MPE_Describe_state (dfp_puz_START, dfp_puz_STOP, "dfp_puz", "ivory4");
  MPE_Describe_state (dfp_pud_START, dfp_pud_STOP, "dfp_pud", "honeydew1");
  MPE_Describe_state (dfp_pub_START, dfp_pub_STOP, "dfp_pub", "honeydew4");
  MPE_Describe_state (bck_puz_START, bck_puz_STOP, "bck_puz", "LavenderBlush1");
  MPE_Describe_state (bck_pud_START, bck_pud_STOP, "bck_pud", "LavenderBlush4");
  MPE_Describe_state (bck_pub_START, bck_pub_STOP, "bck_pub", "MistyRose1");
  MPE_Describe_state (bfp_puz_START, bfp_puz_STOP, "bfp_puz", "MistyRose4");
  MPE_Describe_state (bfp_pud_START, bfp_pud_STOP, "bfp_pud", "azure1");
  MPE_Describe_state (bfp_pub_START, bfp_pub_STOP, "bfp_pub", "azure4");
  MPE_Describe_state (smp_pez_START, smp_pez_STOP, "smp_pez", "SlateBlue1");
  MPE_Describe_state (smp_ped_START, smp_ped_STOP, "smp_ped", "SlateBlue4");
  MPE_Describe_state (smp_peb_START, smp_peb_STOP, "smp_peb", "RoyalBlue1");
  MPE_Describe_state (fpm_pez_START, fpm_pez_STOP, "fpm_pez", "RoyalBlue4");
  MPE_Describe_state (fpm_ped_START, fpm_ped_STOP, "fpm_ped", "blue1");
  MPE_Describe_state (fpm_peb_START, fpm_peb_STOP, "fpm_peb", "blue4");
  MPE_Describe_state (dpe_pez_START, dpe_pez_STOP, "dpe_pez", "DodgerBlue1");
  MPE_Describe_state (dpe_ped_START, dpe_ped_STOP, "dpe_ped", "DodgerBlue4");
  MPE_Describe_state (dpe_peb_START, dpe_peb_STOP, "dpe_peb", "SteelBlue1");
  MPE_Describe_state (bck_pez_START, bck_pez_STOP, "bck_pez", "SteelBlue4");
  MPE_Describe_state (bck_ped_START, bck_ped_STOP, "bck_ped", "DeepSkyBlue1");
  MPE_Describe_state (bck_peb_START, bck_peb_STOP, "bck_peb", "DeepSkyBlue4");
  MPE_Describe_state (bfp_pez_START, bfp_pez_STOP, "bfp_pez", "SkyBlue1");
  MPE_Describe_state (bfp_ped_START, bfp_ped_STOP, "bfp_ped", "SkyBlue4");
  MPE_Describe_state (bfp_peb_START, bfp_peb_STOP, "bfp_peb", "LightSkyBlue1");
  MPE_Describe_state (dfp_pez_START, dfp_pez_STOP, "dfp_pez", "LightSkyBlue4");
  MPE_Describe_state (dfp_ped_START, dfp_ped_STOP, "dfp_ped", "SlateGray1");
  MPE_Describe_state (dfp_peb_START, dfp_peb_STOP, "dfp_peb", "SlateGray4");
  MPE_Describe_state (add_ruz_START, add_ruz_STOP, "add_ruz", "LightSteelBlue1");
  MPE_Describe_state (mul_ruz_START, mul_ruz_STOP, "mul_ruz", "LightSteelBlue4");
  MPE_Describe_state (max_ruz_START, max_ruz_STOP, "max_ruz", "LightBlue1");
  MPE_Describe_state (min_ruz_START, min_ruz_STOP, "min_ruz", "LightBlue4");
  MPE_Describe_state (and_ruz_START, and_ruz_STOP, "and_ruz", "LightCyan1");
  MPE_Describe_state (ior_ruz_START, ior_ruz_STOP, "ior_ruz", "LightCyan4");
  MPE_Describe_state (xor_ruz_START, xor_ruz_STOP, "xor_ruz", "PaleTurquoise1");
  MPE_Describe_state (add_rud_START, add_rud_STOP, "add_rud", "PaleTurquoise4");
  MPE_Describe_state (mul_rud_START, mul_rud_STOP, "mul_rud", "CadetBlue1");
  MPE_Describe_state (max_rud_START, max_rud_STOP, "max_rud", "CadetBlue4");
  MPE_Describe_state (min_rud_START, min_rud_STOP, "min_rud", "turquoise1");
  MPE_Describe_state (and_rub_START, and_rub_STOP, "and_rub", "turquoise4");
  MPE_Describe_state (ior_rub_START, ior_rub_STOP, "ior_rub", "cyan1");
  MPE_Describe_state (xor_rub_START, xor_rub_STOP, "xor_rub", "cyan4");
  MPE_Describe_state (add_suz_START, add_suz_STOP, "add_suz", "DarkSlateGray1");
  MPE_Describe_state (mul_suz_START, mul_suz_STOP, "mul_suz", "DarkSlateGray4");
  MPE_Describe_state (and_suz_START, and_suz_STOP, "and_suz", "aquamarine1");
  MPE_Describe_state (xor_suz_START, xor_suz_STOP, "xor_suz", "aquamarine4");
  MPE_Describe_state (add_sud_START, add_sud_STOP, "add_sud", "DarkSeaGreen1");
  MPE_Describe_state (mul_sud_START, mul_sud_STOP, "mul_sud", "DarkSeaGreen4");
  MPE_Describe_state (and_sub_START, and_sub_STOP, "and_sub", "SeaGreen1");
  MPE_Describe_state (xor_sub_START, xor_sub_STOP, "xor_sub", "SeaGreen4");
  MPE_Describe_state (max_suz_START, max_suz_STOP, "max_suz", "PaleGreen1");
  MPE_Describe_state (min_suz_START, min_suz_STOP, "min_suz", "PaleGreen4");
  MPE_Describe_state (ior_suz_START, ior_suz_STOP, "ior_suz", "SpringGreen1");
  MPE_Describe_state (max_sud_START, max_sud_STOP, "max_sud", "SpringGreen4");
  MPE_Describe_state (min_sud_START, min_sud_STOP, "min_sud", "green1");
  MPE_Describe_state (ior_sub_START, ior_sub_STOP, "ior_sub", "green4");
  MPE_Describe_state (add_rez_START, add_rez_STOP, "add_rez", "chartreuse1");
  MPE_Describe_state (mul_rez_START, mul_rez_STOP, "mul_rez", "chartreuse4");
  MPE_Describe_state (max_rez_START, max_rez_STOP, "max_rez", "OliveDrab1");
  MPE_Describe_state (min_rez_START, min_rez_STOP, "min_rez", "OliveDrab4");
  MPE_Describe_state (and_rez_START, and_rez_STOP, "and_rez", "DarkOliveGreen1");
  MPE_Describe_state (ior_rez_START, ior_rez_STOP, "ior_rez", "DarkOliveGreen4");
  MPE_Describe_state (xor_rez_START, xor_rez_STOP, "xor_rez", "khaki1");
  MPE_Describe_state (add_red_START, add_red_STOP, "add_red", "khaki4");
  MPE_Describe_state (mul_red_START, mul_red_STOP, "mul_red", "LightGoldenrod1");
  MPE_Describe_state (max_red_START, max_red_STOP, "max_red", "LightGoldenrod4");
  MPE_Describe_state (min_red_START, min_red_STOP, "min_red", "LightYellow1");
  MPE_Describe_state (and_reb_START, and_reb_STOP, "and_reb", "LightYellow4");
  MPE_Describe_state (ior_reb_START, ior_reb_STOP, "ior_reb", "yellow1");
  MPE_Describe_state (xor_reb_START, xor_reb_STOP, "xor_reb", "yellow4");
  MPE_Describe_state (add_sez_START, add_sez_STOP, "add_sez", "gold1");
  MPE_Describe_state (mul_sez_START, mul_sez_STOP, "mul_sez", "gold4");
  MPE_Describe_state (max_sez_START, max_sez_STOP, "max_sez", "goldenrod1");
  MPE_Describe_state (min_sez_START, min_sez_STOP, "min_sez", "goldenrod4");
  MPE_Describe_state (and_sez_START, and_sez_STOP, "and_sez", "DarkGoldenrod1");
  MPE_Describe_state (ior_sez_START, ior_sez_STOP, "ior_sez", "DarkGoldenrod4");
  MPE_Describe_state (xor_sez_START, xor_sez_STOP, "xor_sez", "RosyBrown1");
  MPE_Describe_state (add_sed_START, add_sed_STOP, "add_sed", "RosyBrown4");
  MPE_Describe_state (mul_sed_START, mul_sed_STOP, "mul_sed", "IndianRed1");
  MPE_Describe_state (max_sed_START, max_sed_STOP, "max_sed", "IndianRed4");
  MPE_Describe_state (min_sed_START, min_sed_STOP, "min_sed", "sienna1");
  MPE_Describe_state (and_seb_START, and_seb_STOP, "and_seb", "sienna4");
  MPE_Describe_state (ior_seb_START, ior_seb_STOP, "ior_seb", "burlywood1");
  MPE_Describe_state (xor_seb_START, xor_seb_STOP, "xor_seb", "burlywood4");
  MPE_Describe_state (dis_vuz_START, dis_vuz_STOP, "dis_vuz", "wheat1");
  MPE_Describe_state (dis_vud_START, dis_vud_STOP, "dis_vud", "wheat4");
  MPE_Describe_state (dis_vub_START, dis_vub_STOP, "dis_vub", "tan1");
  MPE_Describe_state (rep_vuz_START, rep_vuz_STOP, "rep_vuz", "tan4");
  MPE_Describe_state (rep_vud_START, rep_vud_STOP, "rep_vud", "chocolate1");
  MPE_Describe_state (rep_vub_START, rep_vub_STOP, "rep_vub", "chocolate4");
  MPE_Describe_state (ext_vuz_START, ext_vuz_STOP, "ext_vuz", "firebrick1");
  MPE_Describe_state (ext_vud_START, ext_vud_STOP, "ext_vud", "firebrick4");
  MPE_Describe_state (ext_vub_START, ext_vub_STOP, "ext_vub", "brown1");
  MPE_Describe_state (dis_vez_START, dis_vez_STOP, "dis_vez", "brown4");
  MPE_Describe_state (dis_ved_START, dis_ved_STOP, "dis_ved", "salmon1");
  MPE_Describe_state (dis_veb_START, dis_veb_STOP, "dis_veb", "thistle4");
  MPE_Describe_state (rep_vez_START, rep_vez_STOP, "rep_vez", "grey50");
  MPE_Describe_state (rep_ved_START, rep_ved_STOP, "rep_ved", "grey100");
  MPE_Describe_state (rep_veb_START, rep_veb_STOP, "rep_veb", "DarkBlue");
  MPE_Describe_state (ext_vez_START, ext_vez_STOP, "ext_vez", "DarkCyan");
  MPE_Describe_state (ext_ved_START, ext_ved_STOP, "ext_ved", "DarkMagenta");
  MPE_Describe_state (ext_veb_START, ext_veb_STOP, "ext_veb", "DarkRed");
#endif

#if !defined(BUF_SIZE)
  /* The user should set his own buffer size and receive parameters.
   * For simplicity, we only look at the end of the command line, and
   * strip any flags we find from it. */
  {
    int argc = *p_argc;
    char **argv = *p_argv;

    /* Search for -bxxx -ryyy as last two command-line flags */
    if ((argv[argc - 2][1] == 'b') && (argv[argc - 1][1] == 'r')) {
      BUF_SIZE = atoi (argv[argc - 2] + 2);
      RCV_EVERY = atoi (argv[argc - 1] + 2);
      *p_argc = argc - 2;

      if (Self == 0)
	printf ("Buffer size %d, receive every %d\n", BUF_SIZE,
		RCV_EVERY);
    } else {
      if (Self == 0)
	printf ("Couldn't find -bxxx -ryyy as last two flags\n");
      exit (1);
    }
  }
#endif

  /* Set up our own segmented scan operators. */
  _setup_scan_ops ();

  /* Set up buffers and receives. */
  _setup_buffers ();

  /* Initialize random numbers */
  rnd_foz (0);

  /* Make sure everyone has receives in place before continuing . */
  MPI_Barrier (MPI_COMM_WORLD);
}


void CVL_quit (void)
{
#ifdef CVLPROFILE
  MPE_Finish_log ("cvl.log");
#endif
  MPI_Finalize ();
}
