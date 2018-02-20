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

#include <malloc.h>
#include <cm/cmmd.h>
#include <cvl.h>
#include "cm5cvl.h"
#include "node.h"


/* ----------------------- Timing Functions -------------------------- */

/* Timer 0 has already been started in the CM5 initialization
 * process.  So stop it, return the max elapsed time, and restart it
 */
void tgt_fos_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  CMMD_node_timer_stop (0);
  CMMD_reduce_to_host_double (CMMD_node_timer_busy(0), CMMD_combiner_dmax);
  CMMD_node_timer_start (0);
}


/* ----------------------- Memory Functions -------------------------- */

void alo_foz_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  vec_p mem = (vec_p) malloc (buf[1]);
  CMMD_reduce_to_host_int ((int) mem, CMMD_combiner_min);
}


void fre_fov_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  free ((char *) buf[1]);
}


/* mov_fov must handle overlaps properly.
 * XXX replace this with a system function?
 */

void mov_fov_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  oneword *dst = (oneword *) buf[1];
  oneword *src = (oneword *) buf[2];
  int mysize = Num_Here (buf[3]);
  int i;

  if (dst < src) {
    for (i = 0; i < mysize; i++) {
      dst[i] = src[i];
    }
  } else {
    for (i = mysize - 1; i >= 0; i--) {
      dst[i] = src[i];
    }
  }
}


/* ---------------------- Segment Descriptors ------------------------ */
/* XXX Would be nice to make the segment start counts into bits; who
 * uses the count except for segmented reduce?	(and can we do that
 * another way?)
 */

/* Increments the destination.
 */
static void int_inc (posn)
int *posn;
{
  *posn = (*posn) + 1;
  num_rcvd++;
}


#define Inc(PROC, DST)							\
    {									\
      int *addr = DST;							\
      if (proc == thisproc) {						\
	*addr += 1;							\
      } else {								\
	CMAML_rpc (PROC, int_inc, addr);				\
	num_sent++;							\
      }									\
    }


void mke_fov_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  unsigned *segstart = (unsigned *) buf[1];
  int *lengths = (int *) buf[2];
  int nseg = buf[3];
  int nelt = buf[4];
  int *segcount = (int *) buf[1] + Space_For (nseg);
  int *firstseg = segcount + Space_For (nelt);
  int *firststart = firstseg + 1;
  int myelt = Num_Here (nelt);
  int myseg, first, i, nsegs = 0, lastseg = 0;

  /* Reset all the segment start counts. */
  for (i = myelt - 1; i >= 0; i--) {
    segcount[i] = 0;
  }

  /* First segment is implicit. */
  if (thisproc == 0) {
    segstart[0] = 0;
  }

  /* Things can go a lot faster if we only have 1 segment.
   */
  if (nseg == 1) {
    if (thisproc == 0) {
      segcount[0] = 1;		/* one segment starts at posn 0 */
      if (lengths[0] == 0) {	/* is segment empty? */
	Set_Empty_Seg (segstart[0]);
      }
    }
    *firstseg = 0;		/* segment number of first element */
    *firststart = 0;		/* start position of first segment */
  } else {
    /* Plus-scan across the lengths, put the results into segstart[].
     * This gives us the starting positions of each segment.
     */
    add_suz_ (thisproc, logprocs, nprocs, buf);

    /* Look at each segment length stored on this processor.  Increment
     * the segment start count corresponding to each segment.  Also, for
     * all empty segments, set their start position to Empty_Seg.
     */
    myseg = Num_Here (nseg);
    Prep (nelt);
    for (i = 0; i < myseg; i++) {
      if (nelt != segstart[i]) {
	Map_To_Proc_And_Posn (segstart[i]);
	Inc (proc, &segcount[posn]);
      }
      if (lengths[i] == 0) {
	Set_Empty_Seg (segstart[i]);
      }
      Poll ();
    }
    Wait ();

    /* Count the number of segment starts on this processor.  Also, find
     * the start address of the last segment.
     */
    first = First_Elt_Here (nelt);
    for (i = myelt - 1; i >= 0; i--) {
      if (segcount[i]) {
	nsegs += segcount[i];
	if (lastseg == 0) lastseg = i + first;
      }
    }

    /* XXX combine these two scans into one vector scan */

    /* Plus-scan across nsegs to get the segment number of the first
     * element on this processor.
     */
    *firstseg = CMMD_scan_int (nsegs, CMMD_combiner_add, CMMD_upward,
			       CMMD_none, 0, CMMD_exclusive) - 1;
    if (*firstseg == -1) *firstseg = 0;

    /* Max-scan across lastseg to get the start address of the first
     * segment on this processor.
     */
    *firststart = CMMD_scan_int (lastseg, CMMD_combiner_max, CMMD_upward,
				 CMMD_none, 0, CMMD_exclusive);
    if (thisproc == 0) {
      *firststart = 0;
    }
  }
}


/* Given a segment descriptor, calculate the lengths of the segments.
 */
void len_fos_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  int *lengths = (int *) buf[1];
  unsigned *segstart = (unsigned *) buf[2];
  int nelt = buf[3];
  int nseg = buf[4];
  int i = 0;
  int sent, rcvd;

  sent = Actual_Value(segstart[0]);
  
  if (thisproc == 0) {
    CMMD_receive_block (1, CMMD_DEFAULT_TAG, &rcvd, sizeof(int));
  } else if (thisproc == nprocs - 1) {
    CMMD_send_block (nprocs - 2, CMMD_DEFAULT_TAG, &sent, sizeof(int));
  } else {
    CMMD_send_and_receive (thisproc + 1, CMMD_DEFAULT_TAG, &rcvd, sizeof(int),
			   thisproc - 1, CMMD_DEFAULT_TAG, &sent, sizeof(int));
  }

  /* Now loop through all the segment start positions on this processor,
   * working out the difference between each pair (== length of a
   * segment).
   * XXX cache to avoid touching every location twice
   */
  for (i = 0; i < Num_Here (nseg) - 1; i++) {
    lengths[i] = Actual_Value (segstart[i+1]) - Actual_Value (segstart[i]);
  }

  /* Compute the last length using the value we received (or the length of
   * the vector, if we're the node with the final segment).
   */
  if (((thisproc + 1) * (Space_For(nseg))) >= nseg) {
    lengths[i] = nelt - Actual_Value (segstart[i]);
  } else {
    lengths[i] = rcvd - Actual_Value (segstart[i]);
  }
}


/* ----------------------- Vector Conversion ------------------------- */

#define Make_v2c(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  int size = sizeof (TYPE) * Space_For (buf[2]);			\
  CMMD_concat_elements_to_host ((void *) buf[1], size);			\
}									\

Make_v2c (v2c_fuz_, int)
Make_v2c (v2c_fud_, double)
Make_v2c (v2c_fub_, cvl_bool)


#define Make_c2v(NAME, TYPE)						\
void NAME (thisproc, logprocs, nprocs, buf)				\
int thisproc, logprocs, nprocs, *buf;					\
{									\
  int size = sizeof (TYPE) * Space_For (buf[2]);			\
  CMMD_receive_element_from_host ((void *) buf[1], size);		\
}									\

Make_c2v (c2v_fuz_, int)
Make_c2v (c2v_fud_, double)
Make_c2v (c2v_fub_, cvl_bool)


/* ------------------------- Random Numbers -------------------------- */

#ifdef FIBRNG
int *genptr = NULL;
extern int *init_rng_d_int();
extern void free_rng_int();
#else
extern void srandom ();
#endif

void rnd_foz_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  int seed = buf[1];
#ifdef FIBRNG
  /* init_rng_d_int has parameters (gennum, length, start, totalgen), */
  /* but we can't change length or start on subsequent calls. */
  if (genptr == NULL) {
    /* The first call -- use default values. */
    genptr = init_rng_d_int (0, 17, 0, nprocs);
  } else {
    /* Throw away old generator, pretend we're initializing (nprocs*(seed+1)) 
       generators, and use the (nprocs*seed)+thisproc 'th. */
    free_rng_int (genptr);
    genptr = init_rng_d_int ((nprocs*seed)+thisproc, 0, 0, (nprocs*(seed+1)));
  }
#else
  srandom ((seed * nprocs) + thisproc);
#endif
}
