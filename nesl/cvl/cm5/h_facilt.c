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

#include <memory.h>
#include <malloc.h>
#include <assert.h>
#include <cm/cmmd.h>
#include <cvl.h>
#include "cm5cvl.h"
#include "host.h"


/* ----------------------- Timing Functions -------------------------- */

void tgt_fos (time)
cvl_timer_t *time;
{
  Func0 (tgt_fos);
  *time = CMMD_reduce_from_nodes_double (0.0, CMMD_combiner_dmax);
}


double tdf_fos (time1, time2)
cvl_timer_t *time1, *time2;
{
  return (*time1 - *time2);
}



/* ----------------------- Memory Functions -------------------------- */

/* Return a handle (of type vec_p) to a block of vector memory of size
 * "size", otherwise return NULL.  Size is defined as the number of
 * maxalign-sized units needed per node.  A vec_p is defined as a
 * maxalign-sized offset into the block of memory on each node.
 */
vec_p alo_foz (size)
int size;
{
  vec_p mem;
  /* Multiply size by sizeof(maxalign) to find the number of
   * *bytes* to allocate per processor.
   */ 
  Func1 (alo_foz, (size * sizeof(maxalign)));
  mem = (vec_p) CMMD_reduce_from_nodes_int (MAXINT, CMMD_combiner_min);
  if (mem == NULL) {
    CMMD_error ("alo_foz: unable to allocate %d bytes on every node\n",
		size * sizeof(maxalign));
    exit (1);
  }
  return mem;
}


void fre_fov (mem)
vec_p mem;
{
  Func1 (fre_fov, mem);
}


void mov_fov (dst, src, len, scratch)
vec_p dst, src, scratch;
int len;
{
  Func3 (mov_fov, dst, src, len);
}
Make_No_Scratch (mov_fov)
Make_Inplace (NAME, INPLACE_NONE)


/* ------------------------- Size Functions -------------------------- */

/* XXX simplify through the (length + nprocs - 1) / nprocs trick? */

/* How much size (== number of maxalign-sized units needed per node)
 * does a vector of the given type and length occupy?
 */

#define Make_Siz_Func(NAME, TYPE)					\
int NAME (length)							\
int length;								\
{									\
  int nprocs = Nprocs;							\
									\
  length = (length+1) / (sizeof(maxalign) / sizeof(TYPE));		\
									\
  /* If length isn't exactly divisible by nprocs, round it up */	\
  if (length % nprocs)							\
    length += nprocs;							\
									\
  return (length / nprocs);						\
}									\

Make_Siz_Func (siz_fob, cvl_bool)
Make_Siz_Func (siz_foz, int)
Make_Siz_Func (siz_fod, double)


/* ----------------------- Memory Arithmetic ------------------------- */

vec_p add_fov (v, size)
vec_p v;
int size;
{
  return ((maxalign *) v + size);
}


int sub_fov (v1, v2)
vec_p v1, v2;
{
  return (abs ((maxalign *) v1 - (maxalign *) v2));
}


cvl_bool eql_fov (v1, v2)
vec_p v1, v2;
{
  return (v1 == v2);
}


int cmp_fov (v1, v2)
vec_p v1, v2;
{
  return ((int) v1 - (int) v2);
}


/* ---------------------- Segment Descriptors ------------------------ */

/* A segment descriptor consists of:
 * 1) an nseg-element vector of segment start positions (the position in
 *    the vector at which they start).	If the segment is empty, its
 *    high-order bit is set.  These must be unsigned to be reliable.
 * 2) an nelt-element vector of segment start counts (the number of
 *    segments that begin at that element; may be > 1 if some of the
 *    segments are empty).
 * 3) an nproc-element vector, containing the segment number of the
 *    first element stored on each processor.
 * 4) an nproc-element vector, containing the start address of the
 *    segment of the first element stored on each processor.
 */


/* Just like siz_foz, honest.
 */
int siz_fos (nelt, nseg)
int nelt, nseg;
{
  int nprocs = Nprocs;

  nelt = (nelt+1) / (sizeof(maxalign) / sizeof(int));
  nseg = (nseg+1) / (sizeof(maxalign) / sizeof(int));
  if (nelt % nprocs)
    nelt += nprocs;
  if (nseg % nprocs)
    nseg += nprocs;
  return ((nelt / nprocs) + (nseg / nprocs) + 1);
}


void mke_fov (segd, lengths, nelt, nseg, scratch)
vec_p segd, lengths, scratch;
int nelt, nseg;
{
  /* Reverse order of nseg and nelt so we can do an add_nuz on the first
   * three arguments (segd, length, nseg) on the nodes.
   */
  Func4 (mke_fov, segd, lengths, nseg, nelt);
}
Make_No_Seg_Scratch (mke_fov)
Make_Inplace (mke_fov, INPLACE_NONE)


void len_fos (lengths, segd, nelt, nseg, scratch)
vec_p lengths, segd, scratch;
int nelt, nseg;
{
  if (nseg == 1) {
    rep_vuz (lengths, 0, nelt, nseg, scratch);
  } else {
    Func4 (len_fos, lengths, segd, nelt, nseg);
  }
}
Make_No_Seg_Scratch (len_fos)
Make_Inplace (len_fos, INPLACE_NONE)


/* ----------------------- Vector Conversion ------------------------- */

#define Make_v2c(NAME, TYPE, SINGLE)					\
void NAME (dst, src, len, scratch)					\
TYPE *dst;								\
vec_p src, scratch;							\
int len;								\
{									\
  if (len == 1) {							\
    *dst = SINGLE (src, 0, len, scratch);				\
  } else if (len >= 1) {						\
    int size = sizeof(TYPE) * ((len + Nprocs - 1) >> Logprocs);		\
    void *vector = malloc(Nprocs * size);				\
									\
    if (vector == NULL) {						\
      CMMD_error ("unable to malloc space for v2c\n");			\
    }									\
    { Func2 (NAME, src, len); }						\
    CMMD_gather_from_nodes ((void *) vector, size);			\
    memcpy (dst, vector, len * sizeof(TYPE));				\
    free (vector);							\
  }									\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)

Make_v2c (v2c_fuz, int, ext_vuz)
Make_v2c (v2c_fud, double, ext_vud)
Make_v2c (v2c_fub, cvl_bool, ext_vub)


#define Make_c2v(NAME, TYPE, SINGLE)					\
void NAME (dst, src, len, scratch)					\
vec_p dst, scratch;							\
TYPE *src;								\
int len;								\
{									\
  if (len == 1) {							\
    SINGLE (dst, 0, src[0], len, scratch);				\
  } else if (len >= 1) {						\
    int size = sizeof(TYPE) * ((len + Nprocs - 1) >> Logprocs);		\
    void *vector = malloc(Nprocs * size);				\
									\
    if (vector == NULL) {						\
      CMMD_error ("unable to malloc space for c2v\n");			\
    }									\
    memcpy (vector, src, len * sizeof(TYPE));				\
    { Func2 (NAME, dst, len); }						\
    CMMD_distrib_to_nodes ((void *) vector, size);			\
    free (vector);							\
  }									\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)					\

Make_c2v (c2v_fuz, int, rep_vuz)
Make_c2v (c2v_fud, double, rep_vud)
Make_c2v (c2v_fub, cvl_bool, rep_vub)


void rnd_foz (seed)
int seed;
{
  Func1 (rnd_foz, seed);
}
