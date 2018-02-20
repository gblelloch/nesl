/*
* Copyright (c) 1992 Carnegie Mellon University 
*                    SCAL project: Guy Blelloch, Siddhartha Chatterjee,
*                                  Jonathan Hardwick, Jay Sipelstein,
*                                  Marco Zagha
* All Rights Reserved.
*
* Permission to use, copy, modify and distribute this software and its
* documentation is hereby granted, provided that both the copyright
* notice and this permission notice appear in all copies of the
* software, derivative works or modified versions, and any portions
* thereof, and that both notices appear in supporting documentation.
*
* CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
* CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
* ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
*
* The SCAL project requests users of this software to return to 
*
*  Guy Blelloch				guy.blelloch@cs.cmu.edu
*  School of Computer Science
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3891
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/
#include "defins.h"
#include <cvl.h>
#include "cray.h"

/* -------------------Extract-------------------------------------*/

/* extract ith element from V */
/* extract template */
#define make_extract(_name, _type) \
	_type _name (V, i, len, scratch) \
	vec_p V, scratch; \
	int i, len; \
	{ return ((_type *)V)[i];} \
  make_no_scratch(_name) \
  make_inplace(_name,INPLACE_NONE)

make_extract(ext_vuz, int)
make_extract(ext_vud, double)
make_extract(ext_vub, cvl_bool)



make_no_scratch(ext_vupb)
make_inplace(ext_vupb,INPLACE_NONE)

/* See figure 5 of the Supercomputing '90 paper. 
 *
 * We go directly for the correct word of the compressed
 * transposed boolean vector.  Then grab the right bit.
 * Cray VM store's bits backwards, so bit 0 is high order
 * bit.
 * 
 * See cray C man pages for bit intrinsics.  Compile with -h intrinsics
 * for faster execution.
 */
cvl_bool ext_vupb(Vec, i, len, scratch)
vec_p Vec, scratch;
int i, len;
{
   int ignore_vlen, stride, ignore_rem;
   int *V = (int *)Vec;

  _compute_shape_factors(len, &ignore_vlen, &stride, &ignore_rem);

   /* getbit(data, position) */
#ifdef C90
  return (_gbit(V[(i%stride) * 2 + (i/stride>63)], 63 - (i/stride)%64) != 0);  
#else
  return (_gbit(V[i%stride], 63 - i/stride) != 0);
#endif
}


/* ------------------Replace-------------------------------------*/

/* replace ith element of V with val */

#define make_replace(_name, _type, _funct)		\
	void _name(V, i, val, len, scratch)		\
	vec_p V, scratch;				\
	int i, len;					\
	_type val;					\
	{ ((_type *)V)[i] = _funct(val); }		\
  make_no_scratch(_name)				\
  make_inplace(_name,INPLACE_NONE)

make_replace(rep_vuz, int, ident)
make_replace(rep_vub, cvl_bool, notnot)
make_replace(rep_vud, double, ident)


void ext_vez(d, v, index, sd, n, m, scratch)
vec_p d, v, index, sd, scratch;
int n, m;
{
 if (m == 0) return;
 else if (m == 1) ((int *)d)[0] = ((int *)v)[((int *)index)[0]];
 else {
   void XEXTVEZ();
   vec_p offsets = (vec_p) offsets_from_sd(sd, m);
   int vlen, stride, remain;
   
   _compute_shape_factors(m, &vlen, &stride, &remain);
   XEXTVEZ(d, v, index, offsets, vlen, stride, remain);
 }
}

make_inplace(ext_vez, INPLACE_2)
make_no_seg_scratch(ext_vez)



void ext_veb(d, v, index, sd, n, m, scratch)
vec_p d, v, index, sd, scratch;
int n, m;
{
 if (m == 0) return;
 else if (m == 1) ((int *)d)[0] = ((int *)v)[((int *)index)[0]];
 else {
   void XEXTVEZ();
   vec_p offsets = (vec_p) offsets_from_sd(sd, m);
   int vlen, stride, remain;
   
   _compute_shape_factors(m, &vlen, &stride, &remain);
   XEXTVEZ(d, v, index, offsets, vlen, stride, remain);
 }
}

make_inplace(ext_veb, INPLACE_2)
make_no_seg_scratch(ext_veb)

void ext_ved(d, v, index, sd, n, m, scratch)
vec_p d, index, v, sd, scratch;
int n, m;
{
 if (m == 0) return;
 else if (m == 1) ((int *)d)[0] = ((int *)v)[((int *)index)[0]];
 else {
   void XEXTVEZ();
   vec_p offsets = (vec_p) offsets_from_sd(sd, m);
   int vlen, stride, remain;
   
   _compute_shape_factors(m, &vlen, &stride, &remain);
   XEXTVEZ(d, v, index, offsets, vlen, stride, remain);
 }
}

make_inplace(ext_ved, INPLACE_2)
make_no_seg_scratch(ext_ved)




void rep_vez(d, index, v, sd, n, m, scratch)
vec_p d, index, v, sd, scratch;
int n, m;
{
 if (m == 0) return;
 else if (m == 1) ((int *)d)[((int *)index)[0]] = ((int *)v)[0];
 else {
   void XREPVEZ();
   vec_p offsets = (vec_p) offsets_from_sd(sd, m);
   int vlen, stride, remain;

   _compute_shape_factors(m, &vlen, &stride, &remain);
   /* reorder args to be more like smp_puz */
   XREPVEZ(d, v, index, offsets, vlen, stride, remain);
 }
}

make_inplace(rep_vez, INPLACE_NONE)
make_no_seg_scratch(rep_vez)



void rep_veb(d, index, v, sd, n, m, scratch)
vec_p d, index, v, sd, scratch;
int n, m;
{
 if (m == 0) return;
 else if (m == 1) ((int *)d)[((int *)index)[0]] = ((int *)v)[0];
 else {
   void XREPVEZ();
   vec_p offsets = (vec_p) offsets_from_sd(sd, m);
   int vlen, stride, remain;

   _compute_shape_factors(m, &vlen, &stride, &remain);
   /* reorder args to be more like smp_puz */
   XREPVEZ(d, v, index, offsets, vlen, stride, remain);
 }
}

make_inplace(rep_veb, INPLACE_NONE)
make_no_seg_scratch(rep_veb)


void rep_ved(d, index, v, sd, n, m, scratch)
vec_p d, index, v, sd, scratch;
int n, m;
{
 if (m == 0) return;
 else if (m == 1) ((int *)d)[((int *)index)[0]] = ((int *)v)[0];
 else {
   void XREPVEZ();
   vec_p offsets = (vec_p) offsets_from_sd(sd, m);
   int vlen, stride, remain;

   _compute_shape_factors(m, &vlen, &stride, &remain);
   /* reorder args to be more like smp_puz */
   XREPVEZ(d, v, index, offsets, vlen, stride, remain);
 }
}

make_inplace(rep_ved, INPLACE_NONE)
make_no_seg_scratch(rep_ved)



/* See figure 5 of the Supercomputing '90 paper. 
 *
 * We go directly for the correct word of the compressed
 * transposed boolean vector.  Then grab the right bit.
 * Cray VM store's bits backwards, so bit 0 is high order
 * bit.
 * 
 * See cray C man pages for bit intrinsics.  Compile with -h intrinsics
 * for faster execution.
 */
void rep_vupb(Vec, i, val, len, scratch)
vec_p Vec, scratch;
cvl_bool val;
int i, len;
{
   int ignore_vlen, stride, ignore_rem, index;
   int *V = (int *)Vec;

  _compute_shape_factors(len, &ignore_vlen, &stride, &ignore_rem);

  /* = setbit (setme, position, value) */
#ifdef C90
  index = (i % stride) * 2 + (i/stride>63);
  V[index] = _pbit(V[index], 63 - (i/stride)%64, (val !=0 ));
#else
  index = i % stride;
  V[index] = _pbit(V[index], 63 - i/stride, (val !=0 ));
#endif
}

make_no_scratch(rep_vupb)
make_inplace(rep_vupb,INPLACE_NONE)



void ext_vepb(d, v, index, sd, n, m, scratch)
vec_p d, v, index, sd, scratch;
int n, m;
{
 void XEXTVEZ();
 vec_p offsets = (vec_p) offsets_from_sd(sd, m);
 int vlen, stride, remain;
 int *uncompressed_d = (int *) scratch;
 int *uncompressed_v = uncompressed_d + n;

 if (m > 0) {
   _compute_shape_factors(m, &vlen, &stride, &remain);
   int_wupb(uncompressed_d, d, n, 0);
   XEXTVEZ(uncompressed_v, uncompressed_d, index, offsets, vlen, stride, remain);
   pboo_wuz(v, uncompressed_v, m, 0);
 }
}

make_inplace(ext_vepb, INPLACE_NONE)
int ext_vepb_scratch(n, m) int n, m; {return n+m;}


void rep_vepb(d, index, v, sd, n, m, scratch)
vec_p d, index, v, sd, scratch;
int n, m;
{
 void XREPVEZ();
 vec_p offsets = (vec_p) offsets_from_sd(sd, m);
 int vlen, stride, remain;
 int *uncompressed_d = (int *) scratch;
 int *uncompressed_v = uncompressed_d + n;

 if (m > 0) {
   _compute_shape_factors(m, &vlen, &stride, &remain);
   int_wupb(uncompressed_d, d, n, 0);
   int_wupb(uncompressed_v, v, m, 0);
   /* reorder args to be more like smp_puz */
   XREPVEZ(uncompressed_d, uncompressed_v, index, offsets, vlen, stride, remain);
   pboo_wuz(d, uncompressed_d, n, 0);
 }
}

make_inplace(rep_vepb, INPLACE_NONE)
int rep_vepb_scratch(n, m) int n, m; {return n+m;}

