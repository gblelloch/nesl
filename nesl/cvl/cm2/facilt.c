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

#include <cm/paris.h>
#include <cvl.h>
#include "cm2cvl.h"


/* ----------------------- Timing Functions -------------------------- */

/* Timer 1 has already been started in the CM initialization process.
 * Therefore, stop it, get the CM busy time, and restart it.
 */
void tgt_fos (cvl_timer_p)
cvl_timer_t *cvl_timer_p;
{
  CM_timer_stop (1);
  *cvl_timer_p = CM_timer_read_cm_busy (1);
  CM_timer_start (1);
}


double tdf_fos (t1, t2)
cvl_timer_t *t1, *t2;
{
  return (*t1 - *t2);
}


/* ------------------------ Size Functions --------------------------- */

/* Define "size" to be the number of bits per CM processor.  This is
 * easy to calculate, and makes things a whole lot easier later on...
 */

#define make_size(_name, _bits)						\
int _name (length)							\
int length;								\
{									\
  return (_CVL_elts_per_proc (length) * _bits);				\
}									\

make_size (siz_fob, BOO_BITS)
make_size (siz_foz, INT_BITS)
make_size (siz_fod, DBL_BITS)


/* ----------------------- Memory Functions -------------------------- */

/* Round to a DBL_BITS boundary.
 */
CM_field_id_t alo_foz (size)
int size;
{
  size = (size == 0) ? 0 : ((size / DBL_BITS) + 1) * DBL_BITS;
  return (_CVL_allocate_heap (size));
}


void fre_fov (pointer)
vec_p pointer;
{
  CM_deallocate_heap_field (pointer);
}


void mov_fov (dst, src, size, scratch)
vec_p dst, src, scratch;
int size;
{
  CM_s_move_1L (dst, src, size);
}
make_no_scratch (mov_fov)
make_inplace (mov_fov, INPLACE_NONE)


/* ---------------------- Memory Arithmetic -------------------------- */

/* Add a size to a field id (== vec_p), return another field id.
 */
vec_p add_fov (vec, size)
vec_p vec;
int size;
{
  return (CM_add_offset_to_field_id (vec, size));
}


/* Subtract two vec_p's, return size of vector interval.
 */
int sub_fov (v1, v2)
vec_p v1, v2;
{
  return (abs ((int *) v1 - (int *) v2));
}


/* Compare two vec_p's for equality.
 */
unsigned eql_fov (v1, v2)
vec_p v1, v2;
{
  return ((int *) v1 == (int *) v2);
}


/* Compare two vec_p's for smaller than, equal to, bigger than.
 */
int cmp_fov (v1, v2)
vec_p v1, v2;
{
  return ((int) v1 - (int) v2);
}


/* --------------------- Segment Descriptors -------------------------- */

int siz_fos (nelt, nseg)
int nelt, nseg;
{
  return ((INT_BITS+1) * _CVL_elts_per_proc (nseg) +
	  _CVL_elts_per_proc (nelt));
}


/* Create seg descriptor segdes, which consists of a vector of nseg
 * start-of-segment indices, each of INT_BITS, followed by a vector of
 * nseg empty-segment 1-bit markers, followed by a vector of nelt
 * start-of-segment 1-bit markers.
 */
void mke_fov (segdes, lengths, nelt, nseg, scratch)
vec_p segdes, lengths, scratch;
int nelt, nseg;
{
  CM_field_id_t vp_bit_one, vp_segstart, vp_segindx, vp_segempty;
  CM_field_id_t bit_one, segindx, segstart, segempty;
  int elt_count = _CVL_elts_per_proc (nelt);
  int seg_count = _CVL_elts_per_proc (nseg);

  segindx  = _CVL_allocate_stack (seg_count * INT_BITS);
  segempty = _CVL_allocate_stack (seg_count);
  segstart = _CVL_allocate_stack (elt_count);
  bit_one  = _CVL_allocate_stack (seg_count);
  _CVL_copy_s_field (segindx, lengths, seg_count);

  /* Switch to one VP per element */
  _CVL_set_vp_set (elt_count);
  vp_segstart = CM_make_field_alias (segstart);
  CM_clear_bit_always (vp_segstart);

  if (nseg == 1)			/* Turn on segstart in VP 0 */
    CM_u_write_to_processor (0, vp_segstart, (unsigned) 1, 1);

  /* Switch to one VP per segment*/
  CM_set_vp_set (_CVL_default_vp_set);
  _CVL_set_vp_set_and_context (seg_count, nseg);
  vp_segindx = CM_make_field_alias (segindx);
  vp_segempty = CM_make_field_alias (segempty);
  vp_bit_one = CM_make_field_alias (bit_one);

  /* Set segempty for a segment if its length is zero */
  CM_s_eq_zero_1L (vp_segindx, INT_BITS);
  CM_store_test (vp_segempty);

  /* Only need a scan and send if we've got more than one segment */
  if (nseg == 1) {
    CM_s_move_zero_always_1L (vp_segindx, INT_BITS);
  } else {
    /* Add-scan on lengths puts segment start positions into segindx */
    CM_scan_with_s_add_1L (vp_segindx, vp_segindx, 0, INT_BITS,
			   CM_upward, CM_exclusive, CM_none,
			   CM_no_field);

    /* Send a 1 to every start-of-segment position in segstart */
    CM_set_bit_always (vp_bit_one);
    CM_send_1L (vp_segstart, vp_segindx, vp_bit_one, 1, CM_no_field);
  }

  /* Restore context and VP set */
  CM_set_vp_set (_CVL_default_vp_set);
  CM_set_context ();

  /* Copy segindx, then segempty, and finally segstart */
  _CVL_copy_s_field (segdes, segindx, seg_count);
  BUMP_PTR (segdes, (seg_count * INT_BITS));
  _CVL_copy_u_field (segdes, segempty, seg_count);
  BUMP_PTR (segdes, seg_count);
  _CVL_copy_u_field (segdes, segstart, elt_count);

  CM_remove_field_alias (vp_segstart);
  CM_remove_field_alias (vp_segindx);
  CM_remove_field_alias (vp_segempty);
  CM_remove_field_alias (vp_bit_one);
  CM_deallocate_stack_through (segindx);
}
make_no_seg_scratch (mke_fov)
make_inplace (mke_fov, INPLACE_NONE)


/* Inverse of mke_fov -- given a segment descriptor, returns a vector of
 * the segment lengths.
 */
void len_fos (lengths, segdes, nelt, nseg, scratch)
vec_p lengths, segdes, scratch;
int nelt, nseg;
{
  CM_field_id_t segindx, segindx2, vp_indx, vp_indx2;
  int seg_count = _CVL_elts_per_proc (nseg);

  segindx  = _CVL_allocate_stack (seg_count * INT_BITS);
  segindx2 = _CVL_allocate_stack (seg_count * INT_BITS);
  _CVL_copy_s_field (segindx, segdes, seg_count);

  _CVL_set_vp_set_and_context (seg_count, nseg);
  vp_indx = CM_make_field_alias (segindx);
  vp_indx2 = CM_make_field_alias (segindx2);

  /* Make indx2 be a copy of indx shifted down by one processor */
  CM_send_to_news_1L (vp_indx2, vp_indx, 0, CM_downward, INT_BITS);

  /* Fill in the indx2 slot in the top-most processor */
  CM_s_write_to_processor_1L (nseg-1, vp_indx2, nelt, INT_BITS);

  /* Put the difference between indx2 and indx into indx2 */
  CM_s_subtract_2_1L (vp_indx2, vp_indx, INT_BITS);

  /* Copy the result (indx2) into lengths */
  CM_set_vp_set (_CVL_default_vp_set);
  CM_set_context ();
  _CVL_copy_s_field (lengths, segindx2, seg_count);
  CM_remove_field_alias (vp_indx);
  CM_remove_field_alias (vp_indx2);
  CM_deallocate_stack_through (segindx);
}
make_no_seg_scratch (len_fos)
make_inplace (len_fos, INPLACE_NONE)


/* ----------------------- Vector Conversion ------------------------- */

#define make_v2c(_name, _type, _abbrev, _argbits, _bits, _format)	\
void _name (dst, src, len, scratch)					\
_type *dst;								\
vec_p src, scratch;							\
int len;								\
{									\
  int i, count = _CVL_elts_per_proc (len);				\
  int fe_offset_v[1], cm_start_v[1], cm_end_v[1], cm_axis_v[1];		\
  int fe_dim_v[1];							\
									\
  fe_offset_v[0] = 0;			/* offset into dst */		\
  cm_start_v[0]	 = 0;			/* first NEWS index */		\
  cm_end_v[0]	 = CM_physical_processors_limit; /* and last */		\
  cm_axis_v[0]	 = 0;			/* NEWS axis to use */		\
  fe_dim_v[0]	 = CM_physical_processors_limit; /* size of FE array */	\
									\
  for (i = 1; i < count; i++) {						\
    GLUE3 (CM,_abbrev,read_from_news_array_1L)				\
      (dst, fe_offset_v, cm_start_v, cm_end_v, cm_axis_v, src,		\
       _argbits, 1, fe_dim_v, _format);					\
    dst += CM_physical_processors_limit;				\
    BUMP_PTR (src, _bits);						\
  }									\
									\
  cm_end_v[0] = fe_dim_v[0] = len % CM_physical_processors_limit;	\
  GLUE3 (CM,_abbrev,read_from_news_array_1L)				\
    (dst, fe_offset_v, cm_start_v, cm_end_v, cm_axis_v, src, _argbits,	\
     1, fe_dim_v, _format);						\
}									\
make_no_scratch (_name)
make_inplace (_name, INPLACE_NONE)

make_v2c (v2c_fuz,int,_s_,INT_BITS,INT_BITS,CM_32_bit)
make_v2c (v2c_fud,double,_f_,SIG_EXP,DBL_BITS,CM_float_double)
/* XXX this assumes that cvl_bool is typedef'd to int! */
make_v2c (v2c_fub,unsigned,_u_,BOO_BITS,BOO_BITS,CM_32_bit)


#define make_c2v(_name, _type, _abbrev, _argbits, _bits, _format)	\
void _name (dst, src, len, scratch)					\
vec_p dst, scratch;							\
_type *src;								\
int len;								\
{									\
  int i, count = _CVL_elts_per_proc (len);				\
  int fe_offset_v[1], cm_start_v[1], cm_end_v[1], cm_axis_v[1];		\
  int fe_dim_v[1];							\
									\
  fe_offset_v[0] = 0;			/* offset into dst */		\
  cm_start_v[0]	 = 0;			/* first NEWS index */		\
  cm_end_v[0]	 = CM_physical_processors_limit; /* and last */		\
  cm_axis_v[0]	 = 0;			/* NEWS axis to use */		\
  fe_dim_v[0]	 = CM_physical_processors_limit; /* size of FE array */	\
									\
  for (i = 1; i < count; i++) {						\
    GLUE3 (CM,_abbrev,write_to_news_array_1L)				\
      (src, fe_offset_v, cm_start_v, cm_end_v, cm_axis_v, dst,		\
       _argbits, 1, fe_dim_v, _format);					\
    src += CM_physical_processors_limit;				\
    BUMP_PTR (dst, _bits);						\
  }									\
									\
  cm_end_v[0] = fe_dim_v[0] = len % CM_physical_processors_limit;	\
  GLUE3 (CM,_abbrev,write_to_news_array_1L)				\
    (src, fe_offset_v, cm_start_v, cm_end_v, cm_axis_v, dst, _argbits,	\
     1, fe_dim_v, _format);						\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_NONE)

make_c2v (c2v_fuz,int,_s_,INT_BITS,INT_BITS,CM_32_bit)
make_c2v (c2v_fud,double,_f_,SIG_EXP,DBL_BITS,CM_float_double)


void c2v_fub (dst, src, len, scratch)
vec_p dst, scratch;
unsigned *src;
int len;
{
  if (len == 1) {
    rep_vub (dst, 0, src[0], 1, scratch);
  } else if (len != 0) {
    int count = _CVL_elts_per_proc (len);
    int i;
    int fe_offset_v[1], cm_start_v[1], cm_end_v[1], cm_axis_v[1];
    int fe_dim_v[1];
    vec_p intermediate, ptr_to_intermediate;

    fe_offset_v[0] = 0;			/* offset into dst */
    cm_start_v[0]  = 0;			/* first NEWS index */
    cm_end_v[0]	   = CM_physical_processors_limit; /* and last */
    cm_axis_v[0]   = 0;			/* NEWS axis to use */
    fe_dim_v[0]	   = CM_physical_processors_limit; /* size of FE array */

    /* Can't do a write_to_news_array from 32 bits to 1 bit, so we use a
       32-bit intermediate vector
     */
    intermediate = _CVL_allocate_stack (count * INT_BITS);
    ptr_to_intermediate = intermediate;
    for (i = 1; i < count; i++) {
      CM_u_write_to_news_array_1L
	(src, fe_offset_v, cm_start_v, cm_end_v, cm_axis_v, intermediate,
	 INT_BITS, 1, fe_dim_v, CM_32_bit);
      src += CM_physical_processors_limit;
      BUMP_PTR (intermediate, INT_BITS);
    }

    cm_end_v[0] = fe_dim_v[0] = len % CM_physical_processors_limit;
    CM_u_write_to_news_array_1L
      (src, fe_offset_v, cm_start_v, cm_end_v, cm_axis_v, intermediate,
      INT_BITS, 1, fe_dim_v, CM_32_bit);

    intermediate = ptr_to_intermediate;	/* get saved value */
    for (i = 0; i < count; i++) {
      CM_u_move_2L (dst, intermediate, BOO_BITS, INT_BITS);
      BUMP_PTR (dst, BOO_BITS);
      BUMP_PTR (intermediate, INT_BITS);
    }
  }
}
make_no_scratch (c2v_fub)
make_inplace (c2v_fub, INPLACE_NONE)


void rnd_foz(seed)
int seed;
{
  CM_initialize_random_generator ((unsigned) seed);
}
