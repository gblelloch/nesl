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


/* ----------------------- Unsegmented Scan -------------------------- */

#define make_scan(_name, _abbrev, _funct, _bits, _argbits)		\
void _name (dst, src, len, scratch)					\
vec_p dst, src, scratch;						\
int len;								\
{									\
  CM_field_id_t tmp_src, vp_src;					\
  int count = _CVL_elts_per_proc (len);					\
									\
  tmp_src = _CVL_allocate_stack (count * _bits);			\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, count);		\
									\
  _CVL_set_vp_set_and_context (count, len);				\
  vp_src = CM_make_field_alias (tmp_src);				\
  GLUE3 (CM_scan_with,_funct,1L) (vp_src, vp_src, 0, _argbits,		\
				  CM_upward, CM_exclusive, CM_none,	\
				  CM_no_field);				\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_src, count);		\
  CM_remove_field_alias (vp_src);					\
  CM_deallocate_stack_through (tmp_src);				\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_1)

make_scan (add_suz,_s_,_s_add_,INT_BITS,INT_BITS)
make_scan (min_suz,_s_,_s_min_,INT_BITS,INT_BITS)
make_scan (max_suz,_s_,_s_max_,INT_BITS,INT_BITS)
make_scan (and_suz,_s_,_logand_,INT_BITS,INT_BITS)
make_scan (ior_suz,_s_,_logior_,INT_BITS,INT_BITS)
make_scan (xor_suz,_s_,_logxor_,INT_BITS,INT_BITS)

make_scan (add_sud,_f_,_f_add_,DBL_BITS,SIG_EXP)
make_scan (mul_sud,_f_,_f_multiply_,DBL_BITS,SIG_EXP)
make_scan (min_sud,_f_,_f_min_,DBL_BITS,SIG_EXP)
make_scan (max_sud,_f_,_f_max_,DBL_BITS,SIG_EXP)

make_scan (and_sub,_u_,_logand_,BOO_BITS,BOO_BITS)
make_scan (ior_sub,_u_,_logior_,BOO_BITS,BOO_BITS)
make_scan (xor_sub,_u_,_logxor_,BOO_BITS,BOO_BITS)


/* ------------------------ Segmented Scan --------------------------- */

#define make_segscan(_name, _unseg, _abbrev, _funct, _bits, _argbits)	\
void _name (dst, src, segd, nelt, nseg, scratch)			\
vec_p dst, src, segd, scratch;						\
int nelt, nseg;								\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (nseg == 1) {							\
    _unseg (dst, src, nelt, scratch);					\
  }									\
  else {								\
    CM_field_id_t tmp_src, segindx, segstart, segempty;			\
    CM_field_id_t vp_src, vp_segstart;					\
    int elt_count = _CVL_elts_per_proc (nelt);				\
    int seg_count = _CVL_elts_per_proc (nseg);				\
									\
    /* Allocate fields that we can alias to, copy stuff over */		\
    tmp_src  = _CVL_allocate_stack (elt_count * _bits);			\
    segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segempty = _CVL_allocate_stack (seg_count);				\
    segstart = _CVL_allocate_stack (elt_count);				\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, elt_count);		\
    _CVL_split_segd (segd, elt_count, seg_count, segindx, segempty,	\
		     segstart);						\
									\
    /* Switch to VP set with one VP per element, do a scan */		\
    _CVL_set_vp_set_and_context (elt_count, nelt);			\
    vp_src	= CM_make_field_alias (tmp_src);			\
    vp_segstart = CM_make_field_alias (segstart);			\
									\
    GLUE3 (CM_scan_with,_funct,1L) (vp_src, vp_src, 0, _argbits,	\
				    CM_upward, CM_exclusive,		\
				    CM_segment_bit, vp_segstart);	\
									\
    /* Restore VP set, copy vp_src into dst */				\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_src, elt_count);		\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_segstart);				\
    CM_deallocate_stack_through (tmp_src);				\
  }									\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_1)	/* might call unsegmented version */

make_segscan (add_sez,add_suz,_s_,_s_add_,INT_BITS,INT_BITS)
make_segscan (min_sez,min_suz,_s_,_s_min_,INT_BITS,INT_BITS)
make_segscan (max_sez,max_suz,_s_,_s_max_,INT_BITS,INT_BITS)
make_segscan (and_sez,and_suz,_s_,_logand_,INT_BITS,INT_BITS)
make_segscan (ior_sez,ior_suz,_s_,_logior_,INT_BITS,INT_BITS)
make_segscan (xor_sez,xor_suz,_s_,_logxor_,INT_BITS,INT_BITS)

make_segscan (add_sed,add_sud,_f_,_f_add_,DBL_BITS,SIG_EXP)
make_segscan (mul_sed,mul_sud,_f_,_f_multiply_,DBL_BITS,SIG_EXP)
make_segscan (min_sed,min_sud,_f_,_f_min_,DBL_BITS,SIG_EXP)
make_segscan (max_sed,max_sud,_f_,_f_max_,DBL_BITS,SIG_EXP)

make_segscan (and_seb,and_sub,_u_,_logand_,BOO_BITS,BOO_BITS)
make_segscan (ior_seb,ior_sub,_u_,_logior_,BOO_BITS,BOO_BITS)
make_segscan (xor_seb,xor_sub,_u_,_logxor_,BOO_BITS,BOO_BITS)


/* Paris doesn't provide an integer multiply scan operation (presumably
 * for the very good reason that it quickly leads to overflow), so we
 * have to fake one by converting to doubles, using a float multiply
 * scan, and then converting back again.  mul_suz and mul_sez are just
 * expanded versions of the macros above, with conversion added into the
 * vector copying.
 */

void mul_suz (dst, src, len, scratch)
vec_p dst, src, scratch;
int len;
{
  CM_field_id_t tmp_src, vp_src;
  int count = _CVL_elts_per_proc (len);

  /* Copy the integer vector src into the double vector tmp_src */
  tmp_src = _CVL_allocate_stack (count * DBL_BITS);
  {
    vec_p d = tmp_src;
    vec_p s = src;
    int i;
    for (i = 0; i < count; i++) {
      CM_f_s_float_2_2L (d, s, INT_BITS, SIG_EXP);
      BUMP_PTR (d, DBL_BITS);
      BUMP_PTR (s, INT_BITS);
    }
  }
  _CVL_set_vp_set_and_context (count, len);
  vp_src = CM_make_field_alias (tmp_src);
  CM_scan_with_f_multiply_1L (vp_src, vp_src, 0, SIG_EXP, CM_upward,
			      CM_exclusive, CM_none, CM_no_field);
  CM_set_vp_set (_CVL_default_vp_set);
  CM_set_context ();

  /* Now copy tmp_src back into dst, rounding off to ints */
  { vec_p d = dst;
    vec_p s = tmp_src;
    int i;
    for (i = 0; i < count; i++) {
      CM_s_f_truncate_2_2L (d, s, INT_BITS, SIG_EXP);
      BUMP_PTR (d, INT_BITS);
      BUMP_PTR (s, DBL_BITS);
    }
  }
  CM_remove_field_alias (vp_src);
  CM_deallocate_stack_through (tmp_src);
}
make_no_scratch (mul_suz)
make_inplace (mul_suz, INPLACE_1)


void mul_sez (dst, src, segd, nelt, nseg, scratch)
vec_p dst, src, segd, scratch;
int nelt, nseg;
{
  /* If we only have one segment, use unsegmented routine instead */
  if (nseg == 1) {
    mul_suz (dst, src, nelt, scratch);
  }
  else {
    CM_field_id_t tmp_src, segindx, segstart, segempty, vp_src,
    vp_segstart;
    int elt_count = _CVL_elts_per_proc (nelt);
    int seg_count = _CVL_elts_per_proc (nseg);

    /* Allocate fields that we can alias to, copy stuff over */
    tmp_src  = _CVL_allocate_stack (elt_count * DBL_BITS);
    segindx  = _CVL_allocate_stack (seg_count * INT_BITS);
    segempty = _CVL_allocate_stack (seg_count);
    segstart = _CVL_allocate_stack (elt_count);
    /* Copy the integer vector src into the double vector tmp_src */
    {
      vec_p d = tmp_src;
      vec_p s = src;
      int i;
      for (i = 0; i < elt_count; i++) {
	CM_f_s_float_2_2L (d, s, INT_BITS, SIG_EXP);
	BUMP_PTR (d, DBL_BITS);
	BUMP_PTR (s, INT_BITS);
      }
    }
    _CVL_split_segd (segd, elt_count, seg_count, segindx, segempty,
		     segstart);

    /* Switch to VP set with one VP per element, do a scan */
    _CVL_set_vp_set_and_context (elt_count, nelt);
    vp_src	= CM_make_field_alias (tmp_src);
    vp_segstart = CM_make_field_alias (segstart);

    CM_scan_with_f_multiply_1L (vp_src, vp_src, 0, SIG_EXP, CM_upward,
				CM_exclusive, CM_segment_bit,
				vp_segstart);

    /* Restore VP set, copy vp_src into dst */
    CM_set_vp_set (_CVL_default_vp_set);
    CM_set_context ();

    /* Now copy tmp_src back into src, rounding off to ints */
    { vec_p d = src;
      vec_p s = tmp_src;
      int i;
      for (i = 0; i < elt_count; i++) {
	CM_s_f_floor_2_2L (d, s, INT_BITS, SIG_EXP);
	BUMP_PTR (d, INT_BITS);
	BUMP_PTR (s, DBL_BITS);
      }
    }
    CM_remove_field_alias (vp_src);
    CM_remove_field_alias (vp_segstart);
    CM_deallocate_stack_through (tmp_src);
  }
}
make_no_seg_scratch (mul_sez)
make_inplace (mul_sez, INPLACE_1)
