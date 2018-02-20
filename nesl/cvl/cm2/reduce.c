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


/* Note that multiply reductions are not supported in Paris, so we
 * use scans for all mul_r[ue][dz] operations.	Furthermore, only a
 * floating-point multiply scan is supplied, so conversion is necessary
 * for mul_r[ue]z.  See the end of the file for these two functions.
 */

/* ---------------------- Unsegmented Reduce ------------------------- */

/* For integers and bools, it appears to be faster to do a scan, and
 * then extract the appropriate element, than to use the Paris
 * global functions.  For doubles, the reverse is true.	 In ancient
 * times, Guy helped to hand-optimize the global float operations.
 * Apparently, no-one thought it worth doing the same to the global
 * integer operations, which are now slower than scans as a result.
 * Thus, for the best performance, we use global operations for
 * doubles, and scans for integers and booleans.
 */

#define reduce_skeleton(_name, _type, _abbrev, _bits, _ident, _funct)	\
_type _name (src, len, scratch)						\
vec_p src, scratch;							\
int len;								\
{									\
  CM_field_id_t tmp_src, vp_src;					\
  _type result;								\
  int count;								\
									\
  if (len == 0)								\
    return (_ident);							\
									\
  count = _CVL_elts_per_proc (len);					\
  tmp_src = _CVL_allocate_stack (count * _bits);			\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, count);		\
									\
  _CVL_set_vp_set_and_context (count, len);				\
  vp_src = CM_make_field_alias (tmp_src);				\
  _funct;								\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  CM_remove_field_alias (vp_src);					\
  CM_deallocate_stack_through (tmp_src);				\
  return (result);							\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_NONE)

#define scan_red(_name, _type, _abbrev, _funct, _bits, _argbits, _ident)\
    reduce_skeleton (_name,_type,_abbrev,_bits,_ident,			\
	GLUE3 (CM_scan_with,_funct,1L) (vp_src, vp_src, 0, _argbits,	\
	       CM_upward, CM_inclusive, CM_none, CM_no_field);		\
	result = GLUE3 (CM,_abbrev,read_from_processor_1L)		\
		       (len-1, vp_src, _argbits))


#define glob_red(_name, _type, _abbrev, _funct, _bits, _argbits, _ident)\
    reduce_skeleton (_name,_type,_abbrev,_bits,_ident,			\
	result = GLUE3 (CM_global,_funct,1L) (vp_src, _argbits))

scan_red (add_ruz,int,_s_,_s_add_,INT_BITS,INT_BITS,0)
scan_red (min_ruz,int,_s_,_s_min_,INT_BITS,INT_BITS,MAX_INT)
scan_red (max_ruz,int,_s_,_s_max_,INT_BITS,INT_BITS,MIN_INT)
scan_red (and_ruz,int,_s_,_logand_,INT_BITS,INT_BITS,~0)
scan_red (ior_ruz,int,_s_,_logior_,INT_BITS,INT_BITS,0)
scan_red (xor_ruz,int,_s_,_logxor_,INT_BITS,INT_BITS,0)

scan_red (mul_rud,double,_f_,_f_multiply_,DBL_BITS,SIG_EXP,1.0)

glob_red (add_rud,double,_f_,_f_add_,DBL_BITS,SIG_EXP,0.0)
glob_red (min_rud,double,_f_,_f_min_,DBL_BITS,SIG_EXP,MAX_DBL)
glob_red (max_rud,double,_f_,_f_max_,DBL_BITS,SIG_EXP,MIN_DBL)

scan_red (and_rub,unsigned,_u_,_logand_,BOO_BITS,BOO_BITS,1)
scan_red (ior_rub,unsigned,_u_,_logior_,BOO_BITS,BOO_BITS,0)
scan_red (xor_rub,unsigned,_u_,_logxor_,BOO_BITS,BOO_BITS,0)


/* ----------------------- Segmented Reduce -------------------------- */

#define seg_red(_name, _unseg, _type, _abbrev, _funct, _bits, _argbits, _ident)\
void _name (dst, src, segd, nelt, nseg, scratch)			\
vec_p dst, src, segd, scratch;						\
int nelt, nseg;								\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (nseg == 1) {							\
    _type val = _unseg (src, nelt, scratch);				\
    GLUE3 (CM,_abbrev,move_const_always_1L) (dst, val, _argbits);	\
  }									\
  else {								\
    CM_field_id_t tmp_src, tmp_dst, segindx, segstart, segempty;	\
    CM_field_id_t vp_src, vp_dst, vp_segindx, vp_segstart, vp_segempty;	\
    int elt_count = _CVL_elts_per_proc (nelt);				\
    int seg_count = _CVL_elts_per_proc (nseg);				\
									\
    /* Allocate fields that we can alias, copy stuff over */		\
    tmp_src  = _CVL_allocate_stack (elt_count * _bits);			\
    tmp_dst  = _CVL_allocate_stack (seg_count * _bits);			\
    segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segempty = _CVL_allocate_stack (seg_count);				\
    segstart = _CVL_allocate_stack (elt_count);				\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, elt_count);		\
    _CVL_split_segd (segd, elt_count, seg_count, segindx, segempty,	\
		     segstart);						\
									\
    /* Switch to one VP per elt, do an inclusive segmented scan */	\
    _CVL_set_vp_set_and_context (elt_count, nelt);			\
    vp_src	= CM_make_field_alias (tmp_src);			\
    vp_segstart = CM_make_field_alias (segstart);			\
    GLUE3 (CM_scan_with,_funct,1L) (vp_src, vp_src, 0, _argbits,	\
				    CM_upward, CM_inclusive,		\
				    CM_segment_bit, vp_segstart);	\
									\
    /* Switch to one VP per segment */					\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (seg_count, nseg);			\
    vp_dst	= CM_make_field_alias (tmp_dst);			\
    vp_segindx	= CM_make_field_alias (segindx);			\
    vp_segempty = CM_make_field_alias (segempty);			\
									\
    /* To get reduction of each segment:				\
     * - move all the segindx elements to the left by one VP		\
     * - put the element count into the now-empty last element		\
     * - subtract 1 from every element in segindx, so that it now	\
     *	 contains the positions of the final elements in each segment	\
     * - get from these positions in src into the dst vector		\
     */									\
    CM_send_to_news_1L (vp_segindx, vp_segindx, 0, CM_downward,		\
			INT_BITS);					\
    CM_s_write_to_processor_1L (nseg-1, vp_segindx, nelt, INT_BITS);	\
    CM_s_subtract_constant_2_1L (vp_segindx, 1, INT_BITS);		\
    CM_get_1L (vp_dst, vp_segindx, vp_src, _bits);			\
									\
    /* Zero-length segments get reduction operation identity */		\
    CM_logand_context (vp_segempty);					\
    GLUE3 (CM,_abbrev,move_constant_1L) (vp_dst, _ident, _argbits);	\
									\
    /* Restore context and vp set, copy vp_dst back */			\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, seg_count);		\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_segstart);				\
    CM_remove_field_alias (vp_dst);					\
    CM_remove_field_alias (vp_segindx);					\
    CM_remove_field_alias (vp_segempty);				\
    CM_deallocate_stack_through (tmp_src);				\
  }									\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_NONE) /* might call unsegmented */

seg_red (add_rez,add_ruz,int,_s_,_s_add_,INT_BITS,INT_BITS,0)
seg_red (min_rez,min_ruz,int,_s_,_s_min_,INT_BITS,INT_BITS,MAX_INT)
seg_red (max_rez,max_ruz,int,_s_,_s_max_,INT_BITS,INT_BITS,MIN_INT)
seg_red (and_rez,and_ruz,int,_s_,_logand_,INT_BITS,INT_BITS,~0)
seg_red (ior_rez,ior_ruz,int,_s_,_logior_,INT_BITS,INT_BITS,0)
seg_red (xor_rez,xor_ruz,int,_s_,_logxor_,INT_BITS,INT_BITS,0)

seg_red (mul_red,mul_rud,double,_f_,_f_multiply_,DBL_BITS,SIG_EXP,1.0)
seg_red (add_red,add_rud,double,_f_,_f_add_,DBL_BITS,SIG_EXP,0.0)
seg_red (min_red,min_rud,double,_f_,_f_min_,DBL_BITS,SIG_EXP,MAX_DBL)
seg_red (max_red,max_rud,double,_f_,_f_max_,DBL_BITS,SIG_EXP,MIN_DBL)

seg_red (and_reb,and_rub,unsigned,_u_,_logand_,BOO_BITS,BOO_BITS,1)
seg_red (ior_reb,ior_rub,unsigned,_u_,_logior_,BOO_BITS,BOO_BITS,0)
seg_red (xor_reb,xor_rub,unsigned,_u_,_logxor_,BOO_BITS,BOO_BITS,0)


int mul_ruz (src, len, scratch)
vec_p src, scratch;
int len;
{
  CM_field_id_t tmp_src, vp_src;
  double result;
  int count;

  if (len == 0)
    return (1);

  count = _CVL_elts_per_proc (len);
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
			      CM_inclusive, CM_none, CM_no_field);
  /* XXX convert from float to int */
  result = CM_f_read_from_processor_1L (len-1, vp_src, SIG_EXP);

  CM_set_vp_set (_CVL_default_vp_set);
  CM_set_context ();
  CM_remove_field_alias (vp_src);
  CM_deallocate_stack_through (tmp_src);
  return ((int) result);
}
make_no_scratch (mul_ruz)
make_inplace (mul_ruz, INPLACE_NONE)


void mul_rez (dst, src, segd, nelt, nseg, scratch)
vec_p dst, src, segd, scratch;
int nelt, nseg;
{
  /* If we only have one segment, use unsegmented routine instead */
  if (nseg == 1) {
    int val = mul_ruz (src, nelt, scratch);
    CM_s_move_const_always_1L (dst, val, INT_BITS);
  }
  else {
    CM_field_id_t tmp_src, tmp_dst, segindx, segstart, segempty;
    CM_field_id_t vp_src, vp_dst, vp_segindx, vp_segstart, vp_segempty;
    int elt_count = _CVL_elts_per_proc (nelt);
    int seg_count = _CVL_elts_per_proc (nseg);

    /* Allocate fields that we can alias, copy stuff over */
    tmp_src  = _CVL_allocate_stack (elt_count * DBL_BITS);
    tmp_dst  = _CVL_allocate_stack (seg_count * DBL_BITS);
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

    /* Switch to one VP per elt, do an inclusive segmented scan */
    _CVL_set_vp_set_and_context (elt_count, nelt);
    vp_src	= CM_make_field_alias (tmp_src);
    vp_segstart = CM_make_field_alias (segstart);
    CM_scan_with_f_multiply_1L (vp_src, vp_src, 0, SIG_EXP, CM_upward,
				CM_inclusive, CM_segment_bit,
				vp_segstart);

    /* Switch to one VP per segment */
    CM_set_vp_set (_CVL_default_vp_set);
    _CVL_set_vp_set_and_context (seg_count, nseg);
    vp_dst	= CM_make_field_alias (tmp_dst);
    vp_segindx	= CM_make_field_alias (segindx);
    vp_segempty = CM_make_field_alias (segempty);

    /* To get reduction of each segment:
     * - move all the segindx elements to the left by one VP
     * - put the element count into the now-empty last element
     * - subtract 1 from every element in segindx, so that it now
     *	 contains the positions of the final elements in each segment
     * - get from these positions in src into the dst vector
     */
    CM_send_to_news_1L (vp_segindx, vp_segindx, 0, CM_downward,
			INT_BITS);
    CM_s_write_to_processor_1L (nseg-1, vp_segindx, nelt, INT_BITS);
    CM_s_subtract_constant_2_1L (vp_segindx, 1, INT_BITS);
    CM_get_1L (vp_dst, vp_segindx, vp_src, INT_BITS);

    /* Zero-length segments get reduction operation identity */
    CM_logand_context (vp_segempty);
    CM_s_move_constant_1L (vp_dst, 1, INT_BITS);

    /* Restore context and vp set */
    CM_set_vp_set (_CVL_default_vp_set);
    CM_set_context ();

    /* Copy tmp_dst back into dst, rounding off to ints */
    { vec_p d = dst;
      vec_p s = tmp_dst;
      int i;
      for (i = 0; i < seg_count; i++) {
	CM_s_f_floor_2_2L (d, s, INT_BITS, SIG_EXP);
	BUMP_PTR (d, INT_BITS);
	BUMP_PTR (s, DBL_BITS);
      }
    }

    CM_remove_field_alias (vp_src);
    CM_remove_field_alias (vp_segstart);
    CM_remove_field_alias (vp_dst);
    CM_remove_field_alias (vp_segindx);
    CM_remove_field_alias (vp_segempty);
    CM_deallocate_stack_through (tmp_src);
  }
}
make_no_seg_scratch (mul_rez)
make_inplace (mul_rez, INPLACE_NONE) /* might call unsegmented */
