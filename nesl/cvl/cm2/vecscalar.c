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


/* --------------------- Unsegmented Extract ------------------------- */

#define make_ext(_name, _type, _abbrev, _bits, _argbits)		\
_type _name (src, i, len, scratch)					\
vec_p src, scratch;							\
int i, len;								\
{									\
  int count = _CVL_elts_per_proc (len);					\
  BUMP_PTR (src, (i%count) * _bits);					\
  return (GLUE3 (CM,_abbrev,read_from_processor_1L) (i/count, src,	\
						     _argbits));	\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_NONE)

make_ext (ext_vuz,int,_s_,INT_BITS,INT_BITS)
make_ext (ext_vud,double,_f_,DBL_BITS,SIG_EXP)
make_ext (ext_vub,unsigned,_u_,BOO_BITS,BOO_BITS)


/* ---------------------- Unsegmented Replace ------------------------ */

#define make_rep(_name, _type, _abbrev, _bits, _argbits)		\
void _name (dst, i, val, len, scratch)					\
vec_p dst, scratch;							\
int i, len;								\
_type val;								\
{									\
  int count = _CVL_elts_per_proc (len);					\
  BUMP_PTR (dst, (i%count) * _bits);					\
  GLUE3 (CM,_abbrev,write_to_processor_1L) (i / count, dst, val,	\
					    _argbits);			\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_NONE)

make_rep (rep_vuz,int,_s_,INT_BITS,INT_BITS)
make_rep (rep_vud,double,_f_,DBL_BITS,SIG_EXP)
make_rep (rep_vub,unsigned,_u_,BOO_BITS,BOO_BITS)


/* -------------------- Unsegmented Distribute ----------------------- */

#define make_dist(_name, _type, _abbrev, _bits, _argbits)		\
void _name (dst, val, len, scratch)					\
vec_p dst, scratch;							\
_type val;								\
int len;								\
{									\
  int count = _CVL_elts_per_proc (len);					\
  while (count--) {							\
    GLUE3 (CM,_abbrev,move_const_always_1L) (dst, val, _argbits);	\
    BUMP_PTR (dst, _bits);						\
  }									\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_NONE)

make_dist (dis_vuz,int,_s_,INT_BITS,INT_BITS)
make_dist (dis_vud,double,_f_,DBL_BITS,SIG_EXP)
make_dist (dis_vub,unsigned,_u_,BOO_BITS,BOO_BITS)


/* ----------------------- Segmented Extract ------------------------- */

#define make_segext(_name, _unseg, _type, _abbrev, _bits, _argbits)	\
void _name (dst, src, indx, segd, nelt, nseg, scratch)			\
vec_p dst, src, indx, segd, scratch;					\
int nelt, nseg;								\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (nseg == 1) {							\
    int i = CM_s_read_from_processor_1L (0, indx, INT_BITS);		\
    _type res = _unseg (src, i, nelt, scratch);				\
    GLUE3 (CM,_abbrev,move_const_always_1L) (dst, res, _argbits);	\
  }									\
  else {								\
    CM_field_id_t tmp_src, tmp_dst, tmp_indx;				\
    CM_field_id_t vp_src, vp_dst, vp_indx;				\
    CM_field_id_t segindx, segstart, segempty;				\
    CM_field_id_t vp_segindx, vp_segempty;				\
    int elt_count = _CVL_elts_per_proc (nelt);				\
    int seg_count = _CVL_elts_per_proc (nseg);				\
									\
    /* Allocate aliasable fields */					\
    tmp_src  = _CVL_allocate_stack (elt_count * _bits);			\
    tmp_dst  = _CVL_allocate_stack (seg_count * _bits);			\
    tmp_indx = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segempty = _CVL_allocate_stack (seg_count);				\
    segstart = _CVL_allocate_stack (elt_count);				\
									\
    /* Copy fields and segment descriptor */				\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, elt_count);		\
    _CVL_copy_s_field (tmp_indx, indx, seg_count);			\
    _CVL_split_segd (segd, elt_count, seg_count, segindx, segempty,	\
		     segstart);						\
									\
    /* Switch to one VP per elt, make elt-wise aliases */		\
    _CVL_set_vp_set (elt_count);					\
    vp_src = CM_make_field_alias (tmp_src);				\
									\
    /* Switch to one VP per segment, make seg-wise aliases */		\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (seg_count, nseg);			\
    vp_indx	= CM_make_field_alias (tmp_indx);			\
    vp_segindx	= CM_make_field_alias (segindx);			\
    vp_segempty = CM_make_field_alias (segempty);			\
    vp_dst	= CM_make_field_alias (tmp_dst);			\
									\
    /* Ignore empty segments */						\
    CM_lognot_1_1L (vp_segempty, 1);					\
    CM_logand_context (vp_segempty);					\
									\
    /* Add segment start indices (in segindx) to relative offsets	\
     * within segments (in indx) to give absolute offsets.		\
     */									\
    CM_s_add_2_1L (vp_indx, vp_segindx, INT_BITS);			\
									\
    /* Get from those addresses in src into dst */			\
    CM_get_1L (vp_dst, vp_indx, vp_src, _bits);				\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, seg_count);		\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_indx);					\
    CM_remove_field_alias (vp_segindx);					\
    CM_remove_field_alias (vp_segempty);				\
    CM_remove_field_alias (vp_dst);					\
    CM_deallocate_stack_through (tmp_src);				\
  }									\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_NONE) /* might call unsegmented */

make_segext (ext_vez,ext_vuz,int,_s_,INT_BITS,INT_BITS)
make_segext (ext_ved,ext_vud,double,_f_,DBL_BITS,SIG_EXP)
make_segext (ext_veb,ext_vub,unsigned,_u_,BOO_BITS,BOO_BITS)


/* ----------------------- Segmented Replace ------------------------- */

#define make_segrep(_name, _unseg, _type, _abbrev, _bits, _argbits)	\
void _name (dst, indx, val, segd, nelt, nseg, scratch)			\
vec_p dst, indx, val, segd, scratch;					\
int nelt, nseg;								\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (nseg == 1) {							\
    int i = CM_s_read_from_processor_1L (0, indx, INT_BITS);		\
    _type v = GLUE3 (CM,_abbrev,read_from_processor_1L)			\
		      (0, val, _argbits);				\
    _unseg (dst, i, v, nelt, scratch);					\
  } else {								\
    CM_field_id_t tmp_val, tmp_dst, tmp_indx;				\
    CM_field_id_t vp_val, vp_dst, vp_indx;				\
    CM_field_id_t segindx, segstart, segempty;				\
    CM_field_id_t vp_segindx, vp_segempty;				\
    int elt_count = _CVL_elts_per_proc (nelt);				\
    int seg_count = _CVL_elts_per_proc (nseg);				\
									\
    /* Allocate aliasable fields */					\
    tmp_dst  = _CVL_allocate_stack (elt_count * _bits);			\
    tmp_val  = _CVL_allocate_stack (seg_count * _bits);			\
    tmp_indx = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segempty = _CVL_allocate_stack (seg_count);				\
    segstart = _CVL_allocate_stack (elt_count);				\
									\
    /* Copy fields and segment descriptor */				\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_dst, dst, elt_count);		\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_val, val, seg_count);		\
    _CVL_copy_s_field (tmp_indx, indx, seg_count);			\
    _CVL_split_segd (segd, elt_count, seg_count, segindx,		\
		     segempty, segstart);				\
									\
    /* Switch to one VP per elt, make elt-wise aliases */		\
    _CVL_set_vp_set (elt_count);					\
    vp_dst = CM_make_field_alias (tmp_dst);				\
									\
    /* Switch to one VP per segment, make seg-wise aliases */		\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (seg_count, nseg);			\
    vp_indx	= CM_make_field_alias (tmp_indx);			\
    vp_segindx	= CM_make_field_alias (segindx);			\
    vp_segempty = CM_make_field_alias (segempty);			\
    vp_val	= CM_make_field_alias (tmp_val);			\
									\
    /* Ignore empty segments */						\
    CM_lognot_1_1L (vp_segempty, 1);					\
    CM_logand_context (vp_segempty);					\
									\
    /* Add segment start indices (in segindx) to relative offsets	\
     * within segments (in indx) to give absolute offsets		\
     */									\
    CM_s_add_2_1L (vp_indx, vp_segindx, INT_BITS);			\
									\
    /* Send val to those addresses in dst */				\
    CM_send_1L (vp_dst, vp_indx, vp_val, _bits, CM_no_field);		\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, elt_count);		\
    CM_remove_field_alias (vp_dst);					\
    CM_remove_field_alias (vp_indx);					\
    CM_remove_field_alias (vp_segindx);					\
    CM_remove_field_alias (vp_segempty);				\
    CM_remove_field_alias (vp_val);					\
    CM_deallocate_stack_through (tmp_dst);				\
  }									\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_NONE) /* might call unsegmented */

make_segrep (rep_vez,rep_vuz,int,_s_,INT_BITS,INT_BITS)
make_segrep (rep_ved,rep_vud,double,_f_,DBL_BITS,SIG_EXP)
make_segrep (rep_veb,rep_vub,unsigned,_u_,BOO_BITS,BOO_BITS)


/* --------------------- Segmented Distribute ------------------------ */

#define make_segdist(_name, _unseg, _type, _abbrev, _bits, _argbits)	\
void _name (dst, val, segd, nelt, nseg, scratch)			\
vec_p dst, val, segd, scratch;						\
int nelt, nseg;								\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (nseg == 1) {							\
    _type oneval = GLUE3 (CM,_abbrev,read_from_processor_1L)		\
      (0, val, _argbits);						\
    _unseg (dst, oneval, nelt, scratch);				\
  }									\
  else {								\
    CM_field_id_t tmp_dst, tmp_val;					\
    CM_field_id_t vp_dst, vp_val;					\
    CM_field_id_t segindx, segstart, segempty;				\
    CM_field_id_t vp_segindx, vp_segstart, vp_segempty;			\
    int elt_count = _CVL_elts_per_proc (nelt);				\
    int seg_count = _CVL_elts_per_proc (nseg);				\
									\
    /* Allocate fields that we can alias to, copy stuff over */		\
    tmp_dst  = _CVL_allocate_stack (elt_count * _bits);			\
    tmp_val  = _CVL_allocate_stack (seg_count * _bits);			\
    segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segempty = _CVL_allocate_stack (seg_count);				\
    segstart = _CVL_allocate_stack (elt_count);				\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_val, val, seg_count);		\
    _CVL_split_segd (segd, elt_count, seg_count, segindx, segempty,	\
		     segstart);						\
									\
    /* Switch to VP set with one VP per elt, make aliases */		\
    _CVL_set_vp_set (elt_count);					\
    vp_dst	= CM_make_field_alias (tmp_dst);			\
    vp_segstart = CM_make_field_alias (segstart);			\
									\
    /* Switch to VP set with one VP per segment */			\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (seg_count, nseg);			\
    vp_val	= CM_make_field_alias (tmp_val);			\
    vp_segindx	= CM_make_field_alias (segindx);			\
    vp_segempty = CM_make_field_alias (segempty);			\
									\
    /* Ignore empty segments */						\
    CM_lognot_1_1L (vp_segempty, 1);					\
    CM_logand_context (vp_segempty);					\
									\
    /* Send initial values into start-of-seg positions of dst */	\
    CM_send_1L (vp_dst, vp_segindx, vp_val, _bits, CM_no_field);	\
									\
    /* Switch back to one VP per elt, do a copy-scan on dst */		\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (elt_count, nelt);			\
    CM_scan_with_copy_1L (vp_dst, vp_dst, 0, _bits, CM_upward,		\
			  CM_inclusive, CM_segment_bit, vp_segstart);	\
									\
    /* Reset VP set and context, copy into dst */			\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, elt_count);		\
    CM_remove_field_alias (vp_dst);					\
    CM_remove_field_alias (vp_segstart);				\
    CM_remove_field_alias (vp_val);					\
    CM_remove_field_alias (vp_segindx);					\
    CM_remove_field_alias (vp_segempty);				\
    CM_deallocate_stack_through (tmp_dst);				\
  }									\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_NONE) /* might call unsegmented */

make_segdist (dis_vez,dis_vuz,int,_s_,INT_BITS,INT_BITS)
make_segdist (dis_ved,dis_vud,double,_f_,DBL_BITS,SIG_EXP)
make_segdist (dis_veb,dis_vub,unsigned,_u_,BOO_BITS,BOO_BITS)
