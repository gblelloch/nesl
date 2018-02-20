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


/* We stuff these definitions into the normal permute-defining macros,
 * and thereby turn them into flag-permute-defining macros.  Cunning...
 * Note that we assume the existence of the variable count
 */
#define FLGARG indx,flg

#define FLGDEFINE CM_field_id_t tmp_flg, vp_flg;

#define FLGALLOC tmp_flg = _CVL_allocate_stack (count);			\
		 _CVL_copy_u_field (tmp_flg, flg, count);

#define FLGUSE vp_flg = CM_make_field_alias (tmp_flg);			\
	       CM_logand_context (vp_flg);				\
	       CM_remove_field_alias (vp_flg);


/* ------------------- Unsegmented Simple Permute -------------------- */

#define make_smp(_name, _abbrev, _bits)					\
void _name (dst, src, indx, len, scratch)				\
vec_p dst, src, indx, scratch;						\
int len;								\
{									\
  CM_field_id_t tmp_src, tmp_indx, tmp_dst, vp_src, vp_indx, vp_dst;	\
  int count = _CVL_elts_per_proc (len);					\
									\
  tmp_src  = _CVL_allocate_stack (count * _bits);			\
  tmp_dst  = _CVL_allocate_stack (count * _bits);			\
  tmp_indx = _CVL_allocate_stack (count * INT_BITS);			\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, count);		\
  _CVL_copy_s_field (tmp_indx, indx, count);				\
									\
  _CVL_set_vp_set_and_context (count, len);				\
  vp_dst  = CM_make_field_alias (tmp_dst);				\
  vp_src  = CM_make_field_alias (tmp_src);				\
  vp_indx = CM_make_field_alias (tmp_indx);				\
  CM_send_1L (vp_dst, vp_indx, vp_src, _bits, CM_no_field);		\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, count);		\
  CM_remove_field_alias (vp_dst);					\
  CM_remove_field_alias (vp_src);					\
  CM_remove_field_alias (vp_indx);					\
  CM_deallocate_stack_through (tmp_src);				\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_1 | INPLACE_2)

make_smp (smp_puz,_s_,INT_BITS)
make_smp (smp_pub,_u_,BOO_BITS)
make_smp (smp_pud,_f_,DBL_BITS)


/* ----------------- Unsegmented Simple Flag Permute ----------------- */

#define make_smpflg(_name, _abbrev, _bits)				\
void _name (dst, src, indx, flg, src_len, dst_len, scratch)		\
vec_p dst, src, indx, flg, scratch;					\
int src_len, dst_len;							\
{									\
  CM_field_id_t tmp_src, tmp_indx, tmp_dst, tmp_flg;			\
  CM_field_id_t	 vp_src, vp_indx, vp_dst, vp_flg;			\
  int src_count = _CVL_elts_per_proc (src_len);				\
  int dst_count = _CVL_elts_per_proc (dst_len);				\
									\
  tmp_src  = _CVL_allocate_stack (src_count * _bits);			\
  tmp_dst  = _CVL_allocate_stack (dst_count * _bits);			\
  tmp_indx = _CVL_allocate_stack (src_count * INT_BITS);		\
  tmp_flg  = _CVL_allocate_stack (src_count);				\
  _CVL_copy_u_field (tmp_flg, flg, src_count);				\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, src_count);		\
  _CVL_copy_s_field (tmp_indx, indx, src_count);			\
									\
  _CVL_set_vp_set (dst_count);						\
  vp_dst = CM_make_field_alias (tmp_dst);				\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  _CVL_set_vp_set_and_context (src_count, src_len);			\
  vp_src  = CM_make_field_alias (tmp_src);				\
  vp_indx = CM_make_field_alias (tmp_indx);				\
  vp_flg  = CM_make_field_alias (tmp_flg);				\
  CM_logand_context (vp_flg);						\
  CM_send_1L (vp_dst, vp_indx, vp_src, _bits, CM_no_field);		\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, dst_count);		\
  CM_remove_field_alias (vp_dst);					\
  CM_remove_field_alias (vp_src);					\
  CM_remove_field_alias (vp_indx);					\
  CM_remove_field_alias (vp_flg);					\
  CM_deallocate_stack_through (tmp_src);				\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2)

make_smpflg (fpm_puz,_s_,INT_BITS)
make_smpflg (fpm_pub,_u_,BOO_BITS)
make_smpflg (fpm_pud,_f_,DBL_BITS)


/* -------------------- Unsegmented Back Permute --------------------- */

#define make_bck(_name, _abbrev, _bits)					\
void _name (dst, src, indx, src_len, dst_len, scratch)			\
vec_p dst, src, indx, scratch;						\
int src_len, dst_len;							\
{									\
  CM_field_id_t tmp_src, tmp_indx, tmp_dst, vp_src, vp_indx, vp_dst;	\
  int dst_count = _CVL_elts_per_proc (dst_len);				\
  int src_count = _CVL_elts_per_proc (src_len);				\
									\
  tmp_src  = _CVL_allocate_stack (src_count * _bits);			\
  tmp_dst  = _CVL_allocate_stack (dst_count * _bits);			\
  tmp_indx = _CVL_allocate_stack (dst_count * INT_BITS);		\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, src_count);		\
  _CVL_copy_s_field (tmp_indx, indx, dst_count);			\
									\
  _CVL_set_vp_set (src_count);						\
  vp_src = CM_make_field_alias (tmp_src);				\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  _CVL_set_vp_set_and_context (dst_count, dst_len);			\
  vp_dst  = CM_make_field_alias (tmp_dst);				\
  vp_indx = CM_make_field_alias (tmp_indx);				\
  CM_get_1L (vp_dst, vp_indx, vp_src, _bits);				\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, dst_count);		\
  CM_remove_field_alias (vp_src);					\
  CM_remove_field_alias (vp_dst);					\
  CM_remove_field_alias (vp_indx);					\
  CM_deallocate_stack_through (tmp_src);				\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2)

make_bck (bck_puz,_s_,INT_BITS)
make_bck (bck_pub,_u_,BOO_BITS)
make_bck (bck_pud,_f_,DBL_BITS)


/* ------------------ Unsegmented Back Flag Permute ------------------ */

#define make_bckflg(_name, _abbrev, _bits)				\
void _name (dst, src, indx, flags, src_len, dst_len, scratch)		\
vec_p dst, src, indx, flags, scratch;					\
int src_len, dst_len;							\
{									\
  CM_field_id_t tmp_src, tmp_indx, tmp_dst, tmp_flg;			\
  CM_field_id_t vp_src, vp_indx, vp_dst, vp_flg;			\
  int dst_count = _CVL_elts_per_proc (dst_len);				\
  int src_count = _CVL_elts_per_proc (src_len);				\
									\
  tmp_src  = _CVL_allocate_stack (src_count * _bits);			\
  tmp_dst  = _CVL_allocate_stack (dst_count * _bits);			\
  tmp_indx = _CVL_allocate_stack (dst_count * INT_BITS);		\
  tmp_flg  = _CVL_allocate_stack (dst_count);				\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, src_count);		\
  _CVL_copy_s_field (tmp_indx, indx, dst_count);			\
  _CVL_copy_u_field (tmp_flg, flags, dst_count);			\
									\
  _CVL_set_vp_set (src_count);						\
  vp_src = CM_make_field_alias (tmp_src);				\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  _CVL_set_vp_set_and_context (dst_count, dst_len);			\
  vp_dst  = CM_make_field_alias (tmp_dst);				\
  vp_indx = CM_make_field_alias (tmp_indx);				\
  vp_flg  = CM_make_field_alias (tmp_flg);				\
  CM_logand_context (vp_flg);						\
  CM_get_1L (vp_dst, vp_indx, vp_src, _bits);				\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, dst_count);		\
  CM_remove_field_alias (vp_src);					\
  CM_remove_field_alias (vp_dst);					\
  CM_remove_field_alias (vp_indx);					\
  CM_remove_field_alias (vp_flg);					\
  CM_deallocate_stack_through (tmp_src);				\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2 | INPLACE_3)

make_bckflg (bfp_puz,_s_,INT_BITS)
make_bckflg (bfp_pub,_u_,BOO_BITS)
make_bckflg (bfp_pud,_f_,DBL_BITS)


/* ------------------ Unsegmented Default Permute -------------------- */

#define make_def(_name, _abbrev, _bits)					\
void _name (dst, src, indx, def, src_len, dst_len, scratch)		\
vec_p dst, src, indx, def, scratch;					\
int src_len, dst_len;							\
{									\
  CM_field_id_t tmp_src, tmp_dst, tmp_indx, vp_src, vp_dst, vp_indx;	\
  int src_count = _CVL_elts_per_proc (src_len);				\
  int dst_count = _CVL_elts_per_proc (dst_len);				\
									\
  tmp_src  = _CVL_allocate_stack (src_count * _bits);			\
  tmp_dst  = _CVL_allocate_stack (dst_count * _bits);			\
  tmp_indx = _CVL_allocate_stack (src_count * INT_BITS);		\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, src_count);		\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_dst, def, dst_count);		\
  _CVL_copy_s_field (tmp_indx, indx, src_count);			\
									\
  _CVL_set_vp_set (dst_count);						\
  vp_dst = CM_make_field_alias (tmp_dst);				\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  _CVL_set_vp_set_and_context (src_count, src_len);			\
  vp_src  = CM_make_field_alias (tmp_src);				\
  vp_indx = CM_make_field_alias (tmp_indx);				\
  CM_send_1L (vp_dst, vp_indx, vp_src, _bits, CM_no_field);		\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, dst_count);		\
  CM_remove_field_alias (vp_dst);					\
  CM_remove_field_alias (vp_src);					\
  CM_remove_field_alias (vp_indx);					\
  CM_deallocate_stack_through (tmp_src);				\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2 | INPLACE_3)

make_def (dpe_puz,_s_,INT_BITS);
make_def (dpe_pub,_u_,BOO_BITS);
make_def (dpe_pud,_f_,DBL_BITS);


/* ---------------- Unsegmented Default Flag Permute ----------------- */

#define make_defflg(_name, _abbrev, _bits)				\
void _name (dst, src, indx, flg, def, src_len, dst_len, scratch)	\
vec_p dst, src, indx, flg, def, scratch;				\
int src_len, dst_len;							\
{									\
  CM_field_id_t tmp_src, tmp_dst, tmp_indx, tmp_flg;			\
  CM_field_id_t vp_src, vp_dst, vp_indx, vp_flg;			\
  int src_count = _CVL_elts_per_proc (src_len);				\
  int dst_count = _CVL_elts_per_proc (dst_len);				\
									\
  tmp_src  = _CVL_allocate_stack (src_count * _bits);			\
  tmp_dst  = _CVL_allocate_stack (dst_count * _bits);			\
  tmp_indx = _CVL_allocate_stack (src_count * INT_BITS);		\
  tmp_flg  = _CVL_allocate_stack (src_count);				\
  _CVL_copy_u_field (tmp_flg, flg, src_count);				\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, src_count);		\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_dst, def, dst_count);		\
  _CVL_copy_s_field (tmp_indx, indx, src_count);			\
									\
  _CVL_set_vp_set (dst_count);						\
  vp_dst = CM_make_field_alias (tmp_dst);				\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  _CVL_set_vp_set_and_context (src_count, src_len);			\
  vp_src  = CM_make_field_alias (tmp_src);				\
  vp_indx = CM_make_field_alias (tmp_indx);				\
  vp_flg  = CM_make_field_alias (tmp_flg);				\
  CM_logand_context (vp_flg);						\
  CM_send_1L (vp_dst, vp_indx, vp_src, _bits, CM_no_field);		\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, dst_count);		\
  CM_remove_field_alias (vp_dst);					\
  CM_remove_field_alias (vp_src);					\
  CM_remove_field_alias (vp_indx);					\
  CM_remove_field_alias (vp_flg);					\
  CM_deallocate_stack_through (tmp_src);				\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2 | INPLACE_3 | INPLACE_4)

make_defflg (dfp_puz,_s_,INT_BITS);
make_defflg (dfp_pub,_u_,BOO_BITS);
make_defflg (dfp_pud,_f_,DBL_BITS);


/* -------------------- Segmented Simple Permute --------------------- */

#define make_segsmp(_name, _unseg, _abbrev, _bits)			\
void _name (dst, src, indx, segd, nelt, nseg, scratch)			\
vec_p dst, src, indx, segd, scratch;					\
int nelt, nseg;								\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (nseg == 1) {							\
    _unseg (dst, src, indx, nelt, scratch);				\
  }									\
  else {								\
    CM_field_id_t tmp_src, tmp_dst, tmp_indx;				\
    CM_field_id_t vp_src, vp_dst, vp_indx;				\
    CM_field_id_t segindx, segstart, segempty, addr;			\
    CM_field_id_t vp_segstart, vp_addr;					\
    int elt_count = _CVL_elts_per_proc (nelt);				\
    int seg_count = _CVL_elts_per_proc (nseg);				\
									\
    /* Allocate aliasable fields */					\
    tmp_src  = _CVL_allocate_stack (elt_count * _bits);			\
    tmp_dst  = _CVL_allocate_stack (elt_count * _bits);			\
    tmp_indx = _CVL_allocate_stack (elt_count * INT_BITS);		\
    segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segempty = _CVL_allocate_stack (seg_count);				\
    segstart = _CVL_allocate_stack (elt_count);				\
    addr     = _CVL_allocate_stack (elt_count * INT_BITS);		\
									\
    /* Copy fields and segment descriptor */				\
    _CVL_copy_s_field (tmp_indx, indx, elt_count);			\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, elt_count);		\
    _CVL_split_segd (segd, elt_count, seg_count, segindx, segempty,	\
		     segstart);						\
									\
    /* Switch to one VP per elt, make elt-wise aliases */		\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (elt_count, nelt);			\
    vp_dst	= CM_make_field_alias (tmp_dst);			\
    vp_src	= CM_make_field_alias (tmp_src);			\
    vp_indx	= CM_make_field_alias (tmp_indx);			\
    vp_segstart = CM_make_field_alias (segstart);			\
    vp_addr	= CM_make_field_alias (addr);				\
									\
    /* Set up vp_addr so that in each segment, each elt contains	\
     * the offset (address) of the first element of that segment	\
     */									\
    CM_my_send_address (vp_addr);					\
    CM_scan_with_copy_1L (vp_addr, vp_addr, 0, INT_BITS, CM_upward,	\
			  CM_inclusive, CM_segment_bit, vp_segstart);	\
									\
    /* Now add indices to addr, to give the absolute destination of	\
     * each element: indx just contains the destination relative to	\
     * the start of each segment					\
     */									\
    CM_s_add_2_1L (vp_addr, vp_indx, INT_BITS);				\
									\
    /* Send src to dst, using addr as the address */			\
    CM_send_1L (vp_dst, vp_addr, vp_src, _bits, CM_no_field);		\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, elt_count);		\
    CM_remove_field_alias (vp_dst);					\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_indx);					\
    CM_remove_field_alias (vp_segstart);				\
    CM_remove_field_alias (vp_addr);					\
    CM_deallocate_stack_through (tmp_src);				\
  }									\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2) /* might call unsegmented */

make_segsmp (smp_pez,smp_puz,_s_,INT_BITS)
make_segsmp (smp_peb,smp_pub,_u_,BOO_BITS)
make_segsmp (smp_ped,smp_pud,_f_,DBL_BITS)


/* ------------------ Segmented Simple Flag Permute ------------------ */

#define make_segsmpflg(_name, _unseg, _abbrev, _bits)			\
void _name (dst, src, indx, flg, s_segd, s_nelt, s_nseg, d_segd,	\
	    d_nelt, d_nseg, scratch)					\
vec_p dst, src, indx, flg, s_segd, d_segd, scratch;			\
int s_nelt, s_nseg, d_nelt, d_nseg;					\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (s_nseg == 1) {							\
    _unseg (dst, src, indx, flg, s_nelt, d_nelt, scratch);		\
  }									\
  else {								\
    CM_field_id_t tmp_src, tmp_dst, tmp_indx, tmp_flg;			\
    CM_field_id_t vp_indx, vp_src, vp_dst, vp_flg;			\
    CM_field_id_t segindx, segstart, segempty, addr;			\
    CM_field_id_t vp_segindx, vp_segstart, vp_addr;			\
    int s_elt_count = _CVL_elts_per_proc (s_nelt);			\
    int d_elt_count = _CVL_elts_per_proc (d_nelt);			\
    int seg_count   = _CVL_elts_per_proc (s_nseg);			\
									\
    /* Allocate aliasable fields */					\
    tmp_src  = _CVL_allocate_stack (s_elt_count * _bits);		\
    tmp_dst  = _CVL_allocate_stack (s_elt_count * _bits);		\
    tmp_indx = _CVL_allocate_stack (s_elt_count * INT_BITS);		\
    segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segempty = _CVL_allocate_stack (seg_count);				\
    segstart = _CVL_allocate_stack (s_elt_count);			\
    addr     = _CVL_allocate_stack (s_elt_count * INT_BITS);		\
    tmp_flg  = _CVL_allocate_stack (s_elt_count);			\
									\
    /* Copy fields and segment descriptor */				\
    _CVL_copy_u_field (tmp_flg, flg, s_elt_count);			\
    _CVL_copy_s_field (tmp_indx, indx, s_elt_count);			\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, s_elt_count);	\
    _CVL_split_segd (s_segd, s_elt_count, seg_count, segindx,		\
		     segempty, segstart);				\
									\
    /* Switch to one VP per destination element, make dst alias */	\
    _CVL_set_vp_set (d_elt_count);					\
    vp_dst = CM_make_field_alias (tmp_dst);				\
									\
    /* Switch to one VP per segment, make seg-wise aliases */		\
    _CVL_set_vp_set (seg_count);					\
    vp_segindx = CM_make_field_alias (segindx);				\
									\
    /* Switch to one VP per elt, make source-elt-wise aliases */	\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (s_elt_count, s_nelt);			\
    vp_src	= CM_make_field_alias (tmp_src);			\
    vp_indx	= CM_make_field_alias (tmp_indx);			\
    vp_segstart = CM_make_field_alias (segstart);			\
    vp_addr	= CM_make_field_alias (addr);				\
    vp_flg	= CM_make_field_alias (tmp_flg);			\
									\
    /* Set up vp_addr so that in each segment, each elt contains	\
     * the offset (address) of the first element of that segment	\
     */									\
    CM_my_send_address (vp_addr);					\
    CM_scan_with_copy_1L (vp_addr, vp_addr, 0, INT_BITS, CM_upward,	\
			  CM_inclusive, CM_segment_bit, vp_segstart);	\
									\
    /* Now add indices to addr, to give the absolute destination of	\
     * each element: indx just contains the destination relative to	\
     * the start of each segment					\
     */									\
    CM_s_add_2_1L (vp_addr, vp_indx, INT_BITS);				\
									\
    /* Send src to dst, using addr as the address */			\
    CM_logand_context (vp_flg);						\
    CM_send_1L (vp_dst, vp_addr, vp_src, _bits, CM_no_field);		\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, d_elt_count);	\
    CM_remove_field_alias (vp_dst);					\
    CM_remove_field_alias (vp_segindx);					\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_indx);					\
    CM_remove_field_alias (vp_segstart);				\
    CM_remove_field_alias (vp_addr);					\
    CM_remove_field_alias (vp_flg);					\
    CM_deallocate_stack_through (tmp_src);				\
  }									\
}									\
make_no_seg2_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2 | INPLACE_3)	/* ditto */

make_segsmpflg (fpm_pez,fpm_puz,_s_,INT_BITS)
make_segsmpflg (fpm_peb,fpm_pub,_u_,BOO_BITS)
make_segsmpflg (fpm_ped,fpm_pud,_f_,DBL_BITS)


/* --------------------- Segmented Back Permute ---------------------- */

#define make_segbck(_name, _unseg, _abbrev, _bits)			\
void _name (dst, src, indx, s_segd, s_nelt, s_nseg, d_segd, d_nelt,	\
	    d_nseg, scratch)						\
vec_p dst, src, indx, s_segd, d_segd, scratch;				\
int s_nelt, s_nseg, d_nelt, d_nseg;					\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (s_nseg == 1) {							\
    _unseg (dst, src, indx, s_nelt, d_nelt, scratch);			\
  }									\
  else {								\
    CM_field_id_t tmp_src, tmp_dst, tmp_indx;				\
    CM_field_id_t vp_src, vp_dst, vp_indx;				\
    CM_field_id_t s_segindx, s_segstart, s_segempty, addr;		\
    CM_field_id_t vp_s_segindx, vp_s_segempty, vp_addr;			\
    CM_field_id_t d_segindx, d_segstart, d_segempty;			\
    CM_field_id_t vp_d_segindx, vp_d_segstart;				\
    int s_elt_count = _CVL_elts_per_proc (s_nelt);			\
    int d_elt_count = _CVL_elts_per_proc (d_nelt);			\
    int seg_count   = _CVL_elts_per_proc (s_nseg);			\
									\
    /* Allocate aliasable fields */					\
    tmp_src    = _CVL_allocate_stack (s_elt_count * _bits);		\
    tmp_dst    = _CVL_allocate_stack (d_elt_count * _bits);		\
    tmp_indx   = _CVL_allocate_stack (d_elt_count * INT_BITS);		\
    d_segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    d_segempty = _CVL_allocate_stack (seg_count);			\
    d_segstart = _CVL_allocate_stack (d_elt_count);			\
    s_segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    s_segempty = _CVL_allocate_stack (seg_count);			\
    s_segstart = _CVL_allocate_stack (s_elt_count);			\
    addr       = _CVL_allocate_stack (d_elt_count * INT_BITS);		\
									\
    /* Copy fields and segment descriptors */				\
    _CVL_copy_s_field (tmp_indx, indx, d_elt_count);			\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, s_elt_count);	\
    _CVL_split_segd (d_segd, d_elt_count, seg_count, d_segindx,		\
		     d_segempty, d_segstart);				\
    _CVL_split_segd (s_segd, s_elt_count, seg_count, s_segindx,		\
		     s_segempty, s_segstart);				\
									\
    /* Switch to one VP per src element, make aliases */		\
    _CVL_set_vp_set (s_elt_count);					\
    vp_src = CM_make_field_alias (tmp_src);				\
									\
    /* Switch to one VP per dst element, make aliases */		\
    _CVL_set_vp_set (d_elt_count);					\
    vp_dst	  = CM_make_field_alias (tmp_dst);			\
    vp_indx	  = CM_make_field_alias (tmp_indx);			\
    vp_addr	  = CM_make_field_alias (addr);				\
    vp_d_segstart = CM_make_field_alias (d_segstart);			\
    CM_s_move_zero_always_1L (vp_addr, INT_BITS);			\
									\
    _CVL_set_vp_set (seg_count);					\
    vp_d_segindx = CM_make_field_alias (d_segindx);			\
									\
    /* Switch to one VP per src segment, make aliases */		\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (seg_count, s_nseg);			\
    vp_s_segindx = CM_make_field_alias (s_segindx);			\
									\
    /* Switch off empty src segments */					\
    vp_s_segempty = CM_make_field_alias (s_segempty);			\
    CM_lognot_1_1L (vp_s_segempty, 1);					\
    CM_logand_context (vp_s_segempty);					\
									\
    /* Send start-of-segment positions of src vector into start-of-	\
     * segment positions of addr, which is the same shape as dst.	\
     * Use send-with-max so that if any dst segments are empty (and	\
     * hence multiple src start-of-seg positions get sent to the	\
     * same start-of-segment position in addr), the empty segments	\
     * are ignored							\
     */									\
    CM_send_with_s_max_1L (vp_addr, vp_d_segindx, vp_s_segindx,		\
			   INT_BITS, CM_no_field);			\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (d_elt_count, d_nelt);			\
									\
    /* Do a segmented copy-scan on vp_addr; in each segment, each	\
     * element now contains the offset (address) of the first		\
     * element of the corresponding segment in the src vector		\
     */									\
    CM_scan_with_copy_1L (vp_addr, vp_addr, 0, INT_BITS, CM_upward,	\
			  CM_inclusive, CM_segment_bit, vp_d_segstart);	\
									\
    /* Now add indices to addr, to give the absolute address of		\
     * each element in src (indx just contains the address relative	\
     * to the start of each segment in src)				\
     */									\
    CM_s_add_2_1L (vp_addr, vp_indx, INT_BITS);				\
									\
    /* Get src to dst, using addr as the address */			\
    CM_get_1L (vp_dst, vp_addr, vp_src, _bits);				\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, d_elt_count);	\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_dst);					\
    CM_remove_field_alias (vp_indx);					\
    CM_remove_field_alias (vp_addr);					\
    CM_remove_field_alias (vp_d_segstart);				\
    CM_remove_field_alias (vp_d_segindx);				\
    CM_remove_field_alias (vp_s_segindx);				\
    CM_remove_field_alias (vp_s_segempty);				\
    CM_deallocate_stack_through (tmp_src);				\
  }									\
}									\
make_no_seg2_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2) /* ditto */

make_segbck (bck_pez,bck_puz,_s_,INT_BITS)
make_segbck (bck_peb,bck_pub,_u_,BOO_BITS)
make_segbck (bck_ped,bck_pud,_f_,DBL_BITS)


/* ------------------- Segmented Back Flag Permute ------------------- */

#define make_segbckflg(_name, _unseg, _abbrev, _bits)			\
void _name (dst, src, indx, flg, s_segd, s_nelt, s_nseg, d_segd,	\
	    d_nelt, d_nseg, scratch)					\
vec_p dst, src, indx, flg, s_segd, d_segd, scratch;			\
int s_nelt, s_nseg, d_nelt, d_nseg;					\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (s_nseg == 1) {							\
    _unseg (dst, src, indx, flg, s_nelt, d_nelt, scratch);		\
  }									\
  else {								\
    CM_field_id_t tmp_src, tmp_dst, tmp_indx, tmp_flg, addr;		\
    CM_field_id_t vp_src, vp_dst, vp_indx, vp_flg, vp_addr;		\
    CM_field_id_t s_segindx, s_segstart, s_segempty;			\
    CM_field_id_t vp_s_segindx, vp_s_segempty;				\
    CM_field_id_t d_segindx, d_segstart, d_segempty;			\
    CM_field_id_t vp_d_segindx, vp_d_segstart;				\
    int s_elt_count = _CVL_elts_per_proc (s_nelt);			\
    int d_elt_count = _CVL_elts_per_proc (d_nelt);			\
    int seg_count   = _CVL_elts_per_proc (s_nseg);			\
									\
    /* Allocate aliasable fields */					\
    tmp_src    = _CVL_allocate_stack (s_elt_count * _bits);		\
    tmp_dst    = _CVL_allocate_stack (d_elt_count * _bits);		\
    tmp_indx   = _CVL_allocate_stack (d_elt_count * INT_BITS);		\
    d_segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    d_segempty = _CVL_allocate_stack (seg_count);			\
    d_segstart = _CVL_allocate_stack (d_elt_count);			\
    s_segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    s_segempty = _CVL_allocate_stack (seg_count);			\
    s_segstart = _CVL_allocate_stack (s_elt_count);			\
    addr       = _CVL_allocate_stack (d_elt_count * INT_BITS);		\
    tmp_flg    = _CVL_allocate_stack (d_elt_count);			\
    _CVL_copy_u_field (tmp_flg, flg, d_elt_count);			\
									\
    /* Copy fields and segment descriptors */				\
    _CVL_copy_s_field (tmp_indx, indx, d_elt_count);			\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, s_elt_count);	\
    _CVL_split_segd (d_segd, d_elt_count, seg_count, d_segindx,		\
		     d_segempty, d_segstart);				\
    _CVL_split_segd (s_segd, s_elt_count, seg_count, s_segindx,		\
		     s_segempty, s_segstart);				\
									\
    /* Switch to one VP per src element, make aliases */		\
    _CVL_set_vp_set (s_elt_count);					\
    vp_src = CM_make_field_alias (tmp_src);				\
									\
    /* Switch to one VP per dst element, make aliases */		\
    _CVL_set_vp_set (d_elt_count);					\
    vp_dst	  = CM_make_field_alias (tmp_dst);			\
    vp_indx	  = CM_make_field_alias (tmp_indx);			\
    vp_flg	  = CM_make_field_alias (tmp_flg);			\
    vp_addr	  = CM_make_field_alias (addr);				\
    vp_d_segstart = CM_make_field_alias (d_segstart);			\
    CM_s_move_zero_always_1L (vp_addr, INT_BITS);			\
									\
    _CVL_set_vp_set (seg_count);					\
    vp_d_segindx = CM_make_field_alias (d_segindx);			\
									\
    /* Switch to one VP per src segment, make aliases */		\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (seg_count, s_nseg);			\
    vp_s_segindx = CM_make_field_alias (s_segindx);			\
									\
    /* Switch off empty src segments */					\
    vp_s_segempty = CM_make_field_alias (s_segempty);			\
    CM_lognot_1_1L (vp_s_segempty, 1);					\
    CM_logand_context (vp_s_segempty);					\
									\
    /* Send start-of-segment positions of src vector into start-of-	\
     * segment positions of addr, which is the same shape as dst.	\
     * Use send-with-max so that if any dst segments are empty (and	\
     * hence multiple src start-of-seg positions get sent to the	\
     * same start-of-segment position in addr), the empty segments	\
     * are ignored.							\
     */									\
    CM_send_with_s_max_1L (vp_addr, vp_d_segindx, vp_s_segindx,		\
			   INT_BITS, CM_no_field);			\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (d_elt_count, d_nelt);			\
									\
    /* Do a segmented copy-scan on vp_addr; in each segment, each	\
     * element now contains the offset (address) of the first		\
     * element of the corresponding segment in the src vector.		\
     */									\
    CM_scan_with_copy_1L (vp_addr, vp_addr, 0, INT_BITS, CM_upward,	\
			  CM_inclusive, CM_segment_bit, vp_d_segstart);	\
									\
    /* Now add indices to addr, to give the absolute address of		\
     * each element in src (indx just contains the address relative	\
     * to the start of each segment in src).				\
     */									\
    CM_s_add_2_1L (vp_addr, vp_indx, INT_BITS);				\
									\
    /* Get src to dst, using addr as the address */			\
    CM_logand_context (vp_flg);						\
    CM_get_1L (vp_dst, vp_addr, vp_src, _bits);				\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, d_elt_count);	\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_dst);					\
    CM_remove_field_alias (vp_indx);					\
    CM_remove_field_alias (vp_flg);					\
    CM_remove_field_alias (vp_addr);					\
    CM_remove_field_alias (vp_d_segstart);				\
    CM_remove_field_alias (vp_d_segindx);				\
    CM_remove_field_alias (vp_s_segindx);				\
    CM_remove_field_alias (vp_s_segempty);				\
    CM_deallocate_stack_through (tmp_src);				\
  }									\
}									\
make_no_seg2_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2 | INPLACE_3)	/* ditto */

make_segbckflg (bfp_pez,bfp_puz,_s_,INT_BITS)
make_segbckflg (bfp_peb,bfp_pub,_u_,BOO_BITS)
make_segbckflg (bfp_ped,bfp_pud,_f_,DBL_BITS)


/* ------------------- Segmented Default Permute --------------------- */

#define make_segdef(_name, _unseg, _abbrev, _bits)			\
void _name (dst, src, indx, def, s_segd, s_nelt, s_nseg, d_segd,	\
	    d_nelt, d_nseg, scratch)					\
vec_p dst, src, indx, def, s_segd, d_segd, scratch;			\
int s_nelt, s_nseg, d_nelt, d_nseg;					\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (s_nseg == 1) {							\
    _unseg (dst, src, indx, def, s_nelt, d_nelt, scratch);		\
  }									\
  else {								\
    /* This is ridiculous */						\
    CM_field_id_t tmp_src, tmp_dst, tmp_indx, addr;			\
    CM_field_id_t vp_src, vp_dst, vp_indx, vp_addr;			\
    CM_field_id_t s_segindx, s_segstart, s_segempty;			\
    CM_field_id_t vp_s_segindx, vp_s_segstart;				\
    CM_field_id_t d_segindx, d_segstart, d_segempty;			\
    CM_field_id_t vp_d_segindx, vp_d_segempty;				\
    int s_elt_count = _CVL_elts_per_proc (s_nelt);			\
    int d_elt_count = _CVL_elts_per_proc (d_nelt);			\
    int seg_count   = _CVL_elts_per_proc (s_nseg);			\
									\
    /* Allocate aliasable fields */					\
    tmp_src    = _CVL_allocate_stack (s_elt_count * _bits);		\
    tmp_dst    = _CVL_allocate_stack (d_elt_count * _bits);		\
    tmp_indx   = _CVL_allocate_stack (s_elt_count * INT_BITS);		\
    s_segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    s_segempty = _CVL_allocate_stack (seg_count);			\
    s_segstart = _CVL_allocate_stack (s_elt_count);			\
    d_segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    d_segempty = _CVL_allocate_stack (seg_count);			\
    d_segstart = _CVL_allocate_stack (d_elt_count);			\
    addr       = _CVL_allocate_stack (s_elt_count * INT_BITS);		\
									\
    /* Copy fields and segment descriptors */				\
    _CVL_copy_s_field (tmp_indx, indx, s_elt_count);			\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, s_elt_count);	\
    _CVL_split_segd (s_segd, s_elt_count, seg_count, s_segindx,		\
		     s_segempty, s_segstart);				\
    _CVL_split_segd (d_segd, d_elt_count, seg_count, d_segindx,		\
		     d_segempty, d_segstart);				\
    /* Put the default values into the dst field */			\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_dst, def, d_elt_count);	\
									\
    /* Switch to one VP per src element, make aliases */		\
    _CVL_set_vp_set (s_elt_count);					\
    vp_src	  = CM_make_field_alias (tmp_src);			\
    vp_indx	  = CM_make_field_alias (tmp_indx);			\
    vp_addr	  = CM_make_field_alias (addr);				\
    vp_s_segstart = CM_make_field_alias (s_segstart);			\
    CM_s_move_zero_always_1L (vp_addr, INT_BITS);			\
									\
    /* Switch to one VP per dst element, make aliases */		\
    _CVL_set_vp_set (d_elt_count);					\
    vp_dst = CM_make_field_alias (tmp_dst);				\
									\
    /* Switch to one VP per segment, make aliases */			\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (seg_count, s_nseg);			\
    vp_s_segindx  = CM_make_field_alias (s_segindx);			\
    vp_d_segindx  = CM_make_field_alias (d_segindx);			\
    vp_d_segempty = CM_make_field_alias (d_segempty);			\
									\
    /* Ignore empty segments */						\
    CM_lognot_1_1L (vp_d_segempty, 1);					\
    CM_logand_context (vp_d_segempty);					\
									\
    /* Ok, here's where we get tricky. We send the start-of-segment	\
     * positions of the dst/default vectors to the start-of-segment	\
     * positions of the addr vector, which is the same length as the	\
     * src vector.  Use send_with_max trick (see back permutes).	\
     */									\
    CM_send_with_s_max_1L (vp_addr, vp_s_segindx, vp_d_segindx,		\
			   INT_BITS, CM_no_field);			\
									\
    /* Switch to one VP per src elt, segmented copy-scan on addr */	\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (s_elt_count, s_nelt);			\
    CM_scan_with_copy_1L (vp_addr, vp_addr, 0, INT_BITS, CM_upward,	\
			  CM_inclusive, CM_segment_bit, vp_s_segstart);	\
									\
    /* Now add indx to addr, to give the absolute destination of	\
     * each element.  indx contains the destination relative to the	\
     * start of each (src) segment, and we've made all the elements	\
     * in each segment of addr contain the absolute start address of	\
     * of that segment in the destination vector.  Phew.		\
     */									\
    CM_s_add_2_1L (vp_addr, vp_indx, INT_BITS);				\
									\
    /* Send src to dst, with addr as the address */			\
    CM_send_1L (vp_dst, vp_addr, vp_src, _bits, CM_no_field);		\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, d_elt_count);	\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_indx);					\
    CM_remove_field_alias (vp_addr);					\
    CM_remove_field_alias (vp_s_segstart);				\
    CM_remove_field_alias (vp_s_segindx);				\
    CM_remove_field_alias (vp_dst);					\
    CM_remove_field_alias (vp_d_segindx);				\
    CM_remove_field_alias (vp_d_segempty);				\
    CM_deallocate_stack_through (tmp_src);				\
  }									\
}									\
make_no_seg2_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2 | INPLACE_3)	/* ditto */

make_segdef (dpe_pez,dpe_puz,_s_,INT_BITS)
make_segdef (dpe_peb,dpe_pub,_u_,BOO_BITS)
make_segdef (dpe_ped,dpe_pud,_f_,DBL_BITS)


/* ----------------- Segmented Default Flag Permute ------------------ */

#define make_segdefflg(_name, _unseg, _abbrev, _bits)			\
void _name (dst, src, indx, flg, def, s_segd, s_nelt, s_nseg, d_segd,	\
	    d_nelt, d_nseg, scratch)					\
vec_p dst, src, indx, flg, def, s_segd, d_segd, scratch;		\
int s_nelt, s_nseg, d_nelt, d_nseg;					\
{									\
  /* If we only have one segment, use unsegmented routine instead */	\
  if (s_nseg == 1) {							\
    _unseg (dst, src, indx, flg, def, s_nelt, d_nelt, scratch);		\
  }									\
  else {								\
    /* This is ridiculous */						\
    CM_field_id_t tmp_src, tmp_dst, tmp_indx, addr, tmp_flg;		\
    CM_field_id_t vp_src, vp_dst, vp_indx, vp_addr, vp_flg;		\
    CM_field_id_t s_segindx, s_segstart, s_segempty;			\
    CM_field_id_t vp_s_segindx, vp_s_segstart;				\
    CM_field_id_t d_segindx, d_segstart, d_segempty;			\
    CM_field_id_t vp_d_segindx, vp_d_segempty;				\
    int s_elt_count = _CVL_elts_per_proc (s_nelt);			\
    int d_elt_count = _CVL_elts_per_proc (d_nelt);			\
    int seg_count   = _CVL_elts_per_proc (s_nseg);			\
									\
    /* Allocate aliasable fields */					\
    tmp_src    = _CVL_allocate_stack (s_elt_count * _bits);		\
    tmp_dst    = _CVL_allocate_stack (d_elt_count * _bits);		\
    tmp_indx   = _CVL_allocate_stack (s_elt_count * INT_BITS);		\
    s_segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    s_segempty = _CVL_allocate_stack (seg_count);			\
    s_segstart = _CVL_allocate_stack (s_elt_count);			\
    d_segindx  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    d_segempty = _CVL_allocate_stack (seg_count);			\
    d_segstart = _CVL_allocate_stack (d_elt_count);			\
    addr       = _CVL_allocate_stack (s_elt_count * INT_BITS);		\
    tmp_flg    = _CVL_allocate_stack (s_elt_count);			\
									\
    /* Copy fields and segment descriptors */				\
    _CVL_copy_u_field (tmp_flg, flg, s_elt_count);			\
    _CVL_copy_s_field (tmp_indx, indx, s_elt_count);			\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, s_elt_count);	\
    _CVL_split_segd (s_segd, s_elt_count, seg_count, s_segindx,		\
		     s_segempty, s_segstart);				\
    _CVL_split_segd (d_segd, d_elt_count, seg_count, d_segindx,		\
		     d_segempty, d_segstart);				\
    /* Put the default values into the dst field */			\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_dst, def, d_elt_count);	\
									\
    /* Switch to one VP per src element, make aliases */		\
    _CVL_set_vp_set (s_elt_count);					\
    vp_src	  = CM_make_field_alias (tmp_src);			\
    vp_indx	  = CM_make_field_alias (tmp_indx);			\
    vp_addr	  = CM_make_field_alias (addr);				\
    vp_flg	  = CM_make_field_alias (tmp_flg);			\
    vp_s_segstart = CM_make_field_alias (s_segstart);			\
    CM_s_move_zero_always_1L (vp_addr, INT_BITS);			\
									\
    /* Switch to one VP per dst element, make aliases */		\
    _CVL_set_vp_set (d_elt_count);					\
    vp_dst = CM_make_field_alias (tmp_dst);				\
									\
    /* Switch to one VP per segment, make aliases */			\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (seg_count, s_nseg);			\
    vp_s_segindx  = CM_make_field_alias (s_segindx);			\
    vp_d_segindx  = CM_make_field_alias (d_segindx);			\
    vp_d_segempty = CM_make_field_alias (d_segempty);			\
									\
    /* Ignore empty segments */						\
    CM_lognot_1_1L (vp_d_segempty, 1);					\
    CM_logand_context (vp_d_segempty);					\
									\
    /* Ok, here's where we get tricky. We send the start-of-segment	\
     * positions of the dst/default vectors to the start-of-segment	\
     * positions of the addr vector, which is the same length as the	\
     * src vector.  Use send_with_max trick (see back permutes).	\
     */									\
    CM_send_with_s_max_1L (vp_addr, vp_s_segindx, vp_d_segindx,		\
			   INT_BITS, CM_no_field);			\
									\
    /* Switch to one VP per src elt, segmented copy-scan on addr */	\
    CM_set_vp_set (_CVL_default_vp_set);				\
    _CVL_set_vp_set_and_context (s_elt_count, s_nelt);			\
    CM_scan_with_copy_1L (vp_addr, vp_addr, 0, INT_BITS, CM_upward,	\
			  CM_inclusive, CM_segment_bit, vp_s_segstart);	\
									\
    /* Now add indx to addr, to give the absolute destination of	\
     * each element.  indx contains the destination relative to the	\
     * start of each (src) segment, and we've made all the elements	\
     * in each segment of addr contain the absolute start address of	\
     * of that segment in the destination vector.  Phew.		\
     */									\
    CM_s_add_2_1L (vp_addr, vp_indx, INT_BITS);				\
									\
    /* Send src to dst, with addr as the address */			\
    CM_logand_context (vp_flg);						\
    CM_send_1L (vp_dst, vp_addr, vp_src, _bits, CM_no_field);		\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    CM_set_context ();							\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, d_elt_count);	\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_indx);					\
    CM_remove_field_alias (vp_addr);					\
    CM_remove_field_alias (vp_flg);					\
    CM_remove_field_alias (vp_s_segstart);				\
    CM_remove_field_alias (vp_s_segindx);				\
    CM_remove_field_alias (vp_dst);					\
    CM_remove_field_alias (vp_d_segindx);				\
    CM_remove_field_alias (vp_d_segempty);				\
    CM_deallocate_stack_through (tmp_src);				\
  }									\
}									\
make_no_seg2_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2 | INPLACE_3 | INPLACE_4)

make_segdefflg (dfp_pez,dfp_puz,_s_,INT_BITS)
make_segdefflg (dfp_peb,dfp_pub,_u_,BOO_BITS)
make_segdefflg (dfp_ped,dfp_pud,_f_,DBL_BITS)
