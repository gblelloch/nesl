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


/* ------------------------- Pack, Part 1 --------------------------- */

/* See reduce.c for why we use scan-and-extract rather than reduce
 */
int pk1_luv (flags, len, scratch)
vec_p flags, scratch;
int len;
{
  CM_field_id_t tmp_flg, vp_flg;
  int result, count = _CVL_elts_per_proc (len);

  tmp_flg = _CVL_allocate_stack (count * INT_BITS);
  _CVL_copy_flag_field (tmp_flg, flags, count);
  _CVL_set_vp_set_and_context (count, len);
  vp_flg = CM_make_field_alias (tmp_flg);
  CM_scan_with_u_add_1L (vp_flg, vp_flg, 0, INT_BITS, CM_downward,
			 CM_inclusive, CM_none, CM_no_field);
  CM_set_vp_set (_CVL_default_vp_set);
  CM_set_context ();
  result = CM_s_read_from_processor_1L (0, tmp_flg, INT_BITS);
  CM_remove_field_alias (vp_flg);
  CM_deallocate_stack_through (tmp_flg);
  return result;
}
make_no_scratch (pk1_luv)
make_inplace (pk1_luv, INPLACE_NONE)


void pk1_lev (dstsd, flags, srcsd, nelt, nseg, scratch)
vec_p dstsd, flags, srcsd, scratch;
int nelt, nseg;
{
  CM_field_id_t tmp_dst, tmp_flg, vp_dst, vp_flg, segindx, segempty;
  CM_field_id_t segstart, vp_segindx, vp_segempty, vp_segstart;
  int elt_count = _CVL_elts_per_proc (nelt);
  int seg_count = _CVL_elts_per_proc (nseg);

  /* Use faster unsegmented code if we only have one segment */
  if (nseg == 1) {
    tmp_flg = _CVL_allocate_stack (elt_count * INT_BITS);
    _CVL_copy_flag_field (tmp_flg, flags, elt_count);
    _CVL_set_vp_set_and_context (elt_count, nelt);
    vp_flg = CM_make_field_alias (tmp_flg);
    CM_scan_with_u_add_1L (vp_flg, vp_flg, 0, INT_BITS, CM_downward,
			   CM_inclusive, CM_none, CM_no_field);
    CM_set_vp_set (_CVL_default_vp_set);
    CM_set_context ();
    CM_s_move_always_1L (dstsd, tmp_flg, INT_BITS);
    CM_remove_field_alias (vp_flg);
    CM_deallocate_stack_through (tmp_flg);
  }
  else {
    tmp_dst  = _CVL_allocate_stack (seg_count * INT_BITS);
    tmp_flg  = _CVL_allocate_stack (elt_count * INT_BITS);
    segindx  = _CVL_allocate_stack (seg_count * INT_BITS);
    segempty = _CVL_allocate_stack (seg_count);
    segstart = _CVL_allocate_stack (elt_count);

    /* Copy stuff over */
    _CVL_split_segd (srcsd, elt_count, seg_count, segindx,
		     segempty, segstart);
    _CVL_copy_flag_field (tmp_flg, flags, elt_count);

    /* Switch to VP set with one VP per element */
    _CVL_set_vp_set_and_context (elt_count, nelt);
    vp_flg	= CM_make_field_alias (tmp_flg);
    vp_segstart = CM_make_field_alias (segstart);

    /* Use a backward +-scan to put the number of set flags in each
     * segment into the first element of the segment
     */
    CM_scan_with_u_add_1L
      (vp_flg, vp_flg, 0, INT_BITS, CM_downward, CM_inclusive,
       CM_segment_bit, vp_segstart);

    /* Switch to VP set with one VP per segment, set up aliases */
    CM_set_vp_set (_CVL_default_vp_set);
    _CVL_set_vp_set_and_context (seg_count, nseg);
    vp_segindx	= CM_make_field_alias (segindx);
    vp_segempty = CM_make_field_alias (segempty);
    vp_dst	= CM_make_field_alias (tmp_dst);

    /* Zero dst, so we get correct behaviour for empty segments */
    CM_s_move_zero_always_1L (vp_dst, INT_BITS);

    /* Switch off empty segments */
    CM_lognot_1_1L (vp_segempty, 1);
    CM_logand_context (vp_segempty);

    /* Get into the dst field of each segment */
    CM_get_1L (vp_dst, vp_segindx, vp_flg, INT_BITS);

    /* Restore context and VP set, copy dst back */
    CM_set_vp_set (_CVL_default_vp_set);
    CM_set_context();
    _CVL_copy_s_field (dstsd, tmp_dst, seg_count);
    CM_remove_field_alias (vp_flg);
    CM_remove_field_alias (vp_segstart);
    CM_remove_field_alias (vp_segindx);
    CM_remove_field_alias (vp_segempty);
    CM_remove_field_alias (vp_dst);
    CM_deallocate_stack_through (tmp_dst);
  }
}
make_no_seg_scratch (pk1_lev)
make_inplace (pk1_lev, INPLACE_NONE) /* we might call pk1_luv :-( */


/* ------------------------- Pack, Part 2 --------------------------- */

#define make_pack(_name, _abbrev, _bits)				\
void _name (dst, src, flags, src_len, dst_len, scratch)			\
vec_p dst, src, flags, scratch;						\
int src_len, dst_len;							\
{									\
  CM_field_id_t tmp_src, tmp_flg, tmp_dst;				\
  CM_field_id_t vp_src, vp_flg, vp_dst, vp_ind;				\
  int src_count = _CVL_elts_per_proc (src_len);				\
  int dst_count = _CVL_elts_per_proc (dst_len);				\
  tmp_dst	= _CVL_allocate_stack (dst_count * _bits);		\
  tmp_src	= _CVL_allocate_stack (src_count * _bits);		\
  tmp_flg	= _CVL_allocate_stack (src_count);			\
									\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, src_count);		\
  _CVL_copy_u_field (tmp_flg, flags, src_count);			\
									\
  _CVL_set_vp_set (dst_count);						\
  vp_dst = CM_make_field_alias (tmp_dst);				\
  CM_set_vp_set (_CVL_default_vp_set);					\
									\
  _CVL_set_vp_set_and_context (src_count, src_len);			\
  vp_src = CM_make_field_alias (tmp_src);				\
  vp_flg = CM_make_field_alias (tmp_flg);				\
  vp_ind = _CVL_allocate_stack (INT_BITS);				\
									\
  CM_u_move_2L (vp_ind, vp_flg, INT_BITS, BOO_BITS);			\
  CM_scan_with_u_add_1L (vp_ind, vp_ind, 0, INT_BITS, CM_upward,	\
			 CM_exclusive, CM_none, CM_no_field);		\
  CM_logand_context (vp_flg);						\
  CM_send_1L (vp_dst, vp_ind, vp_src, _bits, CM_no_field);		\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, dst_count);		\
  CM_remove_field_alias (vp_src);					\
  CM_remove_field_alias (vp_flg);					\
  CM_remove_field_alias (vp_dst);					\
  CM_deallocate_stack_through (tmp_dst);				\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_NONE)

make_pack (pk2_luz,_s_,INT_BITS)
make_pack (pk2_lud,_f_,DBL_BITS)
make_pack (pk2_lub,_u_,BOO_BITS)


/* Part 2 : pk2_le{z,b,d} takes the src, the flags, the segment
 * descriptor of the source, and the segment descriptor of the
 * dest, and packs the src into the dest according to the flags
 */

#define make_seg_pack(_name, _abbrev, _bits)				\
void _name (dst, src, flags, srcsd, srcelt, srcseg, dstsd, dstelt,	\
	    dstseg, scratch)						\
vec_p dst, src, flags, srcsd, dstsd, scratch;				\
int srcelt, srcseg, dstelt, dstseg;					\
{									\
  CM_field_id_t tmp_src, tmp_dst, tmp_flg, indx;			\
  CM_field_id_t vp_src, vp_dst, vp_flg, vp_indx;			\
  int srcelt_count = _CVL_elts_per_proc (srcelt);			\
  int dstelt_count = _CVL_elts_per_proc (dstelt);			\
									\
  /* Allocate aliasable fields */					\
  tmp_src = _CVL_allocate_stack (srcelt_count * _bits);			\
  tmp_dst = _CVL_allocate_stack (dstelt_count * _bits);			\
  tmp_flg = _CVL_allocate_stack (srcelt_count);				\
  indx	  = _CVL_allocate_stack (srcelt_count * INT_BITS);		\
									\
  /* Copy fields */							\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, srcelt_count);		\
  _CVL_copy_u_field (tmp_flg, flags, srcelt_count);			\
									\
  /* Switch to VP set with one VP per dst element, set up aliases */	\
  _CVL_set_vp_set (dstelt_count);					\
  vp_dst = CM_make_field_alias (tmp_dst);				\
									\
  /* Switch to VP set with one VP per src element */			\
  CM_set_vp_set (_CVL_default_vp_set);					\
  _CVL_set_vp_set_and_context (srcelt_count, srcelt);			\
  vp_src  = CM_make_field_alias (tmp_src);				\
  vp_flg  = CM_make_field_alias (tmp_flg);				\
  vp_indx = CM_make_field_alias (indx);					\
									\
  /* Do an exclusive unsegmented +-scan on the flags to work out	\
   * where to send stuff to.						\
   */									\
  CM_u_move_2L (vp_indx, vp_flg, INT_BITS, BOO_BITS);			\
  CM_scan_with_u_add_1L (vp_indx, vp_indx, 0, INT_BITS, CM_upward,	\
			 CM_exclusive, CM_none, CM_no_field);		\
									\
  /* Switch off all non-flagged elements */				\
  CM_logand_context (vp_flg);						\
									\
  /* Send the src field from the flagged elements to the dst field,	\
   * using the indx field as the address.				\
   */									\
  CM_send_1L (vp_dst, vp_indx, vp_src, _bits, CM_no_field);		\
									\
  /* Reset VP and context, copy tmp_dst into dst */			\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, dstelt_count);		\
  CM_remove_field_alias (vp_dst);					\
  CM_remove_field_alias (vp_src);					\
  CM_remove_field_alias (vp_flg);					\
  CM_remove_field_alias (vp_indx);					\
  CM_deallocate_stack_through (tmp_src);				\
}									\
make_no_seg2_scratch (_name)						\
make_inplace (_name, INPLACE_1 | INPLACE_2 | INPLACE_3)

make_seg_pack (pk2_lez,_s_,INT_BITS)
make_seg_pack (pk2_led,_f_,DBL_BITS)
make_seg_pack (pk2_leb,_u_,BOO_BITS)


/* ----------------------- Unsegmented Index ------------------------- */

void ind_luz (dst, init, stride, len, scratch)
vec_p dst, scratch;
int init, stride, len;
{
  CM_field_id_t tmp_dst, vp_dst;
  int count = _CVL_elts_per_proc(len);

  tmp_dst = _CVL_allocate_stack (count * INT_BITS);
  _CVL_set_vp_set (count);
  vp_dst = CM_make_field_alias (tmp_dst);
  CM_u_move_zero_always_1L (vp_dst, INT_BITS);
  CM_my_send_address (vp_dst);
  if (stride != 1) {
    CM_s_multiply_constant_2_1L (vp_dst, stride, INT_BITS);
  }
  if (init != 0) {
    CM_s_add_constant_2_1L (vp_dst, init, INT_BITS);
  }
  CM_set_vp_set (_CVL_default_vp_set);
  _CVL_copy_s_field (dst, tmp_dst, count);
  CM_remove_field_alias (vp_dst);
  CM_deallocate_stack_through (tmp_dst);
}
make_no_scratch (ind_luz)
make_inplace (ind_luz, INPLACE_NONE)


/* ------------------------ Segmented Index -------------------------- */

/* Should detect special case of when all init == 0 and all stride == 1,
 * and revert to using the old code.  Use an or-scan on the init vector.
 * Subtract 1 and use an an-scan on the stride vector? XXX
 */

void ind_lez (dst, init, stride, dst_sd, dst_nelt, dst_nseg, scratch)
vec_p dst, init, stride, dst_sd, scratch;
int dst_nelt, dst_nseg;
{
  /* XXX doesn't work.
  if (dst_nseg == 1) {
    int one_init   = CM_s_read_from_processor_1L (0, init, INT_BITS);
    int one_stride = CM_s_read_from_processor_1L (0, stride, INT_BITS);
    int one_length = CM_s_read_from_processor_1L (0, lengths, INT_BITS);
    ind_luz (dst, one_init, one_stride, one_length, scratch);
  }
  else
  */
  {
    CM_field_id_t tmp_dst, tmp_stride, tmp_init, scratch;
    CM_field_id_t vp_dst, vp_stride, vp_init, vp_scratch;
    CM_field_id_t segstart, segempty, segindx;
    CM_field_id_t vp_segstart, vp_segempty, vp_segindx;
    int elt_count = _CVL_elts_per_proc (dst_nelt);
    int seg_count = _CVL_elts_per_proc (dst_nseg);

    tmp_dst    = _CVL_allocate_stack (elt_count * INT_BITS);
    tmp_stride = _CVL_allocate_stack (seg_count * INT_BITS);
    tmp_init   = _CVL_allocate_stack (seg_count * INT_BITS);
    scratch    = _CVL_allocate_stack (elt_count * INT_BITS);
    segindx    = _CVL_allocate_stack (seg_count * INT_BITS);
    segempty   = _CVL_allocate_stack (seg_count);
    segstart   = _CVL_allocate_stack (elt_count);
    _CVL_copy_s_field (tmp_stride, stride, seg_count);
    _CVL_copy_s_field (tmp_init, init, seg_count);
    _CVL_split_segd (dst_sd, elt_count, seg_count, segindx,
		     segempty, segstart);

    /* Switch to VP set with one VP per elt, make aliases */
    _CVL_set_vp_set (elt_count);
    vp_dst	= CM_make_field_alias (tmp_dst);
    vp_scratch	= CM_make_field_alias (scratch);
    vp_segstart = CM_make_field_alias (segstart);

    /* Switch to VP set with one VP per segment */
    CM_set_vp_set (_CVL_default_vp_set);
    _CVL_set_vp_set_and_context (seg_count, dst_nseg);
    vp_segindx	= CM_make_field_alias (segindx);
    vp_segempty = CM_make_field_alias (segempty);
    vp_stride	= CM_make_field_alias (tmp_stride);
    vp_init	= CM_make_field_alias (tmp_init);

    /* Ignore empty segments */
    CM_lognot_1_1L (vp_segempty, 1);
    CM_logand_context (vp_segempty);

    /* Send stride values into start-of-seg positions of dst */
    CM_send_1L (vp_dst, vp_segindx, vp_stride, INT_BITS, CM_no_field);

    /* Send init values into start-of-seg positions of scratch */
    CM_send_1L (vp_scratch, vp_segindx, vp_init, INT_BITS, CM_no_field);

    /* Switch to one VP per elt, do a copy-scan on dst and scratch */
    CM_set_vp_set (_CVL_default_vp_set);
    _CVL_set_vp_set_and_context (elt_count, dst_nelt);
    CM_scan_with_copy_1L (vp_dst, vp_dst, 0, INT_BITS, CM_upward,
			  CM_inclusive, CM_segment_bit, vp_segstart);
    CM_scan_with_copy_1L (vp_scratch, vp_scratch, 0, INT_BITS, CM_upward,
			  CM_inclusive, CM_segment_bit, vp_segstart);

    /* We have a vector of strides in dst, and a vector of initial
       values in scratch.  First, do a segmented +-scan on dst, then add
       scratch to it to get the final result.
     */
    CM_scan_with_s_add_1L (vp_dst, vp_dst, 0, INT_BITS, CM_upward,
			   CM_exclusive, CM_segment_bit, vp_segstart);
    CM_s_add_2_1L (vp_dst, vp_scratch, INT_BITS);

    CM_set_vp_set (_CVL_default_vp_set);
    CM_set_context ();
    _CVL_copy_s_field (dst, tmp_dst, elt_count);
    CM_remove_field_alias (vp_init);
    CM_remove_field_alias (vp_stride);
    CM_remove_field_alias (vp_segempty);
    CM_remove_field_alias (vp_segindx);
    CM_remove_field_alias (vp_segstart);
    CM_remove_field_alias (vp_scratch);
    CM_remove_field_alias (vp_dst);
    CM_deallocate_stack_through (tmp_dst);
  }
}
make_no_seg_scratch (ind_lez)
make_inplace (ind_lez, INPLACE_1 | INPLACE_2 | INPLACE_3)


/* ----------------------- Unsegmented Rank -------------------------- */

#define make_rank(_name, _direction, _abbrev, _bits, _argbits)		\
void _name (dst, src, len, scratch)					\
vec_p dst, src, scratch;						\
int len;								\
{									\
  CM_field_id_t tmp_src, tmp_dst, vp_src, vp_dst;			\
  int count = _CVL_elts_per_proc (len);					\
									\
  tmp_dst = _CVL_allocate_stack (count * INT_BITS);			\
  tmp_src = _CVL_allocate_stack (count * _bits);			\
									\
  GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, count);		\
  /* XXX context only affects writing to destination in Paris rank	\
   * function -- will this be enough?	If not, can flip context, move	\
   * positive infinity into the top-most elements, and flip context.	\
   * And how about zero-length segments?				\
   */									\
  _CVL_set_vp_set_and_context (count, len);				\
  vp_src = CM_make_field_alias (tmp_src);				\
  vp_dst = CM_make_field_alias (tmp_dst);				\
									\
  GLUE3 (CM,_abbrev,rank_2L) (vp_dst, vp_src, 0, INT_BITS, _argbits,	\
			      _direction, CM_none, CM_no_field);	\
									\
  CM_set_vp_set (_CVL_default_vp_set);					\
  CM_set_context ();							\
  GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, count);		\
  CM_remove_field_alias (vp_src);					\
  CM_remove_field_alias (vp_dst);					\
  CM_deallocate_stack_through (tmp_dst);				\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_1)

make_rank (rku_luz,CM_upward,_s_,INT_BITS,INT_BITS)
make_rank (rku_lud,CM_upward,_f_,DBL_BITS,SIG_EXP)
make_rank (rku_lub,CM_upward,_u_,BOO_BITS,BOO_BITS)
make_rank (rkd_luz,CM_downward,_s_,INT_BITS,INT_BITS)
make_rank (rkd_lud,CM_downward,_f_,DBL_BITS,SIG_EXP)
make_rank (rkd_lub,CM_downward,_u_,BOO_BITS,BOO_BITS)


/* ------------------------ Segmented Rank --------------------------- */

#define make_seg_rank(_name, _unseg, _dir, _abbrev, _bits, _argbits)	\
void _name (dst, src, segdes, nelt, nseg, scratch)			\
vec_p dst, src, segdes, scratch;					\
int nelt, nseg;								\
{									\
  /* If we only have one segment, use unsegmented rank instead */	\
  if (nseg == 1) {							\
    _unseg (dst, src, nelt, scratch);					\
  }									\
  else {								\
    CM_field_id_t segindx, segstart, segempty, tmp_src, tmp_dst;	\
    CM_field_id_t tmp_addr, vp_src, vp_dst, vp_segstart, vp_addr;	\
    int elt_count = _CVL_elts_per_proc (nelt);				\
    int seg_count = _CVL_elts_per_proc (nseg);				\
    tmp_dst	  = _CVL_allocate_stack (elt_count * INT_BITS);		\
    tmp_src	  = _CVL_allocate_stack (elt_count * _bits);		\
    tmp_addr	  = _CVL_allocate_stack (elt_count * INT_BITS);		\
    segindx	  = _CVL_allocate_stack (seg_count * INT_BITS);		\
    segstart	  = _CVL_allocate_stack (elt_count);			\
    segempty	  = _CVL_allocate_stack (seg_count);			\
									\
    _CVL_split_segd (segdes, elt_count, seg_count, segindx, segempty,	\
		     segstart);						\
    GLUE3 (_CVL_copy,_abbrev,field) (tmp_src, src, elt_count);		\
    /* XXX context only affects writing to destination in Paris rank	\
     * function -- will this be enough?	 See unsegmented code.		\
     */									\
    _CVL_set_vp_set_and_context (elt_count, nelt);			\
    vp_src = CM_make_field_alias (tmp_src);				\
    vp_dst = CM_make_field_alias (tmp_dst);				\
    vp_addr = CM_make_field_alias (tmp_addr);				\
    vp_segstart = CM_make_field_alias (segstart);			\
									\
    /* To quote the Paris Manual:					\
     * This operation was originally documented to result in a set of	\
     * indices that restart at 0 for each segment.  To obtain that	\
     * effect use the following strategy:				\
     * 1) Use the rank function.					\
     * 2) Set the context bit on for processor with segment bits and	\
     *	  then call CM_my_send_address.					\
     * 3) Use a segmented copy-scan operation to copy the NEWS address	\
     *	  within each segment.						\
     * 4) Subtract the results of the segmented copy scan from the	\
     *	  results of the rank ordering.					\
     */									\
    GLUE3(CM,_abbrev,rank_2L) (vp_dst, vp_src, 0, INT_BITS, _argbits,	\
			       _dir, CM_segment_bit, vp_segstart);	\
    CM_logand_context (vp_segstart);					\
    CM_u_move_zero_always_1L (vp_addr, INT_BITS);			\
    CM_my_send_address (vp_addr);					\
    CM_scan_with_copy_1L (vp_addr, vp_addr, 0, INT_BITS, CM_upward,	\
			  CM_inclusive, CM_segment_bit, vp_segstart);	\
    CM_set_context ();							\
    CM_s_subtract_2_1L (vp_dst, vp_addr, INT_BITS);			\
									\
    CM_set_vp_set (_CVL_default_vp_set);				\
    GLUE3 (_CVL_copy,_abbrev,field) (dst, tmp_dst, elt_count);		\
    CM_remove_field_alias (vp_src);					\
    CM_remove_field_alias (vp_dst);					\
    CM_deallocate_stack_through (tmp_dst);				\
  }									\
}									\
make_no_seg_scratch (_name)						\
make_inplace (_name, INPLACE_1)	/* might call unsegmented version */

make_seg_rank (rku_lez,rku_luz,CM_upward,_s_,INT_BITS,INT_BITS)
make_seg_rank (rku_led,rku_lud,CM_upward,_f_,DBL_BITS,SIG_EXP)
make_seg_rank (rku_leb,rku_lub,CM_upward,_u_,BOO_BITS,BOO_BITS)
make_seg_rank (rkd_lez,rkd_luz,CM_downward,_s_,INT_BITS,INT_BITS)
make_seg_rank (rkd_led,rkd_lud,CM_downward,_f_,DBL_BITS,SIG_EXP)
make_seg_rank (rkd_leb,rkd_lub,CM_downward,_u_,BOO_BITS,BOO_BITS)
