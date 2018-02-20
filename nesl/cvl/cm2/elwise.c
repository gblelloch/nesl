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

#include <math.h>
#include <cvl.h>
#include "cm2cvl.h"


/* ----------- Macro skeleton for one-argument functions  ------------ */

#define make_one(_name, _funct, _bits, _argbits)			\
void _name (dst, src, len, scratch)					\
vec_p dst, src, scratch;						\
int len;								\
{									\
  int i, count = _CVL_elts_per_proc (len);				\
  for (i = 0; i < count; i++) {						\
    GLUE3 (CM,_funct,1L) (dst, src, _argbits);				\
    BUMP_PTR (dst, _bits);						\
    BUMP_PTR (src, _bits);						\
  }									\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_1)


/* ----------- Macro skeleton for two-argument functions ------------- */

#define make_two(_name, _funct, _bits, _argbits)			\
void _name (dst, src1, src2, len, scratch)				\
vec_p dst, src1, src2, scratch;						\
int len;								\
{									\
  int i, count = _CVL_elts_per_proc (len);				\
  for (i = 0; i < count; i++) {						\
    GLUE3 (CM,_funct,3_1L) (dst, src1, src2, _argbits);			\
    BUMP_PTR (dst, _bits);						\
    BUMP_PTR (src1, _bits);						\
    BUMP_PTR (src2, _bits);						\
  }									\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_1 | INPLACE_2)


/* -------- Macro skeleton for test (one-argument) functions --------- */

#define make_test(_name, _funct, _bits, _argbits)			\
void _name (dst, src1, src2, len, scratch)				\
vec_p dst, src1, src2, scratch;						\
int len;								\
{									\
  int i, count = _CVL_elts_per_proc (len);				\
  for (i = 0; i < count; i++) {						\
    GLUE3 (CM,_funct,1L) (src1, src2, _argbits);			\
    CM_store_test (dst);						\
    BUMP_PTR (dst, BOO_BITS);						\
    BUMP_PTR (src1, _bits);						\
    BUMP_PTR (src2, _bits);						\
  }									\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_1 | INPLACE_2)


/* ------ Macro skeleton for selection (one-argument) functions ------ */

#define make_select(_name, _abbrev, _bits, _argbits)			\
void _name (dst, flags, src1, src2, len, scratch)			\
vec_p dst, flags, src1, src2, scratch;					\
int len;								\
{									\
  int i, count = _CVL_elts_per_proc(len);				\
  for (i = 0; i < count; i++) {						\
    CM_load_context (flags);						\
    GLUE3 (CM,_abbrev,move_1L) (dst, src1, _argbits);			\
    CM_invert_context ();						\
    GLUE3 (CM,_abbrev,move_1L) (dst, src2, _argbits);			\
    BUMP_PTR (dst, _bits);						\
    BUMP_PTR (src1, _bits);						\
    BUMP_PTR (src2, _bits);						\
    BUMP_PTR (flags, BOO_BITS);						\
  }									\
  CM_set_context ();							\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_1 | INPLACE_2 | INPLACE_3)


/* ------- Macro skeleton for arbitary one-argument functions -------- */

#define make_any(_name, _functline, _dstbits, _srcbits, _inplace)	\
void _name (dst, src, len, scratch)					\
vec_p dst, src, scratch;						\
int len;								\
{									\
  int i, count = _CVL_elts_per_proc (len);				\
  for (i = 0; i < count; i++) {						\
    _functline;								\
    BUMP_PTR (dst, _dstbits);						\
    BUMP_PTR (src, _srcbits);						\
  }									\
}									\
make_no_scratch (_name)							\
make_inplace (_name, _inplace)


/* ------------- Macro skeleton for integer shifting ----------------- */

#define make_shift(_name, _negate)					\
void _name (dst, src1, src2, len, scratch)				\
vec_p dst, src1, src2, scratch;						\
int len;								\
{									\
  int i, count = _CVL_elts_per_proc (len);				\
  for (i = 0; i < count; i++) {						\
    CM_s_move_1L (dst, src1, INT_BITS);					\
    _negate;								\
    CM_s_s_shift_2_2L (dst, src2, INT_BITS, INT_BITS);			\
    BUMP_PTR (dst, INT_BITS);						\
    BUMP_PTR (src1, INT_BITS);						\
    BUMP_PTR (src2, INT_BITS);						\
  }									\
}									\
make_no_scratch (_name)							\
make_inplace (_name, INPLACE_1 | INPLACE_2)

/* Arithmetic, min and max: integers and doubles */
make_two (add_wuz,_s_add_,INT_BITS,INT_BITS)
make_two (sub_wuz,_s_subtract_,INT_BITS,INT_BITS)
make_two (mul_wuz,_s_multiply_,INT_BITS,INT_BITS)
make_two (div_wuz,_s_truncate_,INT_BITS,INT_BITS)
make_two (mod_wuz,_s_mod_,INT_BITS,INT_BITS)
make_two (max_wuz,_s_max_,INT_BITS,INT_BITS)
make_two (min_wuz,_s_min_,INT_BITS,INT_BITS)
make_two (add_wud,_f_add_,DBL_BITS,SIG_EXP)
make_two (sub_wud,_f_subtract_,DBL_BITS,SIG_EXP)
make_two (mul_wud,_f_multiply_,DBL_BITS,SIG_EXP)
make_two (div_wud,_f_divide_,DBL_BITS,SIG_EXP)
make_two (max_wud,_f_max_,DBL_BITS,SIG_EXP)
make_two (min_wud,_f_min_,DBL_BITS,SIG_EXP)

/* Comparisons: integers and doubles */
make_test (grt_wuz,_s_gt_,INT_BITS,INT_BITS)
make_test (les_wuz,_s_lt_,INT_BITS,INT_BITS)
make_test (leq_wuz,_s_le_,INT_BITS,INT_BITS)
make_test (geq_wuz,_s_ge_,INT_BITS,INT_BITS)
make_test (grt_wud,_f_gt_,DBL_BITS,SIG_EXP)
make_test (les_wud,_f_lt_,DBL_BITS,SIG_EXP)
make_test (leq_wud,_f_le_,DBL_BITS,SIG_EXP)
make_test (geq_wud,_f_ge_,DBL_BITS,SIG_EXP)

/* Equality: integers, doubles, and booleans */
make_test (eql_wuz,_s_eq_,INT_BITS,INT_BITS)
make_test (neq_wuz,_s_ne_,INT_BITS,INT_BITS)
make_test (eql_wud,_f_eq_,DBL_BITS,SIG_EXP)
make_test (neq_wud,_f_ne_,DBL_BITS,SIG_EXP)
make_test (eql_wub,_u_eq_,BOO_BITS,BOO_BITS)
make_test (neq_wub,_u_ne_,BOO_BITS,BOO_BITS)

/* Bitwise operations: integers and booleans */
make_one (not_wuz,_lognot_2_,INT_BITS,INT_BITS)
make_two (ior_wuz,_logior_always_,INT_BITS,INT_BITS)
make_two (and_wuz,_logand_always_,INT_BITS,INT_BITS)
make_two (xor_wuz,_logxor_always_,INT_BITS,INT_BITS)
make_one (not_wub,_lognot_2_,BOO_BITS,BOO_BITS)
make_two (ior_wub,_logior_always_,BOO_BITS,BOO_BITS)
make_two (and_wub,_logand_always_,BOO_BITS,BOO_BITS)
make_two (xor_wub,_logxor_always_,BOO_BITS,BOO_BITS)

/* Copying: integers, doubles, and booleans */
make_one (cpy_wuz,_s_move_,INT_BITS,INT_BITS)
make_one (cpy_wud,_f_move_,DBL_BITS,SIG_EXP)
make_one (cpy_wub,_s_move_,INT_BITS,BOO_BITS)

/* Arithmetic and trigonometric functions: doubles */
make_one (exp_wud,_f_exp_2_,DBL_BITS,SIG_EXP)
make_one (log_wud,_f_ln_2_,DBL_BITS,SIG_EXP)
make_one (sqt_wud,_f_sqrt_2_,DBL_BITS,SIG_EXP)
make_one (sin_wud,_f_sin_2_,DBL_BITS,SIG_EXP)
make_one (cos_wud,_f_cos_2_,DBL_BITS,SIG_EXP)
make_one (tan_wud,_f_tan_2_,DBL_BITS,SIG_EXP)
make_one (asn_wud,_f_asin_2_,DBL_BITS,SIG_EXP)
make_one (acs_wud,_f_acos_2_,DBL_BITS,SIG_EXP)
make_one (atn_wud,_f_atan_2_,DBL_BITS,SIG_EXP)
make_one (snh_wud,_f_sinh_2_,DBL_BITS,SIG_EXP)
make_one (csh_wud,_f_cosh_2_,DBL_BITS,SIG_EXP)
make_one (tnh_wud,_f_tanh_2_,DBL_BITS,SIG_EXP)

/* Selection operations */
make_select (sel_wuz,_s_,INT_BITS,INT_BITS)
make_select (sel_wub,_u_,BOO_BITS,BOO_BITS)
make_select (sel_wud,_f_,DBL_BITS,SIG_EXP)

/* Rounding operations */
make_any (flr_wud, CM_s_f_floor_2_2L (dst, src, INT_BITS, SIG_EXP),
	  INT_BITS, DBL_BITS, INPLACE_NONE)
make_any (cei_wud, CM_s_f_ceiling_2_2L (dst, src, INT_BITS, SIG_EXP),
	  INT_BITS, DBL_BITS, INPLACE_NONE)
make_any (trn_wud, CM_s_f_truncate_2_2L (dst, src, INT_BITS, SIG_EXP),
	  INT_BITS, DBL_BITS, INPLACE_NONE)
make_any (rou_wud, CM_s_f_round_2_2L (dst, src, INT_BITS, SIG_EXP),
	  INT_BITS, DBL_BITS, INPLACE_NONE)

/* Type conversion operations */
make_any (int_wub, CM_u_move_2L (dst, src, INT_BITS, BOO_BITS),
	  INT_BITS, BOO_BITS, INPLACE_1)
make_any (int_wud, CM_s_f_floor_2_2L (dst, src, INT_BITS, SIG_EXP),
	  INT_BITS, DBL_BITS, INPLACE_NONE)
make_any (dbl_wuz, CM_f_s_float_2_2L (dst, src, INT_BITS, SIG_EXP),
	  DBL_BITS, INT_BITS, INPLACE_NONE)
make_any (boo_wuz, CM_u_move_2L (dst, src, BOO_BITS, INT_BITS),
	  BOO_BITS, INT_BITS, INPLACE_1)

/* Random numbers: integers only */
make_any (rnd_wuz,
	  CM_u_random_1L (dst, INT_BITS, MAX_INT>>1);
	  CM_s_mod_2_1L (dst, src, INT_BITS),
	  INT_BITS, INT_BITS, INPLACE_NONE)

/* Shifting: integers only */
make_shift (lsh_wuz, )
make_shift (rsh_wuz, CM_s_negate_1_1L (src2, INT_BITS))


/* Copying a segment descriptor.  See also functions in facilt.c
 */
void cpy_wus (dst, src, nelt, nseg, scratch)
vec_p dst, src, scratch;
int nelt, nseg;
{
  int elt_count = _CVL_elts_per_proc (nelt);
  int seg_count = _CVL_elts_per_proc (nseg);
  _CVL_copy_s_field (dst, src, seg_count);
  BUMP_PTR (dst, seg_count * INT_BITS);
  BUMP_PTR (src, seg_count * INT_BITS);
  _CVL_copy_u_field (dst, src, seg_count);
  BUMP_PTR (dst, seg_count);
  BUMP_PTR (src, seg_count);
  _CVL_copy_u_field (dst, src, elt_count);
}
make_no_seg_scratch (cpy_wus)
make_inplace (cpy_wus, INPLACE_NONE)
