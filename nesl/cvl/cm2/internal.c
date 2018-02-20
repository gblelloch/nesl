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

#include <stdio.h>
#include <cm/paris.h>
#include <cvl.h>
#include "cm2cvl.h"


/* ---------------------- Stuff we need to share --------------------- */

CM_vp_set_id_t _CVL_default_vp_set;
CM_geometry_id_t _CVL_geometry;


/* ------------------- Stuff we don't need to share ------------------ */

static int _CVL_started_detached;
static int _CVL_log2_procs;
static CM_vp_set_id_t _CVL_vp_set [LOG_MAX_VP_RATIO];
static CM_field_id_t _CVL_proc_num;


/* --------------------- Sippy's Cunning Utilites -------------------- */

#define exp2(n) (1 << (n))

/* Assumes IEEE-754; exponent stored XS127.   Casts number to float,
 * this normalizes number; grab exponent.  Sippy thought this up; don't
 * blame me...
 */
static int _CVL_log2 (n)
int n;
{
  int answer;

  if (n <= 1)
    answer = 0;
  else {
    double d = n;
    unsigned short *s = ((unsigned short *) &d);
    answer = ((s[0] >> 4) & 0x7ff) - 0x3ff; /* big endianness */
  }
  return answer;
}


/* 0.....n  => 1
 * n+1...2n => 2
 * 2n+1..4n => 4
 * 4n+1..8n => 8
 * and so on
 */
int _CVL_elts_per_proc (length)
int length;
{
  if (length <= CM_physical_processors_limit) {
    return (1);
  } else {
    double d = (length-1) >> _CVL_log2_procs;
    unsigned short *s = (unsigned short *) &d;
    return (2 << (((s[0]>>4)&0x7ff) - 0x3ff)); /* ditto */
  }
}


/* -------------------- Initialization Functions -------------------- */

char *malloc();


static CM_geometry_id_t create_1d_send_order_geometry (procs)
int procs;
{
  CM_axis_descriptor_t descriptor_array[1], axis1;

  axis1 = (CM_axis_descriptor_t)
    malloc (sizeof (struct CM_axis_descriptor));
  bzero ((char *) axis1, sizeof (struct CM_axis_descriptor));
  descriptor_array[0] = axis1;
  axis1->length = procs;
  axis1->weight = 1;
  axis1->ordering = CM_send_order;
  return (CM_create_detailed_geometry (descriptor_array, 1));
}


void CVL_init (safety_mode)
int safety_mode;
{
  int i;

  if (!CM_attached ()) {
    if (CM_attach () == 0) {		/* attached to zero procs? */
      fprintf (stderr, "CVL: cm_attach failed, giving up\n");
      exit (0);
    }
    CM_cold_boot ();
    _CVL_started_detached = 1;
  } else {
    _CVL_started_detached = 0;
  }
  CM_init ();
  CM_set_safety_mode ((unsigned) safety_mode);
  CM_initialize_random_generator (357);	/* uhhhh.... XXX */
  _CVL_geometry = create_1d_send_order_geometry
    (CM_physical_processors_limit);
  for (i = 0; i < LOG_MAX_VP_RATIO; i++) {
    _CVL_vp_set[i] = CM_allocate_vp_set
      (create_1d_send_order_geometry
       (CM_physical_processors_limit * exp2(i)));
  }
  _CVL_log2_procs = _CVL_log2 (CM_physical_processors_limit);
  _CVL_default_vp_set = _CVL_vp_set[0];
  CM_set_vp_set (_CVL_default_vp_set);
  CM_set_context ();
  _CVL_proc_num = CM_allocate_heap_field (32);
  CM_my_send_address (_CVL_proc_num);
  CM_timer_clear (1);
  CM_timer_start (1);
}


void CVL_quit ()
{
  if (_CVL_started_detached)
    CM_detach ();
}


/* ----------- Internal Functions To Do Commmon Paris Jobs ----------- */

#define make_copyfield(_abbrev, _bits, _argbits)			\
void GLUE3 (_CVL_copy,_abbrev,field) (dst, src, iterations)		\
vec_p dst, src;								\
int iterations;								\
{									\
  int i;								\
  for (i = 0; i < iterations; i++) {					\
    GLUE3 (CM,_abbrev,move_always_1L) (dst, src, _argbits);		\
    BUMP_PTR (dst, _bits);						\
    BUMP_PTR (src, _bits);						\
  }									\
}									\

make_copyfield (_s_,INT_BITS,INT_BITS)
make_copyfield (_u_,BOO_BITS,BOO_BITS)
make_copyfield (_f_,DBL_BITS,SIG_EXP)


/* Specialized form of copy_field that copies a 1-bit flag vector into
 * an INT_BIT vector.
 */
void _CVL_copy_flag_field (dst, src, iterations)
vec_p dst, src;
int iterations;
{
  int i;
  for  (i = 0; i < iterations; i++) {
    CM_u_move_2L (dst, src, INT_BITS, BOO_BITS);
    BUMP_PTR (dst, INT_BITS);
    BUMP_PTR (src, BOO_BITS);
  }
}


/* Takes a segment descriptor and splits it into the three subvectors
 */
void _CVL_split_segd (segd, elt_count, seg_count, index, empty, start)
vec_p segd, index, empty, start;
int elt_count, seg_count;
{
  _CVL_copy_s_field (index, segd, seg_count);
  BUMP_PTR (segd, seg_count * INT_BITS);
  _CVL_copy_u_field (empty, segd, seg_count);
  BUMP_PTR (segd, seg_count);
  _CVL_copy_u_field (start, segd, elt_count);
}


/* Changes to a VP set with VP ratio "vp_ratio"
 */
void _CVL_set_vp_set (vp_ratio)
int vp_ratio;
{
  CM_set_vp_set (_CVL_vp_set [_CVL_log2 (vp_ratio)]);
}


/* Set VP set and context at the same time.  Need to have a VP ratio of 1
 * before calling this function (use CM_set_vp_set (_CVL_default_vp_set))
 */
void _CVL_set_vp_set_and_context (vp_ratio, n)
int vp_ratio, n;
{
  if (vp_ratio == 1) {
    CM_set_context ();
    CM_u_lt_constant_1L (_CVL_proc_num, (unsigned) n, INT_BITS);
    CM_logand_context_with_test ();
  } else {
    CM_field_id_t newcontext, vp_newcontext;

    /* Set up a context array for vp_ratio, zero the whole thing */
    newcontext = _CVL_allocate_stack (vp_ratio);
    CM_u_move_zero_always_1L (newcontext, vp_ratio);

    /* Set the bits for the lowest (n / vp_ratio) VPs */
    CM_set_context ();
    CM_u_lt_constant_1L (_CVL_proc_num, (unsigned) n / vp_ratio,
			 INT_BITS);
    CM_logand_context_with_test ();
    CM_lognot_1_1L (newcontext, vp_ratio);

    /* Now fix up the single processor in the middle */
    CM_set_context ();
    CM_u_eq_constant_1L (_CVL_proc_num, (unsigned) n / vp_ratio,
			 INT_BITS);
    CM_logand_context_with_test ();
    CM_lognot_1_1L (newcontext, n % vp_ratio);

    CM_set_vp_set (_CVL_vp_set [_CVL_log2 (vp_ratio)]);
    vp_newcontext = CM_make_field_alias (newcontext);
    CM_load_context (vp_newcontext);

    CM_remove_field_alias (vp_newcontext);
    CM_deallocate_stack_through (newcontext);
  }
}
