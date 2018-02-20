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

#ifndef cm2_h_included
#define cm2_h_included

#ifndef TRUE
#	define TRUE 1
#	define FALSE 0
#endif

#define LOG_MAX_VP_RATIO 12
#define INT_BITS 32
#define DBL_BITS 64
#define BOO_BITS 1
#define SIG_BITS 52
#define EXP_BITS 11
#define SIG_EXP 52,11
#define MAX_INT 2147483647		/* 2**32 - 1 */
#define MIN_INT (~MAX_INT)
#define MAX_DBL ((double) 1.797693134862315708e+308) /* from values.h */
#define MIN_DBL ((double)-1.797693134862315708e+308)

/* Portable way to glue two (or three) strings together in a macro */
#ifdef __STDC__
#	define GLUE(a,b) a##b
#	define GLUE3(a,b,c) a##b##c
#else */
#	define GLUE(a,b) a/**/b
#	define GLUE3(a,b,c) a/**/b/**/c
#endif */ /* __STDC__ */


/* ---------------------- Scratch Functions -------------------------- */

/* None of the functions use any user-supplied scratch space;
 * temporary vectors are allocated on the CM stack instead.
 */
#define make_no_scratch(_name)						\
int GLUE (_name,_scratch) (len)						\
int len; { return 0; }

#define make_no_seg_scratch(_name)					\
int GLUE (_name,_scratch) (vec_len, seg_len)				\
int vec_len, seg_len; { return 0; }

#define make_no_seg2_scratch(_name)					\
int GLUE (_name,_scratch) (vec_len1, seg_len1, vec_len2, seg_len2)	\
int vec_len1, seg_len1, vec_len2, seg_len2; { return 0; }


/* ---------------------- Inplace Functions -------------------------- */

/* All of the functions can be performed inplace.
 */
#define make_inplace(_name, _args)					\
unsigned int GLUE (_name,_inplace) () { return _args; }


/* ------------------------ Useful Macros ---------------------------- */

/* Increment a field pointer by a number of bits */
#define BUMP_PTR(_fieldptr, _bits)					\
_fieldptr = CM_add_offset_to_field_id (_fieldptr, _bits)

/* Cut down on the amount of typing I need to do */
#define _CVL_allocate_heap(_size) CM_allocate_heap_field ((unsigned) _size)
#define _CVL_allocate_stack(_size) CM_allocate_stack_field ((unsigned) _size)


/* ------------------ Definitions & Declarations --------------------- */

/* Declarations of global variables from internal.c */
extern CM_vp_set_id_t _CVL_default_vp_set;
extern CM_geometry_id_t _CVL_geometry;

/* Declarations of internal CVL functions from internal.c */
extern void _CVL_copy_s_field();
extern void _CVL_copy_u_field();
extern void _CVL_copy_f_field();
extern void _CVL_copy_flag_field();
extern void _CVL_split_segd();
extern void _CVL_set_vp_set();
extern void _CVL_set_vp_set_and_context();
extern int _CVL_elts_per_proc();
extern void CVL_init();
extern void CVL_quit();


/* ------------------------ For Debugging ---------------------------- */

#ifdef DEBUG
#include <stdio.h>

/* Force CM-2 to wait for front end; prevents any "pipelining" effect */
#define STEP CM_global_logior_context()

#define NOT_YET(_type, _funct)						\
_type _funct ()								\
{									\
    static int called_before = 0;					\
    if (! called_before) {						\
	called_before = 1;						\
	fprintf(stderr, "_funct has not been written yet\n");		\
    }									\
}									\
make_no_scratch(_funct)

#define NOT_YET_SEG(_type, _funct)					\
_type _funct ()								\
{									\
    static int called_before = 0;					\
    if (! called_before) {						\
	called_before = 1;						\
	fprintf(stderr, "_funct has not been written yet\n");		\
    }									\
}									\
make_no_seg_scratch(_funct)
#endif /* DEBUG */

#endif cm2_h_included
