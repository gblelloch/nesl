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
*  Pittsburgh PA 15213-3890
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/

#if _MAXVL == 128
#define C90
#endif

#ifdef C90
#define VL               128
#define LOG_VELEMENTS    7
#define WORDS_PER_VM     2
#else
#define VL               64
#define LOG_VELEMENTS    6
#define WORDS_PER_VM     1
#endif




#define IDENT_MUL_Z   ((unsigned long) 1)
#define IDENT_ADD_Z   ((unsigned long) 0)
#define IDENT_MAX_Z   (-0x3FFFFFFFFFFF) /* (((unsigned long) ~0) >> 1) */
#define IDENT_MIN_Z   ( 0x3FFFFFFFFFFF) /* (~(IDENT_MAX_Z)) */
#define IDENT_AND_Z   (~((unsigned long)0))
#define IDENT_IOR_Z   ((unsigned long) 0)
#define IDENT_XOR_Z   ((unsigned long) 0)

#define IDENT_AND_B   (1)
#define IDENT_IOR_B   (0)
#define IDENT_XOR_B   (0)

/* FIX: get correct identities for doubles */
#define IDENT_MUL_D    (1.0)
#define IDENT_ADD_D    (0.0)
#define IDENT_MAX_D    (-1.0e300)
#define IDENT_MIN_D    ( 1.0e300)

#define BANKS_MASK 3

#define _compute_shape_factors(__len, __vlen, __stride, __rem) \
{  int __len1, __s, __div; \
    __len1 = __len-1; \
    __s = (__len1>>LOG_VELEMENTS)+1; \
    __s = __s + !(__s & BANKS_MASK); \
    *(__stride) = __s; \
    __div = __len1/__s; \
    *(__vlen) = __div+1; \
    *(__rem) = __len1-__div * __s+1; \
}



#define _add_fov(v, a)   ((vec_p) ((int *) (v) + (a)))

/* SEGMENT DESCRIPTOR FORMAT:
 *
 * [ LENGTHS ] [ OFFSETS ] [ LAST-START ]  --> continued
 *      m          m            VL         --> continued
 *     
 * [ INCLUSIVE LAST-START ] [NONZEROP FLAGS] [ COMPRESSED FLAGS ]
 *           VL                  siz_fopb(m)      siz_fopb(n)
 *
 *
 */

#define lengths_from_sd(sd, m) (sd)

#define offsets_from_sd(sd, m) _add_fov(sd, (m))

#define last_start_from_sd(sd, m) _add_fov(sd, 2*(m))

#define inclusive_last_start_from_sd(sd, m) _add_fov(sd, 2*(m)+VL)

#define nonzerop_from_sd(sd, m) _add_fov(sd, 2*(m)+VL+VL)

#define bools_from_sd(sd, m) _add_fov(sd, 2*(m)+VL+VL+siz_fopb(m))

 
