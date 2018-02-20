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
*  Pittsburgh PA 15213-3891
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/
#include <cvl.h>
#include "defins.h"
#include "cray.h"


#define makereduce(_name, _asmname, _ident, _type) \
  make_no_scratch(_name) \
  make_inplace(_name,INPLACE_NONE) \
  _type _name (s, n, scratch) \
  vec_p s, scratch; \
  int n; \
  { \
    _type _asmname (); \
    if (n == 0) return _ident; \
    else { \
      int vlen, stride, full; \
      _compute_shape_factors(n, &vlen, &stride, &full); \
      return _asmname(s, _ident, vlen, stride, full); \
      }\
  } \

makereduce(mul_ruz, XMULRUZ, IDENT_MUL_Z, int)
makereduce(add_ruz, XADDRUZ, IDENT_ADD_Z, int)
makereduce(max_ruz, XMAXRUZ, IDENT_MAX_Z, int)
makereduce(min_ruz, XMINRUZ, IDENT_MIN_Z, int)
makereduce(and_ruz, XANDRUZ, IDENT_AND_Z, int)
makereduce(ior_ruz, XIORRUZ, IDENT_IOR_Z, int)
makereduce(xor_ruz, XXORRUZ, IDENT_XOR_Z, int)

makereduce(and_rub, XANDRUZ, IDENT_AND_B, int)
makereduce(ior_rub, XIORRUZ, IDENT_IOR_B, int)
makereduce(xor_rub, XXORRUZ, IDENT_XOR_B, int)

makereduce(mul_rud, XMULRUD, IDENT_MUL_D, double)
makereduce(add_rud, XADDRUD, IDENT_ADD_D, double)
makereduce(max_rud, XMAXRUD, IDENT_MAX_D, double) 
makereduce(min_rud, XMINRUD, IDENT_MIN_D, double) 


/* not used in current implementation */
#define makegenscan(_name, _asmname, _ident, _type) \
  make_no_scratch(_name) \
  make_inplace(_name,INPLACE_NONE) \
  _type _name (d, s, id, n, scratch) \
  vec_p d, s, scratch; \
  _type id; \
  int n; \
  { \
    _type _asmname (); \
    if (n == 0) return id; \
    else { \
      int vlen, stride, full; \
      _compute_shape_factors(n, &vlen, &stride, &full); \
      return _asmname(d, s, id, _ident, vlen, stride, full); \
      }\
  } \


#define makescan(_name, _asmname, _ident, _type) \
  make_no_scratch(_name) \
  make_inplace(_name,INPLACE_NONE) \
  void _name (d, s, n, scratch) \
  vec_p d, s, scratch; \
  int n; \
  { \
    _type _asmname (); \
    if (n != 0) { \
      int vlen, stride, full; \
      _compute_shape_factors(n, &vlen, &stride, &full); \
      /* return */ _asmname(d, s, _ident, _ident, vlen, stride, full); \
      }\
  } \

makescan(mul_suz, XMULSUZ, IDENT_MUL_Z, int)
makescan(add_suz, XADDSUZ, IDENT_ADD_Z, int)
makescan(max_suz, XMAXSUZ, IDENT_MAX_Z, int)
makescan(min_suz, XMINSUZ, IDENT_MIN_Z, int)
makescan(and_suz, XANDSUZ, IDENT_AND_Z, int)
makescan(ior_suz, XIORSUZ, IDENT_IOR_Z, int)
makescan(xor_suz, XXORSUZ, IDENT_XOR_Z, int)

makescan(and_sub, XANDSUZ, IDENT_AND_B, int)
makescan(ior_sub, XIORSUZ, IDENT_IOR_B, int)
makescan(xor_sub, XXORSUZ, IDENT_XOR_B, int)

makescan(mul_sud, XMULSUD, IDENT_MUL_D, double)
makescan(add_sud, XADDSUD, IDENT_ADD_D, double)
makescan(max_sud, XMAXSUD, IDENT_MAX_D, double) 
makescan(min_sud, XMINSUD, IDENT_MIN_D, double) 


#if 0

#define simpscan(_name, _asmname, _ident, _type) \
  make_no_scratch(_name) \
  make_inplace(_name,INPLACE_NONE) \
  void _name (d, s, n, scratch) \
  vec_p d, s, scratch; \
  int n; \
  { \
    _type _asmname (); \
    int vlen, stride, full; \
    if (n == 0) return; \
    _compute_shape_factors(n, &vlen, &stride, &full); \
    _asmname(d, s, _ident, _ident, vlen, stride, full); \
  } \

simpscan(add_nuz, XADDSUZ, IDENT_ADD_Z, int)
simpscan(max_nuz, XMAXSUZ, IDENT_MAX_Z, int)
simpscan(min_nuz, XMINSUZ, IDENT_MIN_Z, int)
simpscan(and_nuz, XANDSUZ, IDENT_AND_Z, int)
simpscan(ior_nuz, XIORSUZ, IDENT_IOR_Z, int)

simpscan(and_nub, XANDSUZ, IDENT_AND_B, int)
simpscan(ior_nub, XIORSUZ, IDENT_IOR_B, int)

simpscan(add_nud, XADDSUD, IDENT_ADD_D, double)
simpscan(max_nud, XMAXSUD, IDENT_MAX_D, double) 
simpscan(min_nud, XMINSUD, IDENT_MIN_D, double) 

#endif



/*
                save original vector (DEST = SRC)
                gather last element of each segment (DEST_SUM = SRC[ENDS])
                add starting values to beginning of seg (DEST[STARTS] op= VAL)
                segmented exclusive scan (DEST = scan(DEST))
                gather last element of each segment (LASTS = DEST[ENDS])
                DEST_SUM op= LASTS
                scatter starts (DEST[STARTS] = VAL)
*/

#define makesegfun(_name, _elwise_op, _asmname, _unsegasmname, _ident) \
  make_no_seg_scratch(_name) \
  make_inplace(_name,INPLACE_NONE) \
  void _name (d1, d2, s, in, sd, n, m, scratch) \
  vec_p d1, d2, s, in, sd, scratch; \
  int n, m; \
  { \
    void _asmname (); \
    int vlen, stride, full; \
    if (n == 0) return; \
    _compute_shape_factors(n, &vlen, &stride, &full); \
    if (m == 1) { \
      _unsegasmname (d1, s, ((int *)in)[0], _ident, vlen, stride, full); \
    } else { \
      vec_p flags, offsets, lengths, begins, ends; \
      offsets = offsets_from_sd(sd, m); \
      lengths = lengths_from_sd(sd, m); \
      flags = bools_from_sd(sd, m); \
      begins =  scratch; \
      ends = _add_fov(begins, siz_foz(m)); \
      cpy_wuz(d1, s, n, 0); \
      xextlast(d2, s, lengths, offsets, m, 0); \
      bck_puz(begins, d1, offsets, n, m, 0); \
      _elwise_op(begins, begins, in, m, 0); \
      smp_puz(d1, begins, offsets, m, 0); \
      _asmname (d1, d1, flags, _ident, vlen, stride, full); \
      xextlast(ends, d1, lengths, offsets, m, 0); \
      _elwise_op(d2, d2, ends, m, 0); \
      smp_puz(d1, in, offsets, m, 0); \
    } \
  }

#if 0
 /* CVL no longer supports "general" scan operations */
makesegfun(add_sez, add_wuz, XADDSEZ, XADDSUZ, IDENT_ADD_Z)
makesegfun(max_sez, max_wuz, XMAXSEZ, XMAXSUZ, IDENT_MAX_Z)
makesegfun(min_sez, min_wuz, XMINSEZ, XMINSUZ, IDENT_MIN_Z)
makesegfun(and_sez, and_wuz, XANDSEZ, XANDSUZ, IDENT_AND_Z)
makesegfun(ior_sez, ior_wuz, XIORSEZ, XIORSUZ, IDENT_IOR_Z)

makesegfun(and_seb, and_wub, XANDSEZ, XANDSUZ, IDENT_AND_B)
makesegfun(ior_seb, ior_wub, XIORSEZ, XIORSUZ, IDENT_IOR_B)

makesegfun(add_sed, add_wud, XADDSED, XADDSUD, IDENT_ADD_D)
makesegfun(max_sed, max_wud, XMAXSED, XMAXSUD, IDENT_MAX_D) 
makesegfun(min_sed, min_wud, XMINSED, XMINSUD, IDENT_MIN_D) 
#endif

#define simpsegscan(_name, _asmname, _unsegasmname, _ident) \
  make_no_seg_scratch(_name) \
  make_inplace(_name,INPLACE_NONE) \
  void _name (d1, s, sd, n, m, scratch) \
  vec_p d1, s, sd, scratch; \
  int n, m; \
  { \
    void _asmname (); \
    int vlen, stride, full; \
    vec_p flags; \
    if (n == 0) return; \
      _compute_shape_factors(n, &vlen, &stride, &full); \
      if (m == 1) {\
         _unsegasmname (d1, s, _ident, _ident, vlen, stride, full); \
       } else {\
         flags = bools_from_sd(sd, m); \
         _asmname (d1, s, flags, _ident, vlen, stride, full); \
    }\
  } \



simpsegscan(mul_sez, XMULSEZ, XMULSUZ, IDENT_MUL_Z)
simpsegscan(add_sez, XADDSEZ, XADDSUZ, IDENT_ADD_Z)
simpsegscan(max_sez, XMAXSEZ, XMAXSUZ, IDENT_MAX_Z)
simpsegscan(min_sez, XMINSEZ, XMINSUZ, IDENT_MIN_Z)
simpsegscan(and_sez, XANDSEZ, XANDSUZ, IDENT_AND_Z)
simpsegscan(ior_sez, XIORSEZ, XIORSUZ, IDENT_IOR_Z)
simpsegscan(xor_sez, XXORSEZ, XXORSUZ, IDENT_XOR_Z)

simpsegscan(and_seb, XANDSEZ, XANDSUZ, IDENT_AND_B)
simpsegscan(ior_seb, XIORSEZ, XIORSUZ, IDENT_IOR_B)
simpsegscan(xor_seb, XXORSEZ, XXORSUZ, IDENT_XOR_B)

simpsegscan(mul_sed, XMULSED, XMULSUD, IDENT_MUL_D)
simpsegscan(add_sed, XADDSED, XADDSUD, IDENT_ADD_D)
simpsegscan(max_sed, XMAXSED, XMAXSUD, IDENT_MAX_D)
simpsegscan(min_sed, XMINSED, XMINSUD, IDENT_MIN_D)


#ifdef OLD_STUPID_SLOW_WAY_USING_INCLUSIVE_SCAN 

#define makesegreduce(_name, _asmname, _unsegasmname, _ident, _ident_t) \
    make_inplace(_name,INPLACE_NONE) \
    int GLUE(_name,_scratch)(n, m) int n, m;{return n+m;}\
    void _name (d, s, sd, n, m, scratch) \
    vec_p d, s, sd, scratch; \
    int n, m; \
    { \
      void _asmname (), dist_sel(); \
      int vlen, stride, full; \
      vec_p flags, offsets, lengths, scanned_s, nonzerop; \
      vec_p temp_d; \
      if (n == 0) { \
        dis_vuz(d, _ident, m, scratch); /* dist. identity m to empty segments */\
        return;\
      }\
      _compute_shape_factors(n, &vlen, &stride, &full); \
      if (m == 1) {\
        ((_ident_t *)d)[0] = \
         _unsegasmname (s, _ident, vlen, stride, full); \
      } else {\
        flags = bools_from_sd(sd, m); \
        offsets = offsets_from_sd(sd, m); \
        lengths = lengths_from_sd(sd, m); \
        nonzerop = nonzerop_from_sd(sd, m); \
        scanned_s = scratch; \
        temp_d = _add_fov(scanned_s, n); \
        _asmname (scanned_s, s, flags, _ident, vlen, stride, full); \
        xextlast(temp_d, scanned_s, lengths, offsets, m, 0); \
        dist_sel(d, nonzerop, temp_d, _ident, m, 0); /* ident where 0 length */ \
      }\
    } \

  makesegreduce(mul_rez, IMULSEZ, XMULRUZ, IDENT_MUL_Z, int)
  makesegreduce(add_rez, IADDSEZ, XADDRUZ, IDENT_ADD_Z, int)
  makesegreduce(max_rez, IMAXSEZ, XMAXRUZ, IDENT_MAX_Z, int)
  makesegreduce(min_rez, IMINSEZ, XMINRUZ, IDENT_MIN_Z, int)
  makesegreduce(and_rez, IANDSEZ, XANDRUZ, IDENT_AND_Z, int)
  makesegreduce(ior_rez, IIORSEZ, XIORRUZ, IDENT_IOR_Z, int)
  makesegreduce(xor_rez, IXORSEZ, XXORRUZ, IDENT_XOR_Z, int)

  makesegreduce(and_reb, IANDSEZ, XANDRUZ, IDENT_AND_B, cvl_bool)
  makesegreduce(ior_reb, IIORSEZ, XIORRUZ, IDENT_IOR_B, cvl_bool)
  makesegreduce(xor_reb, IXORSEZ, XXORRUZ, IDENT_XOR_B, cvl_bool)

  makesegreduce(mul_red, IMULSED, XMULRUD, IDENT_MUL_D, double)
  makesegreduce(add_red, IADDSED, XADDRUD, IDENT_ADD_D, double)
  makesegreduce(max_red, IMAXSED, XMAXRUD, IDENT_MAX_D, double)
  makesegreduce(min_red, IMINSED, XMINRUD, IDENT_MIN_D, double)

#else


#define makesegreduce(_name, _asmname, _unsegasmname, _ident, _ident_t) \
  make_inplace(_name,INPLACE_NONE) \
  int GLUE(_name,_scratch)(n, m) int n, m;{return n;}\
  void _name (d, s, sd, n, m, scratch) \
  vec_p d, s, sd, scratch; \
  int n, m; \
  { \
    void _asmname (), dist_sel(); \
    int vlen, stride, full; \
    vec_p flags, offsets, scanned_s, nonzerop, lastind, new_scratch; \
    if (n == 0) { \
      if (m > 0) { \
        _compute_shape_factors(m, &vlen, &stride, &full); \
        DISVUZ(d, _ident, vlen, stride, full); /* dist. identity m to empty segments */\
      }\
      return;\
    }\
    _compute_shape_factors(n, &vlen, &stride, &full); \
    if (m == 1) {\
      ((_ident_t *)d)[0] = \
       _unsegasmname (s, _ident, vlen, stride, full); \
    } else {\
      flags = bools_from_sd(sd, m); \
      offsets = offsets_from_sd(sd, m); \
      nonzerop = nonzerop_from_sd(sd, m); \
      lastind = inclusive_last_start_from_sd(sd, m); \
      scanned_s = scratch; \
      new_scratch = _add_fov(scratch, n); \
      _asmname (scanned_s, s, _add_fov(flags, WORDS_PER_VM*stride-1), lastind, \
                       _ident, vlen, stride, full); \
      /* OLD: bck_puz(d, scanned_s, offsets, n, m, new_scratch); */ \
      /* OLD: dist_sel(d, nonzerop, d, _ident, m, 0); */ \
      /* gather from scanned_s but use ident where 0 length */ \
      bck_puz_dist_sel(d, scanned_s, offsets, nonzerop, _ident, m, 0); \
    }\
  } \

makesegreduce(mul_rez, XMULREZ, XMULRUZ, IDENT_MUL_Z, int)
makesegreduce(add_rez, XADDREZ, XADDRUZ, IDENT_ADD_Z, int)
makesegreduce(max_rez, XMAXREZ, XMAXRUZ, IDENT_MAX_Z, int)
makesegreduce(min_rez, XMINREZ, XMINRUZ, IDENT_MIN_Z, int)
makesegreduce(and_rez, XANDREZ, XANDRUZ, IDENT_AND_Z, int)
makesegreduce(ior_rez, XIORREZ, XIORRUZ, IDENT_IOR_Z, int)
makesegreduce(xor_rez, XXORREZ, XXORRUZ, IDENT_XOR_Z, int)

makesegreduce(and_reb, XANDREZ, XANDRUZ, IDENT_AND_B, cvl_bool)
makesegreduce(ior_reb, XIORREZ, XIORRUZ, IDENT_IOR_B, cvl_bool)
makesegreduce(xor_reb, XXORREZ, XXORRUZ, IDENT_XOR_B, cvl_bool)

makesegreduce(mul_red, XMULRED, XMULRUD, IDENT_MUL_D, double)
makesegreduce(add_red, XADDRED, XADDRUD, IDENT_ADD_D, double)
makesegreduce(max_red, XMAXRED, XMAXRUD, IDENT_MAX_D, double)
makesegreduce(min_red, XMINRED, XMINRUD, IDENT_MIN_D, double)

#endif
