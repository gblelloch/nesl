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
/* This file contains some library functions */
#include "defins.h"
#include <cvl.h>
#include "cray.h"


make_inplace(smp_puz,INPLACE_NONE)

/* Just have the double and bool versions call the int versions */

int smp_pud_scratch(len) int len; {return 0;}
make_inplace(smp_pud,INPLACE_NONE)


/* Elementwise double permute */
/* ARGSUSED */
void smp_pud(D, SRC, INDEX, LEN, SCRATCH)
vec_p D;
vec_p SRC;
vec_p INDEX;
int LEN; 
vec_p SCRATCH;
{
  void SMPPUZ();
  int vlen, stride, full;

  if (LEN > 0) {
    _compute_shape_factors(LEN, &vlen, &stride, &full);
    SMPPUZ(D, SRC, INDEX, vlen, stride, full);
  }
}



int smp_pub_scratch(len) int len; {return 0;}
make_inplace(smp_pub,INPLACE_NONE)

/* Elementwise boolean permute */
/* ARGSUSED */
void smp_pub(D, SRC, INDEX, LEN, SCRATCH)
vec_p D;
vec_p SRC;
vec_p INDEX;
int LEN; 
vec_p SCRATCH;
{
  void SMPPUZ();
  int vlen, stride, full;

  if (LEN > 0) {
    _compute_shape_factors(LEN, &vlen, &stride, &full);
    SMPPUZ(D, SRC, INDEX, vlen, stride, full);
  }
}


make_no2_scratch(bck_puz)
make_inplace(bck_puz,INPLACE_2)

/* Elementwise integer back permute */
/* ARGSUSED */
void bck_puz(D, SRC, INDEX, LEN, DLEN, SCRATCH)
vec_p D;
vec_p SRC;
vec_p INDEX;
int LEN; 
int DLEN; 
vec_p SCRATCH;
{
  void XBCKPUZ();
  int vlen, stride, full;

  if (DLEN > 0) {
    _compute_shape_factors(DLEN, &vlen, &stride, &full);
    XBCKPUZ(D, SRC, INDEX, vlen, stride, full);
  }
}




make_no2_scratch(bck_pub)
make_inplace(bck_pub,INPLACE_2)

/* Elementwise boolean back permute */
/* ARGSUSED */
void bck_pub(D, SRC, INDEX, LEN, DLEN, SCRATCH)
vec_p D;
vec_p SRC;
vec_p INDEX;
int LEN; 
int DLEN; 
vec_p SCRATCH;
{
  void XBCKPUZ();
  int vlen, stride, full;

  if (LEN > 0) {
    _compute_shape_factors(DLEN, &vlen, &stride, &full);
    XBCKPUZ(D, SRC, INDEX, vlen, stride, full);
  }
}


make_no2_scratch(bck_pud)
make_inplace(bck_pud,INPLACE_2)


/* Elementwise double back permute */
/* ARGSUSED */
void bck_pud(D, SRC, INDEX, LEN, DLEN, SCRATCH)
vec_p D;
vec_p SRC;
vec_p INDEX;
int LEN; 
int DLEN; 
vec_p SCRATCH;
{
  void XBCKPUZ();
  int vlen, stride, full;

  if (LEN > 0) {
    _compute_shape_factors(DLEN, &vlen, &stride, &full);
    XBCKPUZ(D, SRC, INDEX, vlen, stride, full);
  }
}




int fpm_puz_scratch(n, n2) int n, n2; {return VL;}
make_inplace(fpm_puz, INPLACE_NONE)

/* ARGUSED */
void
fpm_puz(d, s, index, f, n, len_dest, scratch)
vec_p d, s, index, f, scratch;
int n, len_dest;
{
  void XSELPER();
  int vlen, stride, full;

  if (n > 0) {
    _compute_shape_factors(n, &vlen, &stride, &full);
    XSELPER(d, s, index, f, scratch, vlen, stride, full);
  }
}



int pfpm_puz_scratch(n, n2) int n, n2; {return VL;}
make_inplace(pfpm_puz, INPLACE_NONE)

/* ARGUSED */
void  /* takes packed flags */
pfpm_puz(d, s, index, f, n, len_dest, scratch)
vec_p d, s, index, f, scratch;
int n, len_dest;
{
  void XPBSELP();
  int vlen, stride, full;

  if (n > 0) {
    _compute_shape_factors(n, &vlen, &stride, &full);
    XPBSELP(d, s, index, f, scratch, vlen, stride, full);
  }
}




int fpm_pub_scratch(n, n2) int n, n2; {return VL;}
make_inplace(fpm_pub, INPLACE_NONE)

/* ARGUSED */
void
fpm_pub(d, s, index, f, n, len_dest, scratch)
vec_p d, s, index, f, scratch;
int n, len_dest;
{
  void XSELPER();
  int vlen, stride, full;

  if (n > 0) {
    _compute_shape_factors(n, &vlen, &stride, &full);
    XSELPER(d, s, index, f, scratch, vlen, stride, full);
  }
}




int fpm_pud_scratch(n, n2) int n, n2; {return VL;}
make_inplace(fpm_pud, INPLACE_NONE)

/* ARGUSED */
void
fpm_pud(d, s, index, f, n, len_dest, scratch)
vec_p d, s, index, f, scratch;
int n, len_dest;
{
  void XSELPER();
  int vlen, stride, full;

  if (n > 0) {
    _compute_shape_factors(n, &vlen, &stride, &full);
    XSELPER(d, s, index, f, scratch, vlen, stride, full);
  }
}




/* ------------------------dpermute ----------------------------*/
/* permute with default.  Any element not filled by the permute
 * gets set to the corresponding element of the default vector.
 * first copy default into dest, then do permute.
 * 	d = destination 
 *	s = source (same type as d)
 *	i = index vector (same length as s)
 *	v = default vector (same length as d)
 *	len_src 
 *	len_dest
 */
void dpe_puz(d, s, i, v, len_src, len_dest, scratch)
    vec_p d, s, i, v, scratch;
    int len_src, len_dest;
    {
    if (v != d) cpy_wuz(d, v, len_dest, scratch);
    smp_puz(d, s, i, len_src, scratch);
    }
make_inplace(dpe_puz, INPLACE_NONE)
int dpe_puz_scratch(n, n2) int n, n2;
  {return max(cpy_wuz_scratch(n2),smp_puz_scratch(n));}

void dpe_pub(d, s, i, v, len_src, len_dest, scratch)
    vec_p d, s, i, v, scratch;
    int len_src, len_dest;
    {
    if (v != d) cpy_wub(d, v, len_dest, scratch);
    smp_pub(d, s, i, len_src, scratch);
    }
make_inplace(dpe_pub, INPLACE_NONE)
int dpe_pub_scratch(n, n2) int n, n2;
  {return max(cpy_wub_scratch(n),smp_pub_scratch(n));}

void dpe_pud(d, s, i, v, len_src, len_dest, scratch)
    vec_p d, s, i, v, scratch;
    int len_src, len_dest;
    {
    if (v != d) cpy_wud(d, v, len_dest, scratch);
    smp_pud(d, s, i, len_src, scratch);
    }
make_inplace(dpe_pud, INPLACE_NONE)
int dpe_pud_scratch(n, n2) int n, n2;
   {return max(cpy_wud_scratch(n),smp_pud_scratch(n));}


/*----------------------dfpermute----------------------------*/
/* default permute with flags.  Any element not filled by the fpermute
 * gets set to the corresponding element of the default vector.
 * first copy default into dest, then do fpermute.
 * 	d = destination 
 *	s = source (same type as d)
 *	i = index vector (same length as s)
 *	v = default vector (same length as d)
 *	len_src 
 *	len_dest
 */
#define make_dfp(_type_let)					\
void GLUE(dfp_pu,_type_let) (d, s, i, f, v, len_src, len_dest, scratch)\
    vec_p d, s, i, f, v, scratch;				\
    int len_src, len_dest;					\
{								\
    if (d != v) GLUE(cpy_wu,_type_let) (d, v, len_dest, scratch);	\
    GLUE(fpm_pu,_type_let) (d, s, i, f, len_src, len_dest, scratch);	\
}
make_dfp(z)
make_dfp(b)
make_dfp(d)

make_inplace(dfp_puz, INPLACE_NONE)
int dfp_puz_scratch(n, n2) {return max(cpy_wuz_scratch(n2),smp_puz_scratch(n));}
make_inplace(dfp_pub, INPLACE_NONE)
int dfp_pub_scratch(n, n2) {return max(cpy_wub_scratch(n2),smp_pub_scratch(n));}
make_inplace(dfp_pud, INPLACE_NONE)
int dfp_pud_scratch(n, n2) {return max(cpy_wud_scratch(n2),smp_pud_scratch(n));}



#define make_seg_dpe(_type_let, _asmname) \
void GLUE(dpe_pe,_type_let) (d, s, i, v, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch)\
    vec_p d, s, i, v, sd_s, sd_d, scratch;			\
    int n_s, n_d, m_s, m_d;					\
{								\
  if (m_s == 1) { \
    GLUE(dpe_pu,_type_let) (d, s, i, v, n_s, n_d, scratch); \
  } else { \
         void _asmname (); \
         int vlen, stride, full; \
         vec_p flags, last_start, offsets_s, offsets_d, nonzerop_s, starts=scratch; \
         if (n_d == 0) return; \
         if (d != v) GLUE(cpy_wu,_type_let) (d, v, n_d, 0); \
         if (n_s == 0) return; \
         flags = bools_from_sd(sd_s, m_s); \
         last_start = last_start_from_sd(sd_s, m_s); \
         offsets_s = offsets_from_sd(sd_s, m_s); \
         offsets_d = offsets_from_sd(sd_d, m_d); \
         nonzerop_s = nonzerop_from_sd(sd_s, m_s); \
         pfpm_puz(starts, offsets_d, offsets_s, nonzerop_s, m_d, n_s, 0);\
         _compute_shape_factors(n_s, &vlen, &stride, &full); \
         _asmname (d, s, i, flags, last_start, starts, vlen, stride, full); \
        } \
}

make_seg_dpe(z, XSEGDPE)
make_seg_dpe(b, XSEGDPE)
make_seg_dpe(d, XSEGDPE)


make_inplace(dpe_pez, INPLACE_NONE)
int dpe_pez_scratch(n_in, m_in, n_out, m_out) { return max(VL,n_in) ;}

make_inplace(dpe_peb, INPLACE_NONE)
int dpe_peb_scratch(n_in, m_in, n_out, m_out) { return max(VL,n_in) ;}

make_inplace(dpe_ped, INPLACE_NONE)
int dpe_ped_scratch(n_in, m_in, n_out, m_out) { return max(VL,n_in) ;}


#define make_seg_dfp(_type_let)					\
void GLUE(dfp_pe,_type_let) (d, s, i, f, v, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch)\
    vec_p d, s, i, f, v, sd_s, sd_d, scratch;			\
    int n_s, n_d, m_s, m_d;					\
{								\
    if (d != v) GLUE(cpy_wu,_type_let) (d, v, n_d, scratch);	\
    GLUE(fpm_pe,_type_let) (d, s, i, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch);\
}

make_seg_dfp(z)
make_seg_dfp(b)
make_seg_dfp(d)

make_inplace(dfp_pez, INPLACE_NONE)
int dfp_pez_scratch(n_in, m_in, n_out, m_out) 
{ return fpm_pez_scratch(n_in, m_in, n_out, m_out);}

make_inplace(dfp_peb, INPLACE_NONE)
int dfp_peb_scratch(n_in, m_in, n_out, m_out) 
{ return fpm_peb_scratch(n_in, m_in, n_out, m_out);}

make_inplace(dfp_ped, INPLACE_NONE)
int dfp_ped_scratch(n_in, m_in, n_out, m_out) 
{ return fpm_ped_scratch(n_in, m_in, n_out, m_out);}








#define make_seg_fpm(_type_let, _asmname) \
void GLUE(fpm_pe,_type_let) (d, s, i, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch)\
    vec_p d, s, i, f, sd_s, sd_d, scratch;			\
    int n_s, n_d, m_s, m_d;					\
{								\
  if (m_s == 1) { \
    GLUE(fpm_pu,_type_let) (d, s, i, f, n_s, n_d, scratch); \
  } else { \
         void _asmname (); \
         int vlen, stride, full; \
         vec_p flags, last_start, offsets_s, offsets_d, nonzerop_s, starts=scratch; \
         if (n_d == 0) return; \
         if (n_s == 0) return; \
         flags = bools_from_sd(sd_s, m_s); \
         last_start = last_start_from_sd(sd_s, m_s); \
         offsets_s = offsets_from_sd(sd_s, m_s); \
         offsets_d = offsets_from_sd(sd_d, m_d); \
         nonzerop_s = nonzerop_from_sd(sd_s, m_s); \
         pfpm_puz(starts, offsets_d, offsets_s, nonzerop_s, m_d, n_s, 0);\
         _compute_shape_factors(n_s, &vlen, &stride, &full); \
         _asmname (i, flags, last_start, starts, vlen, stride, full); \
         pfpm_puz(d, s, starts, f, n_s, n_d, 0);\
        } \
}

make_seg_fpm(z, XSEGFPM)
make_seg_fpm(b, XSEGFPM)
make_seg_fpm(d, XSEGFPM)


make_inplace(fpm_pez, INPLACE_NONE)
int fpm_pez_scratch(n_in, m_in, n_out, m_out) { return max(VL,n_in) ;}

make_inplace(fpm_peb, INPLACE_NONE)
int fpm_peb_scratch(n_in, m_in, n_out, m_out) { return max(VL,n_in) ;}

make_inplace(fpm_ped, INPLACE_NONE)
int fpm_ped_scratch(n_in, m_in, n_out, m_out) { return max(VL,n_in) ;}


#define make_bfp(_name, _type)					\
	void _name(d, s, i, f, len_src, len_dest, scratch)	\
	vec_p d, s, i, f, scratch;				\
	int len_src, len_dest;					\
    { \
       void XBSELPE(), SELWUZ(); \
       int vlen, stride, full; \
       int *temp_d = (int *) scratch; \
       int *permute_junk = temp_d + len_dest; \
       if (len_dest == 0) return; \
       _compute_shape_factors(len_dest, &vlen, &stride, &full); \
       XBSELPE(temp_d, s, i, f, permute_junk, vlen, stride, full); \
       SELWUZ(d, f, temp_d, d, vlen, stride, full); \
    } \
make_inplace(_name, INPLACE_NONE) 

int bfp_puz_scratch(n, n2) int n, n2; {return VL + n2;}
int bfp_pud_scratch(n, n2) int n, n2; {return VL + n2;}


make_bfp(bfp_puz, int)
make_bfp(bfp_pub, cvl_bool)
make_bfp(bfp_pud, double)





#define makesegperm(_name, _asmname, _unsegasmname) \
  make_no_seg_scratch(_name) \
  make_inplace(_name,INPLACE_NONE) \
  void _name (d, s, i, sd, n, m, scratch) \
  vec_p d, s, i, sd, scratch; \
  int n, m; \
  { \
    void _asmname (); \
    int vlen, stride, full; \
    vec_p flags, last_start; \
    if (n == 0) return; \
     _compute_shape_factors(n, &vlen, &stride, &full); \
    if (m == 1) {\
      _unsegasmname (d, s, i, vlen, stride, full); \
      } else {\
      last_start = last_start_from_sd(sd, m); \
      flags = bools_from_sd(sd, m); \
      _asmname (d, s, i, flags, last_start, vlen, stride, full); \
      }\
  } \

makesegperm(smp_pez, XSEGPER, SMPPUZ)
makesegperm(smp_peb, XSEGPER, SMPPUZ)
makesegperm(smp_ped, XSEGPER, SMPPUZ)


int bck_pez_scratch(n_s, m_s, n_d, m_d) int n_s, m_s, n_d, m_d; 
{return VL+n_d;}

int bck_peb_scratch(n_s, m_s, n_d, m_d) int n_s, m_s, n_d, m_d; 
{return VL+n_d;}

int bck_ped_scratch(n_s, m_s, n_d, m_d) int n_s, m_s, n_d, m_d; 
{return VL+n_d;}


#define makesegbperm(_name, _asmname, _general_asmname, _unsegasmname) \
  make_inplace(_name,INPLACE_NONE)  \
  void _name (d, s, i, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch) \
  vec_p d, s, i, sd_s, sd_d, scratch; \
  int n_s, n_d, m_s, m_d; \
  { \
    void _asmname (); \
    int vlen, stride, full; \
    vec_p flags, last_start; \
    if (n_d == 0) return; \
     _compute_shape_factors(n_d, &vlen, &stride, &full); \
    flags = bools_from_sd(sd_d, m_d); \
    last_start = last_start_from_sd(sd_d, m_d); \
    if (m_d == 1) {\
        _unsegasmname (d, s, i, vlen, stride, full); \
      } else if (sd_s == sd_d) {\
        _asmname (d, s, i, flags, last_start, vlen, stride, full); \
      } else {\
        vec_p starts = scratch;\
        vec_p offsets_s = offsets_from_sd(sd_s, m_s); \
        vec_p offsets_d = offsets_from_sd(sd_d, m_d); \
        vec_p nonzerop_d = nonzerop_from_sd(sd_d, m_d); \
        pfpm_puz(starts, offsets_s, offsets_d, nonzerop_d, m_s, n_d, 0);\
        _general_asmname (i, flags, last_start, starts, vlen, stride, full); \
        bck_puz(d, s, starts, n_s, n_d, 0);\
      }\
  } \

makesegbperm(bck_pez, XSEGBPE, XSEGFPM, XBCKPUZ)
makesegbperm(bck_peb, XSEGBPE, XSEGFPM, XBCKPUZ)
makesegbperm(bck_ped, XSEGBPE, XSEGFPM, XBCKPUZ)



/* FIX: optimize function below for case when source and destination
 *     segment descriptors are the same.
 *
 *     } else if (sd_s == sd_d) {
 *       _asmname (d, s, i, flags, last_start, vlen, stride, full); 
 *       ...
 */

#define make_seg_bfp(_name, _asmname, _unsegasmname, _type)		 \
void _name(d, s, i, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch)  \
vec_p d, s, i, f, sd_s, sd_d, scratch;			\
int n_s, m_s, n_d, m_d;					\
{							\
    void _asmname (); \
    int vlen, stride, full; \
    vec_p flags, last_start; \
    if (n_d == 0) return; \
     _compute_shape_factors(n_d, &vlen, &stride, &full); \
    if (m_d == 1) {\
        _unsegasmname (d, s, i, f, vlen, stride, full); \
      } else {\
        vec_p starts = scratch;\
        vec_p new_scratch = _add_fov(scratch, siz_foz(n_d));\
        vec_p offsets_s = offsets_from_sd(sd_s, m_s); \
        vec_p offsets_d = offsets_from_sd(sd_d, m_d); \
        vec_p nonzerop_d = nonzerop_from_sd(sd_d, m_d); \
        flags = bools_from_sd(sd_d, m_d); \
        last_start = last_start_from_sd(sd_d, m_d); \
        pfpm_puz(starts, offsets_s, offsets_d, nonzerop_d, m_s, n_d, 0);\
        _asmname (i, flags, last_start, starts, vlen, stride, full); \
        bfp_puz(d, s, starts, f, n_s, n_d, new_scratch);\
      }\
  } \
  make_inplace(_name, INPLACE_NONE) 

make_seg_bfp(bfp_pez, XSEGFPM, bfp_puz, int)
make_seg_bfp(bfp_ped, XSEGFPM, bfp_pud, double)
make_seg_bfp(bfp_peb, XSEGFPM, bfp_pub, cvl_bool)


int bfp_pez_scratch(n_s, m_s, n_d, m_d) int n_s, m_s, n_d, m_d; 
{return VL + n_d + n_d;}

int bfp_peb_scratch(n_s, m_s, n_d, m_d) int n_s, m_s, n_d, m_d; 
{return VL + n_d + n_d;}

int bfp_ped_scratch(n_s, m_s, n_d, m_d) int n_s, m_s, n_d, m_d; 
{return VL + n_d + n_d;}






#define makesegdist(_name, _asmname, _unsegasmname) \
  make_no_seg_scratch(_name) \
  make_inplace(_name,INPLACE_NONE) \
  void _name (d, s, sd, n, m, scratch) \
  vec_p d, s, sd, scratch; \
  int n, m; \
  { \
    void _asmname (); \
    int vlen, stride, full; \
    vec_p flags, offsets, last_start, nonzerop; \
    if (n == 0) return; \
    _compute_shape_factors(n, &vlen, &stride, &full); \
    if (m == 1) {\
        _unsegasmname (d, ((int *)s)[0], vlen, stride, full); \
      } else {\
        offsets = offsets_from_sd(sd, m); \
        last_start = last_start_from_sd(sd, m); \
        nonzerop = nonzerop_from_sd(sd, m); \
        pfpm_puz(d, s, offsets, nonzerop, m, n, scratch);\
        flags = bools_from_sd(sd, m); \
        _asmname (d, d, flags, last_start, vlen, stride, full); \
      }\
  } \

makesegdist(dis_vez, XCPYSCA, DISVUZ)
makesegdist(dis_veb, XCPYSCA, DISVUZ)
makesegdist(dis_ved, XCPYSCA, DISVUZ)


/**************************************************************/
#ifdef OLD_CRUSTY_CVL_WITH_OLD_LIBRARY_FUNCS


void spl_lez(), spl_leb(), spl_led();

#define makesplit(_name, _asmname) \
  make_no_scratch(_name) \
  make_inplace(_name,INPLACE_NONE) \
  int _name (d, s, flags, n, scratch) \
  vec_p d, s, flags, scratch; \
  int n; \
  { \
    int _asmname (); \
    if (n == 0) return 0; \
    else { \
      int vlen, stride, full; \
      _compute_shape_factors(n, &vlen, &stride, &full); \
      return _asmname(d, s, flags, vlen, stride, full); \
      }\
  } \

makesplit(spl_luz, XSPLLUZ)
makesplit(spl_lub, XSPLLUZ)
makesplit(spl_lud, XSPLLUZ)




spl_leb_inplace() {return 0;}
spl_leb_scratch(n, m) int n, m; {return spl_lez_scratch(n, m);}
void spl_leb(d, d2, s, f1, sd, n, m, scratch) 
vec_p d, d2, s, f1, sd, scratch;
int n, m;
{
  spl_lez(d, d2, s, f1, sd, n, m, scratch);
}

unsigned int spl_led_inplace() {return 0;}
spl_led_scratch(n, m) int n, m; {return spl_lez_scratch(n, m);}
void spl_led(d, d2, s, f1, sd, n, m, scratch) 
vec_p d, d2, s, f1, sd, scratch;
int n, m;
{
  spl_lez(d, d2, s, f1, sd, n, m, scratch);
}

unsigned int spl_lez_inplace() {return 0;}
spl_lez_scratch(n, m) int n, m; {return 3*n + 2*m + n /* add_rez */;}   /* ??????? */

/*

Guy's pseudo-code:

    3) Way fast segmented split (a rough outline)...there are
         certainly other ways of doing it.
  
       Assume the flags are unpacked.
       Assume same segments as above

       Flags:
             1   0   0   1
             0   1   1   1
             1   1   0   1
             0   0   1   0
             
       Pass 1:   (cost 2.6n)
            regular plus scan on unpacked flag, but keep column sums
            also, write out inverse flags on phase 1.

             0   2   4   6
             1   2   4   7
             1   3   5   8
             2   4   5   9

             2   2   2   3    column sums

       Gather from begining of each segment (cost 2m)
             0  2  2  5  8   call this a

       Shift (put total sum at end)         (cost 0)
             2  2  5  8  9   call this b

       b - a (number of 1s in each segment) (cost m)
             2  0  3  3  1   call this t_length

       length - t_length (number of 0s in each segment)  (cost m)
             1  2  2  1  1   call this f_length
   
       For flags, add to the beginning of each segment
       f_lenght of the segment:   (4m)

       Flags:
             2x  0   0   1x
             0   3x  1x  1x
             1x  1x  1   2x
             2   0   1x  0     x marks true flags

       For inverse flags, add to the beginning of each segment
       t_lenght of the >previous< segment:   (4m)

       Inverse Flags:
             0   1   1   0
             1   0   0   0
             0   0   4   3
             3   1   0   1

     Pass 2
       Now if we do an inclusive plus scan on Flags with
       a starting value of -1.

             1x  4   8  12x
             1   7x  9x 13x
             2x  8x 10  15x
             4   8  11x 15    x marks true flags

       Similarly we do an inclusive scan of inverse flags 
       with starting value -1
       
            -1   4x  6x 10
             0x  4   6  10
             0   4  10x 13
             3x  5x 10  14x   x marks false flags

        Now we can do an unsegmented select and permute, and we are done!!

        However, we do not need to do phase 1 of the two scans.
        We can use the column index pointers and the column sums to skip 
        them.   This means that pass 2 can be done in a single so-loop,
        which is the same as the second loop for the unsegmented split.
        (cost 3.5n --- guestimate).

To do:
  compute shape factors only once
  figure out scratch space exactly
  improve iadd_sz2 to remove phase 1 work
*/
void spl_lez(d, d2, s, f1, sd, n, m, scratch) 
vec_p d, d2, s, f1, sd, scratch;
int n, m;
{
  void xspl(), iadd_sz2();
  vec_p lengths = (vec_p) lengths_from_sd(sd, m);
  vec_p starts = (vec_p) offsets_from_sd(sd, m);
  vec_p f_length = d2;
  vec_p f = scratch;
  vec_p packed_f = _add_fov(f, siz_fob(n));
  vec_p not_f = _add_fov(packed_f, siz_fopb(n));
  vec_p t_length = _add_fov(not_f, siz_fob(n));
  vec_p ntos = _add_fov(t_length, siz_foz(m));

  if (n == 0) return;
  if (m == 1) {
    int count;
    count = spl_luz(d, s, f1, n, scratch);
    ((int *)d2)[0] = count;
    return;
  }
  not_wub(not_f, f1, n, ntos);
  cpy_wub(f, f1, n, ntos);
  pboo_wuz(packed_f, f, n, ntos);
  add_rez(t_length, f, sd, n, m, ntos);
  sub_wuz(f_length, lengths, t_length, m, ntos);

  /* f[starts[j]] += f_length[j] */
  xspl(f, f_length, starts, m, ntos);  

  /* not_f[starts[j+1]] += t_length[j] */
  xspl(not_f, t_length, (vec_p) (((int *) starts) + 1), m-1, ntos);

  (((int *) f)[0])--;  /* inclusive scan with start value of -1 */
  (((int *) not_f)[0])--;  /* inclusive scan with start value of -1 */

  /* iadd_sz2 does this:
   * index = select(f, inclusive_scan(f), inclusive_scan(not_f)));
   * d = permute(s, index);
   */ 
  iadd_sz2(d, f, not_f, packed_f, s, 0, n, ntos);
}


#define make_sfl(_name, _type)		     \
	void _name(d, s1, s2, len, scratch)  \
	vec_p d, s1, s2, scratch;	     \
	int len;			     \
{	                                     \
  void ELTS();                               \
  int *S1=(int*)s1;                          \
  int *S2=(int*)s2;                          \
  int *D1=(int*)d;                           \
  int *D2=D1+1;                              \
  ELTS(D1, S1, 2, 1, len);                   \
  ELTS(D2, S2, 2, 1, len);                   \
}					     \
	make_no_scratch(_name)		     \
	make_inplace(_name,INPLACE_NONE)    

make_sfl(sfl_luz, int)
make_sfl(sfl_lub, cvl_bool)
make_sfl(sfl_lud, double)
#endif
