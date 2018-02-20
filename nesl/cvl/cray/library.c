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


make_inplace(ind_luz,INPLACE_NONE) 

/* Segmented index
        scatter strides                          (DEST[STARTS] = STRIDE)
        copy scan on DEST                        (DEST = copyscan(DEST)
        move starting values to beginning of seg (DEST[STARTS] = VAL)
        segmented INclusive scan                 (DEST = iscan(DEST))
*/

make_no_seg_scratch(ind_lez) 
make_inplace(ind_lez,INPLACE_NONE) 


void ind_lez (d, init, strid, sd, n, m, scratch) 
vec_p d, init, strid, sd, scratch; 
int n, m; 
{ 
  void XCPYSCA(), IADDSEZ();
/* printf("in %d %d\n", n, m); */
  if (n == 0) return; 
  if (m == 1) { 
    ind_luz (d, ((int *)init)[0], ((int *)strid)[0], n, scratch);
  } else { 
    int vlen, stride, full; 
    vec_p flags, offsets, last_start, nonzerop;
    _compute_shape_factors(n, &vlen, &stride, &full); 
    offsets = offsets_from_sd(sd, m); 
    last_start  = last_start_from_sd(sd, m); 
    flags = bools_from_sd(sd, m); 
    nonzerop = nonzerop_from_sd(sd, m); 
    pfpm_puz(d, strid, offsets, nonzerop, m, n, 0);
    XCPYSCA(d, d, flags, last_start, vlen, stride, full); 
    pfpm_puz(d, init, offsets, nonzerop, m, n, 0);
    IADDSEZ(d, d, flags, 0, vlen, stride, full);
  }
/* printf("out %d %d\n", n, m); */
}


#if 0
make_no_seg_scratch(indlez)
make_inplace(indlez, INPLACE_NONE)

void ind_lez(d, sd, n, m, scratch)
vec_p d, sd, scratch;
int n, m; 
{
  void XINDLEZ();
  int vlen, stride, full;
  vec_p flags;

  if (n == 0) return; 
  _compute_shape_factors(n, &vlen, &stride, &full);
  flags = bools_from_sd(sd, m);
  XINDLEZ(d, flags, vlen, stride, full);
}
#endif




int pk1_luv(f, len, scratch)
vec_p f, scratch;
int len;
{
  return add_ruz(f, len, scratch);
}
make_no_scratch(pk1_luv)
make_inplace(pk1_luv,INPLACE_NONE)


#define makepack(_name, _asmname) \
  make_no_scratch(_name) \
  make_inplace(_name,INPLACE_1|INPLACE_2)  \
  void _name (d, s, flags, n, dlen, scratch) \
  vec_p d, s, flags, scratch; \
  int n, dlen; \
  { \
    if (n != 0) _asmname(d, s, flags, n); \
  } \

makepack(pk2_luz, XPCKLUZ)
makepack(pk2_lub, XPCKLUZ)
makepack(pk2_lud, XPCKLUZ)





pk1_lev_scratch(n, m) int n, m; {return (n + add_rez_scratch(n, m));}
make_inplace(pk1_lev, INPLACE_NONE)

void pk1_lev(d, f, sd, n, m, scratch)
vec_p d, f, sd, scratch;
int n, m;
{
  if (n > 0) {
    if (m == 1) {
      ((int *) d)[0] = add_ruz(f, n, scratch);
    } else {
      add_rez(d, f, sd, n, m, scratch);
    }
  } else {
    if (m > 0)
      dis_vuz(d, 0, m, scratch); /* distribute zero to each (empty) segment */
  }
}



int pk2_lez_scratch(n, m, n2, m2) int n, m, n2, m2; {return pk2_luz_scratch(n);}
make_inplace(pk2_lez, INPLACE_1|INPLACE_2) 

void pk2_lez(d, s, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch)
vec_p d, s, f, sd_s, sd_d, scratch;
int n_s, m_s, n_d, m_d;
{
  if (n_s > 0) pk2_luz(d, s, f, n_s, 0, scratch);
}



int pk2_leb_scratch(n, m, n2, m2) int n, m, n2, m2; {return pk2_luz_scratch(n);}
make_inplace(pk2_leb, INPLACE_1|INPLACE_2) 

void pk2_leb(d, s, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch)
vec_p d, s, f, sd_s, sd_d, scratch;
int n_s, m_s, n_d, m_d;
{
  if (n_s > 0) pk2_luz(d, s, f, n_s, 0, scratch);
}


int pk2_led_scratch(n, m, n2, m2) int n, m, n2, m2; {return pk2_luz_scratch(n);}
make_inplace(pk2_led, INPLACE_1|INPLACE_2) 

void pk2_led(d, s, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch)
vec_p d, s, f, sd_s, sd_d, scratch;
int n_s, m_s, n_d, m_d;
{
  if (n_s > 0) pk2_luz(d, s, f, n_s, 0, scratch);
}
