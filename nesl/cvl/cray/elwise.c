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
#include <math.h>
#include "defins.h"
#include <cvl.h>
#include "cray.h"

make_inplace(add_wuz, INPLACE_1|INPLACE_2)
make_inplace(sub_wuz, INPLACE_1|INPLACE_2)
make_inplace(mul_wuz, INPLACE_1|INPLACE_2)
make_inplace(div_wuz, INPLACE_1|INPLACE_2)
make_inplace(mod_wuz, INPLACE_1|INPLACE_2)
make_inplace(max_wuz, INPLACE_1|INPLACE_2)
make_inplace(min_wuz, INPLACE_1|INPLACE_2)
make_inplace(add_wud, INPLACE_1|INPLACE_2)
make_inplace(sub_wud, INPLACE_1|INPLACE_2)
make_inplace(mul_wud, INPLACE_1|INPLACE_2)
make_inplace(div_wud, INPLACE_1|INPLACE_2)
make_inplace(max_wud, INPLACE_1|INPLACE_2)
make_inplace(min_wud, INPLACE_1|INPLACE_2)
make_inplace(and_wuz, INPLACE_1|INPLACE_2)
make_inplace(ior_wuz, INPLACE_1|INPLACE_2)
make_inplace(xor_wuz, INPLACE_1|INPLACE_2)
make_inplace(grt_wuz, INPLACE_1|INPLACE_2)
make_inplace(geq_wuz, INPLACE_1|INPLACE_2)
make_inplace(les_wuz, INPLACE_1|INPLACE_2)
make_inplace(leq_wuz, INPLACE_1|INPLACE_2)
make_inplace(eql_wuz, INPLACE_1|INPLACE_2)
make_inplace(neq_wuz, INPLACE_1|INPLACE_2)
make_inplace(grt_wud, INPLACE_1|INPLACE_2)
make_inplace(geq_wud, INPLACE_1|INPLACE_2)
make_inplace(les_wud, INPLACE_1|INPLACE_2)
make_inplace(leq_wud, INPLACE_1|INPLACE_2)
make_inplace(eql_wud, INPLACE_1|INPLACE_2)
make_inplace(neq_wud, INPLACE_1|INPLACE_2)

make_inplace(not_wuz, INPLACE_1)
make_inplace(not_wub, INPLACE_1)
make_inplace(boo_wuz, INPLACE_1)
make_inplace(sel_wuz, INPLACE_1|INPLACE_2|INPLACE_3)
make_inplace(sel_wub, INPLACE_1|INPLACE_2|INPLACE_3)
make_inplace(sel_wud, INPLACE_1|INPLACE_2|INPLACE_3)
make_inplace(dis_vuz, INPLACE_NONE)
make_inplace(dis_vub, INPLACE_NONE)
make_inplace(dis_vud, INPLACE_NONE)



              /* VERY IMPORTANT:    
                    compile this file with -h ivdep to get the loops
                    below to vectorize
               */
/* --------------Function definition macros --------------------*/
#define onefun(_name, _funct, _srctype, _desttype)          \
    void _name (d, s, len, scratch)                         \
    vec_p d, s, scratch;                                    \
    int len;                                                \
    {                                                       \
        int jjj;                                            \
        for (jjj = 0; jjj < len; jjj++) {                   \
          ((_desttype *)(d))[jjj] =                         \
            _funct(((_srctype *)s)[jjj]);                   \
        }                                                   \
    }                                                       \
    make_no_scratch(_name)                                  \
    make_inplace(_name,INPLACE_1)

#define twofun(_name, _funct, _srctype, _desttype)          \
    void _name (d, s1, s2, len, scratch)                    \
    vec_p d, s1, s2, scratch;                               \
    int len;                                                \
    {                                                       \
        int jj;                                             \
        for (jj = 0; jj < len; jj++) {                      \
          ((_desttype *)(d))[jj] =                          \
            _funct(((_srctype *)s1)[jj],                    \
                   ((_srctype *)s2)[jj]);                   \
        }                                                   \
    }                                                       \
    make_no_scratch(_name)                                  \
    make_inplace(_name,INPLACE_1|INPLACE_2)


/* shifts only on integers */
twofun(lsh_wuz, lshift, int, int)
twofun(rsh_wuz, rshift, int, int)
onefun(rnd_wuz, cvlrand, int, int)

/* conversion routines */
onefun(int_wud, d_to_z, double, int)
onefun(dbl_wuz, z_to_d, int, double)

/* double routines */
onefun(flr_wud, cvl_floor, double, int)
onefun(cei_wud, cvl_ceil, double, int)
onefun(exp_wud, exp, double, double)
onefun(log_wud, log, double, double)
onefun(sqt_wud, sqrt, double, double)

onefun(trn_wud, d_to_z, double, int)
onefun(rou_wud, cvl_round, double, int)

onefun(sin_wud, sin, double, double)
onefun(cos_wud, cos, double, double)
onefun(tan_wud, tan, double, double)
onefun(asn_wud, asin, double, double)
onefun(acs_wud, acos, double, double)
onefun(atn_wud, atan, double, double)
onefun(snh_wud, sinh, double, double)
onefun(csh_wud, cosh, double, double)
onefun(tnh_wud, tanh, double, double)

/* Boolean operations */

make_no_scratch(and_wub)
make_inplace(and_wub, INPLACE_1|INPLACE_2)

void and_wub(D, S1, S2, N, SCRATCH)
vec_p D;
vec_p S1;
vec_p S2;
int N;
vec_p SCRATCH;
{
  void and_wuz();
  if (N > 0) and_wuz(D, S1, S2, N, SCRATCH);
}


make_no_scratch(ior_wub)
make_inplace(ior_wub, INPLACE_1|INPLACE_2)

void ior_wub(D, S1, S2, N, SCRATCH)
vec_p D;
vec_p S1;
vec_p S2;
int N;
vec_p SCRATCH;
{
  void ior_wuz();
  if (N > 0) ior_wuz(D, S1, S2, N, SCRATCH);
}


make_no_scratch(xor_wub)
make_inplace(xor_wub, INPLACE_1|INPLACE_2)

void xor_wub(D, S1, S2, N, SCRATCH)
vec_p D;
vec_p S1;
vec_p S2;
int N;
vec_p SCRATCH;
{
  void xor_wuz();
  if (N > 0) xor_wuz(D, S1, S2, N, SCRATCH);
}



int cpy_wuz_scratch(len) int len; {return 0;}
make_inplace(cpy_wuz, INPLACE_1)


/* CPY_WUZ: An unrolled elwise-op (V-SET) */
void cpy_wuz(D, S, LEN, SCRATCH)
vec_p D;
vec_p S;
int LEN; 
vec_p SCRATCH;
{
  void XCPYWUZ();
  int vlen, stride, full;

  if ((LEN > 0) && (D != S)) {
    _compute_shape_factors(LEN, &vlen, &stride, &full);
    XCPYWUZ(D, S, vlen, stride, full);
  }
}


int cpy_wud_scratch(len) int len; {return 0;}
make_inplace(cpy_wud, INPLACE_1)

/* CPY_WUD: An unrolled elwise-op (V-SET) */
void cpy_wud(D, S, LEN, SCRATCH)
vec_p D;
vec_p S;
int LEN; 
vec_p SCRATCH;
{
  void XCPYWUZ();
  int vlen, stride, full;

  if ((LEN > 0) && (D != S)) {
    _compute_shape_factors(LEN, &vlen, &stride, &full);
    XCPYWUZ(D, S, vlen, stride, full);
  }
}



make_no_scratch(cpy_wub)
make_inplace(cpy_wub, INPLACE_1)

void cpy_wub(D, S, N, SCRATCH)
vec_p D;
vec_p S;
int N;
vec_p SCRATCH;
{
  void cpy_wuz();
  if (N > 0) cpy_wuz(D, S, N, SCRATCH);
}




make_no_scratch(eql_wub)
make_inplace(eql_wub, INPLACE_1|INPLACE_2)

void eql_wub(D, S1, S2, N, SCRATCH)
vec_p D;
vec_p S1;
vec_p S2;
int N;
vec_p SCRATCH;
{
  void eql_wuz();
  if (N > 0) eql_wuz(D, S1, S2, N, SCRATCH);
}



make_no_scratch(neq_wub)
make_inplace(neq_wub, INPLACE_1|INPLACE_2)

void neq_wub(D, S1, S2, N, SCRATCH)
vec_p D;
vec_p S1;
vec_p S2;
int N;
vec_p SCRATCH;
{
  void neq_wuz();
  if (N > 0) neq_wuz(D, S1, S2, N, SCRATCH);
}


int int_wub_scratch(len) int len; {return 0;}
make_inplace(int_wub, INPLACE_1)

void int_wub(D, S, LEN, SCRATCH)
vec_p D;
vec_p S;
int LEN; 
vec_p SCRATCH;
{
  void cpy_wuz();
  if (LEN > 0) {
    cpy_wuz(D, S, LEN, SCRATCH);
  }
}

