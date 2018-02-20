/*
* Copyright (c) 1993 Carnegie Mellon University 
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
*  Guy Blelloch	and Marco Zagha		guy.blelloch@cs.cmu.edu
*  School of Computer Science           marco.zagha@cs.cmu.edu
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3891
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/
/* This file generated automatically by assembler on 12/6/1993 11:51 */

#include "cray.h"




int add_wuz_scratch(len) int len; {return 0;}

/* ADD_WUZ: An unrolled elwise-op (V-+) */
void add_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void ADDWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   ADDWUZ(D, S1, S2, vlen, stride, full);
  }
}




int sub_wuz_scratch(len) int len; {return 0;}

/* SUB_WUZ: An unrolled elwise-op (V--) */
void sub_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void SUBWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   SUBWUZ(D, S1, S2, vlen, stride, full);
  }
}




int mul_wuz_scratch(len) int len; {return 0;}

/* MUL_WUZ: An elwise-op (V-*) */
void mul_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void MULWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   MULWUZ(D, S1, S2, vlen, stride, full);
  }
}




int div_wuz_scratch(len) int len; {return 0;}

/* DIV_WUZ: An elwise-op (V-/) */
void div_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void DIVWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   DIVWUZ(D, S1, S2, vlen, stride, full);
  }
}




int mod_wuz_scratch(len) int len; {return 0;}

/* MOD_WUZ: An elwise-op (V-MOD) */
void mod_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void MODWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   MODWUZ(D, S1, S2, vlen, stride, full);
  }
}




int max_wuz_scratch(len) int len; {return 0;}

/* MAX_WUZ: An unrolled elwise-op (V-MAX) */
void max_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void MAXWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   MAXWUZ(D, S1, S2, vlen, stride, full);
  }
}




int min_wuz_scratch(len) int len; {return 0;}

/* MIN_WUZ: An unrolled elwise-op (V-MIN) */
void min_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void MINWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   MINWUZ(D, S1, S2, vlen, stride, full);
  }
}




int xcpy_wuz_scratch(len) int len; {return 0;}

/* XCPY_WUZ: An unrolled elwise-op (V-SET) */
void xcpy_wuz(D, S, LEN, SCRATCH)
int *D;
int *S;
int LEN; 
int *SCRATCH;
{
  void XCPYWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XCPYWUZ(D, S, vlen, stride, full);
  }
}




int add_wud_scratch(len) int len; {return 0;}

/* ADD_WUD: An unrolled elwise-op (V-+F) */
void add_wud(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void ADDWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   ADDWUD(D, S1, S2, vlen, stride, full);
  }
}




int sub_wud_scratch(len) int len; {return 0;}

/* SUB_WUD: An unrolled elwise-op (V--F) */
void sub_wud(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void SUBWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   SUBWUD(D, S1, S2, vlen, stride, full);
  }
}




int mul_wud_scratch(len) int len; {return 0;}

/* MUL_WUD: An unrolled elwise-op (V-*F) */
void mul_wud(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void MULWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   MULWUD(D, S1, S2, vlen, stride, full);
  }
}




int div_wud_scratch(len) int len; {return 0;}

/* DIV_WUD: An elwise-op (V-/F) */
void div_wud(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void DIVWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   DIVWUD(D, S1, S2, vlen, stride, full);
  }
}




int max_wud_scratch(len) int len; {return 0;}

/* MAX_WUD: An unrolled elwise-op (V-MAXF) */
void max_wud(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void MAXWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   MAXWUD(D, S1, S2, vlen, stride, full);
  }
}




int min_wud_scratch(len) int len; {return 0;}

/* MIN_WUD: An unrolled elwise-op (V-MINF) */
void min_wud(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void MINWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   MINWUD(D, S1, S2, vlen, stride, full);
  }
}




int and_wuz_scratch(len) int len; {return 0;}

/* AND_WUZ: An unrolled elwise-op (V-AND) */
void and_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void ANDWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   ANDWUZ(D, S1, S2, vlen, stride, full);
  }
}




int ior_wuz_scratch(len) int len; {return 0;}

/* IOR_WUZ: An unrolled elwise-op (V-OR) */
void ior_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void IORWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   IORWUZ(D, S1, S2, vlen, stride, full);
  }
}




int not_wuz_scratch(len) int len; {return 0;}

/* NOT_WUZ: An unrolled elwise-op (V-NOT) */
void not_wuz(D, SOURCE, LEN, SCRATCH)
int *D;
int *SOURCE;
int LEN; 
int *SCRATCH;
{
  void NOTWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   NOTWUZ(D, SOURCE, vlen, stride, full);
  }
}




int not_wub_scratch(len) int len; {return 0;}

/* NOT_WUB: An unrolled elwise-op (V-NOT-01) */
void not_wub(D, SOURCE, LEN, SCRATCH)
int *D;
int *SOURCE;
int LEN; 
int *SCRATCH;
{
  void NOTWUB();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   NOTWUB(D, SOURCE, vlen, stride, full);
  }
}




int xor_wuz_scratch(len) int len; {return 0;}

/* XOR_WUZ: An unrolled elwise-op (V-XOR) */
void xor_wuz(D, S1, S2, LEN, SCRATCH)
int *D;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void XORWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XORWUZ(D, S1, S2, vlen, stride, full);
  }
}




int pboo_wuz_scratch(len) int len; {return 0;}

/* PBOO_WUZ: A packed boolean elwise-op (PACKED-NONZERO) */
void pboo_wuz(D1, S1, LEN, SCRATCH)
int *D1;
int *S1;
int LEN; 
int *SCRATCH;
{
  void PBOOWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   PBOOWUZ(D1, S1, vlen, stride, full);
  }
}




int grt_wuz_scratch(len) int len; {return 0;}

/* GRT_WUZ: An upacked boolean elwise-op (UNPACKED-GT) */
void grt_wuz(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void GRTWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   GRTWUZ(VM, S1, S2, vlen, stride, full);
  }
}




int geq_wuz_scratch(len) int len; {return 0;}

/* GEQ_WUZ: An upacked boolean elwise-op (UNPACKED-GEQ) */
void geq_wuz(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void GEQWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   GEQWUZ(VM, S1, S2, vlen, stride, full);
  }
}




int les_wuz_scratch(len) int len; {return 0;}

/* LES_WUZ: An upacked boolean elwise-op (UNPACKED-LT) */
void les_wuz(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void LESWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   LESWUZ(VM, S1, S2, vlen, stride, full);
  }
}




int leq_wuz_scratch(len) int len; {return 0;}

/* LEQ_WUZ: An upacked boolean elwise-op (UNPACKED-LEQ) */
void leq_wuz(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void LEQWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   LEQWUZ(VM, S1, S2, vlen, stride, full);
  }
}




int eql_wuz_scratch(len) int len; {return 0;}

/* EQL_WUZ: An upacked boolean elwise-op (UNPACKED-EQUAL) */
void eql_wuz(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void EQLWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   EQLWUZ(VM, S1, S2, vlen, stride, full);
  }
}




int neq_wuz_scratch(len) int len; {return 0;}

/* NEQ_WUZ: An upacked boolean elwise-op (UNPACKED-NOT-EQUAL) */
void neq_wuz(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void NEQWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   NEQWUZ(VM, S1, S2, vlen, stride, full);
  }
}




int boo_wuz_scratch(len) int len; {return 0;}

/* BOO_WUZ: An upacked boolean elwise-op (UNPACKED-NONZERO) */
void boo_wuz(VM, S1, LEN, SCRATCH)
int *VM;
int *S1;
int LEN; 
int *SCRATCH;
{
  void BOOWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   BOOWUZ(VM, S1, vlen, stride, full);
  }
}




int grt_wud_scratch(len) int len; {return 0;}

/* GRT_WUD: An upacked boolean elwise-op (UNPACKED-GTF) */
void grt_wud(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void GRTWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   GRTWUD(VM, S1, S2, vlen, stride, full);
  }
}




int geq_wud_scratch(len) int len; {return 0;}

/* GEQ_WUD: An upacked boolean elwise-op (UNPACKED-GEQF) */
void geq_wud(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void GEQWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   GEQWUD(VM, S1, S2, vlen, stride, full);
  }
}




int les_wud_scratch(len) int len; {return 0;}

/* LES_WUD: An upacked boolean elwise-op (UNPACKED-LTF) */
void les_wud(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void LESWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   LESWUD(VM, S1, S2, vlen, stride, full);
  }
}




int leq_wud_scratch(len) int len; {return 0;}

/* LEQ_WUD: An upacked boolean elwise-op (UNPACKED-LEQF) */
void leq_wud(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void LEQWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   LEQWUD(VM, S1, S2, vlen, stride, full);
  }
}




int eql_wud_scratch(len) int len; {return 0;}

/* EQL_WUD: An upacked boolean elwise-op (UNPACKED-EQUALF) */
void eql_wud(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void EQLWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   EQLWUD(VM, S1, S2, vlen, stride, full);
  }
}




int neq_wud_scratch(len) int len; {return 0;}

/* NEQ_WUD: An upacked boolean elwise-op (UNPACKED-NOT-EQUALF) */
void neq_wud(VM, S1, S2, LEN, SCRATCH)
int *VM;
int *S1;
int *S2;
int LEN; 
int *SCRATCH;
{
  void NEQWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   NEQWUD(VM, S1, S2, vlen, stride, full);
  }
}




int int_wupb_scratch(len) int len; {return 0;}

/* Converts compressed booleans (vlen to a word) to 1/0 flags (one per word) */
void int_wupb(D, S, LEN, SCRATCH)
int *D;
int *S;
int LEN; 
int *SCRATCH;
{
  void INTWUPB();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   INTWUPB(D, S, vlen, stride, full);
  }
}




int sel_wuz_scratch(len) int len; {return 0;}

/* Selects one of sources using uncompressed booleans */
void sel_wuz(DEST, FLAGS, S1, S2, LEN, SCRATCH)
int *DEST; 
int *FLAGS; 
int *S1; 
int *S2; 
int LEN; 
int *SCRATCH;
{
  void SELWUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   SELWUZ(DEST, FLAGS, S1, S2, vlen, stride, full);
  }
}




int sel_wub_scratch(len) int len; {return 0;}

/* Selects one of sources using uncompressed booleans */
void sel_wub(DEST, FLAGS, S1, S2, LEN, SCRATCH)
int *DEST; 
int *FLAGS; 
int *S1; 
int *S2; 
int LEN; 
int *SCRATCH;
{
  void SELWUB();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   SELWUB(DEST, FLAGS, S1, S2, vlen, stride, full);
  }
}




int sel_wud_scratch(len) int len; {return 0;}

/* Selects one of sources using uncompressed booleans */
void sel_wud(DEST, FLAGS, S1, S2, LEN, SCRATCH)
double *DEST; 
double *FLAGS; 
double *S1; 
double *S2; 
int LEN; 
int *SCRATCH;
{
  void SELWUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   SELWUD(DEST, FLAGS, S1, S2, vlen, stride, full);
  }
}




int bck_puz_dist_sel_scratch(len) int len; {return 0;}

/* Selects gathered vector element or scalar using compressed booleans */
void bck_puz_dist_sel(DEST, SRC, INDEX, FLAGS, SCAL2, LEN, SCRATCH)
int DEST; 
int *SRC;
int *INDEX;
int *FLAGS;
int SCAL2; 
int LEN; 
int *SCRATCH;
{
  void BCKPUZD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   BCKPUZD(DEST, SRC, INDEX, FLAGS, SCAL2, vlen, stride, full);
  }
}




int dis_vuz_scratch(len) int len; {return 0;}

/* Distribute */
void dis_vuz(D, V, LEN, SCRATCH)
int *D; 
int V; 
int LEN; 
int *SCRATCH;
{
  void DISVUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   DISVUZ(D, V, vlen, stride, full);
  }
}




int dis_vub_scratch(len) int len; {return 0;}

/* Distribute */
void dis_vub(D, V, LEN, SCRATCH)
int *D; 
int V; 
int LEN; 
int *SCRATCH;
{
  void DISVUB();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   DISVUB(D, V, vlen, stride, full);
  }
}




int dis_vud_scratch(len) int len; {return 0;}

/* Distribute */
void dis_vud(D, V, LEN, SCRATCH)
double *D; 
double V; 
int LEN; 
int *SCRATCH;
{
  void DISVUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   DISVUD(D, V, vlen, stride, full);
  }
}
