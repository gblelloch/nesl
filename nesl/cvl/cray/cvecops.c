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
/* This file generated automatically by assembler on 12/6/1993 11:52 */

#include "cray.h"




int xpck_luz_scratch(len) int len; {return 0;}

/* Packs s to d where flags are true.  Caller must make sure d is big enough */
int xpck_luz(D, S, FLAGS, N, SCRATCH)
int *D;
int *S;
int *FLAGS;
int N; 
int *SCRATCH;
{
  int XPCKLUZ();
   return XPCKLUZ(D, S, FLAGS, N);
}




int smp_puz_scratch(len) int len; {return 0;}

/* Elementwise integer permute */
void smp_puz(D, SRC, INDEX, LEN, SCRATCH)
int *D;
int *SRC;
int *INDEX;
int LEN; 
int *SCRATCH;
{
  void SMPPUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   SMPPUZ(D, SRC, INDEX, vlen, stride, full);
  }
}




int xbck_puz_scratch(len) int len; {return 0;}

/* Elementwise integer back permute */
void xbck_puz(D, SRC, INDEX, LEN, SCRATCH)
int *D;
int *SRC;
int *INDEX;
int LEN; 
int *SCRATCH;
{
  void XBCKPUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XBCKPUZ(D, SRC, INDEX, vlen, stride, full);
  }
}




int xscatterconst_scratch(len) int len; {return 0;}

/* Internal function to make segdes.  Scatters a constant */
void xscatterconst(D, CONST, INDEX, LEN, SCRATCH)
int *D;
int CONST; 
int *INDEX;
int LEN; 
int *SCRATCH;
{
  void XSCATTE();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XSCATTE(D, CONST, INDEX, vlen, stride, full);
  }
}




int xrepvez_scratch(len) int len; {return 0;}

/* Internal function for segmented replace.  Adds index to offset and scatters */
void xrepvez(D, SRC, INDEX, OFFSETS, LEN, SCRATCH)
int *D;
int *SRC;
int *INDEX;
int *OFFSETS;
int LEN; 
int *SCRATCH;
{
  void XREPVEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XREPVEZ(D, SRC, INDEX, OFFSETS, vlen, stride, full);
  }
}




int xextvez_scratch(len) int len; {return 0;}

/* Internal function for segmented extract.  Adds index to offset and gathers */
void xextvez(D, SRC, INDEX, OFFSETS, LEN, SCRATCH)
int *D;
int *SRC;
int *INDEX;
int *OFFSETS;
int LEN; 
int *SCRATCH;
{
  void XEXTVEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XEXTVEZ(D, SRC, INDEX, OFFSETS, vlen, stride, full);
  }
}




int xextlast_scratch(len) int len; {return 0;}

/* Get last elt of seg for seg reduce.  Adds index-1 to offset and gathers */
void xextlast(D, SRC, INDEX, OFFSETS, LEN, SCRATCH)
int *D;
int *SRC;
int *INDEX;
int *OFFSETS;
int LEN; 
int *SCRATCH;
{
  void XEXTLAS();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XEXTLAS(D, SRC, INDEX, OFFSETS, vlen, stride, full);
  }
}




int xpbselperm_scratch(len) int len; {return 0;}

/* packed bool select permute: where (flags[j]) d[index[j]] = src[j] */
void xpbselperm(D, SRC, INDEX, FLAGS, SSCRATCH, LEN, SCRATCH)
int *D;
int *SRC;
int *INDEX;
int *FLAGS;
int *SSCRATCH; 
int LEN; 
int *SCRATCH;
{
  void XPBSELP();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XPBSELP(D, SRC, INDEX, FLAGS, SSCRATCH, vlen, stride, full);
  }
}




int xselperm_scratch(len) int len; {return 0;}

/* unpacked bool select permute: where (flags[j]) d[index[j]] = src[j] */
void xselperm(D, SRC, INDEX, FLAGS, SSCRATCH, LEN, SCRATCH)
int *D;
int *SRC;
int *INDEX;
int *FLAGS;
int *SSCRATCH; 
int LEN; 
int *SCRATCH;
{
  void XSELPER();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XSELPER(D, SRC, INDEX, FLAGS, SSCRATCH, vlen, stride, full);
  }
}




int xbselperm_scratch(len) int len; {return 0;}

/* 1st phase of unpacked bool select bpermute: where (flags[j]) d[j] = src[index[j]] */
void xbselperm(D, SRC, INDEX, FLAGS, SSCRATCH, LEN, SCRATCH)
int *D;
int *SRC;
int *INDEX;
int *FLAGS;
int *SSCRATCH; 
int LEN; 
int *SCRATCH;
{
  void XBSELPE();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XBSELPE(D, SRC, INDEX, FLAGS, SSCRATCH, vlen, stride, full);
  }
}




int xsegperm_scratch(len) int len; {return 0;}

/* XSEGPERM: A segmented permute function */
void xsegperm(D, VALUES, INDEX, FLAGS, LASTIND, LEN, SCRATCH)
int *D;
int *VALUES;
int *INDEX;
int *FLAGS;
int *LASTIND; 
int LEN; 
int *SCRATCH;
{
  void XSEGPER();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XSEGPER(D, VALUES, INDEX, FLAGS, LASTIND, vlen, stride, full);
  }
}




int xsegdpe_scratch(len) int len; {return 0;}

/* XSEGDPE: A segmented permute function */
void xsegdpe(D, VALUES, INDEX, FLAGS, LASTIND, STARTS, LEN, SCRATCH)
int *D;
int *VALUES;
int *INDEX;
int *FLAGS; 
int *LASTIND; 
int *STARTS;
int LEN; 
int *SCRATCH;
{
  void XSEGDPE();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XSEGDPE(D, VALUES, INDEX, FLAGS, LASTIND, STARTS, vlen, stride, full);
  }
}




int xsegfpm_scratch(len) int len; {return 0;}

/* XSEGFPM: A segmented permute function */
void xsegfpm(INDEX, FLAGS, LASTIND, STARTS, LEN, SCRATCH)
int *INDEX;
int *FLAGS;
int *LASTIND; 
int *STARTS;
int LEN; 
int *SCRATCH;
{
  void XSEGFPM();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XSEGFPM(INDEX, FLAGS, LASTIND, STARTS, vlen, stride, full);
  }
}




int xsegbperm_scratch(len) int len; {return 0;}

/* XSEGBPERM: A segmented permute function */
void xsegbperm(D, VALUES, INDEX, FLAGS, LASTIND, LEN, SCRATCH)
int *D;
int *VALUES;
int *INDEX;
int *FLAGS;
int *LASTIND; 
int LEN; 
int *SCRATCH;
{
  void XSEGBPE();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XSEGBPE(D, VALUES, INDEX, FLAGS, LASTIND, vlen, stride, full);
  }
}




int iadd_sz2_scratch(len) int len; {return 0;}

/* IADD_SZ2: 2 unsegmented inplace inclusive scans (V-+) */
void iadd_sz2(D, VALUES, VALUES2, FLAGS, SOURCE, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *VALUES2;
int *FLAGS; 
int *SOURCE; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void IADDSZ2();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   IADDSZ2(D, VALUES, VALUES2, FLAGS, SOURCE, ROWIDENT, vlen, stride, full);
  }
}
