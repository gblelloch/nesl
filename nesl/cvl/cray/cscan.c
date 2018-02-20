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
/* This file generated automatically by assembler on 12/6/1993 11:49 */

#include "cray.h"




int xmul_ruz_scratch(len) int len; {return 0;}

/* XMUL_RUZ: a reduce function (V-*) */
int xmul_ruz(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  int XMULRUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMULRUZ(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xadd_ruz_scratch(len) int len; {return 0;}

/* XADD_RUZ: a reduce function (V-+) */
int xadd_ruz(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  int XADDRUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XADDRUZ(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xmax_ruz_scratch(len) int len; {return 0;}

/* XMAX_RUZ: a reduce function (V-MAX) */
int xmax_ruz(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  int XMAXRUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMAXRUZ(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xmin_ruz_scratch(len) int len; {return 0;}

/* XMIN_RUZ: a reduce function (V-MIN) */
int xmin_ruz(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  int XMINRUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMINRUZ(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xand_ruz_scratch(len) int len; {return 0;}

/* XAND_RUZ: a reduce function (V-AND) */
int xand_ruz(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  int XANDRUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XANDRUZ(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xior_ruz_scratch(len) int len; {return 0;}

/* XIOR_RUZ: a reduce function (V-OR) */
int xior_ruz(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  int XIORRUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XIORRUZ(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xxor_ruz_scratch(len) int len; {return 0;}

/* XXOR_RUZ: a reduce function (V-XOR) */
int xxor_ruz(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  int XXORRUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XXORRUZ(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xmul_rud_scratch(len) int len; {return 0;}

/* XMUL_RUD: a reduce function (V-*F) */
double xmul_rud(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  double XMULRUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMULRUD(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xadd_rud_scratch(len) int len; {return 0;}

/* XADD_RUD: a reduce function (V-+F) */
double xadd_rud(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  double XADDRUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XADDRUD(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xmax_rud_scratch(len) int len; {return 0;}

/* XMAX_RUD: a reduce function (V-MAXF) */
double xmax_rud(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  double XMAXRUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMAXRUD(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xmin_rud_scratch(len) int len; {return 0;}

/* XMIN_RUD: a reduce function (V-MINF) */
double xmin_rud(VALUES, IDENTITY, LEN, SCRATCH)
int *VALUES;
int *IDENTITY; 
int LEN; 
int *SCRATCH;
{
  double XMINRUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMINRUD(VALUES, IDENTITY, vlen, stride, full);
  }
}




int xmul_suz_scratch(len) int len; {return 0;}

/* XMUL_SUZ: An unsegmented scan function (V-*) */
int xmul_suz(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int IDENTITY; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  int XMULSUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMULSUZ(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xadd_suz_scratch(len) int len; {return 0;}

/* XADD_SUZ: An unsegmented scan function (V-+) */
int xadd_suz(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int IDENTITY; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  int XADDSUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XADDSUZ(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xmax_suz_scratch(len) int len; {return 0;}

/* XMAX_SUZ: An unsegmented scan function (V-MAX) */
int xmax_suz(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int IDENTITY; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  int XMAXSUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMAXSUZ(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xmin_suz_scratch(len) int len; {return 0;}

/* XMIN_SUZ: An unsegmented scan function (V-MIN) */
int xmin_suz(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int IDENTITY; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  int XMINSUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMINSUZ(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xand_suz_scratch(len) int len; {return 0;}

/* XAND_SUZ: An unsegmented scan function (V-AND) */
int xand_suz(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int IDENTITY; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  int XANDSUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XANDSUZ(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xior_suz_scratch(len) int len; {return 0;}

/* XIOR_SUZ: An unsegmented scan function (V-OR) */
int xior_suz(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int IDENTITY; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  int XIORSUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XIORSUZ(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xxor_suz_scratch(len) int len; {return 0;}

/* XXOR_SUZ: An unsegmented scan function (V-XOR) */
int xxor_suz(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int IDENTITY; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  int XXORSUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XXORSUZ(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xmul_sud_scratch(len) int len; {return 0;}

/* XMUL_SUD: An unsegmented scan function (V-*F) */
double xmul_sud(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
double IDENTITY; 
double ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  double XMULSUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMULSUD(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xadd_sud_scratch(len) int len; {return 0;}

/* XADD_SUD: An unsegmented scan function (V-+F) */
double xadd_sud(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
double IDENTITY; 
double ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  double XADDSUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XADDSUD(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xmax_sud_scratch(len) int len; {return 0;}

/* XMAX_SUD: An unsegmented scan function (V-MAXF) */
double xmax_sud(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
double IDENTITY; 
double ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  double XMAXSUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMAXSUD(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xmin_sud_scratch(len) int len; {return 0;}

/* XMIN_SUD: An unsegmented scan function (V-MINF) */
double xmin_sud(D, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
double IDENTITY; 
double ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  double XMINSUD();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return XMINSUD(D, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int xmul_sez_scratch(len) int len; {return 0;}

/* XMUL_SEZ: A segmented scan function (V-*) */
void xmul_sez(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMULSEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMULSEZ(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xadd_sez_scratch(len) int len; {return 0;}

/* XADD_SEZ: A segmented scan function (V-+) */
void xadd_sez(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XADDSEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XADDSEZ(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xmax_sez_scratch(len) int len; {return 0;}

/* XMAX_SEZ: A segmented scan function (V-MAX) */
void xmax_sez(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMAXSEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMAXSEZ(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xmin_sez_scratch(len) int len; {return 0;}

/* XMIN_SEZ: A segmented scan function (V-MIN) */
void xmin_sez(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMINSEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMINSEZ(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xand_sez_scratch(len) int len; {return 0;}

/* XAND_SEZ: A segmented scan function (V-AND) */
void xand_sez(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XANDSEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XANDSEZ(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xior_sez_scratch(len) int len; {return 0;}

/* XIOR_SEZ: A segmented scan function (V-OR) */
void xior_sez(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XIORSEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XIORSEZ(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xxor_sez_scratch(len) int len; {return 0;}

/* XXOR_SEZ: A segmented scan function (V-XOR) */
void xxor_sez(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XXORSEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XXORSEZ(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xmul_sed_scratch(len) int len; {return 0;}

/* XMUL_SED: A segmented scan function (V-*F) */
void xmul_sed(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
double ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMULSED();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMULSED(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xadd_sed_scratch(len) int len; {return 0;}

/* XADD_SED: A segmented scan function (V-+F) */
void xadd_sed(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
double ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XADDSED();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XADDSED(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xmax_sed_scratch(len) int len; {return 0;}

/* XMAX_SED: A segmented scan function (V-MAXF) */
void xmax_sed(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
double ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMAXSED();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMAXSED(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xmin_sed_scratch(len) int len; {return 0;}

/* XMIN_SED: A segmented scan function (V-MINF) */
void xmin_sed(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
double ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMINSED();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMINSED(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int iadd_sez_scratch(len) int len; {return 0;}

/* IADD_SEZ: A segmented inclusive scan function (V-+) */
void iadd_sez(D, VALUES, FLAGS, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void IADDSEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   IADDSEZ(D, VALUES, FLAGS, ROWIDENT, vlen, stride, full);
  }
}




int xmul_rez_scratch(len) int len; {return 0;}

/* XMUL_REZ: A segmented reduce function (V-*-PASS) */
void xmul_rez(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMULREZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMULREZ(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xadd_rez_scratch(len) int len; {return 0;}

/* XADD_REZ: A segmented reduce function (V-+) */
void xadd_rez(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XADDREZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XADDREZ(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xand_rez_scratch(len) int len; {return 0;}

/* XAND_REZ: A segmented reduce function (V-AND) */
void xand_rez(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XANDREZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XANDREZ(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xior_rez_scratch(len) int len; {return 0;}

/* XIOR_REZ: A segmented reduce function (V-OR) */
void xior_rez(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XIORREZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XIORREZ(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xxor_rez_scratch(len) int len; {return 0;}

/* XXOR_REZ: A segmented reduce function (V-XOR) */
void xxor_rez(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XXORREZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XXORREZ(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xmax_rez_scratch(len) int len; {return 0;}

/* XMAX_REZ: A segmented reduce function (V-MAX) */
void xmax_rez(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMAXREZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMAXREZ(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xmin_rez_scratch(len) int len; {return 0;}

/* XMIN_REZ: A segmented reduce function (V-MIN) */
void xmin_rez(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMINREZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMINREZ(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xadd_red_scratch(len) int len; {return 0;}

/* XADD_RED: A segmented reduce function (V-+F) */
void xadd_red(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XADDRED();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XADDRED(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xmul_red_scratch(len) int len; {return 0;}

/* XMUL_RED: A segmented reduce function (V-*F) */
void xmul_red(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMULRED();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMULRED(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xmax_red_scratch(len) int len; {return 0;}

/* XMAX_RED: A segmented reduce function (V-MAXF) */
void xmax_red(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMAXRED();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMAXRED(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xmin_red_scratch(len) int len; {return 0;}

/* XMIN_RED: A segmented reduce function (V-MINF) */
void xmin_red(D, VALUES, FLAGS, LASTIND, ROWIDENT, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  void XMINRED();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XMINRED(D, VALUES, FLAGS, LASTIND, ROWIDENT, vlen, stride, full);
  }
}




int xcpyscan_scratch(len) int len; {return 0;}

/* XCPYSCAN: A segmented copy-scan function */
void xcpyscan(D, VALUES, FLAGS, LASTIND, LEN, SCRATCH)
int *D;
int *VALUES;
int *FLAGS;
int *LASTIND;
int LEN; 
int *SCRATCH;
{
  void XCPYSCA();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XCPYSCA(D, VALUES, FLAGS, LASTIND, vlen, stride, full);
  }
}




int addsuzcpywuz_scratch(len) int len; {return 0;}

/* An unsegmented add scan function folded with a copy */
int addsuzcpywuz(D, COPYD, VALUES, IDENTITY, ROWIDENT, LEN, SCRATCH)
int *D;
int *COPYD;
int *VALUES;
int IDENTITY; 
int ROWIDENT; 
int LEN; 
int *SCRATCH;
{
  int ADDSUZC();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   return ADDSUZC(D, COPYD, VALUES, IDENTITY, ROWIDENT, vlen, stride, full);
  }
}




int ind_luz_scratch(len) int len; {return 0;}

/* Creates vector of strided indices starting from init */
void ind_luz(D, INIT, STR, LEN, SCRATCH)
int *D;
int INIT; 
int STR; 
int LEN; 
int *SCRATCH;
{
  void INDLUZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   INDLUZ(D, INIT, STR, vlen, stride, full);
  }
}




int xindlez_scratch(len) int len; {return 0;}

/* Segmented index function (segmented scan on 1's) */
void xindlez(D, FLAGS, LEN, SCRATCH)
int *D;
int *FLAGS;
int LEN; 
int *SCRATCH;
{
  void XINDLEZ();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   XINDLEZ(D, FLAGS, vlen, stride, full);
  }
}




int clastindex_scratch(len) int len; {return 0;}

/* Finds index of last seg start and compresses flags to flagd  */
void clastindex(D, INCD, FLAGD, FLAGS, LEN, SCRATCH)
int *D;
int *INCD; 
int *FLAGD;
int *FLAGS;
int LEN; 
int *SCRATCH;
{
  void CLASTIN();
  if (LEN > 0) {
   int vlen, stride, full;

   _compute_shape_factors(LEN, &vlen, &stride, &full);
   CLASTIN(D, INCD, FLAGD, FLAGS, vlen, stride, full);
  }
}
