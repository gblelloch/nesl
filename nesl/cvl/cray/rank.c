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
*  Guy Blelloch				guy.blelloch@cs.cmu.edu
*  School of Computer Science
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3891
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/


/* This file contains the rank functions:
 * o upward and downward,
 * o integer and double,
 * o segmented and unsegmented.
 * The result of these functions is a permutation (suitable for use with
 * the smp_p** functions) that gives the sorted order of the elements
 * of the source vector.  The rank functions are stable, and use a radix
 * rank to get linear performance.
 */

#include <cvl.h>
#include "cray.h"

typedef unsigned int ulong;

#define SIGNBIT (1 << 63)

void rku_luz(d, s, len, scratch)
vec_p d, s, scratch;
int len;
{
  unsigned long xorMask = SIGNBIT;
  unsigned long *src = (unsigned long *)s;
  int i;

  if (len == 0) return;
  for (i = 0; i < len; i++) src[i] ^= xorMask;
  zrank(0, scratch, src, d, len);
  for (i = 0; i < len; i++) src[i] ^= xorMask;
}

void rkd_luz(d, s, len, scratch)
vec_p d, s, scratch;
int len;
{
  ulong xorMask = ~SIGNBIT;
  ulong *src = (ulong *)s;
  int i;

  if (len == 0) return;
  for (i = 0; i < len; i++) src[i] ^= xorMask;
  zrank(0, scratch, s, d, len);
  for (i = 0; i < len; i++) src[i] ^= xorMask;
}


void rku_lud(d, s, len, scratch)
vec_p d, s, scratch;
int len;
{
  unsigned long *src = (unsigned long *)s;
  int i;

  if (len == 0) return;

  for (i = 0; i < len; i++) 
    src[i] = (src[i] & SIGNBIT) ? ~src[i] : (src[i] ^ SIGNBIT);
  zrank(0, scratch, src, d, len);
  for (i = 0; i < len; i++) 
    src[i] = (src[i] & SIGNBIT) ? (src[i] ^ SIGNBIT): ~src[i] ;
}

void rkd_lud(d, s, len, scratch)
vec_p d, s, scratch;
int len;
{
  unsigned long *src = (unsigned long *)s;
  int i;

  if (len == 0) return;

  for (i = 0; i < len; i++) 
    src[i] = (src[i] & SIGNBIT) ? src[i] : (src[i] ^ ~SIGNBIT);
  zrank(0, scratch, src, d, len);
  for (i = 0; i < len; i++) 
    src[i] = (src[i] & SIGNBIT) ? src[i] : (src[i] ^ ~SIGNBIT);
}

int rku_luz_scratch(len) int len; { return 1024 + len + len/2; }
unsigned int rku_luz_inplace() { return INPLACE_NONE;}

int rkd_luz_scratch(len) int len; { return 1024 + len + len/2; } 
unsigned int rkd_luz_inplace() { return INPLACE_NONE;}

int rku_lud_scratch(len) int len; { return 1024 + len + len/2; } 
unsigned int rku_lud_inplace() { return INPLACE_NONE;}

int rkd_lud_scratch(len) int len; { return 1024 + len + len/2; } 
unsigned int rkd_lud_inplace() { return INPLACE_NONE;}



static void seg_rank(d, s, sd, n, m, scratch)
vec_p d, s, sd, scratch;
int n, m;
{
  ulong *src = (ulong *)s;
  ulong *dst = (ulong *)d;
  ulong *iscratch = (ulong *)scratch;
  ulong *index = iscratch + 1024 + n + n/2;
  ulong *nonzerop, *offsets, *flags, *last_start;
  int i;
  int vlen, stride, full; 

  if (n == 0) return;
  _compute_shape_factors(n, &vlen, &stride, &full); 
  offsets =    (ulong *) offsets_from_sd(sd, m); 
  last_start = (ulong *) last_start_from_sd(sd, m); 
  nonzerop =   (ulong *) nonzerop_from_sd(sd, m); 
  flags =      (ulong *) bools_from_sd(sd, m); 
  zorders(0, iscratch, src, index, n);

    /* for (j = 0; j < m; j++) if (nonzerop[j])dst[offsets[j]] = j; */
  ind_luz(iscratch, 0, 1, m, 0);
  pfpm_puz(dst, iscratch, offsets, nonzerop, m, n, 0);

  XCPYSCA(dst, dst, flags, last_start, vlen, stride, full);
  zorders(10, iscratch, dst, index, n);
#pragma ivdep
  for (i = 0; i < n; i++) dst[index[i]] = i;

    /* for (j = 0; j < m; j++) if (nonzerop[j]) iscratch[offsets[j]] = offsets[j]; */
  pfpm_puz(iscratch, offsets, offsets, nonzerop, m, n, 0);

  XCPYSCA(iscratch, iscratch, flags, last_start, vlen, stride, full);
#pragma ivdep
  for (i = 0; i < n; i++) dst[i] -= iscratch[i];
}



void rku_lez(d, s, sd, n, m, scratch)
vec_p d, s, sd, scratch;
int n, m;
{
  ulong xorMask = SIGNBIT;
  ulong *src = (ulong *)s;
  int i;

  if (m == 1) {rku_luz(d, s, n, scratch); return;}
  for (i = 0; i < n; i++) src[i] ^= xorMask;
  seg_rank(d, s, sd, n, m, scratch);
  for (i = 0; i < n; i++) src[i] ^= xorMask;
}

void rkd_lez(d, s, sd, n, m, scratch)
vec_p d, s, sd, scratch;
int n, m;
{
  ulong xorMask = ~SIGNBIT;
  ulong *src = (ulong *)s;
  int i;

  if (m == 1) {rkd_luz(d, s, n, scratch); return;}
  for (i = 0; i < n; i++) src[i] ^= xorMask;
  seg_rank(d, s, sd, n, m, scratch);
  for (i = 0; i < n; i++) src[i] ^= xorMask;
}

void rku_led(d, s, sd, n, m, scratch)
vec_p d, s, sd, scratch;
int n, m;
{
  unsigned long *src = (unsigned long *)s;
  int i;

  if (n == 0) return;
  if (m == 1) {rku_lud(d, s, n, scratch); return;}
  for (i = 0; i < n; i++) 
    src[i] = (src[i] & SIGNBIT) ? ~src[i] : (src[i] ^ SIGNBIT);
  seg_rank(d, s, sd, n, m, scratch);
  for (i = 0; i < n; i++) 
    src[i] = (src[i] & SIGNBIT) ? (src[i] ^ SIGNBIT): ~src[i] ;
}

void rkd_led(d, s, sd, n, m, scratch)
vec_p d, s, sd, scratch;
int n, m;
{
  unsigned long *src = (unsigned long *)s;
  int i;

  if (n == 0) return;
  if (m == 1) {rkd_lud(d, s, n, scratch); return;}
  for (i = 0; i < n; i++) 
    src[i] = (src[i] & SIGNBIT) ? src[i] : (src[i] ^ ~SIGNBIT);
  seg_rank(d, s, sd, n, m, scratch);
  for (i = 0; i < n; i++) 
    src[i] = (src[i] & SIGNBIT) ? src[i] : (src[i] ^ ~SIGNBIT);
}


int rkd_lez_scratch(len, seg_count) 
int len, seg_count; 
{ return 1024 + len + len + len/2 + seg_count; }
unsigned int rkd_lez_inplace() { return INPLACE_NONE;}

int rku_lez_scratch(len, seg_count) 
int len, seg_count; 
{ return 1024 + len + len + len/2 + seg_count; }
unsigned int rku_lez_inplace() { return INPLACE_NONE;}


int rku_led_scratch(len, seg_count) 
int len,seg_count; 
{ return 1024 + len + len + len/2 + seg_count; }
unsigned int rku_led_inplace() { return INPLACE_NONE;}

int rkd_led_scratch(len, seg_count) 
int len,seg_count; 
{ return 1024 + len + len + len/2 + seg_count; }
unsigned int rkd_led_inplace() { return INPLACE_NONE;}

