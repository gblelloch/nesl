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


/* This file has been machine-generated */


#include <math.h>
#include <mpi.h>
#include <cvl.h>
#include "mpicvl.h"
#include "messages.h"



/* Simple math: defined on integers and doubles.
 */
void max_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (max_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = max (src_one[i], src_two[i]);
  }
  LOG_STOP (max_wuz, nelts);
}

unsigned int max_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int max_wuz_scratch (int nelts)
{
  return 0;
}

void min_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (min_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = min (src_one[i], src_two[i]);
  }
  LOG_STOP (min_wuz, nelts);
}

unsigned int min_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int min_wuz_scratch (int nelts)
{
  return 0;
}

void add_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (add_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = add (src_one[i], src_two[i]);
  }
  LOG_STOP (add_wuz, nelts);
}

unsigned int add_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int add_wuz_scratch (int nelts)
{
  return 0;
}

void sub_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (sub_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = sub (src_one[i], src_two[i]);
  }
  LOG_STOP (sub_wuz, nelts);
}

unsigned int sub_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int sub_wuz_scratch (int nelts)
{
  return 0;
}

void mul_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (mul_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = mul (src_one[i], src_two[i]);
  }
  LOG_STOP (mul_wuz, nelts);
}

unsigned int mul_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int mul_wuz_scratch (int nelts)
{
  return 0;
}

void div_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (div_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = dvd (src_one[i], src_two[i]);
  }
  LOG_STOP (div_wuz, nelts);
}

unsigned int div_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int div_wuz_scratch (int nelts)
{
  return 0;
}

void max_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  double *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (max_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = max (src_one[i], src_two[i]);
  }
  LOG_STOP (max_wud, nelts);
}

unsigned int max_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int max_wud_scratch (int nelts)
{
  return 0;
}

void min_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  double *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (min_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = min (src_one[i], src_two[i]);
  }
  LOG_STOP (min_wud, nelts);
}

unsigned int min_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int min_wud_scratch (int nelts)
{
  return 0;
}

void add_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  double *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (add_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = add (src_one[i], src_two[i]);
  }
  LOG_STOP (add_wud, nelts);
}

unsigned int add_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int add_wud_scratch (int nelts)
{
  return 0;
}

void sub_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  double *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (sub_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = sub (src_one[i], src_two[i]);
  }
  LOG_STOP (sub_wud, nelts);
}

unsigned int sub_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int sub_wud_scratch (int nelts)
{
  return 0;
}

void mul_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  double *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (mul_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = mul (src_one[i], src_two[i]);
  }
  LOG_STOP (mul_wud, nelts);
}

unsigned int mul_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int mul_wud_scratch (int nelts)
{
  return 0;
}

void div_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  double *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (div_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = dvd (src_one[i], src_two[i]);
  }
  LOG_STOP (div_wud, nelts);
}

unsigned int div_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int div_wud_scratch (int nelts)
{
  return 0;
}


/* Comparisons: defined on integers and doubles.
 */
void grt_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (grt_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = grt (src_one[i], src_two[i]);
  }
  LOG_STOP (grt_wuz, nelts);
}

unsigned int grt_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int grt_wuz_scratch (int nelts)
{
  return 0;
}

void les_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (les_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = les (src_one[i], src_two[i]);
  }
  LOG_STOP (les_wuz, nelts);
}

unsigned int les_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int les_wuz_scratch (int nelts)
{
  return 0;
}

void geq_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (geq_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = geq (src_one[i], src_two[i]);
  }
  LOG_STOP (geq_wuz, nelts);
}

unsigned int geq_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int geq_wuz_scratch (int nelts)
{
  return 0;
}

void leq_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (leq_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = leq (src_one[i], src_two[i]);
  }
  LOG_STOP (leq_wuz, nelts);
}

unsigned int leq_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int leq_wuz_scratch (int nelts)
{
  return 0;
}

void grt_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (grt_wud, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = grt (src_one[i], src_two[i]);
  }
  LOG_STOP (grt_wud, nelts);
}

unsigned int grt_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int grt_wud_scratch (int nelts)
{
  return 0;
}

void les_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (les_wud, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = les (src_one[i], src_two[i]);
  }
  LOG_STOP (les_wud, nelts);
}

unsigned int les_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int les_wud_scratch (int nelts)
{
  return 0;
}

void geq_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (geq_wud, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = geq (src_one[i], src_two[i]);
  }
  LOG_STOP (geq_wud, nelts);
}

unsigned int geq_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int geq_wud_scratch (int nelts)
{
  return 0;
}

void leq_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (leq_wud, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = leq (src_one[i], src_two[i]);
  }
  LOG_STOP (leq_wud, nelts);
}

unsigned int leq_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int leq_wud_scratch (int nelts)
{
  return 0;
}


/* Equality: defined on booleans, integers and doubles.
 */
void eql_wub (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  cvl_bool *src_one, *src_two;
  int i, n_here;

  LOG_START (eql_wub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (cvl_bool *) (Mem + src_one_v);
  src_two = (cvl_bool *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = eql (src_one[i], src_two[i]);
  }
  LOG_STOP (eql_wub, nelts);
}

unsigned int eql_wub_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int eql_wub_scratch (int nelts)
{
  return 0;
}

void neq_wub (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  cvl_bool *src_one, *src_two;
  int i, n_here;

  LOG_START (neq_wub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (cvl_bool *) (Mem + src_one_v);
  src_two = (cvl_bool *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = neq (src_one[i], src_two[i]);
  }
  LOG_STOP (neq_wub, nelts);
}

unsigned int neq_wub_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int neq_wub_scratch (int nelts)
{
  return 0;
}

void eql_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (eql_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = eql (src_one[i], src_two[i]);
  }
  LOG_STOP (eql_wuz, nelts);
}

unsigned int eql_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int eql_wuz_scratch (int nelts)
{
  return 0;
}

void neq_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (neq_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = neq (src_one[i], src_two[i]);
  }
  LOG_STOP (neq_wuz, nelts);
}

unsigned int neq_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int neq_wuz_scratch (int nelts)
{
  return 0;
}

void eql_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (eql_wud, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = eql (src_one[i], src_two[i]);
  }
  LOG_STOP (eql_wud, nelts);
}

unsigned int eql_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int eql_wud_scratch (int nelts)
{
  return 0;
}

void neq_wud (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  double *src_one, *src_two;
  int i, n_here;

  LOG_START (neq_wud, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = neq (src_one[i], src_two[i]);
  }
  LOG_STOP (neq_wud, nelts);
}

unsigned int neq_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int neq_wud_scratch (int nelts)
{
  return 0;
}


/* Shifts and mods: defined on integers.
 */
void lsh_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (lsh_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = lsh (src_one[i], src_two[i]);
  }
  LOG_STOP (lsh_wuz, nelts);
}

unsigned int lsh_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int lsh_wuz_scratch (int nelts)
{
  return 0;
}

void rsh_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (rsh_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = rsh (src_one[i], src_two[i]);
  }
  LOG_STOP (rsh_wuz, nelts);
}

unsigned int rsh_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int rsh_wuz_scratch (int nelts)
{
  return 0;
}

void mod_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (mod_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = mod (src_one[i], src_two[i]);
  }
  LOG_STOP (mod_wuz, nelts);
}

unsigned int mod_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int mod_wuz_scratch (int nelts)
{
  return 0;
}


/* Good random numbers aren't standard.  Sigh
 */
#if defined(FIBRNG)
extern int get_rn_int (int *genptr);
extern int *genptr;

#elif defined(__hpux)
extern long lrand48 (void);

#define RANDOM lrand48
#elif defined(__alpha) && defined(__osf__)
extern int random (void);

#define RANDOM random
#else
extern long random (void);

#define RANDOM random
#endif

void rnd_wuz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst, *src;
  int i, n_here;

  LOG_START (rnd_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
#ifdef FIBRNG
    dst[i] = (get_rn_int (genptr) & 0xfffffff) % src[i];
#else
    dst[i] = (((int) RANDOM ()) & 0xfffffff) % src[i];
#endif
  }
  LOG_STOP (rnd_wuz, nelts);
}

unsigned int rnd_wuz_inplace (void)
{
  return INPLACE_1;
}

int rnd_wuz_scratch (int nelts)
{
  return 0;
}


/* Selection: defined on booleans, integers and doubles.
 */
void sel_wuz (vec_p dst_v, vec_p flags_v, vec_p src_one_v, vec_p src_two_v,
	       int nelts, vec_p scratch_v)
{
  int *dst, *src_one, *src_two;
  cvl_bool *flags;
  int i, n_here;

  LOG_START (sel_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  flags = (cvl_bool *) (Mem + flags_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = pickone (flags[i], src_one[i], src_two[i]);
  }
  LOG_STOP (sel_wuz, nelts);
}

unsigned int sel_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2 | INPLACE_3);
}

int sel_wuz_scratch (int nelts)
{
  return 0;
}

void sel_wud (vec_p dst_v, vec_p flags_v, vec_p src_one_v, vec_p src_two_v,
	       int nelts, vec_p scratch_v)
{
  double *dst, *src_one, *src_two;
  cvl_bool *flags;
  int i, n_here;

  LOG_START (sel_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  flags = (cvl_bool *) (Mem + flags_v);
  src_one = (double *) (Mem + src_one_v);
  src_two = (double *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = pickone (flags[i], src_one[i], src_two[i]);
  }
  LOG_STOP (sel_wud, nelts);
}

unsigned int sel_wud_inplace (void)
{
  return (INPLACE_1 | INPLACE_2 | INPLACE_3);
}

int sel_wud_scratch (int nelts)
{
  return 0;
}

void sel_wub (vec_p dst_v, vec_p flags_v, vec_p src_one_v, vec_p src_two_v,
	       int nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src_one, *src_two;
  cvl_bool *flags;
  int i, n_here;

  LOG_START (sel_wub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  flags = (cvl_bool *) (Mem + flags_v);
  src_one = (cvl_bool *) (Mem + src_one_v);
  src_two = (cvl_bool *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = pickone (flags[i], src_one[i], src_two[i]);
  }
  LOG_STOP (sel_wub, nelts);
}

unsigned int sel_wub_inplace (void)
{
  return (INPLACE_1 | INPLACE_2 | INPLACE_3);
}

int sel_wub_scratch (int nelts)
{
  return 0;
}


/* Logical functions: defined on booleans.
 */
void not_wub (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *dst;
  cvl_bool *src;
  int i, n_here;

  LOG_START (not_wub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = not (src[i]);
  }
  LOG_STOP (not_wub, nelts);
}

unsigned int not_wub_inplace (void)
{
  return INPLACE_1;
}

int not_wub_scratch (int nelts)
{
  return 0;
}

void xor_wub (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  cvl_bool *src_one, *src_two;
  int i, n_here;

  LOG_START (xor_wub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (cvl_bool *) (Mem + src_one_v);
  src_two = (cvl_bool *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = xor (src_one[i], src_two[i]);
  }
  LOG_STOP (xor_wub, nelts);
}

unsigned int xor_wub_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int xor_wub_scratch (int nelts)
{
  return 0;
}

void ior_wub (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  cvl_bool *src_one, *src_two;
  int i, n_here;

  LOG_START (ior_wub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (cvl_bool *) (Mem + src_one_v);
  src_two = (cvl_bool *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = ior (src_one[i], src_two[i]);
  }
  LOG_STOP (ior_wub, nelts);
}

unsigned int ior_wub_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int ior_wub_scratch (int nelts)
{
  return 0;
}

void and_wub (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  cvl_bool *dst;
  cvl_bool *src_one, *src_two;
  int i, n_here;

  LOG_START (and_wub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src_one = (cvl_bool *) (Mem + src_one_v);
  src_two = (cvl_bool *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = and (src_one[i], src_two[i]);
  }
  LOG_STOP (and_wub, nelts);
}

unsigned int and_wub_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int and_wub_scratch (int nelts)
{
  return 0;
}


/* Bitwise functions: defined on integers.
 */
void not_wuz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst;
  int *src;
  int i, n_here;

  LOG_START (not_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = bnot (src[i]);
  }
  LOG_STOP (not_wuz, nelts);
}

unsigned int not_wuz_inplace (void)
{
  return INPLACE_1;
}

int not_wuz_scratch (int nelts)
{
  return 0;
}

void xor_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (xor_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = xor (src_one[i], src_two[i]);
  }
  LOG_STOP (xor_wuz, nelts);
}

unsigned int xor_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int xor_wuz_scratch (int nelts)
{
  return 0;
}

void ior_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (ior_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = bor (src_one[i], src_two[i]);
  }
  LOG_STOP (ior_wuz, nelts);
}

unsigned int ior_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int ior_wuz_scratch (int nelts)
{
  return 0;
}

void and_wuz (vec_p dst_v, vec_p src_one_v, vec_p src_two_v, int nelts,
	       vec_p scratch_v)
{
  int *dst;
  int *src_one, *src_two;
  int i, n_here;

  LOG_START (and_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src_one = (int *) (Mem + src_one_v);
  src_two = (int *) (Mem + src_two_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = bnd (src_one[i], src_two[i]);
  }
  LOG_STOP (and_wuz, nelts);
}

unsigned int and_wuz_inplace (void)
{
  return (INPLACE_1 | INPLACE_2);
}

int and_wuz_scratch (int nelts)
{
  return 0;
}


/* Math functions: defined on doubles.
 */
void flr_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst;
  double *src;
  int i, n_here;

  LOG_START (flr_wud, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = cvl_floor (src[i]);
  }
  LOG_STOP (flr_wud, nelts);
}

unsigned int flr_wud_inplace (void)
{
  return INPLACE_1;
}

int flr_wud_scratch (int nelts)
{
  return 0;
}

void cei_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst;
  double *src;
  int i, n_here;

  LOG_START (cei_wud, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = cvl_ceil (src[i]);
  }
  LOG_STOP (cei_wud, nelts);
}

unsigned int cei_wud_inplace (void)
{
  return INPLACE_1;
}

int cei_wud_scratch (int nelts)
{
  return 0;
}

void trn_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst;
  double *src;
  int i, n_here;

  LOG_START (trn_wud, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = d_to_z (src[i]);
  }
  LOG_STOP (trn_wud, nelts);
}

unsigned int trn_wud_inplace (void)
{
  return INPLACE_1;
}

int trn_wud_scratch (int nelts)
{
  return 0;
}

void rou_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst;
  double *src;
  int i, n_here;

  LOG_START (rou_wud, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = cvl_round (src[i]);
  }
  LOG_STOP (rou_wud, nelts);
}

unsigned int rou_wud_inplace (void)
{
  return INPLACE_1;
}

int rou_wud_scratch (int nelts)
{
  return 0;
}

void exp_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (exp_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = exp (src[i]);
  }
  LOG_STOP (exp_wud, nelts);
}

unsigned int exp_wud_inplace (void)
{
  return INPLACE_1;
}

int exp_wud_scratch (int nelts)
{
  return 0;
}

void log_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (log_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = log (src[i]);
  }
  LOG_STOP (log_wud, nelts);
}

unsigned int log_wud_inplace (void)
{
  return INPLACE_1;
}

int log_wud_scratch (int nelts)
{
  return 0;
}

void sqt_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (sqt_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = sqrt (src[i]);
  }
  LOG_STOP (sqt_wud, nelts);
}

unsigned int sqt_wud_inplace (void)
{
  return INPLACE_1;
}

int sqt_wud_scratch (int nelts)
{
  return 0;
}

void sin_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (sin_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = sin (src[i]);
  }
  LOG_STOP (sin_wud, nelts);
}

unsigned int sin_wud_inplace (void)
{
  return INPLACE_1;
}

int sin_wud_scratch (int nelts)
{
  return 0;
}

void cos_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (cos_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = cos (src[i]);
  }
  LOG_STOP (cos_wud, nelts);
}

unsigned int cos_wud_inplace (void)
{
  return INPLACE_1;
}

int cos_wud_scratch (int nelts)
{
  return 0;
}

void tan_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (tan_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = tan (src[i]);
  }
  LOG_STOP (tan_wud, nelts);
}

unsigned int tan_wud_inplace (void)
{
  return INPLACE_1;
}

int tan_wud_scratch (int nelts)
{
  return 0;
}

void asn_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (asn_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = asin (src[i]);
  }
  LOG_STOP (asn_wud, nelts);
}

unsigned int asn_wud_inplace (void)
{
  return INPLACE_1;
}

int asn_wud_scratch (int nelts)
{
  return 0;
}

void acs_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (acs_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = acos (src[i]);
  }
  LOG_STOP (acs_wud, nelts);
}

unsigned int acs_wud_inplace (void)
{
  return INPLACE_1;
}

int acs_wud_scratch (int nelts)
{
  return 0;
}

void atn_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (atn_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = atan (src[i]);
  }
  LOG_STOP (atn_wud, nelts);
}

unsigned int atn_wud_inplace (void)
{
  return INPLACE_1;
}

int atn_wud_scratch (int nelts)
{
  return 0;
}

void snh_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (snh_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = sinh (src[i]);
  }
  LOG_STOP (snh_wud, nelts);
}

unsigned int snh_wud_inplace (void)
{
  return INPLACE_1;
}

int snh_wud_scratch (int nelts)
{
  return 0;
}

void csh_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (csh_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = cosh (src[i]);
  }
  LOG_STOP (csh_wud, nelts);
}

unsigned int csh_wud_inplace (void)
{
  return INPLACE_1;
}

int csh_wud_scratch (int nelts)
{
  return 0;
}

void tnh_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (tnh_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = tanh (src[i]);
  }
  LOG_STOP (tnh_wud, nelts);
}

unsigned int tnh_wud_inplace (void)
{
  return INPLACE_1;
}

int tnh_wud_scratch (int nelts)
{
  return 0;
}


/* Type-conversion routines.
 */
void int_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst;
  double *src;
  int i, n_here;

  LOG_START (int_wud, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = d_to_z (src[i]);
  }
  LOG_STOP (int_wud, nelts);
}

unsigned int int_wud_inplace (void)
{
  return INPLACE_1;
}

int int_wud_scratch (int nelts)
{
  return 0;
}

void int_wub (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst;
  cvl_bool *src;
  int i, n_here;

  LOG_START (int_wub, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = b_to_z (src[i]);
  }
  LOG_STOP (int_wub, nelts);
}

unsigned int int_wub_inplace (void)
{
  return INPLACE_1;
}

int int_wub_scratch (int nelts)
{
  return 0;
}

void dbl_wuz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  int *src;
  int i, n_here;

  LOG_START (dbl_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = z_to_d (src[i]);
  }
  LOG_STOP (dbl_wuz, nelts);
}

unsigned int dbl_wuz_inplace (void)
{
  return INPLACE_1;
}

int dbl_wuz_scratch (int nelts)
{
  return 0;
}

void boo_wuz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *dst;
  int *src;
  int i, n_here;

  LOG_START (boo_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = z_to_b (src[i]);
  }
  LOG_STOP (boo_wuz, nelts);
}

unsigned int boo_wuz_inplace (void)
{
  return INPLACE_1;
}

int boo_wuz_scratch (int nelts)
{
  return 0;
}


/* Copying routines.
 */
void cpy_wuz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst;
  int *src;
  int i, n_here;

  LOG_START (cpy_wuz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = ident (src[i]);
  }
  LOG_STOP (cpy_wuz, nelts);
}

unsigned int cpy_wuz_inplace (void)
{
  return INPLACE_1;
}

int cpy_wuz_scratch (int nelts)
{
  return 0;
}

void cpy_wud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst;
  double *src;
  int i, n_here;

  LOG_START (cpy_wud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = ident (src[i]);
  }
  LOG_STOP (cpy_wud, nelts);
}

unsigned int cpy_wud_inplace (void)
{
  return INPLACE_1;
}

int cpy_wud_scratch (int nelts)
{
  return 0;
}

void cpy_wub (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *dst;
  cvl_bool *src;
  int i, n_here;

  LOG_START (cpy_wub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);

  for (i = 0; i < n_here; i++) {
    dst[i] = ident (src[i]);
  }
  LOG_STOP (cpy_wub, nelts);
}

unsigned int cpy_wub_inplace (void)
{
  return INPLACE_1;
}

int cpy_wub_scratch (int nelts)
{
  return 0;
}


/* This is tied very closely to mke_fov in facilt.c.
 */
void cpy_wus (vec_p dst_v, vec_p src_v, int nelts, int nsegs,
	       vec_p scratch_v)
{
  int *dst, *src;
  int i, nsegs_here, nelts_here, space;

  LOG_START (cpy_wus, nelts);
  nelts_here = _num_here (nelts);
  nsegs_here = _num_here (nsegs);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);

  /* Copy segment lengths. */
  for (i = 0; i < nsegs_here; i++) {
    dst[i] = src[i];
  }
  space = SpaceFor (nsegs);
  dst += space;
  src += space;

  /* Copy segment start addresses. */
  for (i = 0; i < nsegs_here; i++) {
    dst[i] = src[i];
  }
  dst += space;
  src += space;

  /* Copy segment each element is in. */
  for (i = 0; i < nelts_here; i++) {
    dst[i] = src[i];
  }
  LOG_STOP (cpy_wus, nelts);
}

unsigned int cpy_wus_inplace (void)
{
  return INPLACE_NONE;
}

int cpy_wus_scratch (int nelts, int nsegs)
{
  return 0;
}
