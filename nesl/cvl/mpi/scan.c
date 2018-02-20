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


#include <limits.h>
#include <float.h>
#include <mpi.h>
#include <cvl.h>
#include "mpicvl.h"
#include "messages.h"


/* An explanation of MPI scan_ops, as of MPICH 1.0.7.
 * MPI_Op (*in, *inout, ...)
 * The operation won't be called at all on processor 0.
 * Contribution from processors on our "left" arrives in *in.
 * Input from this processor should be put in *inout.
 * Final result goes out in *inout.
 */

/* A segmented copy-scan for each data type, and a two-element segmented
 * copy-scan just for ints
 */

static MPI_Op MPI_op_cpy_int;
static int _result_of_seg_scan_int;

static void op_cpy_int (int *in, int *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_int = *in;
  if (!Segment_Here) {
    *inout = *in;
  }
}

void _copy_scan_int (int *p_inout)
{
  int out;

  MPI_Scan (p_inout, &out, 1, MPI_INT, MPI_op_cpy_int,
	    MPI_COMM_WORLD);
  *p_inout = _result_of_seg_scan_int;
}

static MPI_Op MPI_op_cpy_double;
static double _result_of_seg_scan_double;

static void op_cpy_double (double *in, double *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_double = *in;
  if (!Segment_Here) {
    *inout = *in;
  }
}

void _copy_scan_double (double *p_inout)
{
  double out;

  MPI_Scan (p_inout, &out, 1, MPI_DOUBLE, MPI_op_cpy_double,
	    MPI_COMM_WORLD);
  *p_inout = _result_of_seg_scan_double;
}

static MPI_Op MPI_op_cpy_cvl_bool;
static cvl_bool _result_of_seg_scan_cvl_bool;

static void op_cpy_cvl_bool (cvl_bool * in, cvl_bool * inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_cvl_bool = *in;
  if (!Segment_Here) {
    *inout = *in;
  }
}

void _copy_scan_cvl_bool (cvl_bool * p_inout)
{
  cvl_bool out;

  MPI_Scan (p_inout, &out, 1, MPI_BOOL, MPI_op_cpy_cvl_bool,
	    MPI_COMM_WORLD);
  *p_inout = _result_of_seg_scan_cvl_bool;
}



static MPI_Op MPI_op_cpy_two_int;

static int result_of_seg_scan[2];

static void op_cpy_two_int (int *in, int *inout, int len,
			     MPI_Datatype * dptr)
{
  result_of_seg_scan[0] = in[0];
  result_of_seg_scan[1] = in[1];
  if (!Segment_Here) {
    inout[0] = in[0];
    inout[1] = in[1];
  }
}

void _copy_scan_two_int (int *index_p, int *stride_p)
{
  int in[2], out[2];

  in[0] = *index_p;
  in[1] = *stride_p;

  MPI_Scan (in, out, 2, MPI_INT, MPI_op_cpy_two_int,
	    MPI_COMM_WORLD);
  *index_p = result_of_seg_scan[0];
  *stride_p = result_of_seg_scan[1];
}


/* ----------------------- Unsegmented Scan -------------------------- */

/* MPI scans are inclusive.  CVL scans are exclusive.  We have a
 * problem.  Where the operator has an inverse, we can use it to negate
 * the effect of the current element on the inclusive scan, and thus
 * give an exclusive result.
 */

void add_suz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst, *src;
  int total, incl, excl, swap;
  int n_here, i;

  LOG_START (add_suz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);
  total = 0;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = add (total, src[i]);
    dst[i] = swap;
  }

  /* Inclusive scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

  /* Use inverse of OP to turn inclusive scan into exclusive scan */
  excl = sub (incl, total);

  /* Combine result of exclusive scan with partial scan on proc */
  for (i = 0; i < n_here; i++) {
    dst[i] = add (dst[i], excl);
  }
  LOG_STOP (add_suz, nelts);
}

unsigned int add_suz_inplace (void)
{
  return INPLACE_1;
}

int add_suz_scratch (int nelts)
{
  return 0;
}

void mul_suz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst, *src;
  int total, incl, excl, swap;
  int n_here, i;

  LOG_START (mul_suz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);
  total = 1;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = mul (total, src[i]);
    dst[i] = swap;
  }

  /* Inclusive scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_INT, MPI_PROD, MPI_COMM_WORLD);

  /* Use inverse of OP to turn inclusive scan into exclusive scan */
  excl = dvd (incl, total);

  /* Combine result of exclusive scan with partial scan on proc */
  for (i = 0; i < n_here; i++) {
    dst[i] = mul (dst[i], excl);
  }
  LOG_STOP (mul_suz, nelts);
}

unsigned int mul_suz_inplace (void)
{
  return INPLACE_1;
}

int mul_suz_scratch (int nelts)
{
  return 0;
}

void xor_suz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst, *src;
  int total, incl, excl, swap;
  int n_here, i;

  LOG_START (xor_suz, nelts);
  n_here = _num_here (nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);
  total = 0;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = xor (total, src[i]);
    dst[i] = swap;
  }

  /* Inclusive scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_INT, MPI_BXOR, MPI_COMM_WORLD);

  /* Use inverse of OP to turn inclusive scan into exclusive scan */
  excl = xor (incl, total);

  /* Combine result of exclusive scan with partial scan on proc */
  for (i = 0; i < n_here; i++) {
    dst[i] = xor (dst[i], excl);
  }
  LOG_STOP (xor_suz, nelts);
}

unsigned int xor_suz_inplace (void)
{
  return INPLACE_1;
}

int xor_suz_scratch (int nelts)
{
  return 0;
}

void add_sud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst, *src;
  double total, incl, excl, swap;
  int n_here, i;

  LOG_START (add_sud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);
  total = 0.0;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = add (total, src[i]);
    dst[i] = swap;
  }

  /* Inclusive scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

  /* Use inverse of OP to turn inclusive scan into exclusive scan */
  excl = sub (incl, total);

  /* Combine result of exclusive scan with partial scan on proc */
  for (i = 0; i < n_here; i++) {
    dst[i] = add (dst[i], excl);
  }
  LOG_STOP (add_sud, nelts);
}

unsigned int add_sud_inplace (void)
{
  return INPLACE_1;
}

int add_sud_scratch (int nelts)
{
  return 0;
}

void mul_sud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst, *src;
  double total, incl, excl, swap;
  int n_here, i;

  LOG_START (mul_sud, nelts);
  n_here = _num_here (nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);
  total = 1.0;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = mul (total, src[i]);
    dst[i] = swap;
  }

  /* Inclusive scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_DOUBLE, MPI_PROD, MPI_COMM_WORLD);

  /* Use inverse of OP to turn inclusive scan into exclusive scan */
  excl = dvd (incl, total);

  /* Combine result of exclusive scan with partial scan on proc */
  for (i = 0; i < n_here; i++) {
    dst[i] = mul (dst[i], excl);
  }
  LOG_STOP (mul_sud, nelts);
}

unsigned int mul_sud_inplace (void)
{
  return INPLACE_1;
}

int mul_sud_scratch (int nelts)
{
  return 0;
}

void xor_sub (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  cvl_bool total, incl, excl, swap;
  int n_here, i;

  LOG_START (xor_sub, nelts);
  n_here = _num_here (nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);
  total = FALSE;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = xor (total, src[i]);
    dst[i] = swap;
  }

  /* Inclusive scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_BOOL, MPI_BXOR, MPI_COMM_WORLD);

  /* Use inverse of OP to turn inclusive scan into exclusive scan */
  excl = xor (incl, total);

  /* Combine result of exclusive scan with partial scan on proc */
  for (i = 0; i < n_here; i++) {
    dst[i] = xor (dst[i], excl);
  }
  LOG_STOP (xor_sub, nelts);
}

unsigned int xor_sub_inplace (void)
{
  return INPLACE_1;
}

int xor_sub_scratch (int nelts)
{
  return 0;
}




void max_suz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst, *src;
  MPI_Status status;
  int total, incl, excl, swap;
  int n_here, left, right, i;

  LOG_START (max_suz, nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);
  n_here = _num_here (nelts);
  total = INT_MIN;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = max (total, src[i]);
    dst[i] = swap;
  }

  /* Scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);

  /* Shift incl to the right into excl, putting identity on far left */
  left = Self - 1;
  right = Self + 1;
  if (Self == 0) {
    left = MPI_PROC_NULL;	/* do not receive from anyone */
    excl = INT_MIN;		/* and preset the result */
  }
  if (Self == NumProcs - 1) {
    right = MPI_PROC_NULL;	/* do not send to anyone */
  }
  MPI_Sendrecv (&incl, 1, MPI_INT, right, max_suz_tag,
		&excl, 1, MPI_INT, left, max_suz_tag,
		CommShift, &status);

  /* Combine result with partial scan */
  for (i = 0; i < n_here; i++) {
    dst[i] = max (dst[i], excl);
  }
  LOG_STOP (max_suz, nelts);
}

unsigned int max_suz_inplace (void)
{
  return INPLACE_1;
}

int max_suz_scratch (int nelts)
{
  return 0;
}

void min_suz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst, *src;
  MPI_Status status;
  int total, incl, excl, swap;
  int n_here, left, right, i;

  LOG_START (min_suz, nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);
  n_here = _num_here (nelts);
  total = INT_MAX;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = min (total, src[i]);
    dst[i] = swap;
  }

  /* Scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);

  /* Shift incl to the right into excl, putting identity on far left */
  left = Self - 1;
  right = Self + 1;
  if (Self == 0) {
    left = MPI_PROC_NULL;	/* do not receive from anyone */
    excl = INT_MAX;		/* and preset the result */
  }
  if (Self == NumProcs - 1) {
    right = MPI_PROC_NULL;	/* do not send to anyone */
  }
  MPI_Sendrecv (&incl, 1, MPI_INT, right, min_suz_tag,
		&excl, 1, MPI_INT, left, min_suz_tag,
		CommShift, &status);

  /* Combine result with partial scan */
  for (i = 0; i < n_here; i++) {
    dst[i] = min (dst[i], excl);
  }
  LOG_STOP (min_suz, nelts);
}

unsigned int min_suz_inplace (void)
{
  return INPLACE_1;
}

int min_suz_scratch (int nelts)
{
  return 0;
}

void and_suz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst, *src;
  MPI_Status status;
  int total, incl, excl, swap;
  int n_here, left, right, i;

  LOG_START (and_suz, nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);
  n_here = _num_here (nelts);
  total = ~0;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = bnd (total, src[i]);
    dst[i] = swap;
  }

  /* Scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_INT, MPI_BAND, MPI_COMM_WORLD);

  /* Shift incl to the right into excl, putting identity on far left */
  left = Self - 1;
  right = Self + 1;
  if (Self == 0) {
    left = MPI_PROC_NULL;	/* do not receive from anyone */
    excl = ~0;			/* and preset the result */
  }
  if (Self == NumProcs - 1) {
    right = MPI_PROC_NULL;	/* do not send to anyone */
  }
  MPI_Sendrecv (&incl, 1, MPI_INT, right, and_suz_tag,
		&excl, 1, MPI_INT, left, and_suz_tag,
		CommShift, &status);

  /* Combine result with partial scan */
  for (i = 0; i < n_here; i++) {
    dst[i] = bnd (dst[i], excl);
  }
  LOG_STOP (and_suz, nelts);
}

unsigned int and_suz_inplace (void)
{
  return INPLACE_1;
}

int and_suz_scratch (int nelts)
{
  return 0;
}

void ior_suz (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  int *dst, *src;
  MPI_Status status;
  int total, incl, excl, swap;
  int n_here, left, right, i;

  LOG_START (ior_suz, nelts);
  dst = (int *) (Mem + dst_v);
  src = (int *) (Mem + src_v);
  n_here = _num_here (nelts);
  total = 0;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = bor (total, src[i]);
    dst[i] = swap;
  }

  /* Scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_INT, MPI_BOR, MPI_COMM_WORLD);

  /* Shift incl to the right into excl, putting identity on far left */
  left = Self - 1;
  right = Self + 1;
  if (Self == 0) {
    left = MPI_PROC_NULL;	/* do not receive from anyone */
    excl = 0;			/* and preset the result */
  }
  if (Self == NumProcs - 1) {
    right = MPI_PROC_NULL;	/* do not send to anyone */
  }
  MPI_Sendrecv (&incl, 1, MPI_INT, right, ior_suz_tag,
		&excl, 1, MPI_INT, left, ior_suz_tag,
		CommShift, &status);

  /* Combine result with partial scan */
  for (i = 0; i < n_here; i++) {
    dst[i] = bor (dst[i], excl);
  }
  LOG_STOP (ior_suz, nelts);
}

unsigned int ior_suz_inplace (void)
{
  return INPLACE_1;
}

int ior_suz_scratch (int nelts)
{
  return 0;
}

void max_sud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst, *src;
  MPI_Status status;
  double total, incl, excl, swap;
  int n_here, left, right, i;

  LOG_START (max_sud, nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);
  n_here = _num_here (nelts);
  total = -DBL_MAX;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = max (total, src[i]);
    dst[i] = swap;
  }

  /* Scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);

  /* Shift incl to the right into excl, putting identity on far left */
  left = Self - 1;
  right = Self + 1;
  if (Self == 0) {
    left = MPI_PROC_NULL;	/* do not receive from anyone */
    excl = -DBL_MAX;		/* and preset the result */
  }
  if (Self == NumProcs - 1) {
    right = MPI_PROC_NULL;	/* do not send to anyone */
  }
  MPI_Sendrecv (&incl, 1, MPI_DOUBLE, right, max_sud_tag,
		&excl, 1, MPI_DOUBLE, left, max_sud_tag,
		CommShift, &status);

  /* Combine result with partial scan */
  for (i = 0; i < n_here; i++) {
    dst[i] = max (dst[i], excl);
  }
  LOG_STOP (max_sud, nelts);
}

unsigned int max_sud_inplace (void)
{
  return INPLACE_1;
}

int max_sud_scratch (int nelts)
{
  return 0;
}

void min_sud (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  double *dst, *src;
  MPI_Status status;
  double total, incl, excl, swap;
  int n_here, left, right, i;

  LOG_START (min_sud, nelts);
  dst = (double *) (Mem + dst_v);
  src = (double *) (Mem + src_v);
  n_here = _num_here (nelts);
  total = DBL_MAX;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = min (total, src[i]);
    dst[i] = swap;
  }

  /* Scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_DOUBLE, MPI_MIN, MPI_COMM_WORLD);

  /* Shift incl to the right into excl, putting identity on far left */
  left = Self - 1;
  right = Self + 1;
  if (Self == 0) {
    left = MPI_PROC_NULL;	/* do not receive from anyone */
    excl = DBL_MAX;		/* and preset the result */
  }
  if (Self == NumProcs - 1) {
    right = MPI_PROC_NULL;	/* do not send to anyone */
  }
  MPI_Sendrecv (&incl, 1, MPI_DOUBLE, right, min_sud_tag,
		&excl, 1, MPI_DOUBLE, left, min_sud_tag,
		CommShift, &status);

  /* Combine result with partial scan */
  for (i = 0; i < n_here; i++) {
    dst[i] = min (dst[i], excl);
  }
  LOG_STOP (min_sud, nelts);
}

unsigned int min_sud_inplace (void)
{
  return INPLACE_1;
}

int min_sud_scratch (int nelts)
{
  return 0;
}

void and_sub (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  MPI_Status status;
  cvl_bool total, incl, excl, swap;
  int n_here, left, right, i;

  LOG_START (and_sub, nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);
  n_here = _num_here (nelts);
  total = TRUE;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = and (total, src[i]);
    dst[i] = swap;
  }

  /* Scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_BOOL, MPI_BAND, MPI_COMM_WORLD);

  /* Shift incl to the right into excl, putting identity on far left */
  left = Self - 1;
  right = Self + 1;
  if (Self == 0) {
    left = MPI_PROC_NULL;	/* do not receive from anyone */
    excl = TRUE;		/* and preset the result */
  }
  if (Self == NumProcs - 1) {
    right = MPI_PROC_NULL;	/* do not send to anyone */
  }
  MPI_Sendrecv (&incl, 1, MPI_BOOL, right, and_sub_tag,
		&excl, 1, MPI_BOOL, left, and_sub_tag,
		CommShift, &status);

  /* Combine result with partial scan */
  for (i = 0; i < n_here; i++) {
    dst[i] = and (dst[i], excl);
  }
  LOG_STOP (and_sub, nelts);
}

unsigned int and_sub_inplace (void)
{
  return INPLACE_1;
}

int and_sub_scratch (int nelts)
{
  return 0;
}

void ior_sub (vec_p dst_v, vec_p src_v, int nelts, vec_p scratch_v)
{
  cvl_bool *dst, *src;
  MPI_Status status;
  cvl_bool total, incl, excl, swap;
  int n_here, left, right, i;

  LOG_START (ior_sub, nelts);
  dst = (cvl_bool *) (Mem + dst_v);
  src = (cvl_bool *) (Mem + src_v);
  n_here = _num_here (nelts);
  total = FALSE;

  /* Serial exclusive scan from src to dst on each processor */
  for (i = 0; i < n_here; i++) {
    swap = total;
    total = ior (total, src[i]);
    dst[i] = swap;
  }

  /* Scan across total into incl */
  MPI_Scan (&total, &incl, 1, MPI_BOOL, MPI_BOR, MPI_COMM_WORLD);

  /* Shift incl to the right into excl, putting identity on far left */
  left = Self - 1;
  right = Self + 1;
  if (Self == 0) {
    left = MPI_PROC_NULL;	/* do not receive from anyone */
    excl = FALSE;		/* and preset the result */
  }
  if (Self == NumProcs - 1) {
    right = MPI_PROC_NULL;	/* do not send to anyone */
  }
  MPI_Sendrecv (&incl, 1, MPI_BOOL, right, ior_sub_tag,
		&excl, 1, MPI_BOOL, left, ior_sub_tag,
		CommShift, &status);

  /* Combine result with partial scan */
  for (i = 0; i < n_here; i++) {
    dst[i] = ior (dst[i], excl);
  }
  LOG_STOP (ior_sub, nelts);
}

unsigned int ior_sub_inplace (void)
{
  return INPLACE_1;
}

int ior_sub_scratch (int nelts)
{
  return 0;
}



/* ------------------------ Segmented Scan --------------------------- */


/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_add_sez;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_add_sez (int *in, int *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_int = *in;
  /* *inout = (Segment_Here) ? *in : add(*in, *inout); */
  if (!Segment_Here) {
    *inout = add (*in, *inout);
  }
}

void add_sez (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  int *dst, *src;
  int *segd, *segment;
  int swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (add_sez, nelts);
  if (nsegs == 1) {
    add_suz (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = 0;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = 0;
      }
      swap = result;
      result = add (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_INT,
	      MPI_op_add_sez, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = 0;
    } else {
      total_before_us = _result_of_seg_scan_int;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = add (dst[i], total_before_us);
    }
  }
  LOG_STOP (add_sez, nelts);
}

unsigned int add_sez_inplace (void)
{
  return INPLACE_1;
}

int add_sez_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_mul_sez;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_mul_sez (int *in, int *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_int = *in;
  /* *inout = (Segment_Here) ? *in : mul(*in, *inout); */
  if (!Segment_Here) {
    *inout = mul (*in, *inout);
  }
}

void mul_sez (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  int *dst, *src;
  int *segd, *segment;
  int swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (mul_sez, nelts);
  if (nsegs == 1) {
    mul_suz (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = 1;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = 1;
      }
      swap = result;
      result = mul (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_INT,
	      MPI_op_mul_sez, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = 1;
    } else {
      total_before_us = _result_of_seg_scan_int;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = mul (dst[i], total_before_us);
    }
  }
  LOG_STOP (mul_sez, nelts);
}

unsigned int mul_sez_inplace (void)
{
  return INPLACE_1;
}

int mul_sez_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_max_sez;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_max_sez (int *in, int *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_int = *in;
  /* *inout = (Segment_Here) ? *in : max(*in, *inout); */
  if (!Segment_Here) {
    *inout = max (*in, *inout);
  }
}

void max_sez (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  int *dst, *src;
  int *segd, *segment;
  int swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (max_sez, nelts);
  if (nsegs == 1) {
    max_suz (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = INT_MIN;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = INT_MIN;
      }
      swap = result;
      result = max (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_INT,
	      MPI_op_max_sez, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = INT_MIN;
    } else {
      total_before_us = _result_of_seg_scan_int;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = max (dst[i], total_before_us);
    }
  }
  LOG_STOP (max_sez, nelts);
}

unsigned int max_sez_inplace (void)
{
  return INPLACE_1;
}

int max_sez_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_min_sez;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_min_sez (int *in, int *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_int = *in;
  /* *inout = (Segment_Here) ? *in : min(*in, *inout); */
  if (!Segment_Here) {
    *inout = min (*in, *inout);
  }
}

void min_sez (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  int *dst, *src;
  int *segd, *segment;
  int swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (min_sez, nelts);
  if (nsegs == 1) {
    min_suz (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = INT_MAX;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = INT_MAX;
      }
      swap = result;
      result = min (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_INT,
	      MPI_op_min_sez, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = INT_MAX;
    } else {
      total_before_us = _result_of_seg_scan_int;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = min (dst[i], total_before_us);
    }
  }
  LOG_STOP (min_sez, nelts);
}

unsigned int min_sez_inplace (void)
{
  return INPLACE_1;
}

int min_sez_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_and_sez;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_and_sez (int *in, int *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_int = *in;
  /* *inout = (Segment_Here) ? *in : bnd(*in, *inout); */
  if (!Segment_Here) {
    *inout = bnd (*in, *inout);
  }
}

void and_sez (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  int *dst, *src;
  int *segd, *segment;
  int swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (and_sez, nelts);
  if (nsegs == 1) {
    and_suz (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = ~0;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = ~0;
      }
      swap = result;
      result = bnd (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_INT,
	      MPI_op_and_sez, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = ~0;
    } else {
      total_before_us = _result_of_seg_scan_int;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = bnd (dst[i], total_before_us);
    }
  }
  LOG_STOP (and_sez, nelts);
}

unsigned int and_sez_inplace (void)
{
  return INPLACE_1;
}

int and_sez_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_ior_sez;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_ior_sez (int *in, int *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_int = *in;
  /* *inout = (Segment_Here) ? *in : bor(*in, *inout); */
  if (!Segment_Here) {
    *inout = bor (*in, *inout);
  }
}

void ior_sez (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  int *dst, *src;
  int *segd, *segment;
  int swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (ior_sez, nelts);
  if (nsegs == 1) {
    ior_suz (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = 0;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = 0;
      }
      swap = result;
      result = bor (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_INT,
	      MPI_op_ior_sez, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = 0;
    } else {
      total_before_us = _result_of_seg_scan_int;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = bor (dst[i], total_before_us);
    }
  }
  LOG_STOP (ior_sez, nelts);
}

unsigned int ior_sez_inplace (void)
{
  return INPLACE_1;
}

int ior_sez_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_xor_sez;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_xor_sez (int *in, int *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_int = *in;
  /* *inout = (Segment_Here) ? *in : xor(*in, *inout); */
  if (!Segment_Here) {
    *inout = xor (*in, *inout);
  }
}

void xor_sez (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  int *dst, *src;
  int *segd, *segment;
  int swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (xor_sez, nelts);
  if (nsegs == 1) {
    xor_suz (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (int *) (Mem + dst_v);
    src = (int *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = 0;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = 0;
      }
      swap = result;
      result = xor (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_INT,
	      MPI_op_xor_sez, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = 0;
    } else {
      total_before_us = _result_of_seg_scan_int;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = xor (dst[i], total_before_us);
    }
  }
  LOG_STOP (xor_sez, nelts);
}

unsigned int xor_sez_inplace (void)
{
  return INPLACE_1;
}

int xor_sez_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_add_sed;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_add_sed (double *in, double *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_double = *in;
  /* *inout = (Segment_Here) ? *in : add(*in, *inout); */
  if (!Segment_Here) {
    *inout = add (*in, *inout);
  }
}

void add_sed (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  double *dst, *src;
  int *segd, *segment;
  double swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (add_sed, nelts);
  if (nsegs == 1) {
    add_sud (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = 0.0;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = 0.0;
      }
      swap = result;
      result = add (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_DOUBLE,
	      MPI_op_add_sed, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = 0.0;
    } else {
      total_before_us = _result_of_seg_scan_double;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = add (dst[i], total_before_us);
    }
  }
  LOG_STOP (add_sed, nelts);
}

unsigned int add_sed_inplace (void)
{
  return INPLACE_1;
}

int add_sed_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_mul_sed;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_mul_sed (double *in, double *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_double = *in;
  /* *inout = (Segment_Here) ? *in : mul(*in, *inout); */
  if (!Segment_Here) {
    *inout = mul (*in, *inout);
  }
}

void mul_sed (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  double *dst, *src;
  int *segd, *segment;
  double swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (mul_sed, nelts);
  if (nsegs == 1) {
    mul_sud (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = 1.0;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = 1.0;
      }
      swap = result;
      result = mul (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_DOUBLE,
	      MPI_op_mul_sed, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = 1.0;
    } else {
      total_before_us = _result_of_seg_scan_double;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = mul (dst[i], total_before_us);
    }
  }
  LOG_STOP (mul_sed, nelts);
}

unsigned int mul_sed_inplace (void)
{
  return INPLACE_1;
}

int mul_sed_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_max_sed;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_max_sed (double *in, double *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_double = *in;
  /* *inout = (Segment_Here) ? *in : max(*in, *inout); */
  if (!Segment_Here) {
    *inout = max (*in, *inout);
  }
}

void max_sed (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  double *dst, *src;
  int *segd, *segment;
  double swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (max_sed, nelts);
  if (nsegs == 1) {
    max_sud (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = -DBL_MAX;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = -DBL_MAX;
      }
      swap = result;
      result = max (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_DOUBLE,
	      MPI_op_max_sed, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = -DBL_MAX;
    } else {
      total_before_us = _result_of_seg_scan_double;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = max (dst[i], total_before_us);
    }
  }
  LOG_STOP (max_sed, nelts);
}

unsigned int max_sed_inplace (void)
{
  return INPLACE_1;
}

int max_sed_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_min_sed;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_min_sed (double *in, double *inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_double = *in;
  /* *inout = (Segment_Here) ? *in : min(*in, *inout); */
  if (!Segment_Here) {
    *inout = min (*in, *inout);
  }
}

void min_sed (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  double *dst, *src;
  int *segd, *segment;
  double swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (min_sed, nelts);
  if (nsegs == 1) {
    min_sud (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (double *) (Mem + dst_v);
    src = (double *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = DBL_MAX;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = DBL_MAX;
      }
      swap = result;
      result = min (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_DOUBLE,
	      MPI_op_min_sed, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = DBL_MAX;
    } else {
      total_before_us = _result_of_seg_scan_double;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = min (dst[i], total_before_us);
    }
  }
  LOG_STOP (min_sed, nelts);
}

unsigned int min_sed_inplace (void)
{
  return INPLACE_1;
}

int min_sed_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_and_seb;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_and_seb (cvl_bool * in, cvl_bool * inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_cvl_bool = *in;
  /* *inout = (Segment_Here) ? *in : and(*in, *inout); */
  if (!Segment_Here) {
    *inout = and (*in, *inout);
  }
}

void and_seb (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  cvl_bool *dst, *src;
  int *segd, *segment;
  cvl_bool swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (and_seb, nelts);
  if (nsegs == 1) {
    and_sub (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = TRUE;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = TRUE;
      }
      swap = result;
      result = and (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_BOOL,
	      MPI_op_and_seb, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = TRUE;
    } else {
      total_before_us = _result_of_seg_scan_cvl_bool;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = and (dst[i], total_before_us);
    }
  }
  LOG_STOP (and_seb, nelts);
}

unsigned int and_seb_inplace (void)
{
  return INPLACE_1;
}

int and_seb_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_ior_seb;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_ior_seb (cvl_bool * in, cvl_bool * inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_cvl_bool = *in;
  /* *inout = (Segment_Here) ? *in : ior(*in, *inout); */
  if (!Segment_Here) {
    *inout = ior (*in, *inout);
  }
}

void ior_seb (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  cvl_bool *dst, *src;
  int *segd, *segment;
  cvl_bool swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (ior_seb, nelts);
  if (nsegs == 1) {
    ior_sub (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = FALSE;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = FALSE;
      }
      swap = result;
      result = ior (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_BOOL,
	      MPI_op_ior_seb, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = FALSE;
    } else {
      total_before_us = _result_of_seg_scan_cvl_bool;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = ior (dst[i], total_before_us);
    }
  }
  LOG_STOP (ior_seb, nelts);
}

unsigned int ior_seb_inplace (void)
{
  return INPLACE_1;
}

int ior_seb_scratch (int nelts, int nsegs)
{
  return 0;
}

/* Declare a handle to be bound to our new scan operation
 */
static MPI_Op MPI_op_xor_seb;

/* The MPI scan function (only defined for len == 1).  The value passed
 * from our left (== result of exclusive scan) is stored in
 * _result_of_seg_scan_TYPE.  The global variable Segment_Here can be
 * set to create a segmented scan, but this function is then not
 * commutative.	 Note that this function is NOT called for the first
 * processor, since MPI always assumes an inclusive scan.  Sigh...
 */
static void op_xor_seb (cvl_bool * in, cvl_bool * inout, int len, MPI_Datatype * dptr)
{
  _result_of_seg_scan_cvl_bool = *in;
  /* *inout = (Segment_Here) ? *in : xor(*in, *inout); */
  if (!Segment_Here) {
    *inout = xor (*in, *inout);
  }
}

void xor_seb (dst_v, src_v, segd_v, nelts, nsegs, scratch_v)
  vec_p dst_v, src_v, segd_v;
  int nelts, nsegs;
  vec_p scratch_v;
{
  cvl_bool *dst, *src;
  int *segd, *segment;
  cvl_bool swap, result, total_before_us;
  int first_seg, nelts_here, current_seg, i;

  LOG_START (xor_seb, nelts);
  if (nsegs == 1) {
    xor_sub (dst_v, src_v, nelts, scratch_v);
  } else {
    nelts_here = _num_here (nelts);
    dst = (cvl_bool *) (Mem + dst_v);
    src = (cvl_bool *) (Mem + src_v);
    segd = (int *) (Mem + segd_v);
    segment = SegdSegment (segd, nelts, nsegs);
    first_seg = *(SegdBefore (segd, nelts, nsegs));
    current_seg = first_seg;
    Segment_Here = FALSE;
    result = FALSE;

    /* Serial segmented exclusive scan.	 Set a flag if we hit any
     * segment boundaries. */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != current_seg) {
	current_seg = segment[i];
	Segment_Here = TRUE;
	result = FALSE;
      }
      swap = result;
      result = xor (result, src[i]);
      dst[i] = swap;
    }

    /* Do a segmented scan */
    MPI_Scan (&result, &total_before_us, 1, MPI_BOOL,
	      MPI_op_xor_seb, MPI_COMM_WORLD);

    if (Self == 0) {
      total_before_us = FALSE;
    } else {
      total_before_us = _result_of_seg_scan_cvl_bool;
    }

    /* Combine this value with everything in the first segment */
    for (i = 0; i < nelts_here; i++) {
      if (segment[i] != first_seg) {
	break;
      }
      dst[i] = xor (dst[i], total_before_us);
    }
  }
  LOG_STOP (xor_seb, nelts);
}

unsigned int xor_seb_inplace (void)
{
  return INPLACE_1;
}

int xor_seb_scratch (int nelts, int nsegs)
{
  return 0;
}



void _setup_scan_ops (void)
{
  MPI_Op_create ((MPI_User_function *) op_add_sez, FALSE, &MPI_op_add_sez);
  MPI_Op_create ((MPI_User_function *) op_mul_sez, FALSE, &MPI_op_mul_sez);
  MPI_Op_create ((MPI_User_function *) op_max_sez, FALSE, &MPI_op_max_sez);
  MPI_Op_create ((MPI_User_function *) op_min_sez, FALSE, &MPI_op_min_sez);
  MPI_Op_create ((MPI_User_function *) op_and_sez, FALSE, &MPI_op_and_sez);
  MPI_Op_create ((MPI_User_function *) op_ior_sez, FALSE, &MPI_op_ior_sez);
  MPI_Op_create ((MPI_User_function *) op_xor_sez, FALSE, &MPI_op_xor_sez);
  MPI_Op_create ((MPI_User_function *) op_add_sed, FALSE, &MPI_op_add_sed);
  MPI_Op_create ((MPI_User_function *) op_mul_sed, FALSE, &MPI_op_mul_sed);
  MPI_Op_create ((MPI_User_function *) op_max_sed, FALSE, &MPI_op_max_sed);
  MPI_Op_create ((MPI_User_function *) op_min_sed, FALSE, &MPI_op_min_sed);
  MPI_Op_create ((MPI_User_function *) op_and_seb, FALSE, &MPI_op_and_seb);
  MPI_Op_create ((MPI_User_function *) op_ior_seb, FALSE, &MPI_op_ior_seb);
  MPI_Op_create ((MPI_User_function *) op_xor_seb, FALSE, &MPI_op_xor_seb);

  MPI_Op_create ((MPI_User_function *) op_cpy_int, FALSE, &MPI_op_cpy_int);
  MPI_Op_create ((MPI_User_function *) op_cpy_double, FALSE,
		 &MPI_op_cpy_double);
  MPI_Op_create ((MPI_User_function *) op_cpy_cvl_bool, FALSE,
		 &MPI_op_cpy_cvl_bool);

  MPI_Op_create ((MPI_User_function *) op_cpy_two_int, FALSE,
		 &MPI_op_cpy_two_int);
}


void _exclusive_max_scan (int *index_p)
{
  int left, right, tmp;
  MPI_Status status;

  /* Inclusive max-scan across index into tmp */
  MPI_Scan (index_p, &tmp, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);

  /* Shift tmp right into *index_p to make an exclusive max-scan, with
   * identity 0 in processor 0. */
  left = Self - 1;
  right = Self + 1;
  if (Self == 0) {
    left = MPI_PROC_NULL;	/* don't receive from anyone */
    *index_p = 0;		/* and preset the result */
  }
  if (Self == NumProcs - 1) {
    right = MPI_PROC_NULL;	/* don't send to anyone */
  }
  MPI_Sendrecv (&tmp, 1, MPI_INT, right, CVL_max_tag,
		index_p, 1, MPI_INT, left, CVL_max_tag,
		CommShift, &status);
}
