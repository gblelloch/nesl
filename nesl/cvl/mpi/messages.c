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


#include <stdlib.h>
#include <mpi.h>
#include <cvl.h>
#include "mpicvl.h"

/* Declarations of global variables, and most of the routines that write
 * them.  See messages.h for definitions.
 */

/* BG_SEND determines whether the code will use additional buffers, in
 * the expectation that the machine has the ability to send buffers in
 * the background.  We could #ifdef out some code with BG_RECV too, but
 * it would need a bit more rewriting than BG_SEND (and we only need to
 * do BG_SEND to get things working on the CM-5, hopefully).
 */

MPI_Comm CommShift;
int NumProcs;
int Self;
maxalign *Mem = NULL;
int Segment_Here;

maxalign *Usr_send[MAX_PROC];	/* Send buffer available to user */
maxalign *Usr_recv[MAX_PROC];	/* Receive buffer available to user */
maxalign *Sys_recv[MAX_PROC];	/* Receive buffer undergoing DMA */
MPI_Request Recv_req[MAX_PROC];	/* Outstanding nonblocking requests */

#ifdef BG_SEND
maxalign *Sys_send[MAX_PROC];	/* Send buffer undergoing DMA */
MPI_Request Send_req[MAX_PROC];	/* Outstanding nonblocking sends */

#endif

int Send_ctr[MAX_PROC];		/* Number of elements in Usr_send */


/* Get the rank of this processor, and the total number of processors,
 * and set the two global "const" variables Self and NumProcs.
 */
void _set_self_and_numprocs (void)
{
  MPI_Comm_dup (MPI_COMM_WORLD, &CommShift);
  MPI_Comm_rank (MPI_COMM_WORLD, &Self);
  MPI_Comm_size (MPI_COMM_WORLD, &NumProcs);
}


void _setup_buffers (void)
{
  int proc;

  for (proc = 0; proc < NumProcs; proc++) {
    Usr_send[proc] = (maxalign *) malloc (BUF_SIZE);
    Usr_recv[proc] = (maxalign *) malloc (BUF_SIZE);
    Sys_recv[proc] = (maxalign *) malloc (BUF_SIZE);
    Recv_req[proc] = NULL;

#ifdef BG_SEND
    Sys_send[proc] = (maxalign *) malloc (BUF_SIZE);
    Send_req[proc] = NULL;
#endif

    Send_ctr[proc] = 0;

    /* Set up a nonblocking receive from every other processor. */
    MPI_Irecv (Sys_recv[proc], BUF_SIZE, MPI_BYTE, proc,
	       MPI_ANY_TAG, MPI_COMM_WORLD, &Recv_req[proc]);
  }
}


/* Send nbytes from Usr_send[proc] to proc, and (if we can send in the
 * background) flip the double buffers.
 */
void _send_buffer (int proc, int tag, int n_bytes)
{
#ifdef BG_SEND
  maxalign *swap;
  MPI_Status status;

  /* Wait for the Sending buffer to become free */
  MPI_Wait (&Send_req[proc], &status);
  swap = Usr_send[proc];
  Usr_send[proc] = Sys_send[proc];
  Sys_send[proc] = swap;
  MPI_Isend (Sys_send[proc], n_bytes, MPI_BYTE, proc, tag,
	     MPI_COMM_WORLD, &Send_req[proc]);
#else
   MPI_Send (Usr_send[proc], n_bytes, MPI_BYTE, proc, tag,
	      MPI_COMM_WORLD);
#endif

  Send_ctr[proc] = 0;
}


/* Send out all remaining buffers, and complete all outstanding sends.
 * Return the new value of n_sent (number of messages sent).
 */
int _flush_buffers (int tag, int elt_size, int n_sent)
{
  int proc;

#ifdef BG_SEND
  MPI_Status status[MAX_PROC];

#endif

  /* Send any partial buffers */
  for (proc = 0; proc < NumProcs; proc++) {
    if (Send_ctr[proc] != 0) {
      _send_buffer (proc, tag, Send_ctr[proc] * elt_size);
      n_sent++;
    }
  }

#ifdef BG_SEND
  /* Wait for all sends to complet.e */
  MPI_Waitall (NumProcs, Send_req, status);
#endif

  return n_sent;
}


/* Return the number of receives that have completed, and put their
 * indices into rcvd_index[], their number of bytes into rcvd_count[],
 * and their tags into rcvd_tag[].
 */
int _recv_buffers (int rcvd_index[MAX_PROC], int rcvd_count[MAX_PROC],
		    int rcvd_tag[MAX_PROC])
{
  MPI_Status status[MAX_PROC];	/* Statuses returned */
  maxalign *swap;
  int n_rcvd, proc, i;

  /* Did any of our outstanding receives complete? */
  MPI_Testsome (NumProcs, Recv_req, &n_rcvd, rcvd_index, status);

  /* If so, grab their stats and replace them with new receives. */
  for (i = 0; i < n_rcvd; i++) {
    MPI_Get_count (&status[i], MPI_BYTE, &rcvd_count[i]);
    rcvd_tag[i] = status[i].MPI_TAG;
    proc = rcvd_index[i];

    /* Make Usr_recv point at the just-received buffer. */
    swap = Usr_recv[proc];
    Usr_recv[proc] = Sys_recv[proc];
    Sys_recv[proc] = swap;
    MPI_Irecv (Sys_recv[proc], BUF_SIZE, MPI_BYTE, proc,
	       MPI_ANY_TAG, MPI_COMM_WORLD, &Recv_req[proc]);
  }
  return n_rcvd;
}




/* -------------------------- Simple Sends --------------------------- */


void _unpack_simple_intx (void *vdst, int proc, int count)
{
  int *dst;
  intx *recv_buf;
  int i;

  dst = (int *) vdst;
  recv_buf = (intx *) Usr_recv[proc];
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = recv_buf[i].value;
  }
}

void _unpack_simple_doublex (void *vdst, int proc, int count)
{
  double *dst;
  doublex *recv_buf;
  int i;

  dst = (double *) vdst;
  recv_buf = (doublex *) Usr_recv[proc];
  count = count / sizeof (doublex);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = recv_buf[i].value;
  }
}

void _unpack_simple_cvl_boolx (void *vdst, int proc, int count)
{
  cvl_bool *dst;
  cvl_boolx *recv_buf;
  int i;

  dst = (cvl_bool *) vdst;
  recv_buf = (cvl_boolx *) Usr_recv[proc];
  count = count / sizeof (cvl_boolx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = recv_buf[i].value;
  }
}



/* Receive 0 or 1 sends from every other processor, check that they have
 * the correct tag, and unpack them into dst using the given function.
 * Note that this function is not specialized for type; the unpack
 * function is instead.
 */
int _recv_simple (void *dst, int tag, void (*unpack) (void *, int, int))
{
  int rcvd_index[MAX_PROC], rcvd_count[MAX_PROC], rcvd_tag[MAX_PROC];
  int n_rcvd, i;

  n_rcvd = _recv_buffers (rcvd_index, rcvd_count, rcvd_tag);

  for (i = 0; i < n_rcvd; i++) {
    unpack (dst, rcvd_index[i], rcvd_count[i]);
  }
  return n_rcvd;
}


/* Make sure all remaining buffers are sent, received, and unpacked.
 */
void _finish_simple (void *dst, int tag, int n_sent, int n_rcvd,
		      int elt_size, void (*unpack) (void *, int, int))
{
  int local[2], global[2];

  local[0] = _flush_buffers (tag, elt_size, n_sent);
  local[1] = n_rcvd;

  /* Keep receiving until number sent == number received. */
  do {
    local[1] += _recv_simple (dst, tag, unpack);
    MPI_Allreduce (local, global, 2, MPI_INT, MPI_SUM, CommShift);
  } while (global[0] != global[1]);
}


/* ------------------------- Combining Send -------------------------- */


void _unpack_add_intx (void *vdst, int proc, int count)
{
  int *dst;
  intx *recv_buf;
  int i;

  dst = (int *) vdst;
  recv_buf = (intx *) Usr_recv[proc];
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = add (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_mul_intx (void *vdst, int proc, int count)
{
  int *dst;
  intx *recv_buf;
  int i;

  dst = (int *) vdst;
  recv_buf = (intx *) Usr_recv[proc];
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = mul (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_max_intx (void *vdst, int proc, int count)
{
  int *dst;
  intx *recv_buf;
  int i;

  dst = (int *) vdst;
  recv_buf = (intx *) Usr_recv[proc];
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = max (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_min_intx (void *vdst, int proc, int count)
{
  int *dst;
  intx *recv_buf;
  int i;

  dst = (int *) vdst;
  recv_buf = (intx *) Usr_recv[proc];
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = min (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_bnd_intx (void *vdst, int proc, int count)
{
  int *dst;
  intx *recv_buf;
  int i;

  dst = (int *) vdst;
  recv_buf = (intx *) Usr_recv[proc];
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = bnd (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_bor_intx (void *vdst, int proc, int count)
{
  int *dst;
  intx *recv_buf;
  int i;

  dst = (int *) vdst;
  recv_buf = (intx *) Usr_recv[proc];
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = bor (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_xor_intx (void *vdst, int proc, int count)
{
  int *dst;
  intx *recv_buf;
  int i;

  dst = (int *) vdst;
  recv_buf = (intx *) Usr_recv[proc];
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = xor (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_add_doublex (void *vdst, int proc, int count)
{
  double *dst;
  doublex *recv_buf;
  int i;

  dst = (double *) vdst;
  recv_buf = (doublex *) Usr_recv[proc];
  count = count / sizeof (doublex);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = add (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_mul_doublex (void *vdst, int proc, int count)
{
  double *dst;
  doublex *recv_buf;
  int i;

  dst = (double *) vdst;
  recv_buf = (doublex *) Usr_recv[proc];
  count = count / sizeof (doublex);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = mul (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_max_doublex (void *vdst, int proc, int count)
{
  double *dst;
  doublex *recv_buf;
  int i;

  dst = (double *) vdst;
  recv_buf = (doublex *) Usr_recv[proc];
  count = count / sizeof (doublex);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = max (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_min_doublex (void *vdst, int proc, int count)
{
  double *dst;
  doublex *recv_buf;
  int i;

  dst = (double *) vdst;
  recv_buf = (doublex *) Usr_recv[proc];
  count = count / sizeof (doublex);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = min (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_and_cvl_boolx (void *vdst, int proc, int count)
{
  cvl_bool *dst;
  cvl_boolx *recv_buf;
  int i;

  dst = (cvl_bool *) vdst;
  recv_buf = (cvl_boolx *) Usr_recv[proc];
  count = count / sizeof (cvl_boolx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = and (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_ior_cvl_boolx (void *vdst, int proc, int count)
{
  cvl_bool *dst;
  cvl_boolx *recv_buf;
  int i;

  dst = (cvl_bool *) vdst;
  recv_buf = (cvl_boolx *) Usr_recv[proc];
  count = count / sizeof (cvl_boolx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = ior (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}

void _unpack_xor_cvl_boolx (void *vdst, int proc, int count)
{
  cvl_bool *dst;
  cvl_boolx *recv_buf;
  int i;

  dst = (cvl_bool *) vdst;
  recv_buf = (cvl_boolx *) Usr_recv[proc];
  count = count / sizeof (cvl_boolx);

  for (i = 0; i < count; i++) {
    dst[recv_buf[i].index] = xor (dst[recv_buf[i].index],
				  recv_buf[i].value);
  }
}



/* ---------------------------- Fetches ----------------------------- */

/* We've got a request buffer in Usr_recv[proc] containing destination
 * indices saying where on the destination the TYPE elements should go,
 * and source indices saying where on the source (us) they should come
 * from.  Fill up a reply buffer with the right elements and the
 * destination indices and send it.
 * Format is [ dst1, src1, dst2, src2, ... ]
 */

/* If BUF_SIZE isn't defined its a variable: most compilers can't define
 * arrays whose size is a variable, so we have to define a new constant
 */

#ifndef BUF_SIZE
#define BUF_SIZE_MAX 16384
#else
#define BUF_SIZE_MAX BUF_SIZE
#endif


void _unpack_fetch_intx (void *vsrc, int proc, int count, int tag)
{
  int *src;
  fetch *recv_buf;
  intx send_buf[BUF_SIZE_MAX / sizeof (intx)];
  int i;

  src = (int *) vsrc;
  recv_buf = (fetch *) Usr_recv[proc];
  count = count / sizeof (fetch);

  for (i = 0; i < count; i++) {
    send_buf[i].index = recv_buf[i].dst_index;
    send_buf[i].value = src[recv_buf[i].src_index];
  }
  MPI_Send (send_buf, count * sizeof (intx), MPI_BYTE, proc,
	    Reply_To (tag), MPI_COMM_WORLD);
}

void _unpack_fetch_doublex (void *vsrc, int proc, int count, int tag)
{
  double *src;
  fetch *recv_buf;
  doublex send_buf[BUF_SIZE_MAX / sizeof (doublex)];
  int i;

  src = (double *) vsrc;
  recv_buf = (fetch *) Usr_recv[proc];
  count = count / sizeof (fetch);

  for (i = 0; i < count; i++) {
    send_buf[i].index = recv_buf[i].dst_index;
    send_buf[i].value = src[recv_buf[i].src_index];
  }
  MPI_Send (send_buf, count * sizeof (doublex), MPI_BYTE, proc,
	    Reply_To (tag), MPI_COMM_WORLD);
}

void _unpack_fetch_cvl_boolx (void *vsrc, int proc, int count, int tag)
{
  cvl_bool *src;
  fetch *recv_buf;
  cvl_boolx send_buf[BUF_SIZE_MAX / sizeof (cvl_boolx)];
  int i;

  src = (cvl_bool *) vsrc;
  recv_buf = (fetch *) Usr_recv[proc];
  count = count / sizeof (fetch);

  for (i = 0; i < count; i++) {
    send_buf[i].index = recv_buf[i].dst_index;
    send_buf[i].value = src[recv_buf[i].src_index];
  }
  MPI_Send (send_buf, count * sizeof (cvl_boolx), MPI_BYTE, proc,
	    Reply_To (tag), MPI_COMM_WORLD);
}



/* Rather than returning the total number of buffers, we return the
 * total number of *send* buffers, i.e. completed requests (fetches). */
int _recv_fetch (void *dst, void *src, int tag,
		  void (*unpack_fetch) (void *, int, int, int),
		  void (*unpack_simple) (void *, int, int))
{
  int rcvd_index[MAX_PROC], rcvd_count[MAX_PROC], rcvd_tag[MAX_PROC];
  int n_rcvd, sends_rcvd, i;

  n_rcvd = _recv_buffers (rcvd_index, rcvd_count, rcvd_tag);
  sends_rcvd = 0;

  for (i = 0; i < n_rcvd; i++) {
    if (rcvd_tag[i] == tag) {
      unpack_fetch (src, rcvd_index[i], rcvd_count[i], tag);
    } else {
      unpack_simple (dst, rcvd_index[i], rcvd_count[i]);
      sends_rcvd++;
    }
  }
  return sends_rcvd;
}


void _finish_fetch (void *dst, void *src, int tag, int n_sent, int
		 n_rcvd, void (*unpack_fetch) (void *, int, int, int),
		     void (*unpack_send) (void *, int, int))
{
  int local[2], global[2];

  local[0] = _flush_buffers (tag, sizeof (fetch), n_sent);
  local[1] = n_rcvd;

  /* Keep receiving until number sent == number received. */
  do {
    local[1] += _recv_fetch (dst, src, tag, unpack_fetch, unpack_send);
    MPI_Allreduce (local, global, 2, MPI_INT, MPI_SUM, CommShift);
  } while (global[0] != global[1]);
}


/* ------------- Send one value, keep track of max index ------------- */


void _unpack_intx_maxindx (void *vdst, int *max_index_p,
			    int proc, int count)
{
  int *dst;
  intx *recv_buf;
  int max_index, index, i;

  dst = (int *) vdst;
  recv_buf = (intx *) Usr_recv[proc];
  max_index = *max_index_p;
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    index = recv_buf[i].index;
    if (index > max_index)
      max_index = index;
    dst[recv_buf[i].index] = recv_buf[i].value;
  }
  *max_index_p = max_index;
}

void _unpack_doublex_maxindx (void *vdst, int *max_index_p,
			       int proc, int count)
{
  double *dst;
  doublex *recv_buf;
  int max_index, index, i;

  dst = (double *) vdst;
  recv_buf = (doublex *) Usr_recv[proc];
  max_index = *max_index_p;
  count = count / sizeof (doublex);

  for (i = 0; i < count; i++) {
    index = recv_buf[i].index;
    if (index > max_index)
      max_index = index;
    dst[recv_buf[i].index] = recv_buf[i].value;
  }
  *max_index_p = max_index;
}

void _unpack_cvl_boolx_maxindx (void *vdst, int *max_index_p,
				 int proc, int count)
{
  cvl_bool *dst;
  cvl_boolx *recv_buf;
  int max_index, index, i;

  dst = (cvl_bool *) vdst;
  recv_buf = (cvl_boolx *) Usr_recv[proc];
  max_index = *max_index_p;
  count = count / sizeof (cvl_boolx);

  for (i = 0; i < count; i++) {
    index = recv_buf[i].index;
    if (index > max_index)
      max_index = index;
    dst[recv_buf[i].index] = recv_buf[i].value;
  }
  *max_index_p = max_index;
}



int _recv_maxindx (void *dst, int *max_index_p, int tag,
		    void (*unpack) (void *, int *, int, int))
{
  int rcvd_index[MAX_PROC], rcvd_count[MAX_PROC], rcvd_tag[MAX_PROC];
  int n_rcvd, i;

  n_rcvd = _recv_buffers (rcvd_index, rcvd_count, rcvd_tag);

  for (i = 0; i < n_rcvd; i++) {
    unpack (dst, max_index_p, rcvd_index[i], rcvd_count[i]);
  }
  return n_rcvd;
}


void _finish_maxindx (void *dst, int tag, int n_sent, int n_rcvd,
		       int elt_size, int *max_index_p,
		       void (*unpack) (void *, int *, int, int))
{
  int local[2], global[2];

  local[0] = _flush_buffers (tag, elt_size, n_sent);
  local[1] = n_rcvd;

  /* Keep receiving until number sent == number received. */
  do {
    local[1] += _recv_maxindx (dst, max_index_p, tag, unpack);
    MPI_Allreduce (local, global, 2, MPI_INT, MPI_SUM, CommShift);
  } while (global[0] != global[1]);
}


/* ------------- Send two ints, keep track of max index ------------- */

void _unpack_twointx_maxindx (int *dst1, int *dst2, int *max_index_p,
			       int proc, int count)
{
  two_intx *recv_buf;
  int max_index, index, i;

  recv_buf = (two_intx *) Usr_recv[proc];
  max_index = *max_index_p;
  count = count / sizeof (two_intx);

  for (i = 0; i < count; i++) {
    index = recv_buf[i].index;
    if (index > max_index)
      max_index = index;
    dst1[index] = recv_buf[i].value1;
    dst2[index] = recv_buf[i].value2;
  }
  *max_index_p = max_index;
}


int _recv_twointx_maxindx (int *dst1, int *dst2, int tag,
			    int *max_index_p)
{
  int rcvd_index[MAX_PROC], rcvd_count[MAX_PROC], rcvd_tag[MAX_PROC];
  int n_rcvd, i;

  n_rcvd = _recv_buffers (rcvd_index, rcvd_count, rcvd_tag);

  for (i = 0; i < n_rcvd; i++) {
    _unpack_twointx_maxindx (dst1, dst2, max_index_p, rcvd_index[i],
			     rcvd_count[i]);
  }
  return n_rcvd;
}


void _finish_twointx_maxindx (int *dst1, int *dst2, int tag, int n_sent,
			       int n_rcvd, int *max_index_p)
{
  int local[2], global[2];

  local[0] = _flush_buffers (tag, sizeof (two_intx), n_sent);
  local[1] = n_rcvd;

  /* Keep receiving until number sent == number received. */
  do {
    local[1] += _recv_twointx_maxindx (dst1, dst2, tag, max_index_p);
    MPI_Allreduce (local, global, 2, MPI_INT, MPI_SUM, CommShift);
  } while (global[0] != global[1]);
}


/* ---------- Increment a location, keep track of max index ---------- */

void _unpack_increment (int *dst, int *max_index_p, int proc, int count)
{
  int *recv_buf;
  int max_index, index, i;

  recv_buf = (int *) Usr_recv[proc];
  max_index = *max_index_p;
  count = count / sizeof (int);

  for (i = 0; i < count; i++) {
    index = recv_buf[i];
    dst[index]++;
    if (index > max_index)
      max_index = index;
  }
  *max_index_p = max_index;
}


int _recv_increment (int *dst, int tag, int *max_index_p)
{
  int rcvd_index[MAX_PROC], rcvd_count[MAX_PROC], rcvd_tag[MAX_PROC];
  int n_rcvd, i;

  n_rcvd = _recv_buffers (rcvd_index, rcvd_count, rcvd_tag);

  for (i = 0; i < n_rcvd; i++) {
    _unpack_increment (dst, max_index_p, rcvd_index[i], rcvd_count[i]);
  }
  return n_rcvd;
}


void _finish_increment (int *dst, int tag, int n_sent, int n_rcvd,
			 int *max_index_p)
{
  int local[2], global[2];

  local[0] = _flush_buffers (tag, sizeof (int), n_sent);
  local[1] = n_rcvd;

  /* Keep receiving until number sent == number received. */
  do {
    local[1] += _recv_increment (dst, tag, max_index_p);
    MPI_Allreduce (local, global, 2, MPI_INT, MPI_SUM, CommShift);
  } while (global[0] != global[1]);
}


/* -------- Send an integer, keep track of max value recvd --------- */

void _unpack_intx_maxval (int *dst, int *max_p, int proc, int count)
{
  intx *recv_buf;
  int max_value, value, i;

  recv_buf = (intx *) Usr_recv[proc];
  max_value = *max_p;
  count = count / sizeof (intx);

  for (i = 0; i < count; i++) {
    value = recv_buf[i].value;
    if (value > max_value)
      max_value = value;
    dst[recv_buf[i].index] = value;
  }
  *max_p = max_value;
}


int _recv_intx_maxval (int *dst, int tag, int *max_p)
{
  int rcvd_index[MAX_PROC], rcvd_count[MAX_PROC], rcvd_tag[MAX_PROC];
  int n_rcvd, i;

  n_rcvd = _recv_buffers (rcvd_index, rcvd_count, rcvd_tag);

  for (i = 0; i < n_rcvd; i++) {
    _unpack_intx_maxval (dst, max_p, rcvd_index[i], rcvd_count[i]);
  }
  return n_rcvd;
}


void _finish_intx_maxval (int *dst, int *max_p, int tag, int n_sent,
			   int n_rcvd)
{
  int local[2], global[2];

  local[0] = _flush_buffers (tag, sizeof (intx), n_sent);
  local[1] = n_rcvd;

  /* Keep receiving until number sent == number received. */
  do {
    local[1] += _recv_intx_maxval (dst, tag, max_p);
    MPI_Allreduce (local, global, 2, MPI_INT, MPI_SUM, CommShift);
  } while (global[0] != global[1]);
}


/* ------------------- Send perm + 32 bit key ----------------------- */

void _unpack_t32bits (t32bits * key_dst, int *perm_dst, int proc, int
		       count)
{
  two_intx *recv_buf;
  int index, i;

  recv_buf = (two_intx *) Usr_recv[proc];
  count = count / sizeof (two_intx);

  for (i = 0; i < count; i++) {
    index = recv_buf[i].index;
    key_dst[index].word[0] = recv_buf[i].value1;
    perm_dst[index] = recv_buf[i].value2;
  }
}


int _recv_t32bits (t32bits * key_dst, int tag, int *perm_dst)
{
  int rcvd_index[MAX_PROC], rcvd_count[MAX_PROC], rcvd_tag[MAX_PROC];
  int n_rcvd, i;

  n_rcvd = _recv_buffers (rcvd_index, rcvd_count, rcvd_tag);

  for (i = 0; i < n_rcvd; i++) {
    _unpack_t32bits (key_dst, perm_dst, rcvd_index[i], rcvd_count[i]);
  }
  return n_rcvd;
}


void _finish_t32bits (t32bits * key_dst, int *perm_dst, int tag,
		       int n_sent, int n_rcvd)
{
  int local[2], global[2];

  local[0] = _flush_buffers (tag, sizeof (two_intx), n_sent);
  local[1] = n_rcvd;

  /* Keep receiving until number sent == number received. */
  do {
    local[1] += _recv_t32bits (key_dst, tag, perm_dst);
    MPI_Allreduce (local, global, 2, MPI_INT, MPI_SUM, CommShift);
  } while (global[0] != global[1]);
}


/* ------------------- Send perm + 64 bit key ----------------------- */

void _unpack_t64bits (t64bits * key_dst, int *perm_dst, int proc, int
		       count)
{
  three_intx *recv_buf;
  int index, i;

  recv_buf = (three_intx *) Usr_recv[proc];
  count = count / sizeof (three_intx);

  for (i = 0; i < count; i++) {
    index = recv_buf[i].index;
    key_dst[index].word[0] = recv_buf[i].value1;
    key_dst[index].word[1] = recv_buf[i].value2;
    perm_dst[index] = recv_buf[i].value3;
  }
}


int _recv_t64bits (t64bits * key_dst, int tag, int *perm_dst)
{
  int rcvd_index[MAX_PROC], rcvd_count[MAX_PROC], rcvd_tag[MAX_PROC];
  int n_rcvd, i;

  n_rcvd = _recv_buffers (rcvd_index, rcvd_count, rcvd_tag);

  for (i = 0; i < n_rcvd; i++) {
    _unpack_t64bits (key_dst, perm_dst, rcvd_index[i], rcvd_count[i]);
  }
  return n_rcvd;
}


void _finish_t64bits (t64bits * key_dst, int *perm_dst, int tag,
		       int n_sent, int n_rcvd)
{
  int local[2], global[2];

  local[0] = _flush_buffers (tag, sizeof (three_intx), n_sent);
  local[1] = n_rcvd;

  /* Keep receiving until number sent == number received. */
  do {
    local[1] += _recv_t64bits (key_dst, tag, perm_dst);
    MPI_Allreduce (local, global, 2, MPI_INT, MPI_SUM, CommShift);
  } while (global[0] != global[1]);
}


/* ------------------- Send perm + 96 bit key ----------------------- */

void _unpack_t96bits (t96bits * key_dst, int *perm_dst, int proc, int
		       count)
{
  four_intx *recv_buf = (four_intx *) Usr_recv[proc];
  int index, i;

  recv_buf = (four_intx *) Usr_recv[proc];
  count = count / sizeof (four_intx);

  for (i = 0; i < count; i++) {
    index = recv_buf[i].index;
    key_dst[index].word[0] = recv_buf[i].value1;
    key_dst[index].word[1] = recv_buf[i].value2;
    key_dst[index].word[2] = recv_buf[i].value3;
    perm_dst[index] = recv_buf[i].value4;
  }
}


int _recv_t96bits (t96bits * key_dst, int tag, int *perm_dst)
{
  int rcvd_index[MAX_PROC], rcvd_count[MAX_PROC], rcvd_tag[MAX_PROC];
  int n_rcvd, i;

  n_rcvd = _recv_buffers (rcvd_index, rcvd_count, rcvd_tag);

  for (i = 0; i < n_rcvd; i++) {
    _unpack_t96bits (key_dst, perm_dst, rcvd_index[i], rcvd_count[i]);
  }
  return n_rcvd;
}


void _finish_t96bits (t96bits * key_dst, int *perm_dst, int tag,
		       int n_sent, int n_rcvd)
{
  int local[2], global[2];

  local[0] = _flush_buffers (tag, sizeof (four_intx), n_sent);
  local[1] = n_rcvd;

  /* Keep receiving until number sent == number received. */
  do {
    local[1] += _recv_t96bits (key_dst, tag, perm_dst);
    MPI_Allreduce (local, global, 2, MPI_INT, MPI_SUM, CommShift);
  } while (global[0] != global[1]);
}
