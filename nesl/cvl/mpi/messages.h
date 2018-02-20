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

/* Definitions of global variables, and the routines and macros that
 * write them.	Mostly, message stuff.	Very closely linked with
 * messages.c -- don't change one file without the other!
 */

extern const int NumProcs;	/* total number of processors */
extern const int Self;		/* rank of processor (0..NumProcs-1) */
extern const maxalign *Mem;	/* ptr to vector memory */
extern int Segment_Here;	/* segment bit - set if any segments
				   begin on this processor */
extern MPI_Comm CommShift;	/* separate communicator for scan.c */


/* Results of exclusive scans -- manipulated in scan.c
 */
extern int int_result_of_excl_scan;
extern double double_result_of_excl_scan;
extern cvl_bool cvl_bool_result_of_excl_scan;


/* These global variables are accessed by macros defined later on, and
 * also by functions defined in messages.c.
 */
#ifdef BG_SEND
  extern MPI_Request Send_req[MAX_PROC];
#endif
extern MPI_Request Recv_req[MAX_PROC];

#ifdef BG_SEND
  extern maxalign *Sys_send[MAX_PROC]; /* Send buffer undergoing DMA */
#endif
extern maxalign *Usr_send[MAX_PROC]; /* Send buffer being filled */
extern maxalign *Usr_recv[MAX_PROC]; /* Receive buffer being unpacked */
extern maxalign *Sys_recv[MAX_PROC]; /* Receive buffer undergoing DMA */

extern int Send_ctr[MAX_PROC];	/* Number of "things" in Usr_send */
