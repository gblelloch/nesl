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

#include <assert.h>
#include <cm/cmmd.h>
#include <cvl.h>
#include "cm5cvl.h"
#include "host.h"


int Nprocs;
int Logprocs;


void CVL_quit ()
{
  Func0 (CVL_qui);
}


/* Assumes IEEE-754; exponent stored XS127.   Casts number to float,
 * this normalizes number; grab exponent.  Sippy thought this up; don't
 * blame me...
 */
static int log2 (n)
int n;
{
  int answer;

  if (n <= 1)
    answer = 0;
  else {
    double d = n;
    unsigned short *s = ((unsigned short *) &d);
    answer = ((s[0] >> 4) & 0x7ff) - 0x3ff;
  }
  return answer;
}


void CVL_init ()
{
  CMMD_enable ();		/* download the node code, start it running */
  CMMD_set_global_or (0);	/* host makes no contribution */
  Nprocs = CMMD_partition_size ();
  Logprocs = log2 (Nprocs);
}
