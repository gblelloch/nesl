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


/* Globals defined in h_internal.c.
 */
extern int Nprocs;
extern int Logprocs;


#define Make_Inplace(NAME, TRUE_OR_FALSE)				\
unsigned int Glue (NAME,_inplace) () { return TRUE_OR_FALSE; }


#define Make_No_Scratch(NAME)						\
int Glue (NAME,_scratch) (len) int len; { return 0; }


#define Make_No_Seg_Scratch(NAME)					\
int Glue (NAME,_scratch) (nseg, nelt) int nseg, nelt; { return 0; }


#define Func0(FUNC)							\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func1(FUNC, D1)							\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func2(FUNC, D1, D2)						\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func3(FUNC, D1, D2, D3)						\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  buf.d[3] = (int) D3;							\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func4(FUNC, D1, D2, D3, D4)					\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  buf.d[3] = (int) D3;							\
  buf.d[4] = (int) D4;							\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func5(FUNC, D1, D2, D3, D4, D5)					\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  buf.d[3] = (int) D3;							\
  buf.d[4] = (int) D4;							\
  buf.d[5] = (int) D5;							\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func6(FUNC, D1, D2, D3, D4, D5, D6)				\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  buf.d[3] = (int) D3;							\
  buf.d[4] = (int) D4;							\
  buf.d[5] = (int) D5;							\
  buf.d[6] = (int) D6;							\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func7(FUNC, D1, D2, D3, D4, D5, D6, D7)				\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  buf.d[3] = (int) D3;							\
  buf.d[4] = (int) D4;							\
  buf.d[5] = (int) D5;							\
  buf.d[6] = (int) D6;							\
  buf.d[7] = (int) D7;							\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func8(FUNC, D1, D2, D3, D4, D5, D6, D7, D8)			\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  buf.d[3] = (int) D3;							\
  buf.d[4] = (int) D4;							\
  buf.d[5] = (int) D5;							\
  buf.d[6] = (int) D6;							\
  buf.d[7] = (int) D7;							\
  buf.d[8] = (int) D8;							\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func9(FUNC, D1, D2, D3, D4, D5, D6, D7, D8, D9)			\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  buf.d[3] = (int) D3;							\
  buf.d[4] = (int) D4;							\
  buf.d[5] = (int) D5;							\
  buf.d[6] = (int) D6;							\
  buf.d[7] = (int) D7;							\
  buf.d[8] = (int) D8;							\
  buf.d[9] = (int) D9;							\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func10(FUNC, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10)		\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  buf.d[3] = (int) D3;							\
  buf.d[4] = (int) D4;							\
  buf.d[5] = (int) D5;							\
  buf.d[6] = (int) D6;							\
  buf.d[7] = (int) D7;							\
  buf.d[8] = (int) D8;							\
  buf.d[9] = (int) D9;							\
  buf.d[10] = (int) D10;						\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\
  
#define Func11(FUNC, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11)	\
  bcbuf buf;								\
  buf.d[0] = (int) Glue(FUNC,_tag);					\
  buf.d[1] = (int) D1;							\
  buf.d[2] = (int) D2;							\
  buf.d[3] = (int) D3;							\
  buf.d[4] = (int) D4;							\
  buf.d[5] = (int) D5;							\
  buf.d[6] = (int) D6;							\
  buf.d[7] = (int) D7;							\
  buf.d[8] = (int) D8;							\
  buf.d[9] = (int) D9;							\
  buf.d[10] = (int) D10;						\
  buf.d[11] = (int) D11;						\
  CMMD_bc_from_host ((void *) buf.d, sizeof(bcbuf));			\

