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


/* Globals defined in n_internal.c.  These are used by CMAM message handlers
 */
#ifdef __STDC__
extern volatile int num_rcvd;
extern volatile int max_rcvd;
#else
extern int num_rcvd;
extern int max_rcvd;
#endif


/* Macros defining the message-passing system used.  These expand into
 * calls to the CMAML active messages library.
 */
#define Poll()								\
  CMAML_poll ()


/* Never use two of these in succession without a global synchronization
 * of some sort (sync_with_nodes, scan, reduction, bc_to_nodes, etc.) in
 * between.  Otherwise there can be a race on the global_or flag.
 */
#define Wait()								\
    CMMD_set_global_or (0);						\
    while (CMMD_get_global_or ()) {					\
      CMAML_poll ();			/* until everyone has sent */	\
    }									\
    while (CMMD_reduce_int ((num_sent-num_rcvd), CMMD_combiner_add)) {	\
      CMAML_poll ();							\
    }					/* loop until all received */	\
    CMMD_set_global_or (1);		/* reset it for next time */	\
  }					/* closes the prep block */


/* This is your basic one-way send
 */
#define Send(TYPE, PROC, DST, DATA)					\
    {									\
      TYPE *addr = DST;							\
      TYPE data = DATA;							\
      if (PROC == thisproc) {						\
	*addr = data;							\
      } else {								\
	CMAML_rpc (PROC, Glue (TYPE,_send), addr, data);		\
	num_sent++;							\
      }									\
    }


/* This is a request-reply protocol to get values, rather than send
 * them.
 */
#define Get(TYPE, PROC, DST, SRC)					\
    {									\
      TYPE *to = DST;							\
      TYPE *from = SRC;							\
      if (PROC == thisproc) {						\
	*to = *from;							\
      } else {								\
	CMAML_request (PROC, Glue (TYPE,_get), to, thisproc, from);	\
	num_sent++;							\
      }									\
    }



/* Small function names */
#define max(i,j)	((i) > (j) ? (i) : (j))
#define min(i,j)	((i) < (j) ? (i) : (j))
#define lshift(i,j)	((i) << (j))
#define rshift(i,j)	((i) >> (j))
#define plus(i,j)	((i) + (j))
#define minus(i,j)	((i) - (j))
#define times(i,j)	((i) * (j))
#define divide(i,j)	((i) / (j))
#define mod(i,j)	((i) % (j))
#define gt(i,j)		((i) > (j))
#define lt(i,j)		((i) < (j))
#define eq(i,j)		((i) == (j))
#define neq(i,j)	((i) != (j))
#define leq(i,j)	((i) <= (j))
#define geq(i,j)	((i) >= (j))

#ifdef FIBRNG
extern int get_rn_int();
extern int *genptr;
/* Extra "& 0xfffffff" is probably gratuitous, but it's cheap */
#define cvlrand(i)	((get_rn_int(genptr) & 0xfffffff) % i)
#else
extern long random();
#define cvlrand(i)      ((((int) random()) & 0xfffffff) % i)
#endif

#define or(i,j)		((i) || (j))		 /* logical or */
#define bor(i,j)	((i) | (j))		 /* bitwise or */
#define and(i,j)	((i) && (j))
#define band(i,j)	((i) & (j))
#define not(i)		(! (i))
#define bnot(i)		(~ (i))
#define xor(i,j)	((i) ^ (j))

#define neg(i)		(-(i))
#define ident(i)	(i)
#define notnot(i)	(!!(i))

#define select(i,j,k)	((i) ? (j) : (k))

#define d_to_z(x)	((int) (x))
#define b_to_z(x)	((int) (x))
#define z_to_d(x)	((double) (x))
#define z_to_b(x)	((cvl_bool) notnot(x))

#define cvl_round(x)	((int) rint(x))
#define cvl_floor(x)	((int) floor(x))
#define cvl_ceil(x)	((int) ceil(x))


/* Macros to manipulate the top bit of segment counts, which are used as
 * an "empty segment" flag.
 */
#define Empty_Seg(x) (x & 0x80000000)
#define Set_Empty_Seg(x) (x |= 0x80000000)
#define Actual_Value(x) (int) (x & 0x7fffffff)


#if defined(__STDC__)
#  define P_(s) s
#else
#  define P_(s) ()
#endif

void int_send P_((int *posn, int data));
void cvl_bool_send P_((cvl_bool *posn, cvl_bool data));
void double_send P_((double *posn, double data));
void int_get P_((int *dstposn, int proc, int *srcposn));
void cvl_bool_get P_((cvl_bool *dstposn, int proc, cvl_bool *srcposn));
void double_get P_((double *dstposn, int proc, double *srcposn));

#define bufargs P_((int thisproc, int logprocs, int nprocs, int *buf))

void max_wuz_ bufargs;
void max_wud_ bufargs;
void min_wuz_ bufargs;
void min_wud_ bufargs;
void add_wuz_ bufargs;
void add_wud_ bufargs;
void sub_wuz_ bufargs;
void sub_wud_ bufargs;
void mul_wuz_ bufargs;
void mul_wud_ bufargs;
void div_wuz_ bufargs;
void div_wud_ bufargs;
void grt_wuz_ bufargs;
void grt_wud_ bufargs;
void les_wuz_ bufargs;
void les_wud_ bufargs;
void geq_wuz_ bufargs;
void geq_wud_ bufargs;
void leq_wuz_ bufargs;
void leq_wud_ bufargs;
void lsh_wuz_ bufargs;
void rsh_wuz_ bufargs;
void mod_wuz_ bufargs;
void rnd_wuz_ bufargs;
void eql_wub_ bufargs;
void eql_wuz_ bufargs;
void eql_wud_ bufargs;
void neq_wub_ bufargs;
void neq_wuz_ bufargs;
void neq_wud_ bufargs;
void sel_wuz_ bufargs;
void sel_wub_ bufargs;
void sel_wud_ bufargs;
void sel_wuz_ bufargs;
void not_wub_ bufargs;
void xor_wub_ bufargs;
void ior_wub_ bufargs;
void and_wub_ bufargs;
void ior_wuz_ bufargs;
void and_wuz_ bufargs;
void not_wuz_ bufargs;
void xor_wuz_ bufargs;
void int_wud_ bufargs;
void int_wub_ bufargs;
void dbl_wuz_ bufargs;
void boo_wuz_ bufargs;
void flr_wud_ bufargs;
void cei_wud_ bufargs;
void trn_wud_ bufargs;
void rou_wud_ bufargs;
void rou_wud_ bufargs;
void exp_wud_ bufargs;
void log_wud_ bufargs;
void sqt_wud_ bufargs;
void sin_wud_ bufargs;
void cos_wud_ bufargs;
void tan_wud_ bufargs;
void asn_wud_ bufargs;
void acs_wud_ bufargs;
void atn_wud_ bufargs;
void snh_wud_ bufargs;
void csh_wud_ bufargs;
void tnh_wud_ bufargs;
void cpy_wuz_ bufargs;
void cpy_wub_ bufargs;
void cpy_wud_ bufargs;
void cpy_wus_ bufargs;
void add_suz_ bufargs;
void add_sud_ bufargs;
void mul_suz_ bufargs;
void mul_sud_ bufargs;
void min_suz_ bufargs;
void min_sud_ bufargs;
void max_suz_ bufargs;
void max_sud_ bufargs;
void and_sub_ bufargs;
void and_suz_ bufargs;
void ior_sub_ bufargs;
void ior_suz_ bufargs;
void xor_sub_ bufargs;
void xor_suz_ bufargs;
void add_sez_ bufargs;
void add_sed_ bufargs;
void mul_sez_ bufargs;
void mul_sed_ bufargs;
void min_sez_ bufargs;
void min_sed_ bufargs;
void max_sez_ bufargs;
void max_sed_ bufargs;
void and_seb_ bufargs;
void and_sez_ bufargs;
void ior_seb_ bufargs;
void ior_sez_ bufargs;
void xor_seb_ bufargs;
void xor_sez_ bufargs;
void add_ruz_ bufargs;
void add_rud_ bufargs;
void mul_ruz_ bufargs;
void mul_rud_ bufargs;
void min_ruz_ bufargs;
void min_rud_ bufargs;
void max_ruz_ bufargs;
void max_rud_ bufargs;
void and_rub_ bufargs;
void and_ruz_ bufargs;
void ior_rub_ bufargs;
void ior_ruz_ bufargs;
void xor_rub_ bufargs;
void xor_ruz_ bufargs;
void add_rez_ bufargs;
void add_red_ bufargs;
void mul_rez_ bufargs;
void mul_red_ bufargs;
void min_rez_ bufargs;
void min_red_ bufargs;
void max_rez_ bufargs;
void max_red_ bufargs;
void and_reb_ bufargs;
void and_rez_ bufargs;
void ior_reb_ bufargs;
void ior_rez_ bufargs;
void xor_reb_ bufargs;
void xor_rez_ bufargs;
void ext_vuz_ bufargs;
void ext_vub_ bufargs;
void ext_vud_ bufargs;
void ext_vez_ bufargs;
void ext_veb_ bufargs;
void ext_ved_ bufargs;
void rep_vuz_ bufargs;
void rep_vub_ bufargs;
void rep_vud_ bufargs;
void rep_vez_ bufargs;
void rep_veb_ bufargs;
void rep_ved_ bufargs;
void dis_vuz_ bufargs;
void dis_vub_ bufargs;
void dis_vud_ bufargs;
void dis_vez_ bufargs;
void dis_veb_ bufargs;
void dis_ved_ bufargs;
void smp_puz_ bufargs;
void smp_pub_ bufargs;
void smp_pud_ bufargs;
void smp_pez_ bufargs;
void smp_peb_ bufargs;
void smp_ped_ bufargs;
void bck_puz_ bufargs;
void bck_pub_ bufargs;
void bck_pud_ bufargs;
void bck_pez_ bufargs;
void bck_peb_ bufargs;
void bck_ped_ bufargs;
void fpm_puz_ bufargs;
void fpm_pub_ bufargs;
void fpm_pud_ bufargs;
void fpm_pez_ bufargs;
void fpm_peb_ bufargs;
void fpm_ped_ bufargs;
void bfp_puz_ bufargs;
void bfp_pub_ bufargs;
void bfp_pud_ bufargs;
void bfp_pez_ bufargs;
void bfp_peb_ bufargs;
void bfp_ped_ bufargs;
void dpe_puz_ bufargs;
void dpe_pub_ bufargs;
void dpe_pud_ bufargs;
void dpe_pez_ bufargs;
void dpe_peb_ bufargs;
void dpe_ped_ bufargs;
void dfp_puz_ bufargs;
void dfp_pub_ bufargs;
void dfp_pud_ bufargs;
void dfp_pez_ bufargs;
void dfp_peb_ bufargs;
void dfp_ped_ bufargs;
void tgt_fos_ bufargs;
void mke_fov_ bufargs;
void len_fos_ bufargs;
void alo_foz_ bufargs;
void fre_fov_ bufargs;
void mov_fov_ bufargs;
void v2c_fuz_ bufargs;
void v2c_fub_ bufargs;
void v2c_fud_ bufargs;
void c2v_fuz_ bufargs;
void c2v_fub_ bufargs;
void c2v_fud_ bufargs;
void ind_luz_ bufargs;
void ind_lez_ bufargs;
void pk1_luv_ bufargs;
void pk2_luz_ bufargs;
void pk2_lub_ bufargs;
void pk2_lud_ bufargs;
void rku_luz_ bufargs;
void rkd_luz_ bufargs;
void rku_lez_ bufargs;
void rkd_lez_ bufargs;
void rku_lud_ bufargs;
void rkd_lud_ bufargs;
void rku_led_ bufargs;
void rkd_led_ bufargs;
void rnd_foz_ bufargs;
void CVL_quit_ bufargs;
