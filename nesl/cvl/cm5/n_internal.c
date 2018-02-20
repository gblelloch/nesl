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

#include <cm/cmmd.h>
#include <cvl.h>
#include "cm5cvl.h"
#include "node.h"


static void * nodefunc[NUM_TAGS];
static void setup_function_pointers ();


/* Global variables shared by CMAM message senders and handlers.
 */
#ifdef __STDC__
volatile int num_rcvd;
volatile int max_rcvd;
#else
int num_rcvd;
int max_rcvd;
#endif


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


void CVL_init_ ()
{
  CMMD_enable_host ();
  CMMD_set_global_or (1);
  CMMD_node_timer_clear (0);
  CMMD_node_timer_start (0);
  CMAML_disable_interrupts ();
}


void CVL_quit_ (thisproc, logprocs, nprocs, buf)
int thisproc, logprocs, nprocs, *buf;
{
  exit (0);
}


/* Nodes sit in a loop waiting for broadcast instruction, then treat the
 * first word of the packet as a pointer to a function to execute.
 */
void main ()
{
  int thisproc, logprocs, nprocs;
  int seed = 0;
  bcbuf buf;
  void (*funcptr) ();

  CVL_init_ ();
  thisproc = CMMD_self_address ();
  nprocs = CMMD_partition_size ();
  logprocs = log2 (nprocs);
  setup_function_pointers ();
  rnd_foz_ (thisproc, logprocs, nprocs, &seed);

  while (TRUE) {
    CMMD_receive_bc_from_host (buf.d, sizeof(bcbuf));
    funcptr = (void *) nodefunc[buf.d[0]];
    (*funcptr) (thisproc, logprocs, nprocs, buf.d);
  }
}


/* Common CMMD 3.0 active message handler functions.
 */

/* Bad Things happen if chars (an old represention of cvl_bools)
 * aren't promoted to ints when passed in Active Messages.
 */

#define Make_Send(TYPE, CMAMTYPE)					\
void Glue (TYPE,_send) (posn, data)					\
TYPE *posn;								\
CMAMTYPE data;								\
{									\
  *posn = (TYPE) data;							\
  num_rcvd++;								\
}									\

Make_Send (int, int)
Make_Send (double, double)
Make_Send (cvl_bool, cvl_bool)


#define Make_Request(TYPE, CMAMTYPE)					\
void Glue (TYPE,_get) (dstposn, proc, srcposn)				\
TYPE *dstposn, *srcposn;						\
int proc;								\
{									\
  CMAML_reply (proc, Glue (TYPE,_send), dstposn, (CMAMTYPE) *srcposn);	\
}									\

Make_Request (int, int)
Make_Request (double, double)
Make_Request (cvl_bool, cvl_bool)


/* XXX replace with statically-declared array
 */
static void setup_function_pointers ()
{
  nodefunc[CVL_qui_tag] = CVL_quit_;
  nodefunc[max_wuz_tag] = max_wuz_;
  nodefunc[max_wud_tag] = max_wud_;
  nodefunc[min_wuz_tag] = min_wuz_;
  nodefunc[min_wud_tag] = min_wud_;
  nodefunc[add_wuz_tag] = add_wuz_;
  nodefunc[add_wud_tag] = add_wud_;
  nodefunc[sub_wuz_tag] = sub_wuz_;
  nodefunc[sub_wud_tag] = sub_wud_;
  nodefunc[mul_wuz_tag] = mul_wuz_;
  nodefunc[mul_wud_tag] = mul_wud_;
  nodefunc[div_wuz_tag] = div_wuz_;
  nodefunc[div_wud_tag] = div_wud_;
  nodefunc[grt_wuz_tag] = grt_wuz_;
  nodefunc[grt_wud_tag] = grt_wud_;
  nodefunc[les_wuz_tag] = les_wuz_;
  nodefunc[les_wud_tag] = les_wud_;
  nodefunc[geq_wuz_tag] = geq_wuz_;
  nodefunc[geq_wud_tag] = geq_wud_;
  nodefunc[leq_wuz_tag] = leq_wuz_;
  nodefunc[leq_wud_tag] = leq_wud_;
  nodefunc[lsh_wuz_tag] = lsh_wuz_;
  nodefunc[rsh_wuz_tag] = rsh_wuz_;
  nodefunc[mod_wuz_tag] = mod_wuz_;
  nodefunc[rnd_wuz_tag] = rnd_wuz_;
  nodefunc[eql_wub_tag] = eql_wub_;
  nodefunc[eql_wuz_tag] = eql_wuz_;
  nodefunc[eql_wud_tag] = eql_wud_;
  nodefunc[neq_wub_tag] = neq_wub_;
  nodefunc[neq_wuz_tag] = neq_wuz_;
  nodefunc[neq_wud_tag] = neq_wud_;
  nodefunc[sel_wuz_tag] = sel_wuz_;
  nodefunc[sel_wub_tag] = sel_wub_;
  nodefunc[sel_wud_tag] = sel_wud_;
  nodefunc[not_wub_tag] = not_wub_;
  nodefunc[xor_wub_tag] = xor_wub_;
  nodefunc[ior_wub_tag] = ior_wub_;
  nodefunc[and_wub_tag] = and_wub_;
  nodefunc[ior_wuz_tag] = ior_wuz_;
  nodefunc[and_wuz_tag] = and_wuz_;
  nodefunc[not_wuz_tag] = not_wuz_;
  nodefunc[xor_wuz_tag] = xor_wuz_;
  nodefunc[int_wud_tag] = int_wud_;
  nodefunc[int_wub_tag] = int_wub_;
  nodefunc[dbl_wuz_tag] = dbl_wuz_;
  nodefunc[boo_wuz_tag] = boo_wuz_;
  nodefunc[flr_wud_tag] = flr_wud_;
  nodefunc[cei_wud_tag] = cei_wud_;
  nodefunc[trn_wud_tag] = trn_wud_;
  nodefunc[rou_wud_tag] = rou_wud_;
  nodefunc[exp_wud_tag] = exp_wud_;
  nodefunc[log_wud_tag] = log_wud_;
  nodefunc[sqt_wud_tag] = sqt_wud_;
  nodefunc[sin_wud_tag] = sin_wud_;
  nodefunc[cos_wud_tag] = cos_wud_;
  nodefunc[tan_wud_tag] = tan_wud_;
  nodefunc[asn_wud_tag] = asn_wud_;
  nodefunc[acs_wud_tag] = acs_wud_;
  nodefunc[atn_wud_tag] = atn_wud_;
  nodefunc[snh_wud_tag] = snh_wud_;
  nodefunc[csh_wud_tag] = csh_wud_;
  nodefunc[tnh_wud_tag] = tnh_wud_;
  nodefunc[cpy_wuz_tag] = cpy_wuz_;
  nodefunc[cpy_wub_tag] = cpy_wub_;
  nodefunc[cpy_wud_tag] = cpy_wud_;
  nodefunc[cpy_wus_tag] = cpy_wus_;
  nodefunc[add_suz_tag] = add_suz_;
  nodefunc[add_sud_tag] = add_sud_;
  nodefunc[mul_suz_tag] = mul_sud_;
  nodefunc[mul_sud_tag] = mul_sud_;
  nodefunc[min_suz_tag] = min_suz_;
  nodefunc[min_sud_tag] = min_sud_;
  nodefunc[max_suz_tag] = max_suz_;
  nodefunc[max_sud_tag] = max_sud_;
  nodefunc[and_sub_tag] = and_sub_;
  nodefunc[and_suz_tag] = and_suz_;
  nodefunc[ior_sub_tag] = ior_sub_;
  nodefunc[ior_suz_tag] = ior_suz_;
  nodefunc[xor_sub_tag] = xor_sub_;
  nodefunc[xor_suz_tag] = xor_suz_;
  nodefunc[add_sez_tag] = add_sez_;
  nodefunc[add_sed_tag] = add_sed_;
  nodefunc[mul_sez_tag] = mul_sez_;
  nodefunc[mul_sed_tag] = mul_sed_;
  nodefunc[min_sez_tag] = min_sez_;
  nodefunc[min_sed_tag] = min_sed_;
  nodefunc[max_sez_tag] = max_sez_;
  nodefunc[max_sed_tag] = max_sed_;
  nodefunc[and_seb_tag] = and_seb_;
  nodefunc[and_sez_tag] = and_sez_;
  nodefunc[ior_seb_tag] = ior_seb_;
  nodefunc[ior_sez_tag] = ior_sez_;
  nodefunc[xor_seb_tag] = xor_seb_;
  nodefunc[xor_sez_tag] = xor_sez_;
  nodefunc[add_ruz_tag] = add_ruz_;
  nodefunc[add_rud_tag] = add_rud_;
  nodefunc[mul_ruz_tag] = mul_ruz_;
  nodefunc[mul_rud_tag] = mul_rud_;
  nodefunc[min_ruz_tag] = min_ruz_;
  nodefunc[min_rud_tag] = min_rud_;
  nodefunc[max_ruz_tag] = max_ruz_;
  nodefunc[max_rud_tag] = max_rud_;
  nodefunc[and_rub_tag] = and_rub_;
  nodefunc[and_ruz_tag] = and_ruz_;
  nodefunc[ior_rub_tag] = ior_rub_;
  nodefunc[ior_ruz_tag] = ior_ruz_;
  nodefunc[xor_rub_tag] = xor_rub_;
  nodefunc[xor_ruz_tag] = xor_ruz_;
  nodefunc[add_rez_tag] = add_rez_;
  nodefunc[add_red_tag] = add_red_;
  nodefunc[mul_rez_tag] = mul_rez_;
  nodefunc[mul_red_tag] = mul_red_;
  nodefunc[min_rez_tag] = min_rez_;
  nodefunc[min_red_tag] = min_red_;
  nodefunc[max_rez_tag] = max_rez_;
  nodefunc[max_red_tag] = max_red_;
  nodefunc[and_reb_tag] = and_reb_;
  nodefunc[and_rez_tag] = and_rez_;
  nodefunc[ior_reb_tag] = ior_reb_;
  nodefunc[ior_rez_tag] = ior_rez_;
  nodefunc[xor_reb_tag] = xor_reb_;
  nodefunc[xor_rez_tag] = xor_rez_;
  nodefunc[ext_vuz_tag] = ext_vuz_;
  nodefunc[ext_vub_tag] = ext_vub_;
  nodefunc[ext_vud_tag] = ext_vud_;
  nodefunc[ext_vez_tag] = ext_vez_;
  nodefunc[ext_veb_tag] = ext_veb_;
  nodefunc[ext_ved_tag] = ext_ved_;
  nodefunc[rep_vuz_tag] = rep_vuz_;
  nodefunc[rep_vub_tag] = rep_vub_;
  nodefunc[rep_vud_tag] = rep_vud_;
  nodefunc[rep_vez_tag] = rep_vez_;
  nodefunc[rep_veb_tag] = rep_veb_;
  nodefunc[rep_ved_tag] = rep_ved_;
  nodefunc[dis_vuz_tag] = dis_vuz_;
  nodefunc[dis_vub_tag] = dis_vub_;
  nodefunc[dis_vud_tag] = dis_vud_;
  nodefunc[dis_vez_tag] = dis_vez_;
  nodefunc[dis_veb_tag] = dis_veb_;
  nodefunc[dis_ved_tag] = dis_ved_;
  nodefunc[smp_puz_tag] = smp_puz_;
  nodefunc[smp_pub_tag] = smp_pub_;
  nodefunc[smp_pud_tag] = smp_pud_;
  nodefunc[smp_pez_tag] = smp_pez_;
  nodefunc[smp_peb_tag] = smp_peb_;
  nodefunc[smp_ped_tag] = smp_ped_;
  nodefunc[bck_puz_tag] = bck_puz_;
  nodefunc[bck_pub_tag] = bck_pub_;
  nodefunc[bck_pud_tag] = bck_pud_;
  nodefunc[bck_pez_tag] = bck_pez_;
  nodefunc[bck_peb_tag] = bck_peb_;
  nodefunc[bck_ped_tag] = bck_ped_;
  nodefunc[fpm_puz_tag] = fpm_puz_;
  nodefunc[fpm_pub_tag] = fpm_pub_;
  nodefunc[fpm_pud_tag] = fpm_pud_;
  nodefunc[fpm_pez_tag] = fpm_pez_;
  nodefunc[fpm_peb_tag] = fpm_peb_;
  nodefunc[fpm_ped_tag] = fpm_ped_;
  nodefunc[bfp_puz_tag] = bfp_puz_;
  nodefunc[bfp_pub_tag] = bfp_pub_;
  nodefunc[bfp_pud_tag] = bfp_pud_;
  nodefunc[bfp_pez_tag] = bfp_pez_;
  nodefunc[bfp_peb_tag] = bfp_peb_;
  nodefunc[bfp_ped_tag] = bfp_ped_;
  nodefunc[dpe_puz_tag] = dpe_puz_;
  nodefunc[dpe_pub_tag] = dpe_pub_;
  nodefunc[dpe_pud_tag] = dpe_pud_;
  nodefunc[dpe_pez_tag] = dpe_pez_;
  nodefunc[dpe_peb_tag] = dpe_peb_;
  nodefunc[dpe_ped_tag] = dpe_ped_;
  nodefunc[dfp_puz_tag] = dfp_puz_;
  nodefunc[dfp_pub_tag] = dfp_pub_;
  nodefunc[dfp_pud_tag] = dfp_pud_;
  nodefunc[dfp_pez_tag] = dfp_pez_;
  nodefunc[dfp_peb_tag] = dfp_peb_;
  nodefunc[dfp_ped_tag] = dfp_ped_;
  nodefunc[tgt_fos_tag] = tgt_fos_;
  nodefunc[mke_fov_tag] = mke_fov_;
  nodefunc[len_fos_tag] = len_fos_;
  nodefunc[alo_foz_tag] = alo_foz_;
  nodefunc[fre_fov_tag] = fre_fov_;
  nodefunc[mov_fov_tag] = mov_fov_;
  nodefunc[ind_luz_tag] = ind_luz_;
  nodefunc[ind_lez_tag] = ind_lez_;
  nodefunc[v2c_fuz_tag] = v2c_fuz_;
  nodefunc[v2c_fub_tag] = v2c_fub_;
  nodefunc[v2c_fud_tag] = v2c_fud_;
  nodefunc[c2v_fuz_tag] = c2v_fuz_;
  nodefunc[c2v_fub_tag] = c2v_fub_;
  nodefunc[c2v_fud_tag] = c2v_fud_;
  nodefunc[ind_luz_tag] = ind_luz_;
  nodefunc[ind_lez_tag] = ind_lez_;
  /* pk1_lev_ is never called; add_rez_ is used instead */
  nodefunc[pk1_luv_tag] = pk1_luv_;
  nodefunc[pk2_luz_tag] = pk2_luz_;
  nodefunc[pk2_lub_tag] = pk2_lub_;
  nodefunc[pk2_lud_tag] = pk2_lud_;
  nodefunc[rku_luz_tag] = rku_luz_;
  nodefunc[rkd_luz_tag] = rkd_luz_;
  nodefunc[rku_lez_tag] = rku_lez_;
  nodefunc[rkd_lez_tag] = rkd_lez_;
  nodefunc[rku_lud_tag] = rku_lud_;
  nodefunc[rkd_lud_tag] = rkd_lud_;
  nodefunc[rku_led_tag] = rku_led_;
  nodefunc[rkd_led_tag] = rkd_led_;
  nodefunc[rnd_foz_tag] = rnd_foz_;
}
