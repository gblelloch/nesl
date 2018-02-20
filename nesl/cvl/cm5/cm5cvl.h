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


typedef union maxalign_u { int i; double d; } maxalign;

/* uint is the CMMD shorthand for unsigned int, used often in naming
 * CMMD functions.  We typedef uint and cvl_bool so that they're the
 * same thing, letting us call function-defining macros with one
 * argument (uint) rather than two (cvl_bool, uint).
 */
typedef int uint;
#define uint_send cvl_bool_send
#define uint_get cvl_bool_get


/* Warning: if you change this structure, look at the assembly code
 * output of GCC (e.g. add_wuz) to make sure that things don't get
 * suddenly worse. 
 */
typedef union oneword_u {
  struct {
    int lo, hi;
  } ints;
  double word;
} oneword;


/* Instructions are broadcast from the master to the nodes in one of
 * these buffers.
 */
#define BUFLEN 12
typedef union bcbuf_u {
  double align;			/* force alignment on 64-bit boundary */
  int d[BUFLEN];		/* payload of a broadcast packet */
} bcbuf;


#define MAXINT	2147483647
#define MININT	-2147483647
#define MAXDBL	1.797693134862315708e+308
/* This fails: -1.7976931348623157e+308, giving
   #.EXCL::*NEGATIVE-INFINITY-DOUBLE* which NESL barfs on */
#define MINDBL -1.797693134862315e+308

/* Macro to glue two strings together */
#ifdef __STDC__
#       define Glue(a,b) a##b
#else
#       define Glue(a,b) a/**/b
#endif


/* Macro to stringize a macro argument */
#ifdef __STDC__
#	define Stringize(token) #token
#else
#	define Stringize(token) "token"
#endif


/* Macros defining vector layout: note that the space allocated for a
 * vector is fixed amongst all processors, but that not all of them will
 * fill it.
 */

#define Space_For(X) (((X) + nprocs - 1) >> logprocs)


#define Num_Here(X)							\
   (((X) - First_Elt_Here(X) > Space_For(X)) ? Space_For(X) :		\
    ((X) - First_Elt_Here(X) >= 0) ? (X) - First_Elt_Here(X) : 0)


#define First_Elt_Here(X) (thisproc * Space_For(X))


#define Prep(LEN)							\
  { 									\
    int eltsper = Space_For (LEN);					\
    double recip = (eltsper != 0) ? (double) 1 / eltsper : 0.0;		\
    int proc, posn, num_sent = 0;					\
    num_rcvd = 0


#define Map_To_Proc_And_Posn(X)						\
    proc = (X) * recip; posn = (X) - (proc * eltsper)


#define Simple_Prep(LEN)						\
  int eltsper = Space_For (LEN);					\
  int proc, posn


#define Simple_Map_To_Proc_And_Posn(X)					\
  proc = (X) / eltsper; posn = (X) - (proc * eltsper)


#define Not_Yet(TYPE, RESULT, NAME, ARGS)				\
TYPE NAME ARGS								\
{									\
  static int called_before = 0;						\
  if (! called_before) {						\
    called_before = 1;							\
    fprintf(stderr, "%s has not been written yet\n", #NAME);		\
  }									\
  return RESULT;							\
}									\
Make_No_Scratch (NAME)							\
Make_Inplace (NAME, INPLACE_NONE)


#define Not_Yet_Seg(TYPE, RESULT, NAME, ARGS)				\
TYPE NAME ARGS								\
{									\
  static int called_before = 0;						\
  if (! called_before) {						\
    called_before = 1;							\
    fprintf(stderr, "%s has not been written yet\n", #NAME);		\
  }									\
  return RESULT;							\
}									\
Make_No_Seg_Scratch (NAME)						\
Make_Inplace (NAME, INPLACE_NONE)


/* Define numeric message tags for all the CVL functions which need
 * node execution (in the order in which they are listed in cvl.h)
 */
#define CVL_qui_tag 0
#define max_wuz_tag 1  
#define max_wud_tag 2  
#define min_wuz_tag 3  
#define min_wud_tag 4  
#define add_wuz_tag 5  
#define add_wud_tag 6  
#define sub_wuz_tag 7  
#define sub_wud_tag 8  
#define mul_wuz_tag 9  
#define mul_wud_tag 10 
#define div_wuz_tag 11 
#define div_wud_tag 12 
#define grt_wuz_tag 13 
#define grt_wud_tag 14 
#define les_wuz_tag 15 
#define les_wud_tag 16 
#define geq_wuz_tag 17 
#define geq_wud_tag 18 
#define leq_wuz_tag 19 
#define leq_wud_tag 20 
#define lsh_wuz_tag 21 
#define rsh_wuz_tag 22 
#define mod_wuz_tag 23 
#define rnd_wuz_tag 24 
#define eql_wub_tag 25 
#define eql_wuz_tag 26 
#define eql_wud_tag 27 
#define neq_wub_tag 28 
#define neq_wuz_tag 29 
#define neq_wud_tag 30 
#define sel_wuz_tag 31 
#define sel_wub_tag 32 
#define sel_wud_tag 33 
#define not_wub_tag 34 
#define xor_wub_tag 35 
#define ior_wub_tag 36 
#define and_wub_tag 37 
#define ior_wuz_tag 38 
#define and_wuz_tag 39 
#define not_wuz_tag 40 
#define xor_wuz_tag 41 
#define int_wud_tag 42 
#define int_wub_tag 43 
#define dbl_wuz_tag 44 
#define boo_wuz_tag 45 
#define flr_wud_tag 46 
#define cei_wud_tag 47 
#define trn_wud_tag 48 
#define rou_wud_tag 49 
#define exp_wud_tag 50 
#define log_wud_tag 51 
#define sqt_wud_tag 52 
#define sin_wud_tag 53 
#define cos_wud_tag 54 
#define tan_wud_tag 55 
#define asn_wud_tag 56 
#define acs_wud_tag 57 
#define atn_wud_tag 58 
#define snh_wud_tag 59 
#define csh_wud_tag 60 
#define tnh_wud_tag 61 
#define cpy_wuz_tag 62 
#define cpy_wub_tag 63 
#define cpy_wud_tag 64 
#define cpy_wus_tag 65 
#define add_suz_tag 66 
#define add_sud_tag 67 
#define mul_suz_tag 68 
#define mul_sud_tag 69 
#define min_suz_tag 70 
#define min_sud_tag 71 
#define max_suz_tag 72 
#define max_sud_tag 73 
#define and_sub_tag 74 
#define and_suz_tag 75 
#define ior_sub_tag 76 
#define ior_suz_tag 77 
#define xor_sub_tag 78 
#define xor_suz_tag 79 
#define add_sez_tag 80 
#define add_sed_tag 81 
#define mul_sez_tag 82 
#define mul_sed_tag 83 
#define min_sez_tag 84 
#define min_sed_tag 85 
#define max_sez_tag 86 
#define max_sed_tag 87 
#define and_seb_tag 88 
#define and_sez_tag 89 
#define ior_seb_tag 90 
#define ior_sez_tag 91 
#define xor_seb_tag 92 
#define xor_sez_tag 93 
#define add_ruz_tag 94 
#define add_rud_tag 95 
#define mul_ruz_tag 96 
#define mul_rud_tag 97 
#define min_ruz_tag 98 
#define min_rud_tag 99 
#define max_ruz_tag 100
#define max_rud_tag 101
#define and_rub_tag 102
#define and_ruz_tag 103
#define ior_rub_tag 104
#define ior_ruz_tag 105
#define xor_rub_tag 106
#define xor_ruz_tag 107
#define add_rez_tag 108
#define add_red_tag 109
#define mul_rez_tag 110
#define mul_red_tag 111
#define min_rez_tag 112
#define min_red_tag 113
#define max_rez_tag 114
#define max_red_tag 115
#define and_reb_tag 116
#define and_rez_tag 117
#define ior_reb_tag 118
#define ior_rez_tag 119
#define xor_reb_tag 120
#define xor_rez_tag 121
#define ext_vuz_tag 122
#define ext_vub_tag 123
#define ext_vud_tag 124
#define ext_vez_tag 125
#define ext_veb_tag 126
#define ext_ved_tag 127
#define rep_vuz_tag 128
#define rep_vub_tag 129
#define rep_vud_tag 130
#define rep_vez_tag 131
#define rep_veb_tag 132
#define rep_ved_tag 133
#define dis_vuz_tag 134
#define dis_vub_tag 135
#define dis_vud_tag 136
#define dis_vez_tag 137
#define dis_veb_tag 138
#define dis_ved_tag 139
#define smp_puz_tag 140
#define smp_pub_tag 141
#define smp_pud_tag 142
#define smp_pez_tag 143
#define smp_peb_tag 144
#define smp_ped_tag 145
#define bck_puz_tag 146
#define bck_pub_tag 147
#define bck_pud_tag 148
#define bck_pez_tag 149
#define bck_peb_tag 150
#define bck_ped_tag 151
#define fpm_puz_tag 152
#define fpm_pub_tag 153
#define fpm_pud_tag 154
#define fpm_pez_tag 155
#define fpm_peb_tag 156
#define fpm_ped_tag 157
#define bfp_puz_tag 158
#define bfp_pub_tag 159
#define bfp_pud_tag 160
#define bfp_pez_tag 161
#define bfp_peb_tag 162
#define bfp_ped_tag 163
#define dpe_puz_tag 164
#define dpe_pub_tag 165
#define dpe_pud_tag 166
#define dpe_pez_tag 167
#define dpe_peb_tag 168
#define dpe_ped_tag 169
#define dfp_puz_tag 170
#define dfp_pub_tag 171
#define dfp_pud_tag 172
#define dfp_pez_tag 173
#define dfp_peb_tag 174
#define dfp_ped_tag 175
#define tgt_fos_tag 176 
#define mke_fov_tag 177
#define len_fos_tag 178
#define alo_foz_tag 179
#define fre_fov_tag 180
#define mov_fov_tag 181
#define v2c_fuz_tag 182
#define v2c_fub_tag 183
#define v2c_fud_tag 184
#define c2v_fuz_tag 185
#define c2v_fub_tag 186
#define c2v_fud_tag 187
#define ind_luz_tag 188
#define ind_lez_tag 189
#define pk1_luv_tag 190
#define pk2_luz_tag 191
#define pk2_lub_tag 192
#define pk2_lud_tag 193
#define rku_luz_tag 194
#define rkd_luz_tag 195
#define rku_lez_tag 196
#define rkd_lez_tag 197
#define rku_lud_tag 198
#define rkd_lud_tag 199
#define rku_led_tag 200
#define rkd_led_tag 201
#define rnd_foz_tag 202
#define NUM_TAGS 203
