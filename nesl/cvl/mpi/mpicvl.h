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

/* ------------------------ Defined constants ------------------------ */

/* BUF_SIZE and RCV_EVERY are normally compile-time constants set in the
 * Makefile.  If they're not defined (to be more precise, if BUF_SIZE is
 * not defined), they're created as variables instead.	This makes it
 * easier to run quick tests with different values.
 */

#if !defined(BUF_SIZE)
  extern int BUF_SIZE;
  extern int RCV_EVERY;
#endif

/* MAX_PROC is the maximum number of processors supported.  Can be
 * a compile-time parameter, but we also provide reasonable defaults.
 */
#if !defined(MAX_PROC)
  #if defined(MPI_rs6000) || defined(MPI_paragon)
    #define MAX_PROC 64
  #elif defined(MPI_cm5)
    #define MAX_PROC 256
  #else
    #define MAX_PROC 8
  #endif
#endif

/* General-purpose sentinel value */
#define SENTINEL -1

/* MPI type equivalent of maxalign */
#define MPI_MAXALIGN MPI_DOUBLE

/* MPI type equivalent of cvl_bool */
#define MPI_BOOL MPI_BYTE

#ifndef TRUE
  #define TRUE 1
  #define FALSE 0
#endif

#ifndef NULL
  #define NULL 0
#endif


/* ----------------------------- Macros ------------------------------ */

/* Macros that define vector layout.  Everyone allocates the same amount
 * of space.  Vector is divided into equal chunks, so computing who has
 * what is easy (although some processors may have nothing, and one may
 * have the leftovers at the end of the vector)
 */
#define SpaceFor(nelts) \
	(((nelts) + NumProcs - 1) / NumProcs)
#define MapToProc(index, nelts) \
	((index) / SpaceFor(nelts))
#define MapToOffset(index, nelts) \
	((index) - (MapToProc(index,nelts) * SpaceFor(nelts)))
#define FirstHere(nelts) \
	(Self * SpaceFor(nelts))

/* Macros to access fields of a segment descriptor (see facilt.c)
 */
#define SegdLength(segd, nelts, nsegs) \
	(segd)
#define SegdStart(segd, nelts, nsegs) \
	((segd)+(SpaceFor(nsegs)))
#define SegdSegment(segd, nelts, nsegs) \
	((segd)+(SpaceFor(nsegs)<<1))
#define SegdBefore(segd, nelts, nsegs) \
	((segd)+(SpaceFor(nsegs)<<1)+(SpaceFor(nelts)))
#define SegdFirst(segd, nelts, nsegs) \
	((segd)+(SpaceFor(nsegs)<<1)+(SpaceFor(nelts))+1)

/* Mathematical operators, used mainly in elwise.c
 */
#define max(x,y) ((x) > (y)) ? (x) : (y)
#define min(x,y) ((x) < (y)) ? (x) : (y)
#define lsh(x,y) ((x) << (y))
#define rsh(x,y) ((x) >> (y))
#define add(x,y) ((x) + (y))
#define sub(x,y) ((x) - (y))
#define mul(x,y) ((x) * (y))
#define dvd(x,y) ((x) / (y))
#define mod(x,y) ((x) % (y))
#define grt(x,y) ((x) > (y))
#define les(x,y) ((x) < (y))
#define eql(x,y) ((x) == (y))
#define neq(x,y) ((x) != (y))
#define leq(x,y) ((x) <= (y))
#define geq(x,y) ((x) >= (y))
#define ior(x,y) ((x) || (y))
#define bor(x,y) ((x) | (y))
#define and(x,y) ((x) && (y))
#define bnd(x,y) ((x) & (y))
#define not(x) (! (x))
#define bnot(x) (~ (x))
#define xor(x,y) ((x) ^ (y))
#define neg(x) (-(x))
#define ident(x) (x)
#define notnot(x) (!!(x))
#define nop(x) (x)
#define pickone(x,y,z) ((x) ? (y) : (z))
#define d_to_z(x) (int) (x)
#define b_to_z(x) (int) (x)
#define z_to_d(x) (double) (x)
#define z_to_b(x) (cvl_bool) notnot(x)
#ifdef MPI_t3d
#define cvl_round(x) ((int) ((x > 0.0 ? floor (x + 0.5) : ceil (x - 0.5))));
#else
#define cvl_round(x) ((int) rint(x))
#endif
#define cvl_floor(x) ((int) floor(x))
#define cvl_ceil(x) ((int) ceil(x))


/* ----------- Error-checking wrappers for MPI functions ------------- */

#ifndef NDEBUG
  #include <assert.h>
  #define MPI_Allreduce(a,b,c,d,e,f) \
	assert(MPI_Allreduce(a,b,c,d,e,f) == MPI_SUCCESS)
  #define MPI_Allgatherv(a,b,c,d,e,f,g,h) \
	assert(MPI_Allgatherv(a,b,c,d,e,f,g,h) == MPI_SUCCESS)
  #define MPI_Barrier(a) \
	assert(MPI_Barrier(a) == MPI_SUCCESS)
  #define MPI_Bcast(a,b,c,d,e) \
	assert(MPI_Bcast(a,b,c,d,e) == MPI_SUCCESS)
  #define MPI_Comm_dup(a,b) \
	assert(MPI_Comm_dup(a,b) == MPI_SUCCESS)
  #define MPI_Comm_rank(a,b) \
	assert(MPI_Comm_rank(a,b) == MPI_SUCCESS)
  #define MPI_Comm_size(a,b) \
	assert(MPI_Comm_size(a,b) == MPI_SUCCESS)
  #define MPI_Finalize() \
	assert(MPI_Finalize() == MPI_SUCCESS)
  #define MPI_Get_count(a,b,c) \
	assert(MPI_Get_count(a,b,c) == MPI_SUCCESS)
  #define MPI_Init(a,b) \
	assert(MPI_Init(a,b) == MPI_SUCCESS)
  #define MPI_Irecv(a,b,c,d,e,f,g) \
	assert(MPI_Irecv(a,b,c,d,e,f,g) == MPI_SUCCESS)
  #define MPI_Isend(a,b,c,d,e,f,g) \
	assert(MPI_Isend(a,b,c,d,e,f,g) == MPI_SUCCESS)
  #define MPI_Op_create(a,b,c) \
	assert(MPI_Op_create(a,b,c) == MPI_SUCCESS)
  #define MPI_Reduce(a,b,c,d,e,f,g) \
        assert(MPI_Reduce(a,b,c,d,e,f,g) == MPI_SUCCESS)
  #define MPI_Recv(a,b,c,d,e,f,g) \
	assert(MPI_Recv(a,b,c,d,e,f,g) == MPI_SUCCESS)
  #define MPI_Scan(a,b,c,d,e,f) \
	assert(MPI_Scan(a,b,c,d,e,f) == MPI_SUCCESS)
  #define MPI_Send(a,b,c,d,e,f) \
	assert(MPI_Send(a,b,c,d,e,f) == MPI_SUCCESS)
  #define MPI_Sendrecv(a,b,c,d,e,f,g,h,i,j,k,l) \
	assert(MPI_Sendrecv(a,b,c,d,e,f,g,h,i,j,k,l) == MPI_SUCCESS)
  #define MPI_Testsome(a,b,c,d,e) \
	assert(MPI_Testsome(a,b,c,d,e) == MPI_SUCCESS)
  #define MPI_Wait(a,b) \
	assert(MPI_Wait(a,b) == MPI_SUCCESS)
  #define MPI_Waitall(a,b,c) \
	assert(MPI_Waitall(a,b,c) == MPI_SUCCESS)
#endif /* !NDEBUG */


/* ----------------- Profiling/tracing all CVL calls ----------------- */

#define HERE printf ("Got here: node %d, %s: %d\n", Self, __FILE__, __LINE__);

extern void print_int (vec_p src, int nelts);
extern void print_bool (vec_p src, int nelts);


#ifdef CVLPROFILE
  #ifndef LOG_LIMIT
    #define LOG_LIMIT 2
  #endif
  #define LOG_START(x,y) if (y >= LOG_LIMIT) MPE_Log_event(x##_START, 0, NULL)
  #define LOG_STOP(x,y) if (y >= LOG_LIMIT) MPE_Log_event(x##_STOP, 0, NULL)
#elif PRINTDEBUG
  #include <stdio.h>
  #define LOG_START(x,y) if (Self == 0) printf ("%s (%d)\n", #x, y)
  #define LOG_STOP(x,y)
#elif PRINTDEBUGONCE
  #include <stdio.h>
  #define LOG_START(x,y) { static int got_here = 0; if ((Self == 0) && (got_here == 0)) printf ("entering %s\n", #x); got_here = 1; }
  #define LOG_STOP(x,y)
#else
  #define LOG_START(x,y)
  #define LOG_STOP(x,y)
#endif

#ifdef CVLPROFILE
#define max_wuz_START 1
#define max_wuz_STOP  2
#define min_wuz_START 3
#define min_wuz_STOP  4
#define add_wuz_START 5
#define add_wuz_STOP  6
#define sub_wuz_START 7
#define sub_wuz_STOP  8
#define mul_wuz_START 9
#define mul_wuz_STOP  10
#define div_wuz_START 11
#define div_wuz_STOP  12
#define max_wud_START 13
#define max_wud_STOP  14
#define min_wud_START 15
#define min_wud_STOP  16
#define add_wud_START 17
#define add_wud_STOP  18
#define sub_wud_START 19
#define sub_wud_STOP  20
#define mul_wud_START 21
#define mul_wud_STOP  22
#define div_wud_START 23
#define div_wud_STOP  24
#define grt_wuz_START 25
#define grt_wuz_STOP  26
#define les_wuz_START 27
#define les_wuz_STOP  28
#define geq_wuz_START 29
#define geq_wuz_STOP  30
#define leq_wuz_START 31
#define leq_wuz_STOP  32
#define grt_wud_START 33
#define grt_wud_STOP  34
#define les_wud_START 35
#define les_wud_STOP  36
#define geq_wud_START 37
#define geq_wud_STOP  38
#define leq_wud_START 39
#define leq_wud_STOP  40
#define eql_wub_START 41
#define eql_wub_STOP  42
#define neq_wub_START 43
#define neq_wub_STOP  44
#define eql_wuz_START 45
#define eql_wuz_STOP  46
#define neq_wuz_START 47
#define neq_wuz_STOP  48
#define eql_wud_START 49
#define eql_wud_STOP  50
#define neq_wud_START 51
#define neq_wud_STOP  52
#define lsh_wuz_START 53
#define lsh_wuz_STOP  54
#define rsh_wuz_START 55
#define rsh_wuz_STOP  56
#define mod_wuz_START 57
#define mod_wuz_STOP  58
#define rnd_wuz_START 59
#define rnd_wuz_STOP  60
#define sel_wuz_START 61
#define sel_wuz_STOP  62
#define sel_wud_START 63
#define sel_wud_STOP  64
#define sel_wub_START 65
#define sel_wub_STOP  66
#define not_wub_START 67
#define not_wub_STOP  68
#define xor_wub_START 69
#define xor_wub_STOP  70
#define ior_wub_START 71
#define ior_wub_STOP  72
#define and_wub_START 73
#define and_wub_STOP  74
#define not_wuz_START 75
#define not_wuz_STOP  76
#define xor_wuz_START 77
#define xor_wuz_STOP  78
#define ior_wuz_START 79
#define ior_wuz_STOP  80
#define and_wuz_START 81
#define and_wuz_STOP  82
#define flr_wud_START 83
#define flr_wud_STOP  84
#define cei_wud_START 85
#define cei_wud_STOP  86
#define trn_wud_START 87
#define trn_wud_STOP  88
#define rou_wud_START 89
#define rou_wud_STOP  90
#define exp_wud_START 91
#define exp_wud_STOP  92
#define log_wud_START 93
#define log_wud_STOP  94
#define sqt_wud_START 95
#define sqt_wud_STOP  96
#define sin_wud_START 97
#define sin_wud_STOP  98
#define cos_wud_START 99
#define cos_wud_STOP  100
#define tan_wud_START 101
#define tan_wud_STOP  102
#define asn_wud_START 103
#define asn_wud_STOP  104
#define acs_wud_START 105
#define acs_wud_STOP  106
#define atn_wud_START 107
#define atn_wud_STOP  108
#define snh_wud_START 109
#define snh_wud_STOP  110
#define csh_wud_START 111
#define csh_wud_STOP  112
#define tnh_wud_START 113
#define tnh_wud_STOP  114
#define int_wud_START 115
#define int_wud_STOP  116
#define int_wub_START 117
#define int_wub_STOP  118
#define dbl_wuz_START 119
#define dbl_wuz_STOP  120
#define boo_wuz_START 121
#define boo_wuz_STOP  122
#define cpy_wuz_START 123
#define cpy_wuz_STOP  124
#define cpy_wud_START 125
#define cpy_wud_STOP  126
#define cpy_wub_START 127
#define cpy_wub_STOP  128
#define cpy_wus_START 129
#define cpy_wus_STOP  130
#define tgt_fos_START 131
#define tgt_fos_STOP  132
#define tdf_fos_START 133
#define tdf_fos_STOP  134
#define siz_foz_START 135
#define siz_foz_STOP  136
#define siz_fod_START 137
#define siz_fod_STOP  138
#define siz_fob_START 139
#define siz_fob_STOP  140
#define siz_fos_START 141
#define siz_fos_STOP  142
#define alo_foz_START 143
#define alo_foz_STOP  144
#define fre_fov_START 145
#define fre_fov_STOP  146
#define mov_fov_START 147
#define mov_fov_STOP  148
#define add_fov_START 149
#define add_fov_STOP  150
#define sub_fov_START 151
#define sub_fov_STOP  152
#define eql_fov_START 153
#define eql_fov_STOP  154
#define cmp_fov_START 155
#define cmp_fov_STOP  156
#define v2c_fuz_START 157
#define v2c_fuz_STOP  158
#define v2c_fud_START 159
#define v2c_fud_STOP  160
#define v2c_fub_START 161
#define v2c_fub_STOP  162
#define c2v_fuz_START 163
#define c2v_fuz_STOP  164
#define c2v_fud_START 165
#define c2v_fud_STOP  166
#define c2v_fub_START 167
#define c2v_fub_STOP  168
#define mke_fov_START 169
#define mke_fov_STOP  170
#define len_fos_START 171
#define len_fos_STOP  172
#define pk1_luv_START 173
#define pk1_luv_STOP  174
#define pk2_luz_START 175
#define pk2_luz_STOP  176
#define pk2_lud_START 177
#define pk2_lud_STOP  178
#define pk2_lub_START 179
#define pk2_lub_STOP  180
#define pk1_lev_START 181
#define pk1_lev_STOP  182
#define pk2_lez_START 183
#define pk2_lez_STOP  184
#define pk2_leb_START 185
#define pk2_leb_STOP  186
#define pk2_led_START 187
#define pk2_led_STOP  188
#define ind_luz_START 189
#define ind_luz_STOP  190
#define ind_lez_START 191
#define ind_lez_STOP  192
#define rku_luz_START 193
#define rku_luz_STOP  194
#define rkd_luz_START 195
#define rkd_luz_STOP  196
#define rku_lez_START 197
#define rku_lez_STOP  198
#define rkd_lez_START 199
#define rkd_lez_STOP  200
#define rku_lud_START 201
#define rku_lud_STOP  202
#define rkd_lud_START 203
#define rkd_lud_STOP  204
#define rku_led_START 205
#define rku_led_STOP  206
#define rkd_led_START 207
#define rkd_led_STOP  208
#define smp_puz_START 209
#define smp_puz_STOP  210
#define smp_pud_START 211
#define smp_pud_STOP  212
#define smp_pub_START 213
#define smp_pub_STOP  214
#define fpm_puz_START 215
#define fpm_puz_STOP  216
#define fpm_pud_START 217
#define fpm_pud_STOP  218
#define fpm_pub_START 219
#define fpm_pub_STOP  220
#define dpe_puz_START 221
#define dpe_puz_STOP  222
#define dpe_pud_START 223
#define dpe_pud_STOP  224
#define dpe_pub_START 225
#define dpe_pub_STOP  226
#define dfp_puz_START 227
#define dfp_puz_STOP  228
#define dfp_pud_START 229
#define dfp_pud_STOP  230
#define dfp_pub_START 231
#define dfp_pub_STOP  232
#define bck_puz_START 233
#define bck_puz_STOP  234
#define bck_pud_START 235
#define bck_pud_STOP  236
#define bck_pub_START 237
#define bck_pub_STOP  238
#define bfp_puz_START 239
#define bfp_puz_STOP  240
#define bfp_pud_START 241
#define bfp_pud_STOP  242
#define bfp_pub_START 243
#define bfp_pub_STOP  244
#define smp_pez_START 245
#define smp_pez_STOP  246
#define smp_ped_START 247
#define smp_ped_STOP  248
#define smp_peb_START 249
#define smp_peb_STOP  250
#define fpm_pez_START 251
#define fpm_pez_STOP  252
#define fpm_ped_START 253
#define fpm_ped_STOP  254
#define fpm_peb_START 255
#define fpm_peb_STOP  256
#define dpe_pez_START 257
#define dpe_pez_STOP  258
#define dpe_ped_START 259
#define dpe_ped_STOP  260
#define dpe_peb_START 261
#define dpe_peb_STOP  262
#define bck_pez_START 263
#define bck_pez_STOP  264
#define bck_ped_START 265
#define bck_ped_STOP  266
#define bck_peb_START 267
#define bck_peb_STOP  268
#define bfp_pez_START 269
#define bfp_pez_STOP  270
#define bfp_ped_START 271
#define bfp_ped_STOP  272
#define bfp_peb_START 273
#define bfp_peb_STOP  274
#define dfp_pez_START 275
#define dfp_pez_STOP  276
#define dfp_ped_START 277
#define dfp_ped_STOP  278
#define dfp_peb_START 279
#define dfp_peb_STOP  280
#define add_ruz_START 281
#define add_ruz_STOP  282
#define mul_ruz_START 283
#define mul_ruz_STOP  284
#define max_ruz_START 285
#define max_ruz_STOP  286
#define min_ruz_START 287
#define min_ruz_STOP  288
#define and_ruz_START 289
#define and_ruz_STOP  290
#define ior_ruz_START 291
#define ior_ruz_STOP  292
#define xor_ruz_START 293
#define xor_ruz_STOP  294
#define add_rud_START 295
#define add_rud_STOP  296
#define mul_rud_START 297
#define mul_rud_STOP  298
#define max_rud_START 299
#define max_rud_STOP  300
#define min_rud_START 301
#define min_rud_STOP  302
#define and_rub_START 303
#define and_rub_STOP  304
#define ior_rub_START 305
#define ior_rub_STOP  306
#define xor_rub_START 307
#define xor_rub_STOP  308
#define add_suz_START 309
#define add_suz_STOP  410
#define mul_suz_START 411
#define mul_suz_STOP  412
#define and_suz_START 413
#define and_suz_STOP  414
#define xor_suz_START 415
#define xor_suz_STOP  416
#define add_sud_START 417
#define add_sud_STOP  418
#define mul_sud_START 419
#define mul_sud_STOP  420
#define and_sub_START 421
#define and_sub_STOP  422
#define xor_sub_START 423
#define xor_sub_STOP  424
#define max_suz_START 425
#define max_suz_STOP  426
#define min_suz_START 427
#define min_suz_STOP  428
#define ior_suz_START 429
#define ior_suz_STOP  430
#define max_sud_START 431
#define max_sud_STOP  432
#define min_sud_START 433
#define min_sud_STOP  434
#define ior_sub_START 435
#define ior_sub_STOP  436
#define add_rez_START 437
#define add_rez_STOP  438
#define mul_rez_START 439
#define mul_rez_STOP  440
#define max_rez_START 441
#define max_rez_STOP  442
#define min_rez_START 443
#define min_rez_STOP  444
#define and_rez_START 445
#define and_rez_STOP  446
#define ior_rez_START 447
#define ior_rez_STOP  448
#define xor_rez_START 449
#define xor_rez_STOP  450
#define add_red_START 451
#define add_red_STOP  452
#define mul_red_START 453
#define mul_red_STOP  454
#define max_red_START 455
#define max_red_STOP  456
#define min_red_START 457
#define min_red_STOP  458
#define and_reb_START 459
#define and_reb_STOP  460
#define ior_reb_START 461
#define ior_reb_STOP  462
#define xor_reb_START 463
#define xor_reb_STOP  464
#define add_sez_START 465
#define add_sez_STOP  466
#define mul_sez_START 467
#define mul_sez_STOP  468
#define max_sez_START 469
#define max_sez_STOP  470
#define min_sez_START 471
#define min_sez_STOP  472
#define and_sez_START 473
#define and_sez_STOP  474
#define ior_sez_START 475
#define ior_sez_STOP  476
#define xor_sez_START 477
#define xor_sez_STOP  478
#define add_sed_START 479
#define add_sed_STOP  480
#define mul_sed_START 481
#define mul_sed_STOP  482
#define max_sed_START 483
#define max_sed_STOP  484
#define min_sed_START 485
#define min_sed_STOP  486
#define and_seb_START 487
#define and_seb_STOP  488
#define ior_seb_START 489
#define ior_seb_STOP  490
#define xor_seb_START 491
#define xor_seb_STOP  492
#define dis_vuz_START 493
#define dis_vuz_STOP  494
#define dis_vud_START 495
#define dis_vud_STOP  496
#define dis_vub_START 497
#define dis_vub_STOP  498
#define rep_vuz_START 499
#define rep_vuz_STOP  400
#define rep_vud_START 401
#define rep_vud_STOP  402
#define rep_vub_START 403
#define rep_vub_STOP  404
#define ext_vuz_START 405
#define ext_vuz_STOP  406
#define ext_vud_START 407
#define ext_vud_STOP  408
#define ext_vub_START 409
#define ext_vub_STOP  510
#define dis_vez_START 511
#define dis_vez_STOP  512
#define dis_ved_START 513
#define dis_ved_STOP  514
#define dis_veb_START 515
#define dis_veb_STOP  516
#define rep_vez_START 517
#define rep_vez_STOP  518
#define rep_ved_START 519
#define rep_ved_STOP  520
#define rep_veb_START 521
#define rep_veb_STOP  522
#define ext_vez_START 523
#define ext_vez_STOP  524
#define ext_ved_START 525
#define ext_ved_STOP  526
#define ext_veb_START 527
#define ext_veb_STOP  528
#define rnd_foz_START 529
#define rnd_foz_STOP  530

#include <mpe.h>
#endif

/* -------------------------- Message Tags -------------------------- */

#define max_suz_tag 0
#define max_sud_tag 1
#define min_suz_tag 2
#define min_sud_tag 3
#define and_suz_tag 4
#define and_sub_tag 5
#define ior_suz_tag 6
#define ior_sub_tag 7
#define add_sez_tag 8
#define add_sed_tag 9
#define mul_sez_tag 10
#define mul_sed_tag 11
#define min_sez_tag 12
#define min_sed_tag 13
#define max_sez_tag 14
#define max_sed_tag 15
#define and_seb_tag 16
#define and_sez_tag 17
#define ior_seb_tag 18
#define ior_sez_tag 19
#define xor_seb_tag 20
#define xor_sez_tag 21
#define add_rez_tag 22
#define add_red_tag 23
#define mul_rez_tag 24
#define mul_red_tag 25
#define min_rez_tag 26
#define min_red_tag 27
#define max_rez_tag 28
#define max_red_tag 29
#define and_reb_tag 30
#define and_rez_tag 31
#define ior_reb_tag 32
#define ior_rez_tag 33
#define xor_reb_tag 34
#define xor_rez_tag 35
#define ext_vez_tag 36
#define ext_veb_tag 37
#define ext_ved_tag 38
#define rep_vez_tag 39
#define rep_veb_tag 40
#define rep_ved_tag 41
#define dis_vez_tag 42
#define dis_veb_tag 43
#define dis_ved_tag 44
#define smp_puz_tag 45
#define smp_pub_tag 46
#define smp_pud_tag 47
#define smp_pez_tag 48
#define smp_peb_tag 49
#define smp_ped_tag 50
#define bck_puz_tag 51
#define bck_pub_tag 52
#define bck_pud_tag 53
#define bck_pez_tag 54
#define bck_peb_tag 55
#define bck_ped_tag 56
#define fpm_puz_tag 57
#define fpm_pub_tag 58
#define fpm_pud_tag 59
#define fpm_pez_tag 60
#define fpm_peb_tag 61
#define fpm_ped_tag 62
#define bfp_puz_tag 63
#define bfp_pub_tag 64
#define bfp_pud_tag 65
#define bfp_pez_tag 66
#define bfp_peb_tag 67
#define bfp_ped_tag 68
#define dpe_puz_tag 69
#define dpe_pub_tag 70
#define dpe_pud_tag 71
#define dpe_pez_tag 72
#define dpe_peb_tag 73
#define dpe_ped_tag 74
#define dfp_puz_tag 75
#define dfp_pub_tag 76
#define dfp_pud_tag 77
#define dfp_pez_tag 78
#define dfp_peb_tag 79
#define dfp_ped_tag 80
#define mke_fov_tag 81
#define ind_lez_tag 82
#define pk2_luz_tag 83
#define pk2_lub_tag 84
#define pk2_lud_tag 85
#define pk1_lev_tag 86
#define pk2_lez_tag 87
#define pk2_leb_tag 88
#define pk2_led_tag 89
#define rku_luz_tag 90
#define rkd_luz_tag 91
#define rku_lez_tag 92
#define rkd_lez_tag 93
#define rku_lud_tag 94
#define rkd_lud_tag 95
#define rku_led_tag 96
#define rkd_led_tag 97
#define CVL_max_tag 98		/* used in _exclusive_max_scan */
#define CVL_trn_tag 99		/* used in transpose_buckets */
#define CVL_unt_tag 100		/* used in untranspose_buckets */

#define NUM_TAGS 101

#define Reply_To(x) (NUM_TAGS+x)


/* ----------------------------- Typedefs ---------------------------- */

/* All vectors must be maximally aligned -- this type ensures they are
 */
typedef union maxalign_u { int i; double d; } maxalign;

/* Structures used to send values between processors
 */
typedef struct intx_struct {
  int value;			/* value to be sent */
  int index;			/* index in dst vector to put val */
} intx;

typedef struct doublex_struct {
  double value;			/* value to be sent */
  int index;			/* index in dst vector to put val */
} doublex;

typedef struct cvl_boolx_struct {
  int index;			/* index in dst vector to put val */
  cvl_bool value;		/* value to be sent */
} cvl_boolx;

typedef struct fetch_struct {
  int src_index;		/* index in src to get from on the
				   receiving node */
  int dst_index;		/* index in dst on the sending node that
				   the receiving node should reply to */
} fetch;

typedef struct two_intx_struct {
  int value1;
  int value2;
  int index;
} two_intx;			/* used in ind_lez and ranks */

typedef struct three_intx_struct {
  int value1;
  int value2;
  int value3;
  int index;
} three_intx;

typedef struct four_intx_struct {
  int value1;
  int value2;
  int value3;
  int value4;
  int index;
} four_intx;

typedef struct t32_bits_t {
  unsigned int word[1];
} t32bits;

typedef struct t64_bits_t {
  unsigned int word[2];
} t64bits;

typedef struct t96_bits_t {
  unsigned int word[3];
} t96bits;


/* ---------------------- Function definitions ----------------------- */

/* From facilt.c
 */
void CVL_init (int *argc, char ***argv);
void CVL_quit (void);
int _num_here (int nelts);

/* From scan.c
 */
void _setup_scan_ops (void);
void _exclusive_max_scan (int *inout);
void _copy_scan_int (int *inout);
void _copy_scan_double (double *inout);
void _copy_scan_cvl_bool (cvl_bool *inout);
void _copy_scan_two_int (int *stride, int *index);

/* From messages.c
 */
void _setup_buffers (void);
void _set_self_and_numprocs (void);
void _send_buffer (int proc, int tag, int n_bytes);
int _recv_buffers (int *rcvd_index, int *rcvd_count, int *rcvd_tag);
int _flush_buffers (int tag, int elt_size, int n_sent);

/* Simple sends */
void _unpack_simple_intx (void *dst, int proc, int count);
void _unpack_simple_doublex (void *dst, int proc, int count);
void _unpack_simple_cvl_boolx (void *dst, int proc, int count);
int _recv_simple (void *dst, int tag, void (*unpack)(void *, int, int));
void _finish_simple (void *dst, int tag, int n_sent, int n_rcvd,
		     int elt_size, void (*unpack)(void *, int, int));

/* Combining sends */
void _unpack_add_intx (void *dst, int proc, int count);
void _unpack_mul_intx (void *dst, int proc, int count);
void _unpack_max_intx (void *dst, int proc, int count);
void _unpack_min_intx (void *dst, int proc, int count);
void _unpack_bnd_intx (void *dst, int proc, int count);
void _unpack_bor_intx (void *dst, int proc, int count);
void _unpack_xor_intx (void *dst, int proc, int count);
void _unpack_add_doublex (void *dst, int proc, int count);
void _unpack_mul_doublex (void *dst, int proc, int count);
void _unpack_max_doublex (void *dst, int proc, int count);
void _unpack_min_doublex (void *dst, int proc, int count);
void _unpack_and_cvl_boolx (void *dst, int proc, int count);
void _unpack_ior_cvl_boolx (void *dst, int proc, int count);
void _unpack_xor_cvl_boolx (void *dst, int proc, int count);

/* Fetches */
void _unpack_fetch_intx (void *src, int proc, int count, int tag);
void _unpack_fetch_doublex (void *src, int proc, int count, int tag);
void _unpack_fetch_cvl_boolx (void *src, int proc, int count, int tag);
int _recv_fetch (void *dst, void *src, int tag,
		 void (*unpack_fetch)(void *, int, int, int),
		 void (*unpack_send)(void *, int, int));
void _finish_fetch (void *dst, void *src, int tag, int n_sent, int
		    n_rcvd, void (*unpack_fetch)(void *, int, int, int),
		    void (*unpack_send)(void *, int, int));

/* Send, keep track of maximum index */
void _unpack_intx_maxindx (void *dst, int *max_index_p, int proc,
			   int count);
void _unpack_doublex_maxindx (void *dst, int *max_index_p, int proc,
			      int count);
void _unpack_cvl_boolx_maxindx (void *dst, int *max_index_p, int proc,
				int count);
int _recv_maxindx (void *dst, int *max_index_p, int tag,
		   void (*unpack)(void *, int *, int, int));
void _finish_maxindx (void *dst, int tag, int n_sent, int n_rcvd,
		      int elt_size, int *max_index_p,
		      void (*unpack)(void *, int *, int, int));

/* Send two integers, keep track of maximum index */
void _unpack_twointx_maxindx (int *dst1, int *dst2, int *max_index_p,
			      int proc, int count);
int _recv_twointx_maxindx (int *dst1, int *dst2, int tag,
			   int *max_index_p);
void _finish_twointx_maxindx (int *dst1, int *dst2, int tag, int
			      n_sent, int n_rcvd, int *max_index_p);

/* Increment a location */
void _unpack_increment (int *dst, int *max_index_p, int proc,
			int count);
int _recv_increment (int *dst, int tag, int *max_index_p);
void _finish_increment (int *dst, int tag, int n_sent, int n_rcvd,
			int *max_index_p);

/* Send an integer, keep track of maximum value */
void _unpack_intx_maxval (int *dst, int *max_p, int proc, int count);
int _recv_intx_maxval (int *dst, int tag, int *max_p);
void _finish_intx_maxval (int *dst, int *max_p, int tag, int n_sent,
			  int n_rcvd);

/* Send a perm and 32, 64 or 96 bits of key */
void _unpack_t32bits (t32bits *key_dst, int *perm_dst, int proc,
		      int count);
int _recv_t32bits (t32bits *key_dst, int tag, int *perm_dst);
void _finish_t32bits (t32bits *key_dst, int *perm_dst, int tag,
		      int n_sent, int n_rcvd);

void _unpack_t64bits (t64bits *key_dst, int *perm_dst, int proc,
		      int count);
int _recv_t64bits (t64bits *key_dst, int tag, int *perm_dst);
void _finish_t64bits (t64bits *key_dst, int *perm_dst, int tag,
		      int n_sent, int n_rcvd);

void _unpack_t96bits (t96bits *key_dst, int *perm_dst, int proc,
		      int count);
int _recv_t96bits (t96bits *key_dst, int tag, int *perm_dst);
void _finish_t96bits (t96bits *key_dst, int *perm_dst, int tag,
		      int n_sent, int n_rcvd);
