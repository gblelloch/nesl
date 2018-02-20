/*
* Copyright (c) 1992 Carnegie Mellon University 
*                    SCAL project: Guy Blelloch, Siddhartha Chatterjee,
*                                  Jonathan Hardwick, Jay Sipelstein,
*                                  Marco Zagha
* All Rights Reserved.
*
* Permission to use, copy, modify and distribute this software and its
* documentation is hereby granted, provided that both the copyright
* notice and this permission notice appear in all copies of the
* software, derivative works or modified versions, and any portions
* thereof, and that both notices appear in supporting documentation.
*
* CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
* CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
* ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
*
* The SCAL project requests users of this software to return to 
*
*  Guy Blelloch				guy.blelloch@cs.cmu.edu
*  School of Computer Science
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3891
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/
#include <cvl.h>
#include "defins.h"
#include "cray.h"

#include <unistd.h>

long rtclock();
long cpused();

/* -----------------------Timing functions-----------------------------*/

/* returns number of seconds and microseconds in argument
 * (number of clock ticks on cray)
 */

/* FIX: someday bury init of use_cpu_time and clock_period in cvl_init */


#define ALWAYS_USE_CPU_TIME

static use_cpu_time=(-1);

void tgt_fos(cvl_timer_p)	
cvl_timer_t *cvl_timer_p;
{
#ifdef ALWAYS_USE_CPU_TIME
  *cvl_timer_p = cpused();
#else
  char *getenv();
  if (use_cpu_time == -1) 
    use_cpu_time = (getenv("CVL_CPU_TIME") != (char *) 0);

  if (use_cpu_time) {
    *cvl_timer_p = cpused();
  } else {
    *cvl_timer_p = rtclock();
  }
#endif
}


/* double precision difference in seconds between two cvl_timer_t values:
 *   t1 occurs after t2
 */
double tdf_fos(t1, t2)
cvl_timer_t *t1, *t2;
{
  static double clock_period=0.0;
  if (clock_period == 0.0) {
    clock_period = 1.0 / (double) ((long) sysconf(_SC_CLK_TCK));
  }
  return (double) clock_period*(double)(*t1 - *t2);
}


int siz_fopb(length)
int length;
{
  int vlen_ignore, stride, rem_ignore;

  if (length == 0) {
    return WORDS_PER_VM;
  } else {
    _compute_shape_factors(length, &vlen_ignore, &stride, &rem_ignore);
    return(WORDS_PER_VM * stride);
  }
}

int siz_fob(length)
int length;
{
  return length;
}


int siz_fod(length)
int length;
{
  return length;
}


int siz_foz(length)
int length;
{
  return length;
}


/* --------------------Segment Descriptors---------------------------*/

/* n = number of elements in segmented vector */
/* m = number of segments in vector */
/* The segment descriptor is an integer vector of m segment lengths */

int siz_fos(n, m)   /* size of segment descriptor */
int n, m;
{
  return (siz_fopb(m) + siz_fopb(n) + 2*m + VL + VL); 
}



int mke_fov_scratch(n, m) int n; {return n;}

make_inplace(mke_fov,INPLACE_NONE)



/* Algorithm:
 *  0)  Test lengths to see if non-zero
 *  1a) Scan lengths to get start offsets
 *  1b) Copy lengths
 *  2)  Initialize uncompressed segdes to 0
 *  3)  Scatter 1's to the start locations
 *  4a) Compress segdes
 *  4b) Find last start for each row. 
 */
void mke_fov(sd, l, n, m, scratch)  /* create segment descriptor sd */
vec_p sd, l, scratch;               /* l is a vector of segment lengths */
int n, m;
{
#ifdef CVL_2_1
  /* This almost certainly works and should speed up unsegmented code,
   * but it hasn't been fully tested yet.
   * (Look at segdpe, segfpm, segbperm, and segbfp more carefully.)
   */
  if (m==1) return;
#endif
  {
    vec_p scanned_lengths, uncompressed_flags, compressed_flags;
    vec_p last_start, inclusive_last_start;
    vec_p nonzerop;
    vec_p new_scratch;
    int addsuzcpywuz();
    
    scanned_lengths = (vec_p) offsets_from_sd(sd, m);
    last_start = (vec_p) last_start_from_sd(sd, m);
    inclusive_last_start = (vec_p) inclusive_last_start_from_sd(sd, m);
    compressed_flags = (vec_p) bools_from_sd(sd, m);
    nonzerop = (vec_p) nonzerop_from_sd(sd, m);
    uncompressed_flags = (vec_p) scratch;
    new_scratch = (vec_p) _add_fov(uncompressed_flags, n);
    
    pboo_wuz(nonzerop, l, m, new_scratch);
    
    addsuzcpywuz(scanned_lengths, (vec_p) lengths_from_sd(sd, m),
                 l, 0, 0, m, new_scratch);
    if (n > 0) {
      dis_vuz(uncompressed_flags, 0, n, new_scratch);
      xscatterconst(uncompressed_flags, 1, scanned_lengths, m, new_scratch);
      clastindex(last_start, inclusive_last_start,
                 compressed_flags, uncompressed_flags, n, new_scratch);
    }
}

#ifdef DEBUG_SEG
if (m>1) {
  printf("NEW SEGDES:\n");
  _print_segdes(sd, n, m);
}
#endif
}



#ifdef DEBUG_SEG
_print_segdes(sd, n, m)  /* for debugging only */
vec_p sd;
int n, m;
{
  int i, j, k;
  int vl, s, rem;
  int vl_m, s_m, rem_m;
  vec_p lengths, offsets, last_start, inclusive_last_start, flags, nonzerop;

  lengths = (vec_p) lengths_from_sd(sd, m);
  offsets = (vec_p) offsets_from_sd(sd, m);
  nonzerop = (vec_p) nonzerop_from_sd(sd, m);
  last_start = (vec_p) last_start_from_sd(sd, m);
  inclusive_last_start = (vec_p) inclusive_last_start_from_sd(sd, m);
  flags = (vec_p) bools_from_sd(sd, m);

  if (n == 0) {printf("EMPTY VECTOR (length = 0)\n"); return 0;}
  _compute_shape_factors(n, &vl, &s, &rem);
  _compute_shape_factors(m, &vl_m, &s_m, &rem_m);

  printf("N=%d  M=%d   VL=%d  STRIDE=%d  REM=%d\n", n, m, vl, s, rem);
  printf("   m shape:  VL=%d  STRIDE=%d  REM=%d\n", vl_m, s_m, rem_m);

  printf("\nLENGTHS:\n");
  for (j = 0; j < m; j++) {
    printf("%d ", ext_vuz(lengths, j, m, 0));
  }
  printf("\nNONZEROP:\n");
  for (j = 0; j < m; j++) {
    printf("%d ", ext_vupb(nonzerop, j, m, 0));
  }
  printf("\nOFFSETS:\n");
  for (j = 0; j < m; j++) {
    printf("%d ", ext_vuz(offsets, j, m, 0));
  }
  printf("\nLAST START:\n");
  for (j = 0; j < vl; j++) {
    printf("%d ", ext_vuz(last_start, j, VL, 0));
  }
  printf("\nINCLUSIVE LAST START:\n");
  for (j = 0; j < vl; j++) {
    printf("%d ", ext_vuz(inclusive_last_start, j, VL, 0));
  }
#if 0
  printf("\nPACKED FLAGS:\n");
  for (j = 0; j < WORDS_PER_VM * s; j++) {
    printf("%x\n", ext_vuz(flags, j, WORDS_PER_VM * s, 0));
  }
#endif
  printf("\nFLAGS:\n");
  i = 0;
  for (k = 0; k < s; k++) {
    for (j = 0; j < vl; j++) {
      if (i >= n) break;
      i++;
      printf("%d", ext_vupb(flags, k+s*j, n, 0));
    }
    if (k == rem-1) vl--;
    printf("\n");
  }
  printf("\n");
  return 0;
}
#endif


void len_fos(l, sd, n, m, scratch)	/* inverse of mke_fos */
vec_p l, sd, scratch;
int n, m;
{
  if (m == 0) return;
  else if (m == 1) ((int *) l)[0] = n;
  else cpy_wuz(l, (vec_p) lengths_from_sd(sd, m), m, scratch);
}


int len_fos_scratch(n, m) int n, m; {return 0; }
make_inplace(len_fos,INPLACE_1)


/* -------------------Memory functions------------------------------*/

vec_p alo_foz(size)					/* allocate vector memory */
int size;							/* returns NULL if unsuccessful */
{
  return (vec_p) malloc(size * sizeof(int));
}

void fre_fov(pointer)				/* free vector memory */
vec_p pointer;
{
  free((char *)pointer);
}

void mov_fov(dest, src, size, scratch)
vec_p dest, src, scratch;
int size;
{
    memmove((char*)dest, (char*)src, (unsigned)size * sizeof(int));
}

int mov_fov_scratch(size)
int size;
{ return 0; }


/* ----------------Memory Arithmetic Functions---------------------*/

/* NB: These functions assume that CVL_SIZE_UNIT is sizeof(int)  */

vec_p add_fov(v, a)
vec_p v;
int a;
{
	return ( (vec_p) ((int *)v + a));
}

/* subtract two pointers and return size of interval in CVL_SIZE_UNITs */
int sub_fov(v1, v2)
vec_p v1, v2;
{
	if ((int *)v1 > (int *)v2)
		return ((int *) v1 - (int *)v2);
	else
		return ((int *) v2 - (int *)v1);
}

/* compare two vec_p's.  We need this because vectors maybe more complicated
 * in some implementations.
 */
cvl_bool eql_fov(v1, v2)
vec_p v1, v2;
{
	return ((int*) v1 == (int*)v2);
}


/* compare two vec_p's for inequality.  Return is same as strcmp. */
int cmp_fov(v1, v2)
vec_p v1, v2;
{
    return ((int *)v1 - (int *)v2);
}


int cpy_wus_scratch(n, m) { return 0; }
make_inplace(cpy_wus,INPLACE_1)

void cpy_wus(d, s, n, m, scratch)
    vec_p d, s, scratch;
    int n, m;
{
  cpy_wuz(d, s, siz_fos(n, m), scratch);
}


/* ----------------------CVL - C conversion functions -------------*/

#define make_v2c(_name, _type)					\
    void _name(d, s, len, scratch)				\
    vec_p s, scratch;						\
    _type * d;							\
    int len;							\
	{							\
	bcopy((char*)s, (char*)d, sizeof(_type) * len);		\
	}							\
    make_no_scratch(_name)					\
    make_inplace(_name,INPLACE_NONE)

make_v2c(v2c_fuz, int)
make_v2c(v2c_fub, cvl_bool)
make_v2c(v2c_fud, double)


#define make_c2v(_name, _type)					\
    void _name(d, s, len, scratch)				\
    vec_p d, scratch;						\
    _type * s;							\
    int len;							\
	{							\
	bcopy((char*)s, (char*)d, sizeof(_type) * len);		\
	}							\
    make_no_scratch(_name)					\
    make_inplace(_name,INPLACE_NONE)

make_c2v(c2v_fuz, int)
make_c2v(c2v_fub, cvl_bool)
make_c2v(c2v_fud, double)

/* ------------------------ Random number functions ------------------- */

void rnd_foz (seed)
int seed;
{
  RANSET(&seed);
}
