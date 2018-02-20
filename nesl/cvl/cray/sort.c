/*
* Copyright (c) 1993 Carnegie Mellon University 
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
*  Marco Zagha and Guy Blelloch		marco.zagha@cs.cmu.edu	
*  School of Computer Science		guy.blelloch@cs.cmu.edu	
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3891
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/
/*
 * 2/14/92 (marcoz):  Distributable code begun
 * 3/13/92 (marcoz):  Added zrank function
 * 3/14/92 (marcoz):  Added fortran entry points
 * 3/16/92 (marcoz):  Implemented signed versions
 * 3/19/92 (marcoz):  Implemented memory efficient versions
 * 3/26/92 (marcoz):  Added multiprocessor versions
 */
#include <stdio.h>

#define WORDSIZE 64

#if _MAXVL == 128
#define C90 
#endif

#ifdef C90
#define LOG_VELEMENTS 7   /* log of number of virtual processors */
#define VL            128
#define P 8              /* number of threads (DON'T CHANGE THIS) */
#define MLOG_VELEMENTS 10 /* log of number of virtual processors (multiproc) */
#else
#define LOG_VELEMENTS 6   /* log of number of virtual processors */
#define VL            64
#define P 8               /* number of threads (DON'T CHANGE THIS) */
#define MLOG_VELEMENTS 9  /* log of number of virtual processors (multiproc) */
#endif

typedef long int  INT;

#define log2(N) (WORDSIZE - _ldzero(N) - 1)

#define MIN_RADIX     3

#ifdef C90
#define MULTIPROC_THRESHOLD  20000  /* see note below before changing this */
#else
#define MULTIPROC_THRESHOLD  10000  /* see note below before changing this */
#endif
/* returns number of bits per digit */
/* NOTE: having a minimum radix implies needing a minimum amount of
 *       temporary memory independent of the problem size N.
 *       The bucket array must be at least 2 ^ (MIN_RADIX + LOG_VELEMENTS)
 *       elements or 2 ^ (MIN_RADIX + MLOG_VELEMENTS) for the multiprocessor
 *       version.  For example, with MIN_RADIX=3, at least 512 buckets on the
 *       Y-MP (1024 on the C90)
 *       are needed for single thread and 512*P (or 1024*P) for P threads.
 *       Generally the MULTIPROC_THRESHOLD is set low enough so that
 *       the amount of memory needed is 2 ^ (MIN_RADIX + LOG_VELEMENTS).
 */
static int radix_o_matic(N)
int N;
{ 
  int best = log2(N) - LOG_VELEMENTS - 1;
  if (best < MIN_RADIX) best = MIN_RADIX;
  return best;
}

#ifdef MULTI
/* returns number of bits per digit */
static int mradix_o_matic(N)
int N;
{ 
  int best = log2(N) - MLOG_VELEMENTS - 1;
  if (best < MIN_RADIX) best = MIN_RADIX;
  return best;
}
#endif

/* returns number of bits to sort */
static int key_size_o_matic(a, data_vlen, data_stride, data_full)
INT a[];
int data_vlen, data_stride, data_full;
{
  int size;
  INT reduction;
  int XIORRUZ();

  reduction = XIORRUZ(a, 0, data_vlen, data_stride, data_full);
  size = log2(reduction) + 1;
  if (size == 0) size = 1;
  return size;
}

#ifdef MULTI
/* returns number of bits to sort */
static int mkey_size_o_matic(a, data_offset, data_vlen, data_stride, data_full)
INT a[];
int data_offset[P], data_vlen[P], data_stride[P], data_full[P];
{
  int j, size;
  INT reduction;
  int tempsums[P];
  void MXIORRUZ();

  MXIORRUZ(a, tempsums, 0, data_offset, data_vlen, data_stride, data_full);
  reduction = 0;
  for (j = 0; j < P; j++) reduction |= tempsums[j];
  size = log2(reduction) + 1;
  if (size == 0) size = 1;
  return size;
}
#endif

/* Make the minimum key zero. */
static INT signed_to_unsigned(a, data_vlen, data_stride, data_full)
INT a[];
int data_vlen, data_stride, data_full;
{
  INT min, XMINRUZ();
  void ADDSCAL();

  min = XMINRUZ(a, a[0], data_vlen, data_stride, data_full);
  /* a[i] -= min_key */
  ADDSCAL(a, a, 0 - min, data_vlen, data_stride, data_full);
  return min;
}

#ifdef MULTI
/* Make the minimum key zero. */
static INT msigned_to_unsigned(a, 
                               data_offset, data_vlen, data_stride, data_full)
INT a[];
int data_offset[P], data_vlen[P], data_stride[P], data_full[P];
{
  int j;
  INT min, tempsums[P];
  void MXMINRUZ(), MADDSCA();

  min = a[0];
  MXMINRUZ(a, tempsums, min, data_offset, data_vlen, data_stride, data_full);
  for (j = 0; j < P; j++) {
    if (tempsums[j] < min) min = tempsums[j];
  }
  /* a[i] -= min_key */
  MADDSCA(a, a, 0 - min, data_offset, data_vlen, data_stride, data_full);
  return min;
}
#endif

/* restore keys to original values */
static void unsigned_to_signed(a, min_key, data_vlen, data_stride, data_full)
INT a[];
INT min_key; 
int data_vlen, data_stride, data_full;
{
  void ADDSCAL();
  /* a[i] += min_key */
  ADDSCAL(a, a, min_key, data_vlen, data_stride, data_full);
}

#ifdef MULTI
/* restore keys to original values */
static void munsigned_to_signed(a, min_key, 
                                data_offset, data_vlen, data_stride, data_full)
INT a[];
INT min_key; 
int data_offset[P], data_vlen[P], data_stride[P], data_full[P];
{
  void MADDSCA();
  /* a[i] += min_key */
  MADDSCA(a, a, min_key, data_offset, data_vlen, data_stride, data_full);
}

/* Multiprocessor scan.  temp must be of size P*VL + P */
maddsuz(d, temp, offset, vlength, stride, remain)
INT *d;
int *temp;
int offset[P], vlength[P], stride[P], remain[P];
{
  void MSCANA(), MSCANB();
  int j, sum, oldsum;
  int *procsums;

  procsums = &temp[P*VL];
  MSCANA(d, temp, 0, offset, vlength, stride, remain);
  sum = oldsum = 0;
  for (j = 0; j < P; j++) {
    sum += procsums[j];
    procsums[j] = oldsum;
    oldsum = sum;    
  }
  MSCANB(d, d, temp, offset, vlength, stride, remain);
}
#endif


#define BANKS_MASK 3   /* don't allow stride to be multiple of 4 */

/* sets vlen to approximately Vl and stride to approximately len/vlen, 
 * but corrects for bad strides and edge effects. 
 */
static void compute_shape_factors(len, vlen, stride, rem)
int len, *vlen, *stride, *rem;
{  
  int len1, s, div;
  len1 = len-1;
  s = (len1>>LOG_VELEMENTS)+1;
  s = s + !(s & BANKS_MASK);  /* increment stride if s is bad stride */
  *stride = s; 
  div = len1/s;
  *vlen = div+1;
  *rem = len1-div * s+1;
}


#ifdef MULTI
/*
 * Divides up the work among P threads giving some threads N/P elements
 * and some N/P + 1 (if necessary).  Then each thread computes shape
 * factors as in the single-threaded versions.  The shape factors
 * are put into arrays, each of P elements, one for each thread.
 */
static void parallel_shape_factors(N, offset, vlength, stride, remain)
int N;
int offset[], vlength[], stride[], remain[];
{
  int j, N_over_P, off, current_len, vlen, str, rem, remainder;

  N_over_P = N/P;
  remainder = N - N_over_P * P;
  off = 0;
  if (remainder != 0) {
    current_len = N_over_P+1;
    compute_shape_factors(current_len, &vlen, &str, &rem);
    for (j = 0; j < remainder; j++) {
      offset[j] = off;
      vlength[j] = vlen;
      remain[j] = rem;
      stride[j] = str;
      off += current_len;
    }       
  }
  current_len = N_over_P;
  compute_shape_factors(current_len, &vlen, &str, &rem);
  for (j = remainder; j < P; j++) {
    offset[j] = off;
    vlength[j] = vlen;
    remain[j] = rem;
    stride[j] = str;
    off += current_len;
  }       
}
#endif

static void check_mode(mode)
int mode;
{
  if ((mode != 0) && (mode != 1) && (mode != 10) && (mode != 11)) {
    fprintf(stderr, 
  "Only signed and unsigned integers are supported in the current release.\n");
    exit(1);
  }
}


/* Fortran entry points: upper case function names, call-by-reference */
void
ZSORT(mode, temp, source, N)
int *mode;
INT *temp;
INT *source;
int *N;
{
  void zsort();
  zsort(*mode, temp, source, *N);
}

#ifdef MULTI
void
MZSORT(mode, temp, source, N)
int *mode;
INT *temp;
INT *source;
int *N;
{
  void mzsort();
  mzsort(*mode, temp, source, *N);
}
#endif

void
ZORDERS(mode, temp, source, index, N)
int *mode;
INT *temp;
INT *source;
INT *index;
int *N;
{
  void zorders();
  zorders(*mode, temp, source, index, *N);
}

#ifdef MULTI
void
MZORDERS(mode, temp, source, index, N)
int *mode;
INT *temp;
INT *source;
INT *index;
int *N;
{
  void mzorders();
  mzorders(*mode, temp, source, index, *N);
}
#endif

void
ZRANK(mode, temp, source, index, N)
int *mode;
INT *temp;
INT *source;
INT *index;
int *N;
{
  void zrank();
  zrank(*mode, temp, source, index, *N);
}

#ifdef MULTI
void
MZRANK(mode, temp, source, index, N)
int *mode;
INT *temp;
INT *source;
INT *index;
int *N;
{
  void mzrank();
  mzrank(*mode, temp, source, index, *N);
}
#endif

/* C entry points */

void
zsort(mode, temp, source, N)
int mode;
INT *temp;
INT *source;
int N;
{
  int j, nbits, bits_per_pass, B, B_extent;
  int bucket_vlen, bucket_stride, bucket_full;
  int data_vlen, data_stride, data_full;
  INT *buckets;
  INT *Vec_swap, *Vec1, *Vec2;
  INT mask, min_key;
  void DISVUZ(), XCPYWUZ(), SRTRHIS(), SRTLHIS(), SRTRPER(), SRTLPER();

  check_mode(mode);
  buckets = temp + N;
  bits_per_pass = radix_o_matic(N);
  B = 1 << bits_per_pass;
  B_extent = B << LOG_VELEMENTS;
  compute_shape_factors(B_extent, &bucket_vlen, &bucket_stride, &bucket_full);
  compute_shape_factors(N, &data_vlen, &data_stride, &data_full);

  Vec1 = source;
  Vec2 = temp;
  mask = B - 1;  /* two's complement hack */
  if ((mode == 1) || (mode == 11)) 
    min_key = signed_to_unsigned(source, data_vlen, data_stride, data_full);
  nbits = key_size_o_matic(source, data_vlen, data_stride, data_full);
  for (j = 0; j < nbits; j += bits_per_pass) {
    /* clear buckets */
    DISVUZ(buckets, 0, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit, convert to a bucket index, and histogram */
    if (j > LOG_VELEMENTS) {
      SRTRHIS(Vec1, buckets, mask, j-LOG_VELEMENTS,
              data_vlen, data_stride, data_full);
    } else {
      SRTLHIS(Vec1, buckets, mask, LOG_VELEMENTS-j,
              data_vlen, data_stride, data_full);
    }
    /* scan buckets */
    ADDSUZ(buckets, buckets, 0, 0, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit, convert to a bucket index, and permute */
    if (j > LOG_VELEMENTS) {
      SRTRPER(Vec1, buckets, mask, j-LOG_VELEMENTS, Vec2,
              data_vlen, data_stride, data_full);
    } else {
      SRTLPER(Vec1, buckets, mask, LOG_VELEMENTS-j, Vec2,
              data_vlen, data_stride, data_full);
    }
    Vec_swap = Vec1; Vec1 = Vec2; Vec2 = Vec_swap;
    mask <<= bits_per_pass;
  }
  if (Vec1 != source) 
    XCPYWUZ(source, Vec1, data_vlen, data_stride, data_full);
  if ((mode == 1) || (mode == 11)) 
    unsigned_to_signed(source, min_key, data_vlen, data_stride, data_full);
}



/* The result is left in index.  
 * To sort using orders: Sorted[i] = source[index[i]]
 */
void
zorders(mode, temp, source, index, N)
int mode;
INT *temp;
INT *source;
INT *index;
int N;
{
  int j, nbits, bits_per_pass, B, B_extent;
  int bucket_vlen, bucket_stride, bucket_full;
  int data_vlen, data_stride, data_full;
  INT *buckets;
  INT *Vec_swap, *Index1, *Index2;
  INT mask, min_key;
  void XINDLUZ(), DISVUZ(), XCPYWUZ();
  void ORDRHIS(), ORDLHIS(), ORDRPER(), ORDLPER();

  check_mode(mode);
  buckets = temp + N;
  bits_per_pass = radix_o_matic(N);
  B = 1 << bits_per_pass;
  B_extent = B << LOG_VELEMENTS;
  compute_shape_factors(B_extent, &bucket_vlen, &bucket_stride, &bucket_full);
  compute_shape_factors(N, &data_vlen, &data_stride, &data_full);

  if (mode < 10) XINDLUZ(index, data_vlen, data_stride, data_full);

  Index1 = index;
  Index2 = temp;
  mask = B - 1;  /* two's complement hack */
  if ((mode == 1) || (mode == 11)) 
    min_key = signed_to_unsigned(source, data_vlen, data_stride, data_full);
  nbits = key_size_o_matic(source, data_vlen, data_stride, data_full);
  for (j = 0; j < nbits; j += bits_per_pass) {
    /* clear buckets */
    DISVUZ(buckets, 0, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit[i] from key[Index[i]] and convert to a bucket index */
    if (j > LOG_VELEMENTS) {
      ORDRHIS(source, Index1, buckets, mask, j-LOG_VELEMENTS,
              data_vlen, data_stride, data_full);
    } else {
      ORDLHIS(source, Index1, buckets, mask, LOG_VELEMENTS-j,
              data_vlen, data_stride, data_full);
    }
    /* scan buckets */
    ADDSUZ(buckets, buckets, 0, 0, bucket_vlen, bucket_stride, bucket_full);
    /* permute current permutation */
    if (j > LOG_VELEMENTS) {
      ORDRPER(source, Index1, buckets, mask, j-LOG_VELEMENTS,
              Index2, data_vlen, data_stride, data_full);
    } else {
      ORDLPER(source, Index1, buckets, mask, LOG_VELEMENTS-j,
              Index2, data_vlen, data_stride, data_full);
    }
    Vec_swap = Index1; Index1 = Index2; Index2 = Vec_swap;
    mask <<= bits_per_pass;
  }
  if (Index1 != index) 
    XCPYWUZ(index, Index1, data_vlen, data_stride, data_full);
  if ((mode == 1) || (mode == 11)) 
    unsigned_to_signed(source, min_key, data_vlen, data_stride, data_full);
}




/* The result is left in index.  
 * To sort using rank: Sorted[index[i]] = source[i]
 *
 * Basic idea behind zrank: just like zorders, except on last pass instead of 
 * doing NewPerm[rank[i]] = Perm[i], do NewPerm[Perm[i]] = rank[i].
 */
void
zrank(mode, temp, source, index, N)
int mode;
INT *temp;
INT *source;
INT *index;
int N;
{
  int j, nbits, bits_per_pass, B, B_extent;
  int bucket_vlen, bucket_stride, bucket_full;
  int data_vlen, data_stride, data_full;
  INT *buckets;
  INT *Vec_swap, *Index1, *Index2;
  INT mask, min_key;
  void XINDLUZ(), XCPYWUZ(), DISVUZ();
  void ORDRHIS(), ORDLHIS(), ORDRPER(), ORDLPER();
  void RNKRPER(), RNKLPER();

  check_mode(mode);
  buckets = temp + N;
  bits_per_pass = radix_o_matic(N);
  B = 1 << bits_per_pass;
  B_extent = B << LOG_VELEMENTS;
  compute_shape_factors(B_extent, &bucket_vlen, &bucket_stride, &bucket_full);
  compute_shape_factors(N, &data_vlen, &data_stride, &data_full);

  if (mode < 10) XINDLUZ(index, data_vlen, data_stride, data_full);

  Index1 = index;
  Index2 = temp;
  mask = B - 1;  /* two's complement hack */
  if ((mode == 1) || (mode == 11)) 
    min_key = signed_to_unsigned(source, data_vlen, data_stride, data_full);
  nbits = key_size_o_matic(source, data_vlen, data_stride, data_full);
  for (j = 0; j < nbits; j += bits_per_pass) {
    /* clear buckets */
    DISVUZ(buckets, 0, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit[i] from key[Index[i]] and convert to a bucket index */
    if (j > LOG_VELEMENTS) {
      ORDRHIS(source, Index1, buckets, mask, j-LOG_VELEMENTS,
              data_vlen, data_stride, data_full);
    } else {
      ORDLHIS(source, Index1, buckets, mask, LOG_VELEMENTS-j,
              data_vlen, data_stride, data_full);
    }
    /* scan buckets */
    ADDSUZ(buckets, buckets, 0, 0, bucket_vlen, bucket_stride, bucket_full);
    /* permute current permutation */
    if (j + bits_per_pass >= nbits) {  /* last pass */
      if (j > LOG_VELEMENTS) {
        RNKRPER(source, Index1, buckets, mask, j-LOG_VELEMENTS,
                Index2, data_vlen, data_stride, data_full);
      } else {
        RNKLPER(source, Index1, buckets, mask, LOG_VELEMENTS-j,
                Index2, data_vlen, data_stride, data_full);
      }
    } else {
      if (j > LOG_VELEMENTS) {
        ORDRPER(source, Index1, buckets, mask, j-LOG_VELEMENTS,
                Index2, data_vlen, data_stride, data_full);
      } else {
        ORDLPER(source, Index1, buckets, mask, LOG_VELEMENTS-j,
                Index2, data_vlen, data_stride, data_full);
      }
    }
    Vec_swap = Index1; Index1 = Index2; Index2 = Vec_swap;
    mask <<= bits_per_pass;
  }
  if (Index1 != index) 
    XCPYWUZ(index, Index1, data_vlen, data_stride, data_full);
  if ((mode == 1) || (mode == 11)) 
    unsigned_to_signed(source, min_key, data_vlen, data_stride, data_full);
}

#ifdef MULTI
void
mzsort(mode, temp, source, N)
int mode;
INT *temp;
INT *source;
int N;
{
  int j, nbits, bits_per_pass, B, B_extent;
  int bucket_offset[P], bucket_vlen[P], bucket_stride[P], bucket_full[P];
  int data_offset[P], data_vlen[P], data_stride[P], data_full[P];
  int scan_temp[P*VL + P];
  INT *buckets;
  INT *Vec_swap, *Vec1, *Vec2;
  INT mask, min_key;
  void MDISVUZ(), MXCPYWUZ(), MSRTRHI(), MSRTLHI(), MSRTRPE(), MSRTLPE();

  if (N < MULTIPROC_THRESHOLD) {
    zsort(mode, temp, source, N);
    return;
  }
  check_mode(mode);
  buckets = temp + N;
  bits_per_pass = mradix_o_matic(N);
  B = 1 << bits_per_pass;
  B_extent = B << MLOG_VELEMENTS;
  parallel_shape_factors(B_extent, bucket_offset, bucket_vlen, 
                         bucket_stride, bucket_full);
  parallel_shape_factors(N, data_offset, data_vlen, 
                         data_stride, data_full);

  Vec1 = source;
  Vec2 = temp;
  mask = B - 1;  /* two's complement hack */
  if ((mode == 1) || (mode == 11)) 
    min_key = msigned_to_unsigned(source, data_offset, data_vlen, 
                                  data_stride, data_full);
  nbits = mkey_size_o_matic(source, 
                            data_offset, data_vlen, data_stride, data_full);
  for (j = 0; j < nbits; j += bits_per_pass) {
    /* clear buckets */
    MDISVUZ(buckets, 0, 
            bucket_offset, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit, convert to a bucket index, and histogram */
    if (j > MLOG_VELEMENTS) {
      MSRTRHI(Vec1, buckets, mask, j-MLOG_VELEMENTS,
              data_offset, data_vlen, data_stride, data_full);
    } else {
      MSRTLHI(Vec1, buckets, mask, MLOG_VELEMENTS-j,
              data_offset, data_vlen, data_stride, data_full);
    }
    /* scan buckets */
    maddsuz(buckets, scan_temp, 
            bucket_offset, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit, convert to a bucket index, and permute */
    if (j > MLOG_VELEMENTS) {
      MSRTRPE(Vec1, buckets, mask, j-MLOG_VELEMENTS, Vec2,
              data_offset, data_vlen, data_stride, data_full);
    } else {
      MSRTLPE(Vec1, buckets, mask, MLOG_VELEMENTS-j, Vec2,
              data_offset, data_vlen, data_stride, data_full);
    }
    Vec_swap = Vec1; Vec1 = Vec2; Vec2 = Vec_swap;
    mask <<= bits_per_pass;
  }
  if (Vec1 != source) 
    MXCPYWUZ(source, Vec1, data_offset, data_vlen, data_stride, data_full);
  if ((mode == 1) || (mode == 11)) 
    munsigned_to_signed(source, min_key,
                        data_offset, data_vlen, data_stride, data_full);
}


void
mzorders(mode, temp, source, index, N)
int mode;
INT *temp;
INT *source;
INT *index;
int N;
{
  int j, nbits, bits_per_pass, B, B_extent;
  int bucket_offset[P], bucket_vlen[P], bucket_stride[P], bucket_full[P];
  int data_offset[P], data_vlen[P], data_stride[P], data_full[P];
  int scan_temp[P*VL + P];
  INT *buckets;
  INT *Vec_swap, *Index1, *Index2;
  INT mask, min_key;
  void MXINDLUZ();
  void MDISVUZ(), MXCPYWUZ(), MORDRHI(), MORDLHI(), MORDRPE(), MORDLPE();

  if (N < MULTIPROC_THRESHOLD) {
    zorders(mode, temp, source, index, N);
    return;
  }
  check_mode(mode);
  buckets = temp + N;
  bits_per_pass = mradix_o_matic(N);
  B = 1 << bits_per_pass;
  B_extent = B << MLOG_VELEMENTS;
  parallel_shape_factors(B_extent, bucket_offset, bucket_vlen, 
                         bucket_stride, bucket_full);
  parallel_shape_factors(N, data_offset, data_vlen, 
                         data_stride, data_full);

  if (mode < 10) 
    MXINDLUZ(index, data_offset, data_vlen, data_stride, data_full);

  Index1 = index;
  Index2 = temp;
  mask = B - 1;  /* two's complement hack */
  if ((mode == 1) || (mode == 11)) 
    min_key = msigned_to_unsigned(source, data_offset, data_vlen, 
                                  data_stride, data_full);
  nbits = mkey_size_o_matic(source, 
                            data_offset, data_vlen, data_stride, data_full);
  for (j = 0; j < nbits; j += bits_per_pass) {
    /* clear buckets */
    MDISVUZ(buckets, 0, 
            bucket_offset, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit, convert to a bucket index, and histogram */
    if (j > MLOG_VELEMENTS) {
      MORDRHI(source, Index1, buckets, mask, j-MLOG_VELEMENTS,
              data_offset, data_vlen, data_stride, data_full);
    } else {
      MORDLHI(source, Index1, buckets, mask, MLOG_VELEMENTS-j,
              data_offset, data_vlen, data_stride, data_full);
    }
    /* scan buckets */
    maddsuz(buckets, scan_temp, 
            bucket_offset, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit, convert to a bucket index, and permute */
    if (j > MLOG_VELEMENTS) {
      MORDRPE(source, Index1, buckets, mask, j-MLOG_VELEMENTS, Index2,
              data_offset, data_vlen, data_stride, data_full);
    } else {
      MORDLPE(source, Index1, buckets, mask, MLOG_VELEMENTS-j, Index2,
              data_offset, data_vlen, data_stride, data_full);
    }
    Vec_swap = Index1; Index1 = Index2; Index2 = Vec_swap;
    mask <<= bits_per_pass;
  }
  if (Index1 != index) 
    MXCPYWUZ(index, Index1, data_offset, data_vlen, data_stride, data_full);
  if ((mode == 1) || (mode == 11)) 
    munsigned_to_signed(source, min_key,
                        data_offset, data_vlen, data_stride, data_full);
}



void
mzrank(mode, temp, source, index, N)
int mode;
INT *temp;
INT *source;
INT *index;
int N;
{
  int j, nbits, bits_per_pass, B, B_extent;
  int bucket_offset[P], bucket_vlen[P], bucket_stride[P], bucket_full[P];
  int data_offset[P], data_vlen[P], data_stride[P], data_full[P];
  int scan_temp[P*VL + P];
  INT  *buckets;
  INT *Vec_swap, *Index1, *Index2;
  INT mask, min_key;
  void MXINDLUZ();
  void MDISVUZ(), MXCPYWUZ(), MORDRHI(), MORDLHI(), MORDRPE(), MORDLPE();
  void MRNKRPE(), MRNKLPE();

  if (N < MULTIPROC_THRESHOLD) {
    zrank(mode, temp, source, index, N);
    return;
  }
  check_mode(mode);
  buckets = temp + N;
  bits_per_pass = mradix_o_matic(N);
  B = 1 << bits_per_pass;
  B_extent = B << MLOG_VELEMENTS;
  parallel_shape_factors(B_extent, bucket_offset, bucket_vlen, 
                         bucket_stride, bucket_full);
  parallel_shape_factors(N, data_offset, data_vlen, 
                         data_stride, data_full);

  if (mode < 10) 
    MXINDLUZ(index, data_offset, data_vlen, data_stride, data_full);

  Index1 = index;
  Index2 = temp;
  mask = B - 1;  /* two's complement hack */
  if ((mode == 1) || (mode == 11)) 
    min_key = msigned_to_unsigned(source, data_offset, data_vlen, 
                                  data_stride, data_full);
  nbits = mkey_size_o_matic(source, 
                            data_offset, data_vlen, data_stride, data_full);
  for (j = 0; j < nbits; j += bits_per_pass) {
    /* clear buckets */
    MDISVUZ(buckets, 0, 
            bucket_offset, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit, convert to a bucket index, and histogram */
    if (j > MLOG_VELEMENTS) {
      MORDRHI(source, Index1, buckets, mask, j-MLOG_VELEMENTS,
              data_offset, data_vlen, data_stride, data_full);
    } else {
      MORDLHI(source, Index1, buckets, mask, MLOG_VELEMENTS-j,
              data_offset, data_vlen, data_stride, data_full);
    }
    /* scan buckets */
    maddsuz(buckets, scan_temp, 
            bucket_offset, bucket_vlen, bucket_stride, bucket_full);
    /* extract digit, convert to a bucket index, and permute */
    if (j + bits_per_pass >= nbits) {  /* last pass */
      if (j > MLOG_VELEMENTS) {
        MRNKRPE(source, Index1, buckets, mask, j-MLOG_VELEMENTS, Index2,
                data_offset, data_vlen, data_stride, data_full);
      } else {
        MRNKLPE(source, Index1, buckets, mask, MLOG_VELEMENTS-j, Index2,
                data_offset, data_vlen, data_stride, data_full);
      }
    } else {
      if (j > MLOG_VELEMENTS) {
        MORDRPE(source, Index1, buckets, mask, j-MLOG_VELEMENTS, Index2,
                data_offset, data_vlen, data_stride, data_full);
      } else {
        MORDLPE(source, Index1, buckets, mask, MLOG_VELEMENTS-j, Index2,
                data_offset, data_vlen, data_stride, data_full);
      }
    }
    Vec_swap = Index1; Index1 = Index2; Index2 = Vec_swap;
    mask <<= bits_per_pass;
  }
  if (Index1 != index) 
    MXCPYWUZ(index, Index1, data_offset, data_vlen, data_stride, data_full);
  if ((mode == 1) || (mode == 11)) 
    munsigned_to_signed(source, min_key,
                        data_offset, data_vlen, data_stride, data_full);
}
#endif
