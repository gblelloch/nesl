
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
*  Guy Blelloch	and Marco Zagha		guy.blelloch@cs.cmu.edu
*  School of Computer Science           marco.zagha@cs.cmu.edu
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3891
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.

* This file generated automatically by assembler on 12/6/1993 11:53

* SRT_LHIST
* Extracts a digit, converts to bucket ptr, and histograms (internal version)
        IDENT   SRTLHIS (SRC, BUCKETS, MASK, SHIFT, VLENGTH, STRIDE, REMAIN)
        ENTRY   SRTLHIS

        BASE    D       * base 10
SRTLHIS ENTER  NP=7    * # parameters
* int *SRC;
* int *BUCKETS;
* int MASK; 
* int SHIFT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,3,ARGPTR=A6
*                               MASK
        ARGADD  A3,4,ARGPTR=A6
*                               SHIFT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
        S3      1       
        V1      0       
        V1      S3+V1        * V1 = S3 + V1 
        V0,VM   V1,N         * Compress Index Where V1 Non-Zero 
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S3      1       
        T70     S3      
        S4      1       
        A0      S2      
        JAZ     L5      
L1      =       P.*     
        S3      S2&S4        * logical and 
* Jump to start if the number of iterations is even
        A0      S3      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5<A3        * V6 = V5 left shift A3 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A2      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        V3      V2+V1        * V3 = V2 + V1 
        A0      A2      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 1st body ***
        S2      S2-S4   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5<A3        * V6 = V5 left shift A3 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A2      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        V3      V2+V1        * V3 = V2 + V1 
        A0      A2      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 2nd body ***
        S2      S2-S4   
        A7      A7+1    
        A0      S2      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S3      T70     
        A0      S3      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S3      0       
        T70     S3      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S3      A5      
        S2      T71     
        S2      S3-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END SRT_LHIST

* SRT_RHIST
* Extracts a digit, converts to bucket ptr, and histograms (internal version)
        IDENT   SRTRHIS (SRC, BUCKETS, MASK, SHIFT, VLENGTH, STRIDE, REMAIN)
        ENTRY   SRTRHIS

        BASE    D       * base 10
SRTRHIS ENTER  NP=7    * # parameters
* int *SRC;
* int *BUCKETS;
* int MASK; 
* int SHIFT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,3,ARGPTR=A6
*                               MASK
        ARGADD  A3,4,ARGPTR=A6
*                               SHIFT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
        S3      1       
        V1      0       
        V1      S3+V1        * V1 = S3 + V1 
        V0,VM   V1,N         * Compress Index Where V1 Non-Zero 
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S3      1       
        T70     S3      
        S4      1       
        A0      S2      
        JAZ     L5      
L1      =       P.*     
        S3      S2&S4        * logical and 
* Jump to start if the number of iterations is even
        A0      S3      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5>A3        * V6 = V5 right shift A3 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A2      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        V3      V2+V1        * V3 = V2 + V1 
        A0      A2      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 1st body ***
        S2      S2-S4   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5>A3        * V6 = V5 right shift A3 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A2      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        V3      V2+V1        * V3 = V2 + V1 
        A0      A2      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 2nd body ***
        S2      S2-S4   
        A7      A7+1    
        A0      S2      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S3      T70     
        A0      S3      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S3      0       
        T70     S3      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S3      A5      
        S2      T71     
        S2      S3-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END SRT_RHIST

* ORD_LHIST
* Gathers a digit, converts to bucket ptr, and histograms (internal version)
        IDENT   ORDLHIS (SRC, INDEX, BUCKETS, MASK, SSHIFT, VLENGTH, STRIDE, REMAIN)
        ENTRY   ORDLHIS

        BASE    D       * base 10
ORDLHIS ENTER  NP=8    * # parameters
* int *SRC;
* int *INDEX;
* int *BUCKETS;
* int MASK; 
* int SSHIFT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               INDEX
        ARGADD  A3,3,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,4,ARGPTR=A6
*                               MASK
        ARGADD  S2,5,ARGPTR=A6
*                               SSHIFT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      S2      
        S4      1       
        V6      0       
        V6      S4+V6        * V6 = S4 + V6 
        V0,VM   V6,N         * Compress Index Where V6 Non-Zero 
        S4      1       
        A6      0       
        B71     A6      
        T71     S3      
        VL      A4      
        S5      1       
        T70     S5      
        S6      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S5      S3&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5<A7        * V6 = V5 left shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        V3      S4+V2        * V3 = S4 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 1st body ***
        S3      S3-S6   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5<A7        * V6 = V5 left shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        V3      S4+V2        * V3 = S4 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 2nd body ***
        S3      S3-S6   
        A6      A6+1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S5      T70     
        A0      S5      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S5      0       
        T70     S5      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S5      A5      
        S3      T71     
        S3      S5-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S3      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END ORD_LHIST

* ORD_RHIST
* Gathers a digit, converts to bucket ptr, and histograms (internal version)
        IDENT   ORDRHIS (SRC, INDEX, BUCKETS, MASK, SSHIFT, VLENGTH, STRIDE, REMAIN)
        ENTRY   ORDRHIS

        BASE    D       * base 10
ORDRHIS ENTER  NP=8    * # parameters
* int *SRC;
* int *INDEX;
* int *BUCKETS;
* int MASK; 
* int SSHIFT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               INDEX
        ARGADD  A3,3,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,4,ARGPTR=A6
*                               MASK
        ARGADD  S2,5,ARGPTR=A6
*                               SSHIFT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      S2      
        S4      1       
        V6      0       
        V6      S4+V6        * V6 = S4 + V6 
        V0,VM   V6,N         * Compress Index Where V6 Non-Zero 
        S4      1       
        A6      0       
        B71     A6      
        T71     S3      
        VL      A4      
        S5      1       
        T70     S5      
        S6      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S5      S3&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5>A7        * V6 = V5 right shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        V3      S4+V2        * V3 = S4 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 1st body ***
        S3      S3-S6   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5>A7        * V6 = V5 right shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        V3      S4+V2        * V3 = S4 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 2nd body ***
        S3      S3-S6   
        A6      A6+1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S5      T70     
        A0      S5      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S5      0       
        T70     S5      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S5      A5      
        S3      T71     
        S3      S5-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S3      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END ORD_RHIST

* SRT_LPERM
* Extracts a digit, converts to bucket ptr, gathers rank, permutes source (internal version)
        IDENT   SRTLPER (SRC, BUCKETS, MASK, SSHIFT, DEST, VLENGTH, STRIDE, REMAIN)
        ENTRY   SRTLPER

        BASE    D       * base 10
SRTLPER ENTER  NP=8    * # parameters
* int *SRC;
* int *BUCKETS;
* int MASK; 
* int SSHIFT; 
* int *DEST;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,3,ARGPTR=A6
*                               MASK
        ARGADD  S2,4,ARGPTR=A6
*                               SSHIFT
        ARGADD  A3,5,ARGPTR=A6
*                               DEST
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      S2      
        S4      1       
        V5      0       
        V5      S4+V5        * V5 = S4 + V5 
        V0,VM   V5,N         * Compress Index Where V5 Non-Zero 
        S4      1       
        A6      0       
        B71     A6      
        T71     S3      
        VL      A4      
        S5      1       
        T70     S5      
        S6      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S5      S3&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A6   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V4      S1&V3        * V4 = S1 AND V3 
        V5      V4<A7        * V5 = V4 left shift A7 
        V6      V5+V0        * V6 = V5 + V0 
        A0      A2      
        V1      ,A0,V6       * Gather into V1 using indices in V6 
        A0      A3      
        ,A0,V1  V3           * Scatter V3 using indices in V1 
        V2      S4+V1        * V2 = S4 + V1 
        A0      A2      
        ,A0,V6  V2           * Scatter V2 using indices in V6 
* *** End 1st body ***
        S3      S3-S6   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A6   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V4      S1&V3        * V4 = S1 AND V3 
        V5      V4<A7        * V5 = V4 left shift A7 
        V7      V5+V0        * V7 = V5 + V0 
        A0      A2      
        V1      ,A0,V7       * Gather into V1 using indices in V7 
        A0      A3      
        ,A0,V1  V3           * Scatter V3 using indices in V1 
        V2      S4+V1        * V2 = S4 + V1 
        A0      A2      
        ,A0,V7  V2           * Scatter V2 using indices in V7 
* *** End 2nd body ***
        S3      S3-S6   
        A6      A6+1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S5      T70     
        A0      S5      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S5      0       
        T70     S5      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S5      A5      
        S3      T71     
        S3      S5-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S3      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END SRT_LPERM

* SRT_RPERM
* Extracts a digit, converts to bucket ptr, gathers rank, permutes source (internal version)
        IDENT   SRTRPER (SRC, BUCKETS, MASK, SSHIFT, DEST, VLENGTH, STRIDE, REMAIN)
        ENTRY   SRTRPER

        BASE    D       * base 10
SRTRPER ENTER  NP=8    * # parameters
* int *SRC;
* int *BUCKETS;
* int MASK; 
* int SSHIFT; 
* int *DEST;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,3,ARGPTR=A6
*                               MASK
        ARGADD  S2,4,ARGPTR=A6
*                               SSHIFT
        ARGADD  A3,5,ARGPTR=A6
*                               DEST
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      S2      
        S4      1       
        V5      0       
        V5      S4+V5        * V5 = S4 + V5 
        V0,VM   V5,N         * Compress Index Where V5 Non-Zero 
        S4      1       
        A6      0       
        B71     A6      
        T71     S3      
        VL      A4      
        S5      1       
        T70     S5      
        S6      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S5      S3&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A6   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V4      S1&V3        * V4 = S1 AND V3 
        V5      V4>A7        * V5 = V4 right shift A7 
        V6      V5+V0        * V6 = V5 + V0 
        A0      A2      
        V1      ,A0,V6       * Gather into V1 using indices in V6 
        A0      A3      
        ,A0,V1  V3           * Scatter V3 using indices in V1 
        V2      S4+V1        * V2 = S4 + V1 
        A0      A2      
        ,A0,V6  V2           * Scatter V2 using indices in V6 
* *** End 1st body ***
        S3      S3-S6   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A6   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V4      S1&V3        * V4 = S1 AND V3 
        V5      V4>A7        * V5 = V4 right shift A7 
        V7      V5+V0        * V7 = V5 + V0 
        A0      A2      
        V1      ,A0,V7       * Gather into V1 using indices in V7 
        A0      A3      
        ,A0,V1  V3           * Scatter V3 using indices in V1 
        V2      S4+V1        * V2 = S4 + V1 
        A0      A2      
        ,A0,V7  V2           * Scatter V2 using indices in V7 
* *** End 2nd body ***
        S3      S3-S6   
        A6      A6+1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S5      T70     
        A0      S5      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S5      0       
        T70     S5      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S5      A5      
        S3      T71     
        S3      S5-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S3      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END SRT_RPERM

* ORD_LPERM
* Gathers a digit, converts to bucket ptr, gathers rank, permutes index (internal version)
        IDENT   ORDLPER (SRC, INDEX, BUCKETS, MASK, SSHIFT, DEST, VLENGTH, STRIDE, REMAIN)
        ENTRY   ORDLPER

        BASE    D       * base 10
ORDLPER ENTER  NP=9    * # parameters
* int *SRC;
* int *INDEX;
* int *BUCKETS;
* int MASK; 
* int SSHIFT; 
* int *DEST; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               INDEX
        ARGADD  A3,3,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,4,ARGPTR=A6
*                               MASK
        ARGADD  S2,5,ARGPTR=A6
*                               SSHIFT
        ARGADD  S3,6,ARGPTR=A6
*                               DEST
        ARGADD  A4,7,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,8,ARGPTR=A6
*                               STRIDE
        ARGADD  S4,9,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      S2      
        S5      1       
        V6      0       
        V6      S5+V6        * V6 = S5 + V6 
        V0,VM   V6,N         * Compress Index Where V6 Non-Zero 
        T70     S2      
        S2      1       
        A6      0       
        B71     A6      
        T72     S4      
        VL      A4      
        S5      1       
        T71     S5      
        S6      1       
        A0      S4      
        JAZ     L5      
L1      =       P.*     
        S5      S4&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5<A7        * V6 = V5 left shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      S3      
        ,A0,V2  V1           * Scatter V1 using indices in V2 
        V3      S2+V2        * V3 = S2 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 1st body ***
        S4      S4-S6   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5<A7        * V6 = V5 left shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      S3      
        ,A0,V2  V1           * Scatter V1 using indices in V2 
        V3      S2+V2        * V3 = S2 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 2nd body ***
        S4      S4-S6   
        A6      A6+1    
        A0      S4      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S5      T71     
        A0      S5      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S5      0       
        T71     S5      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S5      A5      
        S4      T72     
        S4      S5-S4   
* Jump to end if the second part is empty
        A0      S4      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S4      T72     
        A6      B71     
        S2      T70     
        A6      B70     
        EXIT
        END
* END ORD_LPERM

* ORD_RPERM
* Gathers a digit, converts to bucket ptr, gathers rank, permutes index (internal version)
        IDENT   ORDRPER (SRC, INDEX, BUCKETS, MASK, SSHIFT, DEST, VLENGTH, STRIDE, REMAIN)
        ENTRY   ORDRPER

        BASE    D       * base 10
ORDRPER ENTER  NP=9    * # parameters
* int *SRC;
* int *INDEX;
* int *BUCKETS;
* int MASK; 
* int SSHIFT; 
* int *DEST; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               INDEX
        ARGADD  A3,3,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,4,ARGPTR=A6
*                               MASK
        ARGADD  S2,5,ARGPTR=A6
*                               SSHIFT
        ARGADD  S3,6,ARGPTR=A6
*                               DEST
        ARGADD  A4,7,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,8,ARGPTR=A6
*                               STRIDE
        ARGADD  S4,9,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      S2      
        S5      1       
        V6      0       
        V6      S5+V6        * V6 = S5 + V6 
        V0,VM   V6,N         * Compress Index Where V6 Non-Zero 
        T70     S2      
        S2      1       
        A6      0       
        B71     A6      
        T72     S4      
        VL      A4      
        S5      1       
        T71     S5      
        S6      1       
        A0      S4      
        JAZ     L5      
L1      =       P.*     
        S5      S4&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5>A7        * V6 = V5 right shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      S3      
        ,A0,V2  V1           * Scatter V1 using indices in V2 
        V3      S2+V2        * V3 = S2 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 1st body ***
        S4      S4-S6   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5>A7        * V6 = V5 right shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      S3      
        ,A0,V2  V1           * Scatter V1 using indices in V2 
        V3      S2+V2        * V3 = S2 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 2nd body ***
        S4      S4-S6   
        A6      A6+1    
        A0      S4      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S5      T71     
        A0      S5      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S5      0       
        T71     S5      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S5      A5      
        S4      T72     
        S4      S5-S4   
* Jump to end if the second part is empty
        A0      S4      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S4      T72     
        A6      B71     
        S2      T70     
        A6      B70     
        EXIT
        END
* END ORD_RPERM

* RNK_LPERM
* Gathers a digit, converts to bucket ptr, gathers rank, permutes index (internal version)
        IDENT   RNKLPER (SRC, INDEX, BUCKETS, MASK, SSHIFT, DEST, VLENGTH, STRIDE, REMAIN)
        ENTRY   RNKLPER

        BASE    D       * base 10
RNKLPER ENTER  NP=9    * # parameters
* int *SRC;
* int *INDEX;
* int *BUCKETS;
* int MASK; 
* int SSHIFT; 
* int *DEST; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               INDEX
        ARGADD  A3,3,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,4,ARGPTR=A6
*                               MASK
        ARGADD  S2,5,ARGPTR=A6
*                               SSHIFT
        ARGADD  S3,6,ARGPTR=A6
*                               DEST
        ARGADD  A4,7,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,8,ARGPTR=A6
*                               STRIDE
        ARGADD  S4,9,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      S2      
        S5      1       
        V6      0       
        V6      S5+V6        * V6 = S5 + V6 
        V0,VM   V6,N         * Compress Index Where V6 Non-Zero 
        T70     S2      
        S2      1       
        A6      0       
        B71     A6      
        T72     S4      
        VL      A4      
        S5      1       
        T71     S5      
        S6      1       
        A0      S4      
        JAZ     L5      
L1      =       P.*     
        S5      S4&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5<A7        * V6 = V5 left shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      S3      
        ,A0,V1  V2           * Scatter V2 using indices in V1 
        V3      S2+V2        * V3 = S2 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 1st body ***
        S4      S4-S6   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5<A7        * V6 = V5 left shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      S3      
        ,A0,V1  V2           * Scatter V2 using indices in V1 
        V3      S2+V2        * V3 = S2 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 2nd body ***
        S4      S4-S6   
        A6      A6+1    
        A0      S4      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S5      T71     
        A0      S5      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S5      0       
        T71     S5      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S5      A5      
        S4      T72     
        S4      S5-S4   
* Jump to end if the second part is empty
        A0      S4      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S4      T72     
        A6      B71     
        S2      T70     
        A6      B70     
        EXIT
        END
* END RNK_LPERM

* RNK_RPERM
* Gathers a digit, converts to bucket ptr, gathers rank, permutes index (internal version)
        IDENT   RNKRPER (SRC, INDEX, BUCKETS, MASK, SSHIFT, DEST, VLENGTH, STRIDE, REMAIN)
        ENTRY   RNKRPER

        BASE    D       * base 10
RNKRPER ENTER  NP=9    * # parameters
* int *SRC;
* int *INDEX;
* int *BUCKETS;
* int MASK; 
* int SSHIFT; 
* int *DEST; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               SRC
        ARGADD  A2,2,ARGPTR=A6
*                               INDEX
        ARGADD  A3,3,ARGPTR=A6
*                               BUCKETS
        ARGADD  S1,4,ARGPTR=A6
*                               MASK
        ARGADD  S2,5,ARGPTR=A6
*                               SSHIFT
        ARGADD  S3,6,ARGPTR=A6
*                               DEST
        ARGADD  A4,7,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,8,ARGPTR=A6
*                               STRIDE
        ARGADD  S4,9,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      S2      
        S5      1       
        V6      0       
        V6      S5+V6        * V6 = S5 + V6 
        V0,VM   V6,N         * Compress Index Where V6 Non-Zero 
        T70     S2      
        S2      1       
        A6      0       
        B71     A6      
        T72     S4      
        VL      A4      
        S5      1       
        T71     S5      
        S6      1       
        A0      S4      
        JAZ     L5      
L1      =       P.*     
        S5      S4&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5>A7        * V6 = V5 right shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      S3      
        ,A0,V1  V2           * Scatter V2 using indices in V1 
        V3      S2+V2        * V3 = S2 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 1st body ***
        S4      S4-S6   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        V4      ,A0,V1       * Gather into V4 using indices in V1 
        V5      S1&V4        * V5 = S1 AND V4 
        V6      V5>A7        * V6 = V5 right shift A7 
        V7      V6+V0        * V7 = V6 + V0 
        A0      A3      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      S3      
        ,A0,V1  V2           * Scatter V2 using indices in V1 
        V3      S2+V2        * V3 = S2 + V2 
        A0      A3      
        ,A0,V7  V3           * Scatter V3 using indices in V7 
* *** End 2nd body ***
        S4      S4-S6   
        A6      A6+1    
        A0      S4      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S5      T71     
        A0      S5      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S5      0       
        T71     S5      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S5      A5      
        S4      T72     
        S4      S5-S4   
* Jump to end if the second part is empty
        A0      S4      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S4      T72     
        A6      B71     
        S2      T70     
        A6      B70     
        EXIT
        END
* END RNK_RPERM

* ADD_SCALAR
* Add scalar val to s. (internal version)
        IDENT   ADDSCAL (D, S, VAL, VLENGTH, STRIDE, REMAIN)
        ENTRY   ADDSCAL

        BASE    D       * base 10
ADDSCAL ENTER  NP=6    * # parameters
* int *D;
* int *S;
* int VAL; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S
        ARGADD  S1,3,ARGPTR=A6
*                               VAL
        ARGADD  A3,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,6,ARGPTR=A6
*                               REMAIN
        VL      A3      
        A5      0       
        B71     A5      
        T71     S2      
        VL      A3      
        S3      1       
        T70     S3      
        S4      1       
        A0      S2      
        JAZ     L5      
L1      =       P.*     
        S3      S2&S4        * logical and 
* Jump to start if the number of iterations is even
        A0      S3      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A5   
        V0      ,A0,A4       * Vector load V0 with stride A4 
        V2      S1+V0        * V2 = S1 + V0 
        A0      A1+A5   
        ,A0,A4  V2           * Vector store V2 with stride A4 
* *** End 1st body ***
        S2      S2-S4   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V1      ,A0,A4       * Vector load V1 with stride A4 
        V3      S1+V1        * V3 = S1 + V1 
        A0      A1+A5   
        ,A0,A4  V3           * Vector store V3 with stride A4 
* *** End 2nd body ***
        S2      S2-S4   
        A5      A5+1    
        A0      S2      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S3      T70     
        A0      S3      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S3      0       
        T70     S3      
* Decrement vector length for remainder
        A3      A3-1    
        VL      A3      
        S3      A4      
        S2      T71     
        S2      S3-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S2      T71     
        A5      B71     
        A6      B70     
        EXIT
        END
* END ADD_SCALAR

* XIND_LUZ
* Entire unsegmented scan on 1's (index) (internal version)
        IDENT   XINDLUZ (D, VLENGTH, STRIDE, REMAIN)
        ENTRY   XINDLUZ

        BASE    D       * base 10
XINDLUZ ENTER  NP=4    * # parameters
* int *D;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,3,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,4,ARGPTR=A6
*                               REMAIN
        VL      A2      
        S4      A3      
        S3      0       
        S2      1       
        A5      A2      
        A4      0       
L1      =       P.*     
* execute body of serial loop
        V1,A4   S3      
        V0,A4   S2      
        S3      S3+S4   
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L1      
        A4      0       
        B71     A4      
        T71     S1      
        VL      A2      
        S2      1       
        T70     S2      
        S3      1       
        A0      S1      
        JAZ     L6      
L2      =       P.*     
        S2      S1&S3        * logical and 
* Jump to start if the number of iterations is even
        A0      S2      
        JAZ     L3      
* Fix and Jump to middle if the number of iterations is odd
        V2      V1      
        J       L4      
L3      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        ,A0,A3  V1           * Vector store V1 with stride A3 
        V2      V1+V0        * V2 = V1 + V0 
* *** End 1st body ***
        S1      S1-S3   
        A4      A4+1    
L4      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        ,A0,A3  V2           * Vector store V2 with stride A3 
        V1      V2+V0        * V1 = V2 + V0 
* *** End 2nd body ***
        S1      S1-S3   
        A4      A4+1    
        A0      S1      
        JAN     L3      
L6      =       P.*     
* Bail out if finished both parts
        S2      T70     
        A0      S2      
        JAZ     L5      
* Clear flag to indicate done with 'full' part
        S2      0       
        T70     S2      
* Decrement vector length for remainder
        A2      A2-1    
        VL      A2      
        S2      A3      
        S1      T71     
        S1      S2-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L5      
* Jump back up to handle the remainder
        J       L2      
L5      =       P.*     
* readjust the vector length
        A2      A2+1    
        VL      A2      
        S1      T71     
        A4      B71     
        A6      B70     
        EXIT
        END
* END XIND_LUZ

* ADD_SUZ
* ADD_SUZ: An unsegmented scan function (V-+) (internal version)
        IDENT   ADDSUZ (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   ADDSUZ

        BASE    D       * base 10
ADDSUZ  ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int IDENTITY; 
* int ROWIDENT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VALUES
        ARGADD  S1,3,ARGPTR=A6
*                               IDENTITY
        ARGADD  S2,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A3,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,7,ARGPTR=A6
*                               REMAIN
        VL      A3      
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
* Phase 1: generate the column sums
        A5      0       
        B71     A5      
        T71     S3      
        VL      A3      
        S4      1       
        T70     S4      
        S5      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S4      S3&S5        * logical and 
* Jump to start if the number of iterations is even
        A0      S4      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V1      V0+V2        * V1 = V0 + V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1+V3        * V0 = V1 + V3 
* *** End 2nd body ***
        S3      S3-S5   
        A5      A5+1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S4      T70     
        A0      S4      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S4      0       
        T70     S4      
* Decrement vector length for remainder
        A3      A3-1    
        VL      A3      
        S4      A4      
        S3      T71     
        S3      S4-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S3      T71     
        A5      B71     
* Phase 2: serial sum across row
        A7      A3      
        A5      0       
L6      =       P.*     
* execute body of serial loop
        S4      V0,A5   
        V0,A5   S1      
        S1      S1+S4   
        A5      A5+1    
        A7      A7-1    
        A0      A7      
        JAN     L6      
* Phase 3: generate the final sums
        A5      0       
        B71     A5      
        T71     S3      
        VL      A3      
        S4      1       
        T70     S4      
        S5      1       
        A0      S3      
        JAZ     L11     
L7      =       P.*     
        S4      S3&S5        * logical and 
* Jump to start if the number of iterations is even
        A0      S4      
        JAZ     L8      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L9      
L8      =       P.*     
* ***   1st body   ***
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V1      V0+V2        * V1 = V0 + V2 
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L9      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1+V3        * V0 = V1 + V3 
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
* *** End 2nd body ***
        S3      S3-S5   
        A5      A5+1    
        A0      S3      
        JAN     L8      
L11     =       P.*     
* Bail out if finished both parts
        S4      T70     
        A0      S4      
        JAZ     L10     
* Clear flag to indicate done with 'full' part
        S4      0       
        T70     S4      
* Decrement vector length for remainder
        A3      A3-1    
        VL      A3      
        S4      A4      
        S3      T71     
        S3      S4-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L10     
* Jump back up to handle the remainder
        J       L7      
L10     =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S3      T71     
        A5      B71     
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END ADD_SUZ

