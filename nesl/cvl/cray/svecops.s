
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

* This file generated automatically by assembler on 12/6/1993 11:52

* XPCK_LUZ
* Packs s to d where flags are true.  Caller must make sure d is big enough (internal version)
        IDENT   XPCKLUZ (D, S, FLAGS, N)
        ENTRY   XPCKLUZ

        BASE    D       * base 10
XPCKLUZ ENTER  NP=4    * # parameters
* int *D;
* int *S;
* int *FLAGS;
* int N; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S
        ARGADD  A3,3,ARGPTR=A6
*                               FLAGS
        ARGADD  A4,4,ARGPTR=A6
*                               N
        A6      A1      
        B71     A6      
        S4      A4      
        S1      0       
* HANDLE N MOD VL ELEMENTS
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      127     
CRAY16  ELSE            
        S5      63      
CRAY16  ENDIF           
* use VL-1 for masking (n mod VL)
        S5      S5&S4        * logical and 
        A0      S5      
        JAZ     L2      
        A7      S5      
        VL      A7      
        A0      A3      
        V1      ,A0,1        * Vector load V1 
        V2,VM   V1,N         * Compress Index Where V1 Non-Zero 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      VM0     
        S3      VM1     
CRAY16  ELSE            
        S2      VM      
CRAY16  ENDIF           
        A5      PS2          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A6      PS3          * Population count (of 1 bits) 
        A5      A5+A6   
CRAY16  ENDIF           
        A0      A5      
        JAZ     L4      
        V3      S1+V2        * V3 = S1 + V2 
        VL      A5      
        A0      A2      
        V0      ,A0,V3       * Gather into V0 using indices in V3 
        A0      A1      
        ,A0,1   V0           * Vector store V0 
L4      =       P.*     
        S1      S1+S5   
        S4      S4-S5   
        A3      A3+A7   
        A1      A1+A5   
* NOW LENGTH IS A MULTIPLE OF VL
L2      =       P.*     
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A7      128     
CRAY16  ELSE            
        A7      64      
CRAY16  ENDIF           
        S5      A7      
        A0      S4      
        JAZ     L3      
L1      =       P.*     
        VL      A7      
        A0      A3      
        V1      ,A0,1        * Vector load V1 
        V2,VM   V1,N         * Compress Index Where V1 Non-Zero 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      VM0     
        S3      VM1     
CRAY16  ELSE            
        S2      VM      
CRAY16  ENDIF           
        A5      PS2          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A6      PS3          * Population count (of 1 bits) 
        A5      A5+A6   
CRAY16  ENDIF           
        A0      A5      
        JAZ     L5      
        V3      S1+V2        * V3 = S1 + V2 
        VL      A5      
        A0      A2      
        V0      ,A0,V3       * Gather into V0 using indices in V3 
        A0      A1      
        ,A0,1   V0           * Vector store V0 
L5      =       P.*     
        S1      S1+S5   
        S4      S4-S5   
        A3      A3+A7   
        A1      A1+A5   
        A0      S4      
        JAN     L1      
L3      =       P.*     
        A6      B71     
        A6      A1-A6   
        S1      A6           *  return A6 
        A6      B70     
        EXIT
        END
* END XPCK_LUZ

* SMP_PUZ
* Elementwise integer permute (internal version)
        IDENT   SMPPUZ (D, SRC, INDEX, VLENGTH, STRIDE, REMAIN)
        ENTRY   SMPPUZ

        BASE    D       * base 10
SMPPUZ  ENTER  NP=6    * # parameters
* int *D;
* int *SRC;
* int *INDEX;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SRC
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      0       
        B71     A7      
        T71     S1      
        VL      A4      
        S2      1       
        T70     S2      
        S3      1       
        A0      S1      
        JAZ     L5      
L1      =       P.*     
        S2      S1&S3        * logical and 
* Jump to start if the number of iterations is even
        A0      S2      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A7   
        V0      ,A0,A5       * Vector load V0 with stride A5 
        A0      A3+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A1      
        ,A0,V1  V0           * Scatter V0 using indices in V1 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        A0      A3+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A1      
        ,A0,V3  V2           * Scatter V2 using indices in V3 
* *** End 2nd body ***
        S1      S1-S3   
        A7      A7+1    
        A0      S1      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S2      T70     
        A0      S2      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S2      0       
        T70     S2      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S2      A5      
        S1      T71     
        S1      S2-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S1      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END SMP_PUZ

* XBCK_PUZ
* Elementwise integer back permute (internal version)
        IDENT   XBCKPUZ (D, SRC, INDEX, VLENGTH, STRIDE, REMAIN)
        ENTRY   XBCKPUZ

        BASE    D       * base 10
XBCKPUZ ENTER  NP=6    * # parameters
* int *D;
* int *SRC;
* int *INDEX;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SRC
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      0       
        B71     A7      
        T71     S1      
        VL      A4      
        S2      1       
        T70     S2      
        S3      1       
        A0      S1      
        JAZ     L5      
L1      =       P.*     
        S2      S1&S3        * logical and 
* Jump to start if the number of iterations is even
        A0      S2      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A3+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A2      
        V0      ,A0,V1       * Gather into V0 using indices in V1 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A3+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A2      
        V2      ,A0,V3       * Gather into V2 using indices in V3 
        A0      A1+A7   
        ,A0,A5  V2           * Vector store V2 with stride A5 
* *** End 2nd body ***
        S1      S1-S3   
        A7      A7+1    
        A0      S1      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S2      T70     
        A0      S2      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S2      0       
        T70     S2      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S2      A5      
        S1      T71     
        S1      S2-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S1      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XBCK_PUZ

* XSCATTERCONST
* Internal function to make segdes.  Scatters a constant (internal version)
        IDENT   XSCATTE (D, CONST, INDEX, VLENGTH, STRIDE, REMAIN)
        ENTRY   XSCATTE

        BASE    D       * base 10
XSCATTE ENTER  NP=6    * # parameters
* int *D;
* int CONST; 
* int *INDEX;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  S1,2,ARGPTR=A6
*                               CONST
        ARGADD  A2,3,ARGPTR=A6
*                               INDEX
        ARGADD  A3,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,6,ARGPTR=A6
*                               REMAIN
        VL      A3      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V2      V0      
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
        V1      ,A0,A4       * Vector load V1 with stride A4 
        A0      A1      
        ,A0,V1  V0           * Scatter V0 using indices in V1 
* *** End 1st body ***
        S2      S2-S4   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        A0      A1      
        ,A0,V3  V2           * Scatter V2 using indices in V3 
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
* END XSCATTERCONST

* XREPVEZ
* Internal function for segmented replace.  Adds index to offset and scatters (internal version)
        IDENT   XREPVEZ (D, SRC, INDEX, OFFSETS, VLENGTH, STRIDE, REMAIN)
        ENTRY   XREPVEZ

        BASE    D       * base 10
XREPVEZ ENTER  NP=7    * # parameters
* int *D;
* int *SRC;
* int *INDEX;
* int *OFFSETS;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SRC
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               OFFSETS
        ARGADD  A5,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,7,ARGPTR=A6
*                               REMAIN
        VL      A5      
        A6      0       
        B71     A6      
        T71     S1      
        VL      A5      
        S2      1       
        T70     S2      
        S3      1       
        A0      S1      
        JAZ     L5      
L1      =       P.*     
        S2      S1&S3        * logical and 
* Jump to start if the number of iterations is even
        A0      S2      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A3+A6   
        V1      ,A0,A7       * Vector load V1 with stride A7 
        A0      A4+A6   
        V4      ,A0,A7       * Vector load V4 with stride A7 
        V6      V4+V1        * V6 = V4 + V1 
        A0      A2+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
        A0      A1      
        ,A0,V6  V0           * Scatter V0 using indices in V6 
* *** End 1st body ***
        S1      S1-S3   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A3+A6   
        V3      ,A0,A7       * Vector load V3 with stride A7 
        A0      A4+A6   
        V5      ,A0,A7       * Vector load V5 with stride A7 
        V7      V5+V3        * V7 = V5 + V3 
        A0      A2+A6   
        V2      ,A0,A7       * Vector load V2 with stride A7 
        A0      A1      
        ,A0,V7  V2           * Scatter V2 using indices in V7 
* *** End 2nd body ***
        S1      S1-S3   
        A6      A6+1    
        A0      S1      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S2      T70     
        A0      S2      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S2      0       
        T70     S2      
* Decrement vector length for remainder
        A5      A5-1    
        VL      A5      
        S2      A7      
        S1      T71     
        S1      S2-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A5      A5+1    
        VL      A5      
        S1      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END XREPVEZ

* XEXTVEZ
* Internal function for segmented extract.  Adds index to offset and gathers (internal version)
        IDENT   XEXTVEZ (D, SRC, INDEX, OFFSETS, VLENGTH, STRIDE, REMAIN)
        ENTRY   XEXTVEZ

        BASE    D       * base 10
XEXTVEZ ENTER  NP=7    * # parameters
* int *D;
* int *SRC;
* int *INDEX;
* int *OFFSETS;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SRC
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               OFFSETS
        ARGADD  A5,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,7,ARGPTR=A6
*                               REMAIN
        VL      A5      
        A6      0       
        B71     A6      
        T71     S1      
        VL      A5      
        S2      1       
        T70     S2      
        S3      1       
        A0      S1      
        JAZ     L5      
L1      =       P.*     
        S2      S1&S3        * logical and 
* Jump to start if the number of iterations is even
        A0      S2      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A3+A6   
        V1      ,A0,A7       * Vector load V1 with stride A7 
        A0      A4+A6   
        V4      ,A0,A7       * Vector load V4 with stride A7 
        V6      V4+V1        * V6 = V4 + V1 
        A0      A2      
        V0      ,A0,V6       * Gather into V0 using indices in V6 
        A0      A1+A6   
        ,A0,A7  V0           * Vector store V0 with stride A7 
* *** End 1st body ***
        S1      S1-S3   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A3+A6   
        V3      ,A0,A7       * Vector load V3 with stride A7 
        A0      A4+A6   
        V5      ,A0,A7       * Vector load V5 with stride A7 
        V7      V5+V3        * V7 = V5 + V3 
        A0      A2      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      A1+A6   
        ,A0,A7  V2           * Vector store V2 with stride A7 
* *** End 2nd body ***
        S1      S1-S3   
        A6      A6+1    
        A0      S1      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S2      T70     
        A0      S2      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S2      0       
        T70     S2      
* Decrement vector length for remainder
        A5      A5-1    
        VL      A5      
        S2      A7      
        S1      T71     
        S1      S2-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A5      A5+1    
        VL      A5      
        S1      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END XEXTVEZ

* XEXTLAST
* Get last elt of seg for seg reduce.  Adds index-1 to offset and gathers (internal version)
        IDENT   XEXTLAS (D, SRC, INDEX, OFFSETS, VLENGTH, STRIDE, REMAIN)
        ENTRY   XEXTLAS

        BASE    D       * base 10
XEXTLAS ENTER  NP=7    * # parameters
* int *D;
* int *SRC;
* int *INDEX;
* int *OFFSETS;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SRC
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               OFFSETS
        ARGADD  A5,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,7,ARGPTR=A6
*                               REMAIN
        VL      A5      
        S2      -1      
        A6      0       
        B71     A6      
        T71     S1      
        VL      A5      
        S3      1       
        T70     S3      
        S4      1       
        A0      S1      
        JAZ     L5      
L1      =       P.*     
        S3      S1&S4        * logical and 
* Jump to start if the number of iterations is even
        A0      S3      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A3+A6   
        V1      ,A0,A7       * Vector load V1 with stride A7 
        A0      A4+A6   
        V4      ,A0,A7       * Vector load V4 with stride A7 
        V6      V4+V1        * V6 = V4 + V1 
        V7      S2+V6        * V7 = S2 + V6 
        A0      A2      
        V0      ,A0,V7       * Gather into V0 using indices in V7 
        A0      A1+A6   
        ,A0,A7  V0           * Vector store V0 with stride A7 
* *** End 1st body ***
        S1      S1-S4   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A3+A6   
        V3      ,A0,A7       * Vector load V3 with stride A7 
        A0      A4+A6   
        V5      ,A0,A7       * Vector load V5 with stride A7 
        V6      V5+V3        * V6 = V5 + V3 
        V7      S2+V6        * V7 = S2 + V6 
        A0      A2      
        V2      ,A0,V7       * Gather into V2 using indices in V7 
        A0      A1+A6   
        ,A0,A7  V2           * Vector store V2 with stride A7 
* *** End 2nd body ***
        S1      S1-S4   
        A6      A6+1    
        A0      S1      
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
        A5      A5-1    
        VL      A5      
        S3      A7      
        S1      T71     
        S1      S3-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A5      A5+1    
        VL      A5      
        S1      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END XEXTLAST

* XPBSELPERM
* packed bool select permute: where (flags[j]) d[index[j]] = src[j] (internal version)
        IDENT   XPBSELP (D, SRC, INDEX, FLAGS, SSCRATCH, VLENGTH, STRIDE, REMAIN)
        ENTRY   XPBSELP

        BASE    D       * base 10
XPBSELP ENTER  NP=8    * # parameters
* int *D;
* int *SRC;
* int *INDEX;
* int *FLAGS;
* int *SSCRATCH; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SRC
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,5,ARGPTR=A6
*                               SSCRATCH
        ARGADD  A5,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,8,ARGPTR=A6
*                               REMAIN
        VL      A5      
        V1      0       
        V0,VM   V1,Z         * Compress Index Where V1 Zero 
        S3      A1      
        S1      S1-S3   
        V0      S1+V0        * V0 = S1 + V0 
        V7      V0      
        T70     S1      
        A6      0       
        B71     A6      
        T72     S2      
        VL      A5      
        S6      1       
        T71     S6      
        S7      1       
        A0      S2      
        JAZ     L5      
L1      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S3      ,A4     
        A4      A4+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A4     
        A4      A4+1    
CRAY16  ENDIF           
        A0      A3+A6   
        V2      ,A0,A7       * Vector load V2 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S5      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V3      V2!V0&VM     * If mask V2 else V0 
        A0      A2+A6   
        V1      ,A0,A7       * Vector load V1 with stride A7 
        A0      A1      
        ,A0,V3  V1           * Scatter V1 using indices in V3 
* *** End 1st body ***
        S2      S2-S7   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        S4      ,A4     
        A4      A4+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A4     
        A4      A4+1    
CRAY16  ENDIF           
        A0      A3+A6   
        V5      ,A0,A7       * Vector load V5 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S1      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V6      V5!V7&VM     * If mask V5 else V7 
        A0      A2+A6   
        V4      ,A0,A7       * Vector load V4 with stride A7 
        A0      A1      
        ,A0,V6  V4           * Scatter V4 using indices in V6 
* *** End 2nd body ***
        S2      S2-S7   
        A6      A6+1    
        A0      S2      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T71     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T71     S6      
* Decrement vector length for remainder
        A5      A5-1    
        VL      A5      
        S6      A7      
        S2      T72     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A5      A5+1    
        VL      A5      
        S2      T72     
        A6      B71     
        S1      T70     
        A6      B70     
        EXIT
        END
* END XPBSELPERM

* XSELPERM
* unpacked bool select permute: where (flags[j]) d[index[j]] = src[j] (internal version)
        IDENT   XSELPER (D, SRC, INDEX, FLAGS, SSCRATCH, VLENGTH, STRIDE, REMAIN)
        ENTRY   XSELPER

        BASE    D       * base 10
XSELPER ENTER  NP=8    * # parameters
* int *D;
* int *SRC;
* int *INDEX;
* int *FLAGS;
* int *SSCRATCH; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SRC
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,5,ARGPTR=A6
*                               SSCRATCH
        ARGADD  A5,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,8,ARGPTR=A6
*                               REMAIN
        VL      A5      
        V1      0       
        V0,VM   V1,Z         * Compress Index Where V1 Zero 
        S3      A1      
        S1      S1-S3   
        V0      S1+V0        * V0 = S1 + V0 
        A6      0       
        B71     A6      
        T71     S2      
        VL      A5      
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
        A0      A4+A6   
        V7      ,A0,A7       * Vector load V7 with stride A7 
        A0      A3+A6   
        V2      ,A0,A7       * Vector load V2 with stride A7 
        VM      V7,Z    
        V3      V0!V2&VM     * V3 = If mask V2 else V0 
        A0      A2+A6   
        V1      ,A0,A7       * Vector load V1 with stride A7 
        A0      A1      
        ,A0,V3  V1           * Scatter V1 using indices in V3 
* *** End 1st body ***
        S2      S2-S4   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A4+A6   
        V7      ,A0,A7       * Vector load V7 with stride A7 
        A0      A3+A6   
        V5      ,A0,A7       * Vector load V5 with stride A7 
        VM      V7,Z    
        V6      V0!V5&VM     * V6 = If mask V5 else V0 
        A0      A2+A6   
        V4      ,A0,A7       * Vector load V4 with stride A7 
        A0      A1      
        ,A0,V6  V4           * Scatter V4 using indices in V6 
* *** End 2nd body ***
        S2      S2-S4   
        A6      A6+1    
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
        A5      A5-1    
        VL      A5      
        S3      A7      
        S2      T71     
        S2      S3-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A5      A5+1    
        VL      A5      
        S2      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END XSELPERM

* XBSELPERM
* 1st phase of unpacked bool select bpermute: where (flags[j]) d[j] = src[index[j]] (internal version)
        IDENT   XBSELPE (D, SRC, INDEX, FLAGS, SSCRATCH, VLENGTH, STRIDE, REMAIN)
        ENTRY   XBSELPE

        BASE    D       * base 10
XBSELPE ENTER  NP=8    * # parameters
* int *D;
* int *SRC;
* int *INDEX;
* int *FLAGS;
* int *SSCRATCH; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SRC
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,5,ARGPTR=A6
*                               SSCRATCH
        ARGADD  A5,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,8,ARGPTR=A6
*                               REMAIN
        VL      A5      
        V1      0       
        V0,VM   V1,Z         * Compress Index Where V1 Zero 
        S3      A1      
        S1      S1-S3   
        V0      S1+V0        * V0 = S1 + V0 
        A6      0       
        B71     A6      
        T71     S2      
        VL      A5      
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
        A0      A4+A6   
        V7      ,A0,A7       * Vector load V7 with stride A7 
        A0      A3+A6   
        V2      ,A0,A7       * Vector load V2 with stride A7 
        VM      V7,Z    
        V3      V0!V2&VM     * V3 = If mask V2 else V0 
        A0      A2      
        V1      ,A0,V3       * Gather into V1 using indices in V3 
        A0      A1+A6   
        ,A0,A7  V1           * Vector store V1 with stride A7 
* *** End 1st body ***
        S2      S2-S4   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A4+A6   
        V7      ,A0,A7       * Vector load V7 with stride A7 
        A0      A3+A6   
        V5      ,A0,A7       * Vector load V5 with stride A7 
        VM      V7,Z    
        V6      V0!V5&VM     * V6 = If mask V5 else V0 
        A0      A2      
        V4      ,A0,V6       * Gather into V4 using indices in V6 
        A0      A1+A6   
        ,A0,A7  V4           * Vector store V4 with stride A7 
* *** End 2nd body ***
        S2      S2-S4   
        A6      A6+1    
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
        A5      A5-1    
        VL      A5      
        S3      A7      
        S2      T71     
        S2      S3-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A5      A5+1    
        VL      A5      
        S2      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END XBSELPERM

* XSEGPERM
* XSEGPERM: A segmented permute function (internal version)
        IDENT   XSEGPER (D, VALUES, INDEX, FLAGS, LASTIND, VLENGTH, STRIDE, REMAIN)
        ENTRY   XSEGPER

        BASE    D       * base 10
XSEGPER ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *INDEX;
* int *FLAGS;
* int *LASTIND; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VALUES
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,5,ARGPTR=A6
*                               LASTIND
        ARGADD  A5,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,8,ARGPTR=A6
*                               REMAIN
        VL      A5      
        B71     A4      
* Phases 1 and 2: result loaded from last index vector
        A4      S1      
        A0      A4      
        V4      ,A0,1        * Vector load V4 
* Phase 3: generate the final sums
* set v-last-a[i] = i*stride
        V7      0       
        V6,VM   V7,Z         * Compress Index Where V7 Zero 
        S3      16      
        A4      S3      
        S3      A7      
        S3      S3<31        * S3 = S3 left shift 31 
        V6      V6<A4        * V6 = V6 left shift A4 
        V6      S3*V6        * V6 = S3 * V6 
        A4      B71     
        S3      1       
        A6      0       
        B71     A6      
        T71     S2      
        VL      A5      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L5      
L1      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V5      V4      
        V7      V6      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S4      ,A4     
        A4      A4+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A4     
        A4      A4+1    
CRAY16  ENDIF           
        A0      A3+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V5      V6!V4&VM     * If mask V6 else V4 
        V1      V0+V5        * V1 = V0 + V5 
        A0      A2+A6   
        V2      ,A0,A7       * Vector load V2 with stride A7 
        A0      A1      
        ,A0,V1  V2           * Scatter V2 using indices in V1 
        V7      S3+V6        * V7 = S3 + V6 
* *** End 1st body ***
        S2      S2-S7   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        S4      ,A4     
        A4      A4+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A4     
        A4      A4+1    
CRAY16  ENDIF           
        A0      A3+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V4      V7!V5&VM     * If mask V7 else V5 
        V1      V0+V4        * V1 = V0 + V4 
        A0      A2+A6   
        V3      ,A0,A7       * Vector load V3 with stride A7 
        A0      A1      
        ,A0,V1  V3           * Scatter V3 using indices in V1 
        V6      S3+V7        * V6 = S3 + V7 
* *** End 2nd body ***
        S2      S2-S7   
        A6      A6+1    
        A0      S2      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A5      A5-1    
        VL      A5      
        S6      A7      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A5      A5+1    
        VL      A5      
        S2      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END XSEGPERM

* XSEGDPE
* XSEGDPE: A segmented permute function (internal version)
        IDENT   XSEGDPE (D, VALUES, INDEX, FLAGS, LASTIND, STARTS, VLENGTH, STRIDE, REMAIN)
        ENTRY   XSEGDPE

        BASE    D       * base 10
XSEGDPE ENTER  NP=9    * # parameters
* int *D;
* int *VALUES;
* int *INDEX;
* int *FLAGS; 
* int *LASTIND; 
* int *STARTS;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VALUES
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  S1,4,ARGPTR=A6
*                               FLAGS
        ARGADD  S2,5,ARGPTR=A6
*                               LASTIND
        ARGADD  A4,6,ARGPTR=A6
*                               STARTS
        ARGADD  A5,7,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,8,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,9,ARGPTR=A6
*                               REMAIN
        VL      A5      
        B71     A1      
* Phases 1 and 2: result loaded from last index vector
        A1      S2      
        A0      A1      
        V0      ,A0,1        * Vector load V0 
        A0      A4      
        V4      ,A0,V0       * Gather into V4 using indices in V0 
        A1      B71     
* Phase 3: generate the final sums
        T70     S2      
        A6      0       
        B71     A6      
        T72     S3      
        VL      A5      
        S6      1       
        T71     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V5      V4      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S2      A4      
        A4      S1      
        S4      ,A4     
        A4      A4+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A4     
        A4      A4+1    
CRAY16  ENDIF           
        S1      A4      
        A4      S2      
        A0      A3+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
        A0      A4+A6   
        V6      ,A0,A7       * Vector load V6 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V5      V6!V4&VM     * If mask V6 else V4 
        V1      V0+V5        * V1 = V0 + V5 
        A0      A2+A6   
        V2      ,A0,A7       * Vector load V2 with stride A7 
        A0      A1      
        ,A0,V1  V2           * Scatter V2 using indices in V1 
* *** End 1st body ***
        S3      S3-S7   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        S2      A4      
        A4      S1      
        S4      ,A4     
        A4      A4+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A4     
        A4      A4+1    
CRAY16  ENDIF           
        S1      A4      
        A4      S2      
        A0      A3+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
        A0      A4+A6   
        V7      ,A0,A7       * Vector load V7 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V4      V7!V5&VM     * If mask V7 else V5 
        V1      V0+V4        * V1 = V0 + V4 
        A0      A2+A6   
        V3      ,A0,A7       * Vector load V3 with stride A7 
        A0      A1      
        ,A0,V1  V3           * Scatter V3 using indices in V1 
* *** End 2nd body ***
        S3      S3-S7   
        A6      A6+1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T71     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T71     S6      
* Decrement vector length for remainder
        A5      A5-1    
        VL      A5      
        S6      A7      
        S3      T72     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A5      A5+1    
        VL      A5      
        S3      T72     
        A6      B71     
        S2      T70     
        A6      B70     
        EXIT
        END
* END XSEGDPE

* XSEGFPM
* XSEGFPM: A segmented permute function (internal version)
        IDENT   XSEGFPM (INDEX, FLAGS, LASTIND, STARTS, VLENGTH, STRIDE, REMAIN)
        ENTRY   XSEGFPM

        BASE    D       * base 10
XSEGFPM ENTER  NP=7    * # parameters
* int *INDEX;
* int *FLAGS;
* int *LASTIND; 
* int *STARTS;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               INDEX
        ARGADD  A2,2,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,3,ARGPTR=A6
*                               LASTIND
        ARGADD  A3,4,ARGPTR=A6
*                               STARTS
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
        B71     A2      
* Phases 1 and 2: result loaded from last index vector
        A2      S1      
        A0      A2      
        V0      ,A0,1        * Vector load V0 
        A0      A3      
        V4      ,A0,V0       * Gather into V4 using indices in V0 
        A2      B71     
* Phase 3: generate the final sums
        T70     S1      
        A7      0       
        B71     A7      
        T72     S2      
        VL      A4      
        S5      1       
        T71     S5      
        S6      1       
        A0      S2      
        JAZ     L5      
L1      =       P.*     
        S5      S2&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V5      V4      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S3      ,A2     
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      ,A2     
        A2      A2+1    
CRAY16  ENDIF           
        A0      A1+A7   
        V0      ,A0,A5       * Vector load V0 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V5      V6!V4&VM     * If mask V6 else V4 
        V2      V0+V5        * V2 = V0 + V5 
        A0      A3+A7   
        ,A0,A5  V2           * Vector store V2 with stride A5 
* *** End 1st body ***
        S2      S2-S6   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S3      ,A2     
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      ,A2     
        A2      A2+1    
CRAY16  ENDIF           
        A0      A1+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V7      ,A0,A5       * Vector load V7 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V4      V7!V5&VM     * If mask V7 else V5 
        V3      V1+V4        * V3 = V1 + V4 
        A0      A3+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
* *** End 2nd body ***
        S2      S2-S6   
        A7      A7+1    
        A0      S2      
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
        S2      T72     
        S2      S5-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T72     
        A7      B71     
        S1      T70     
        A6      B70     
        EXIT
        END
* END XSEGFPM

* XSEGBPERM
* XSEGBPERM: A segmented permute function (internal version)
        IDENT   XSEGBPE (D, VALUES, INDEX, FLAGS, LASTIND, VLENGTH, STRIDE, REMAIN)
        ENTRY   XSEGBPE

        BASE    D       * base 10
XSEGBPE ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *INDEX;
* int *FLAGS;
* int *LASTIND; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VALUES
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,5,ARGPTR=A6
*                               LASTIND
        ARGADD  A5,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,8,ARGPTR=A6
*                               REMAIN
        VL      A5      
        B71     A4      
* Phases 1 and 2: result loaded from last index vector
        A4      S1      
        A0      A4      
        V4      ,A0,1        * Vector load V4 
* Phase 3: generate the final sums
* set v-last-a[i] = i*stride
        V7      0       
        V6,VM   V7,Z         * Compress Index Where V7 Zero 
        S3      16      
        A4      S3      
        S3      A7      
        S3      S3<31        * S3 = S3 left shift 31 
        V6      V6<A4        * V6 = V6 left shift A4 
        V6      S3*V6        * V6 = S3 * V6 
        A4      B71     
        S3      1       
        A6      0       
        B71     A6      
        T71     S2      
        VL      A5      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L5      
L1      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V5      V4      
        V7      V6      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S4      ,A4     
        A4      A4+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A4     
        A4      A4+1    
CRAY16  ENDIF           
        A0      A3+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V5      V6!V4&VM     * If mask V6 else V4 
        V1      V0+V5        * V1 = V0 + V5 
        A0      A2      
        V2      ,A0,V1       * Gather into V2 using indices in V1 
        A0      A1+A6   
        ,A0,A7  V2           * Vector store V2 with stride A7 
        V7      S3+V6        * V7 = S3 + V6 
* *** End 1st body ***
        S2      S2-S7   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        S4      ,A4     
        A4      A4+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A4     
        A4      A4+1    
CRAY16  ENDIF           
        A0      A3+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V4      V7!V5&VM     * If mask V7 else V5 
        V1      V0+V4        * V1 = V0 + V4 
        A0      A2      
        V3      ,A0,V1       * Gather into V3 using indices in V1 
        A0      A1+A6   
        ,A0,A7  V3           * Vector store V3 with stride A7 
        V6      S3+V7        * V6 = S3 + V7 
* *** End 2nd body ***
        S2      S2-S7   
        A6      A6+1    
        A0      S2      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A5      A5-1    
        VL      A5      
        S6      A7      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A5      A5+1    
        VL      A5      
        S2      T71     
        A6      B71     
        A6      B70     
        EXIT
        END
* END XSEGBPERM

* IADD_SZ2
* IADD_SZ2: 2 unsegmented inplace inclusive scans (V-+) (internal version)
        IDENT   IADDSZ2 (D, VALUES, VALUES2, FLAGS, SOURCE, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   IADDSZ2

        BASE    D       * base 10
IADDSZ2 ENTER  NP=9    * # parameters
* int *D;
* int *VALUES;
* int *VALUES2;
* int *FLAGS; 
* int *SOURCE; 
* int ROWIDENT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VALUES
        ARGADD  A3,3,ARGPTR=A6
*                               VALUES2
        ARGADD  S1,4,ARGPTR=A6
*                               FLAGS
        ARGADD  S2,5,ARGPTR=A6
*                               SOURCE
        ARGADD  S3,6,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,7,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,8,ARGPTR=A6
*                               STRIDE
        ARGADD  S4,9,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
        V0      S3+V0        * V0 = S3 + V0 
        V2      V0      
* Phase 1: generate the column sums
        A7      0       
        B71     A7      
        T71     S4      
        VL      A4      
        S5      1       
        T70     S5      
        S6      1       
        A0      S4      
        JAZ     L5      
L1      =       P.*     
        S5      S4&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V4      V0      
        V6      V2      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        V4      V0+V1        * V4 = V0 + V1 
        A0      A3+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V6      V2+V3        * V6 = V2 + V3 
* *** End 1st body ***
        S4      S4-S6   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V0      V4+V5        * V0 = V4 + V5 
        A0      A3+A7   
        V7      ,A0,A5       * Vector load V7 with stride A5 
        V2      V6+V7        * V2 = V6 + V7 
* *** End 2nd body ***
        S4      S4-S6   
        A7      A7+1    
        A0      S4      
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
        S4      T71     
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
        S4      T71     
        A7      B71     
* Phase 2: serial sum across row
        B71     A1      
        T70     S3      
        S7      0       
        S3      S7      
        A7      A4      
        A1      0       
L6      =       P.*     
* execute body of serial loop
        S5      V0,A1   
        S6      V2,A1   
        V0,A1   S7      
        S7      S7+S5   
        V2,A1   S3      
        S3      S3+S6   
        A1      A1+1    
        A7      A7-1    
        A0      A7      
        JAN     L6      
        A1      B71     
        S3      T70     
* Phase 3: generate the final sums
        T70     S3      
        A7      S2      
        B71     A7      
        A6      0       
        B72     A6      
        S6      S4      
        VL      A4      
        S5      1       
        T71     S5      
        A0      S6      
        JAZ     L9      
L7      =       P.*     
        A0      A2+A6   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        V0      V0+V1        * V0 = V0 + V1 
        A0      A3+A6   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V2      V2+V3        * V2 = V2 + V3 
        A7      S1      
        S3      ,A7     
        A7      A7+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      ,A7     
        A7      A7+1    
CRAY16  ENDIF           
        S1      A7      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S2      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V5      V0!V2&VM     * If mask V0 else V2 
        A7      B71     
        A0      A7+A6   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A1      
        ,A0,V5  V4           * Scatter V4 using indices in V5 
        S6      S6-S5   
        A6      A6+1    
        A0      S6      
        JAN     L7      
L9      =       P.*     
* Bail out if no remainder
        S7      T71     
        A0      S7      
        JAZ     L8      
* Clear flag to indicate done with 'full' part
        S7      0       
        T71     S7      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S7      A5      
        S6      S7-S4   
        A0      S6      
        JAZ     L8      
* Jump back up to handle the remainder
        J       L7      
L8      =       P.*     
        A4      A4+1    
        VL      A4      
        A6      B72     
        S3      T70     
        A6      B70     
        EXIT
        END
* END IADD_SZ2

