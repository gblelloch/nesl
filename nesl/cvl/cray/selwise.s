
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

* This file generated automatically by assembler on 12/6/1993 11:51

* ADD_WUZ
* ADD_WUZ: An unrolled elwise-op (V-+) (internal version)
        IDENT   ADDWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   ADDWUZ

        BASE    D       * base 10
ADDWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V0      V1+V2        * V0 = V1 + V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V3      V4+V5        * V3 = V4 + V5 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END ADD_WUZ

* SUB_WUZ
* SUB_WUZ: An unrolled elwise-op (V--) (internal version)
        IDENT   SUBWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   SUBWUZ

        BASE    D       * base 10
SUBWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V0      V1-V2        * V0 = V1 - V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V3      V4-V5        * V3 = V4 - V5 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END SUB_WUZ

* MUL_WUZ
* MUL_WUZ: An elwise-op (V-*) (internal version)
        IDENT   MULWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   MULWUZ

        BASE    D       * base 10
MULWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      0       
        B71     A7      
        S3      S1      
        VL      A4      
        S2      1       
        T70     S2      
        A0      S3      
        JAZ     L3      
L1      =       P.*     
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        S5      0.6          * Magic float-int conversion const. 
        V5      S5-V1   
        V3      S5-FV5       * Cast src 1 (V1) to float 
        V6      S5-V2   
        V4      S5-FV6       * Cast src 2 (V2) to float 
        V5      V3*FV4       * Float multiply 
        V6      S5-FV5       * Cast result to int 
        V0      S5-V6        * Whew... V0 = V1 * V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
        S3      S3-S2   
        A7      A7+1    
        A0      S3      
        JAN     L1      
L3      =       P.*     
* Bail out if no remainder
        S4      T70     
        A0      S4      
        JAZ     L2      
* Clear flag to indicate done with 'full' part
        S4      0       
        T70     S4      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S4      A5      
        S3      S4-S1   
        A0      S3      
        JAZ     L2      
* Jump back up to handle the remainder
        J       L1      
L2      =       P.*     
        A4      A4+1    
        VL      A4      
        A7      B71     
        A6      B70     
        EXIT
        END
* END MUL_WUZ

* DIV_WUZ
* DIV_WUZ: An elwise-op (V-/) (internal version)
        IDENT   DIVWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   DIVWUZ

        BASE    D       * base 10
DIVWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      0       
        B71     A7      
        S3      S1      
        VL      A4      
        S2      1       
        T70     S2      
        A0      S3      
        JAZ     L3      
L1      =       P.*     
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        S6      0.6          * Magic float-int conversion const. 
        V0      S6-V2   
        V7      S6-FV0  
        V6      /HV7         * Reciprocal approximation 
        V5      S6-V1   
        S5      >1           * High bit 
        V4      S5&V1   
        V3      S6-FV5  
        V2      V7*IV6  
        S5      0.4          * Another magic conversion const. 
        V0      S5!V4   
        V1      V0+FV3  
        V5      V6*FV1  
        V7      V2*RV5  
        V4      S6-FV7  
        V0      S6-V4        * Whew... I'm tired...V0 = V1 / V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
        S3      S3-S2   
        A7      A7+1    
        A0      S3      
        JAN     L1      
L3      =       P.*     
* Bail out if no remainder
        S4      T70     
        A0      S4      
        JAZ     L2      
* Clear flag to indicate done with 'full' part
        S4      0       
        T70     S4      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S4      A5      
        S3      S4-S1   
        A0      S3      
        JAZ     L2      
* Jump back up to handle the remainder
        J       L1      
L2      =       P.*     
        A4      A4+1    
        VL      A4      
        A7      B71     
        A6      B70     
        EXIT
        END
* END DIV_WUZ

* MOD_WUZ
* MOD_WUZ: An elwise-op (V-MOD) (internal version)
        IDENT   MODWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   MODWUZ

        BASE    D       * base 10
MODWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      0       
        B71     A7      
        S3      S1      
        VL      A4      
        S2      1       
        T70     S2      
        A0      S3      
        JAZ     L3      
L1      =       P.*     
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        S6      0.6          * Magic float-int conversion const. 
        V7      S6-V2   
        V6      S6-FV7  
        V5      /HV6         * Reciprocal approximation 
        V4      S6-V1   
        V0      V6*IV5  
        S5      >1           * High bit 
        V3      S5&V1   
        V7      S6-FV4  
        S5      0.4          * Another magic conversion const. 
        V6      S5!V3   
        V4      V5*FV0  
        V3      V6+FV7  
        V5      V3*FV4  
        V0      S6-FV5  
        V7      S6-V2   
        V6      S6-FV7  
        S5      S6-S6   
        V4      S5+V0   
        V3      S6-FV4  
        V5      V6*FV3  
        V2      S6-FV5  
        V7      S6-V2   
        V0      V1-V7        * Whew... I'm tired...V0 = V1 mod V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
        S3      S3-S2   
        A7      A7+1    
        A0      S3      
        JAN     L1      
L3      =       P.*     
* Bail out if no remainder
        S4      T70     
        A0      S4      
        JAZ     L2      
* Clear flag to indicate done with 'full' part
        S4      0       
        T70     S4      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S4      A5      
        S3      S4-S1   
        A0      S3      
        JAZ     L2      
* Jump back up to handle the remainder
        J       L1      
L2      =       P.*     
        A4      A4+1    
        VL      A4      
        A7      B71     
        A6      B70     
        EXIT
        END
* END MOD_WUZ

* MAX_WUZ
* MAX_WUZ: An unrolled elwise-op (V-MAX) (internal version)
        IDENT   MAXWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   MAXWUZ

        BASE    D       * base 10
MAXWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V6      V1-V2   
        VM      V6,P    
        V0      V1!V2&VM     * V0 = max(V1,V2) 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V6      V4-V5   
        VM      V6,P    
        V3      V4!V5&VM     * V3 = max(V4,V5) 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END MAX_WUZ

* MIN_WUZ
* MIN_WUZ: An unrolled elwise-op (V-MIN) (internal version)
        IDENT   MINWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   MINWUZ

        BASE    D       * base 10
MINWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V6      V2-V1   
        VM      V6,P    
        V0      V1!V2&VM     * V0 = min(V1,V2) 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V6      V5-V4   
        VM      V6,P    
        V3      V4!V5&VM     * V3 = min(V4,V5) 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END MIN_WUZ

* XCPY_WUZ
* XCPY_WUZ: An unrolled elwise-op (V-SET) (internal version)
        IDENT   XCPYWUZ (D, S, VLENGTH, STRIDE, REMAIN)
        ENTRY   XCPYWUZ

        BASE    D       * base 10
XCPYWUZ ENTER  NP=5    * # parameters
* int *D;
* int *S;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S
        ARGADD  A3,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,5,ARGPTR=A6
*                               REMAIN
        VL      A3      
        A5      0       
        B71     A5      
        T71     S1      
        VL      A3      
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
        A0      A2+A5   
        V1      ,A0,A4       * Vector load V1 with stride A4 
        V0      V1      
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
* *** End 1st body ***
        S1      S1-S3   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V2      V3      
        A0      A1+A5   
        ,A0,A4  V2           * Vector store V2 with stride A4 
* *** End 2nd body ***
        S1      S1-S3   
        A5      A5+1    
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
        A3      A3-1    
        VL      A3      
        S2      A4      
        S1      T71     
        S1      S2-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S1      T71     
        A5      B71     
        A6      B70     
        EXIT
        END
* END XCPY_WUZ

* ADD_WUD
* ADD_WUD: An unrolled elwise-op (V-+F) (internal version)
        IDENT   ADDWUD (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   ADDWUD

        BASE    D       * base 10
ADDWUD  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V0      V1+FV2       * V0 = V1 + V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V3      V4+FV5       * V3 = V4 + V5 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END ADD_WUD

* SUB_WUD
* SUB_WUD: An unrolled elwise-op (V--F) (internal version)
        IDENT   SUBWUD (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   SUBWUD

        BASE    D       * base 10
SUBWUD  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V0      V1-FV2       * V0 = V1 - V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V3      V4-FV5       * V3 = V4 - V5 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END SUB_WUD

* MUL_WUD
* MUL_WUD: An unrolled elwise-op (V-*F) (internal version)
        IDENT   MULWUD (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   MULWUD

        BASE    D       * base 10
MULWUD  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V0      V1*FV2       * V0 = V1 * V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V3      V4*FV5       * V3 = V4 * V5 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END MUL_WUD

* DIV_WUD
* DIV_WUD: An elwise-op (V-/F) (internal version)
        IDENT   DIVWUD (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   DIVWUD

        BASE    D       * base 10
DIVWUD  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        A7      0       
        B71     A7      
        S3      S1      
        VL      A4      
        S2      1       
        T70     S2      
        A0      S3      
        JAZ     L3      
L1      =       P.*     
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V5      /HV2         * Reciprocal approximation 
        V4      V2*IV5  
        V3      V5*FV4  
        V0      V1*RV3  
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
        S3      S3-S2   
        A7      A7+1    
        A0      S3      
        JAN     L1      
L3      =       P.*     
* Bail out if no remainder
        S4      T70     
        A0      S4      
        JAZ     L2      
* Clear flag to indicate done with 'full' part
        S4      0       
        T70     S4      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S4      A5      
        S3      S4-S1   
        A0      S3      
        JAZ     L2      
* Jump back up to handle the remainder
        J       L1      
L2      =       P.*     
        A4      A4+1    
        VL      A4      
        A7      B71     
        A6      B70     
        EXIT
        END
* END DIV_WUD

* MAX_WUD
* MAX_WUD: An unrolled elwise-op (V-MAXF) (internal version)
        IDENT   MAXWUD (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   MAXWUD

        BASE    D       * base 10
MAXWUD  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V6      V1-FV2  
        VM      V6,P    
        V0      V1!V2&VM     * V0 = max(V1,V2) 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V6      V4-FV5  
        VM      V6,P    
        V3      V4!V5&VM     * V3 = max(V4,V5) 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END MAX_WUD

* MIN_WUD
* MIN_WUD: An unrolled elwise-op (V-MINF) (internal version)
        IDENT   MINWUD (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   MINWUD

        BASE    D       * base 10
MINWUD  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V6      V2-FV1  
        VM      V6,P    
        V0      V1!V2&VM     * V0 = min(V1,V2) 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V6      V5-FV4  
        VM      V6,P    
        V3      V4!V5&VM     * V3 = min(V4,V5) 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END MIN_WUD

* AND_WUZ
* AND_WUZ: An unrolled elwise-op (V-AND) (internal version)
        IDENT   ANDWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   ANDWUZ

        BASE    D       * base 10
ANDWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V0      V1&V2        * V0 = V1 AND V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V3      V4&V5        * V3 = V4 AND V5 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END AND_WUZ

* IOR_WUZ
* IOR_WUZ: An unrolled elwise-op (V-OR) (internal version)
        IDENT   IORWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   IORWUZ

        BASE    D       * base 10
IORWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V0      V1!V2        * V0 = V1 OR V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V3      V4!V5        * V3 = V4 OR V5 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END IOR_WUZ

* NOT_WUZ
* NOT_WUZ: An unrolled elwise-op (V-NOT) (internal version)
        IDENT   NOTWUZ (D, SOURCE, VLENGTH, STRIDE, REMAIN)
        ENTRY   NOTWUZ

        BASE    D       * base 10
NOTWUZ  ENTER  NP=5    * # parameters
* int *D;
* int *SOURCE;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SOURCE
        ARGADD  A3,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,5,ARGPTR=A6
*                               REMAIN
        VL      A3      
        A5      0       
        B71     A5      
        T71     S1      
        VL      A3      
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
        A0      A2+A5   
        V1      ,A0,A4       * Vector load V1 with stride A4 
        S4      -1      
        V0      S4\V1        * V0 = BITWISE NOT V1 
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
* *** End 1st body ***
        S1      S1-S3   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        S4      -1      
        V2      S4\V3        * V2 = BITWISE NOT V3 
        A0      A1+A5   
        ,A0,A4  V2           * Vector store V2 with stride A4 
* *** End 2nd body ***
        S1      S1-S3   
        A5      A5+1    
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
        A3      A3-1    
        VL      A3      
        S2      A4      
        S1      T71     
        S1      S2-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S1      T71     
        A5      B71     
        A6      B70     
        EXIT
        END
* END NOT_WUZ

* NOT_WUB
* NOT_WUB: An unrolled elwise-op (V-NOT-01) (internal version)
        IDENT   NOTWUB (D, SOURCE, VLENGTH, STRIDE, REMAIN)
        ENTRY   NOTWUB

        BASE    D       * base 10
NOTWUB  ENTER  NP=5    * # parameters
* int *D;
* int *SOURCE;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               SOURCE
        ARGADD  A3,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,5,ARGPTR=A6
*                               REMAIN
        VL      A3      
        A5      0       
        B71     A5      
        T71     S1      
        VL      A3      
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
        A0      A2+A5   
        V1      ,A0,A4       * Vector load V1 with stride A4 
        S4      1       
        V0      S4\V1        * V0 = NOT V1 
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
* *** End 1st body ***
        S1      S1-S3   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        S4      1       
        V2      S4\V3        * V2 = NOT V3 
        A0      A1+A5   
        ,A0,A4  V2           * Vector store V2 with stride A4 
* *** End 2nd body ***
        S1      S1-S3   
        A5      A5+1    
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
        A3      A3-1    
        VL      A3      
        S2      A4      
        S1      T71     
        S1      S2-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S1      T71     
        A5      B71     
        A6      B70     
        EXIT
        END
* END NOT_WUB

* XOR_WUZ
* XOR_WUZ: An unrolled elwise-op (V-XOR) (internal version)
        IDENT   XORWUZ (D, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   XORWUZ

        BASE    D       * base 10
XORWUZ  ENTER  NP=6    * # parameters
* int *D;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
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
        V1      ,A0,A5       * Vector load V1 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V0      V1\V2        * V0 = V1 XOR V2 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        A0      A3+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        V3      V4\V5        * V3 = V4 XOR V5 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
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
* END XOR_WUZ

* PBOO_WUZ
* PBOO_WUZ: A packed boolean elwise-op (PACKED-NONZERO) (internal version)
        IDENT   PBOOWUZ (D1, S1, VLENGTH, STRIDE, REMAIN)
        ENTRY   PBOOWUZ

        BASE    D       * base 10
PBOOWUZ ENTER  NP=5    * # parameters
* int *D1;
* int *S1;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D1
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,5,ARGPTR=A6
*                               REMAIN
        VL      A3      
        A5      0       
        B71     A5      
        T71     S1      
        VL      A3      
        S6      1       
        T70     S6      
        S7      1       
        A0      S1      
        JAZ     L5      
L1      =       P.*     
        S6      S1&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A2+A5   
        V0      ,A0,A4       * Vector load V0 with stride A4 
        VM      V0,N         * Set mask for non-zero elements in V0 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      VM0     
        S4      VM1     
CRAY16  ELSE            
        S2      VM      
CRAY16  ENDIF           
        ,A1     S2           * Scalar store S2 
        A1      A1+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        ,A1     S4           * Scalar store S4 
        A1      A1+1    
CRAY16  ENDIF           
* *** End 1st body ***
        S1      S1-S7   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V1      ,A0,A4       * Vector load V1 with stride A4 
        VM      V1,N         * Set mask for non-zero elements in V1 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S3      VM0     
        S5      VM1     
CRAY16  ELSE            
        S3      VM      
CRAY16  ENDIF           
        ,A1     S3           * Scalar store S3 
        A1      A1+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        ,A1     S5           * Scalar store S5 
        A1      A1+1    
CRAY16  ENDIF           
* *** End 2nd body ***
        S1      S1-S7   
        A5      A5+1    
        A0      S1      
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
        A3      A3-1    
        VL      A3      
        S6      A4      
        S1      T71     
        S1      S6-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S1      T71     
        A5      B71     
        A6      B70     
        EXIT
        END
* END PBOO_WUZ

* GRT_WUZ
* GRT_WUZ: An upacked boolean elwise-op (UNPACKED-GT) (internal version)
        IDENT   GRTWUZ (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   GRTWUZ

        BASE    D       * base 10
GRTWUZ  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V4-V3   
        VM      V7,M         * Set mask where V3 > V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V6-V5   
        VM      V7,M         * Set mask where V5 > V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END GRT_WUZ

* GEQ_WUZ
* GEQ_WUZ: An upacked boolean elwise-op (UNPACKED-GEQ) (internal version)
        IDENT   GEQWUZ (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   GEQWUZ

        BASE    D       * base 10
GEQWUZ  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V3-V4   
        VM      V7,P         * Set mask where V3 >= V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V5-V6   
        VM      V7,P         * Set mask where V5 >= V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END GEQ_WUZ

* LES_WUZ
* LES_WUZ: An upacked boolean elwise-op (UNPACKED-LT) (internal version)
        IDENT   LESWUZ (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   LESWUZ

        BASE    D       * base 10
LESWUZ  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V3-V4   
        VM      V7,M         * Set mask where V3 < V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V5-V6   
        VM      V7,M         * Set mask where V5 < V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END LES_WUZ

* LEQ_WUZ
* LEQ_WUZ: An upacked boolean elwise-op (UNPACKED-LEQ) (internal version)
        IDENT   LEQWUZ (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   LEQWUZ

        BASE    D       * base 10
LEQWUZ  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V4-V3   
        VM      V7,P         * Set mask where V3 <= V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V6-V5   
        VM      V7,P         * Set mask where V5 <= V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END LEQ_WUZ

* EQL_WUZ
* EQL_WUZ: An upacked boolean elwise-op (UNPACKED-EQUAL) (internal version)
        IDENT   EQLWUZ (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   EQLWUZ

        BASE    D       * base 10
EQLWUZ  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V4-V3   
        VM      V7,Z         * Set mask where V3 == V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V6-V5   
        VM      V7,Z         * Set mask where V5 == V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END EQL_WUZ

* NEQ_WUZ
* NEQ_WUZ: An upacked boolean elwise-op (UNPACKED-NOT-EQUAL) (internal version)
        IDENT   NEQWUZ (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   NEQWUZ

        BASE    D       * base 10
NEQWUZ  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V4-V3   
        VM      V7,N         * Set mask where V3 != V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V6-V5   
        VM      V7,N         * Set mask where V5 != V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END NEQ_WUZ

* BOO_WUZ
* BOO_WUZ: An upacked boolean elwise-op (UNPACKED-NONZERO) (internal version)
        IDENT   BOOWUZ (VM, S1, VLENGTH, STRIDE, REMAIN)
        ENTRY   BOOWUZ

        BASE    D       * base 10
BOOWUZ  ENTER  NP=5    * # parameters
* int *VM;
* int *S1;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,5,ARGPTR=A6
*                               REMAIN
        VL      A3      
        V0      0       
        A5      0       
        B71     A5      
        T71     S1      
        VL      A3      
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
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        VM      V3,N         * Set mask for non-zero elements in V3 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
* *** End 1st body ***
        S1      S1-S3   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V4      ,A0,A4       * Vector load V4 with stride A4 
        VM      V4,N         * Set mask for non-zero elements in V4 
        V2      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A5   
        ,A0,A4  V2           * Vector store V2 with stride A4 
* *** End 2nd body ***
        S1      S1-S3   
        A5      A5+1    
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
        A3      A3-1    
        VL      A3      
        S2      A4      
        S1      T71     
        S1      S2-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S1      T71     
        A5      B71     
        A6      B70     
        EXIT
        END
* END BOO_WUZ

* GRT_WUD
* GRT_WUD: An upacked boolean elwise-op (UNPACKED-GTF) (internal version)
        IDENT   GRTWUD (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   GRTWUD

        BASE    D       * base 10
GRTWUD  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V4-FV3  
        VM      V7,M         * Set mask where V3 > V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V6-FV5  
        VM      V7,M         * Set mask where V5 > V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END GRT_WUD

* GEQ_WUD
* GEQ_WUD: An upacked boolean elwise-op (UNPACKED-GEQF) (internal version)
        IDENT   GEQWUD (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   GEQWUD

        BASE    D       * base 10
GEQWUD  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V3-FV4  
        VM      V7,P         * Set mask where V3 >= V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V5-FV6  
        VM      V7,P         * Set mask where V5 >= V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END GEQ_WUD

* LES_WUD
* LES_WUD: An upacked boolean elwise-op (UNPACKED-LTF) (internal version)
        IDENT   LESWUD (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   LESWUD

        BASE    D       * base 10
LESWUD  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V3-FV4  
        VM      V7,M         * Set mask where V3 < V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V5-FV6  
        VM      V7,M         * Set mask where V5 < V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END LES_WUD

* LEQ_WUD
* LEQ_WUD: An upacked boolean elwise-op (UNPACKED-LEQF) (internal version)
        IDENT   LEQWUD (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   LEQWUD

        BASE    D       * base 10
LEQWUD  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V4-FV3  
        VM      V7,P         * Set mask where V3 <= V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V6-FV5  
        VM      V7,P         * Set mask where V5 <= V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END LEQ_WUD

* EQL_WUD
* EQL_WUD: An upacked boolean elwise-op (UNPACKED-EQUALF) (internal version)
        IDENT   EQLWUD (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   EQLWUD

        BASE    D       * base 10
EQLWUD  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V4-V3   
        VM      V7,Z         * Set mask where V3 == V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V6-V5   
        VM      V7,Z         * Set mask where V5 == V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END EQL_WUD

* NEQ_WUD
* NEQ_WUD: An upacked boolean elwise-op (UNPACKED-NOT-EQUALF) (internal version)
        IDENT   NEQWUD (VM, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   NEQWUD

        BASE    D       * base 10
NEQWUD  ENTER  NP=6    * # parameters
* int *VM;
* int *S1;
* int *S2;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VM
        ARGADD  A2,2,ARGPTR=A6
*                               S1
        ARGADD  A3,3,ARGPTR=A6
*                               S2
        ARGADD  A4,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,6,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
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
        V3      ,A0,A5       * Vector load V3 with stride A5 
        A0      A3+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V7      V4-V3   
        VM      V7,N         * Set mask where V3 != V4 
        V1      S3!V0&VM     * If mask S3 else V0 
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
* *** End 1st body ***
        S1      S1-S3   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A7   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        A0      A3+A7   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        V7      V6-V5   
        VM      V7,N         * Set mask where V5 != V6 
        V2      S3!V0&VM     * If mask S3 else V0 
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
* END NEQ_WUD

* INT_WUPB
* Converts compressed booleans (vlen to a word) to 1/0 flags (one per word) (internal version)
        IDENT   INTWUPB (D, S, VLENGTH, STRIDE, REMAIN)
        ENTRY   INTWUPB

        BASE    D       * base 10
INTWUPB ENTER  NP=5    * # parameters
* int *D;
* int *S;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               S
        ARGADD  A3,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,5,ARGPTR=A6
*                               REMAIN
        VL      A3      
        V2      0       
        V3      V2      
        S6      1       
        V4      0       
        V4      S6+V4        * V4 = S6 + V4 
        V5      V4      
        A5      0       
        B71     A5      
        T71     S1      
        VL      A3      
        S6      1       
        T70     S6      
        S7      1       
        A0      S1      
        JAZ     L5      
L1      =       P.*     
        S6      S1&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S2      ,A2     
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      ,A2     
        A2      A2+1    
CRAY16  ENDIF           
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S2      
        VM1     S4      
CRAY16  ELSE            
        VM      S2      
CRAY16  ENDIF           
        V0      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
* *** End 1st body ***
        S1      S1-S7   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        S3      ,A2     
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A2     
        A2      A2+1    
CRAY16  ENDIF           
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S5      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V1      V5!V3&VM     * If mask V5 else V3 
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
* *** End 2nd body ***
        S1      S1-S7   
        A5      A5+1    
        A0      S1      
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
        A3      A3-1    
        VL      A3      
        S6      A4      
        S1      T71     
        S1      S6-S1   
* Jump to end if the second part is empty
        A0      S1      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S1      T71     
        A5      B71     
        A6      B70     
        EXIT
        END
* END INT_WUPB

* SEL_WUZ
* Selects one of sources using uncompressed booleans (internal version)
        IDENT   SELWUZ (DEST, FLAGS, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   SELWUZ

        BASE    D       * base 10
SELWUZ  ENTER  NP=7    * # parameters
* int *DEST; 
* int *FLAGS; 
* int *S1; 
* int *S2; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               DEST
        ARGADD  A2,2,ARGPTR=A6
*                               FLAGS
        ARGADD  A3,3,ARGPTR=A6
*                               S1
        ARGADD  A4,4,ARGPTR=A6
*                               S2
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
        A0      A2+A6   
        V3      ,A0,A7       * Vector load V3 with stride A7 
        VM      V3,N         * Set mask for non-zero elements in V3 
        A0      A3+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
        A0      A4+A6   
        V1      ,A0,A7       * Vector load V1 with stride A7 
        V2      V0!V1&VM     * If mask V0 else V1 
        A0      A1+A6   
        ,A0,A7  V2           * Vector store V2 with stride A7 
* *** End 1st body ***
        S1      S1-S3   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A6   
        V7      ,A0,A7       * Vector load V7 with stride A7 
        VM      V7,N         * Set mask for non-zero elements in V7 
        A0      A3+A6   
        V4      ,A0,A7       * Vector load V4 with stride A7 
        A0      A4+A6   
        V5      ,A0,A7       * Vector load V5 with stride A7 
        V6      V4!V5&VM     * If mask V4 else V5 
        A0      A1+A6   
        ,A0,A7  V6           * Vector store V6 with stride A7 
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
* END SEL_WUZ

* SEL_WUB
* Selects one of sources using uncompressed booleans (internal version)
        IDENT   SELWUB (DEST, FLAGS, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   SELWUB

        BASE    D       * base 10
SELWUB  ENTER  NP=7    * # parameters
* int *DEST; 
* int *FLAGS; 
* int *S1; 
* int *S2; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               DEST
        ARGADD  A2,2,ARGPTR=A6
*                               FLAGS
        ARGADD  A3,3,ARGPTR=A6
*                               S1
        ARGADD  A4,4,ARGPTR=A6
*                               S2
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
        A0      A2+A6   
        V3      ,A0,A7       * Vector load V3 with stride A7 
        VM      V3,N         * Set mask for non-zero elements in V3 
        A0      A3+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
        A0      A4+A6   
        V1      ,A0,A7       * Vector load V1 with stride A7 
        V2      V0!V1&VM     * If mask V0 else V1 
        A0      A1+A6   
        ,A0,A7  V2           * Vector store V2 with stride A7 
* *** End 1st body ***
        S1      S1-S3   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A6   
        V7      ,A0,A7       * Vector load V7 with stride A7 
        VM      V7,N         * Set mask for non-zero elements in V7 
        A0      A3+A6   
        V4      ,A0,A7       * Vector load V4 with stride A7 
        A0      A4+A6   
        V5      ,A0,A7       * Vector load V5 with stride A7 
        V6      V4!V5&VM     * If mask V4 else V5 
        A0      A1+A6   
        ,A0,A7  V6           * Vector store V6 with stride A7 
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
* END SEL_WUB

* SEL_WUD
* Selects one of sources using uncompressed booleans (internal version)
        IDENT   SELWUD (DEST, FLAGS, S1, S2, VLENGTH, STRIDE, REMAIN)
        ENTRY   SELWUD

        BASE    D       * base 10
SELWUD  ENTER  NP=7    * # parameters
* double *DEST; 
* double *FLAGS; 
* double *S1; 
* double *S2; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               DEST
        ARGADD  A2,2,ARGPTR=A6
*                               FLAGS
        ARGADD  A3,3,ARGPTR=A6
*                               S1
        ARGADD  A4,4,ARGPTR=A6
*                               S2
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
        A0      A2+A6   
        V3      ,A0,A7       * Vector load V3 with stride A7 
        VM      V3,N         * Set mask for non-zero elements in V3 
        A0      A3+A6   
        V0      ,A0,A7       * Vector load V0 with stride A7 
        A0      A4+A6   
        V1      ,A0,A7       * Vector load V1 with stride A7 
        V2      V0!V1&VM     * If mask V0 else V1 
        A0      A1+A6   
        ,A0,A7  V2           * Vector store V2 with stride A7 
* *** End 1st body ***
        S1      S1-S3   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A6   
        V7      ,A0,A7       * Vector load V7 with stride A7 
        VM      V7,N         * Set mask for non-zero elements in V7 
        A0      A3+A6   
        V4      ,A0,A7       * Vector load V4 with stride A7 
        A0      A4+A6   
        V5      ,A0,A7       * Vector load V5 with stride A7 
        V6      V4!V5&VM     * If mask V4 else V5 
        A0      A1+A6   
        ,A0,A7  V6           * Vector store V6 with stride A7 
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
* END SEL_WUD

* BCK_PUZ_DIST_SEL
* Selects gathered vector element or scalar using compressed booleans (internal version)
        IDENT   BCKPUZD (DEST, SRC, INDEX, FLAGS, SCAL2, VLENGTH, STRIDE, REMAIN)
        ENTRY   BCKPUZD

        BASE    D       * base 10
BCKPUZD ENTER  NP=8    * # parameters
* int DEST; 
* int *SRC;
* int *INDEX;
* int *FLAGS;
* int SCAL2; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               DEST
        ARGADD  A2,2,ARGPTR=A6
*                               SRC
        ARGADD  A3,3,ARGPTR=A6
*                               INDEX
        ARGADD  A4,4,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,5,ARGPTR=A6
*                               SCAL2
        ARGADD  A5,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,8,ARGPTR=A6
*                               REMAIN
        VL      A5      
        V6      0       
        V6      S1+V6        * V6 = S1 + V6 
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
        A0      A2      
        V4      ,A0,V2       * Gather into V4 using indices in V2 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S5      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V0      V4!V6&VM     * If mask V4 else V6 
        A0      A1+A6   
        ,A0,A7  V0           * Vector store V0 with stride A7 
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
        V3      ,A0,A7       * Vector load V3 with stride A7 
        A0      A2      
        V5      ,A0,V3       * Gather into V5 using indices in V3 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S1      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V1      V5!V6&VM     * If mask V5 else V6 
        A0      A1+A6   
        ,A0,A7  V1           * Vector store V1 with stride A7 
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
* END BCK_PUZ_DIST_SEL

* DIS_VUZ
* Distribute (internal version)
        IDENT   DISVUZ (D, V, VLENGTH, STRIDE, REMAIN)
        ENTRY   DISVUZ

        BASE    D       * base 10
DISVUZ  ENTER  NP=5    * # parameters
* int *D; 
* int V; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  S1,2,ARGPTR=A6
*                               V
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
* Assign the input v to all elements of the vector
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V1      V0      
        A4      0       
        B71     A4      
        T71     S2      
        VL      A2      
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
        A0      A1+A4   
        ,A0,A3  V0           * Vector store V0 with stride A3 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        ,A0,A3  V1           * Vector store V1 with stride A3 
* *** End 2nd body ***
        S2      S2-S4   
        A4      A4+1    
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
        A2      A2-1    
        VL      A2      
        S3      A3      
        S2      T71     
        S2      S3-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A2      A2+1    
        VL      A2      
        S2      T71     
        A4      B71     
        A6      B70     
        EXIT
        END
* END DIS_VUZ

* DIS_VUB
* Distribute (internal version)
        IDENT   DISVUB (D, V, VLENGTH, STRIDE, REMAIN)
        ENTRY   DISVUB

        BASE    D       * base 10
DISVUB  ENTER  NP=5    * # parameters
* int *D; 
* int V; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  S1,2,ARGPTR=A6
*                               V
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
* Assign the input v to all elements of the vector
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V1      V0      
        A4      0       
        B71     A4      
        T71     S2      
        VL      A2      
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
        A0      A1+A4   
        ,A0,A3  V0           * Vector store V0 with stride A3 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        ,A0,A3  V1           * Vector store V1 with stride A3 
* *** End 2nd body ***
        S2      S2-S4   
        A4      A4+1    
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
        A2      A2-1    
        VL      A2      
        S3      A3      
        S2      T71     
        S2      S3-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A2      A2+1    
        VL      A2      
        S2      T71     
        A4      B71     
        A6      B70     
        EXIT
        END
* END DIS_VUB

* DIS_VUD
* Distribute (internal version)
        IDENT   DISVUD (D, V, VLENGTH, STRIDE, REMAIN)
        ENTRY   DISVUD

        BASE    D       * base 10
DISVUD  ENTER  NP=5    * # parameters
* double *D; 
* double V; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  S1,2,ARGPTR=A6
*                               V
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
* Assign the input v to all elements of the vector
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V1      V0      
        A4      0       
        B71     A4      
        T71     S2      
        VL      A2      
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
        A0      A1+A4   
        ,A0,A3  V0           * Vector store V0 with stride A3 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        ,A0,A3  V1           * Vector store V1 with stride A3 
* *** End 2nd body ***
        S2      S2-S4   
        A4      A4+1    
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
        A2      A2-1    
        VL      A2      
        S3      A3      
        S2      T71     
        S2      S3-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
* readjust the vector length
        A2      A2+1    
        VL      A2      
        S2      T71     
        A4      B71     
        A6      B70     
        EXIT
        END
* END DIS_VUD

