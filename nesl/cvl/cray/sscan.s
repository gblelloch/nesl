
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

* This file generated automatically by assembler on 12/6/1993 11:49

* XMUL_RUZ
* XMUL_RUZ: a reduce function (V-*) (internal version)
        IDENT   XMULRUZ (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMULRUZ

        BASE    D       * base 10
XMULRUZ ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        S5      0.6          * Magic float-int conversion const. 
        V5      S5-V0   
        V3      S5-FV5       * Cast src 1 (V0) to float 
        V6      S5-V2   
        V4      S5-FV6       * Cast src 2 (V2) to float 
        V5      V3*FV4       * Float multiply 
        V6      S5-FV5       * Cast result to int 
        V1      S5-V6        * Whew... V1 = V0 * V2 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        S5      0.6          * Magic float-int conversion const. 
        V5      S5-V1   
        V3      S5-FV5       * Cast src 1 (V1) to float 
        V6      S5-V2   
        V4      S5-FV6       * Cast src 2 (V2) to float 
        V5      V3*FV4       * Float multiply 
        V6      S5-FV5       * Cast result to int 
        V0      S5-V6        * Whew... V0 = V1 * V2 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S5      0.6     
        S4      S5-S1   
        S1      S5-S3   
        S6      S5-FS4  
        S4      S5-FS1  
        S1      S6*FS4  
        S6      S5-FS1  
        S1      S5-S6   
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMUL_RUZ

* XADD_RUZ
* XADD_RUZ: a reduce function (V-+) (internal version)
        IDENT   XADDRUZ (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XADDRUZ

        BASE    D       * base 10
XADDRUZ ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V1      V0+V2        * V1 = V0 + V2 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V0      V1+V3        * V0 = V1 + V3 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S1      S1+S3   
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XADD_RUZ

* XMAX_RUZ
* XMAX_RUZ: a reduce function (V-MAX) (internal version)
        IDENT   XMAXRUZ (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMAXRUZ

        BASE    D       * base 10
XMAXRUZ ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V4      V0-V2   
        VM      V4,P    
        V1      V0!V2&VM     * V1 = max(V0,V2) 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V4      V1-V3   
        VM      V4,P    
        V0      V1!V3&VM     * V0 = max(V1,V3) 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S0      S3-S1   
        JSM     L7      
        S1      S3           * S1= max(S1,S3) 
L7      =       P.*     
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMAX_RUZ

* XMIN_RUZ
* XMIN_RUZ: a reduce function (V-MIN) (internal version)
        IDENT   XMINRUZ (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMINRUZ

        BASE    D       * base 10
XMINRUZ ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V4      V2-V0   
        VM      V4,P    
        V1      V0!V2&VM     * V1 = min(V0,V2) 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V4      V3-V1   
        VM      V4,P    
        V0      V1!V3&VM     * V0 = min(V1,V3) 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S0      S1-S3   
        JSM     L7      
        S1      S3           * S1= min(S1,S3) 
L7      =       P.*     
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMIN_RUZ

* XAND_RUZ
* XAND_RUZ: a reduce function (V-AND) (internal version)
        IDENT   XANDRUZ (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XANDRUZ

        BASE    D       * base 10
XANDRUZ ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V1      V0&V2        * V1 = V0 AND V2 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V0      V1&V3        * V0 = V1 AND V3 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S1      S1&S3        * logical and 
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XAND_RUZ

* XIOR_RUZ
* XIOR_RUZ: a reduce function (V-OR) (internal version)
        IDENT   XIORRUZ (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XIORRUZ

        BASE    D       * base 10
XIORRUZ ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V1      V0!V2        * V1 = V0 OR V2 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V0      V1!V3        * V0 = V1 OR V3 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S1      S1!S3        * logical or 
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XIOR_RUZ

* XXOR_RUZ
* XXOR_RUZ: a reduce function (V-XOR) (internal version)
        IDENT   XXORRUZ (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XXORRUZ

        BASE    D       * base 10
XXORRUZ ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V1      V0\V2        * V1 = V0 XOR V2 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V0      V1\V3        * V0 = V1 XOR V3 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S1      S1\S3        * logical xor 
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XXOR_RUZ

* XMUL_RUD
* XMUL_RUD: a reduce function (V-*F) (internal version)
        IDENT   XMULRUD (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMULRUD

        BASE    D       * base 10
XMULRUD ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V1      V0*FV2       * V1 = V0 * V2 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V0      V1*FV3       * V0 = V1 * V3 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S1      S1*FS3  
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMUL_RUD

* XADD_RUD
* XADD_RUD: a reduce function (V-+F) (internal version)
        IDENT   XADDRUD (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XADDRUD

        BASE    D       * base 10
XADDRUD ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V1      V0+FV2       * V1 = V0 + V2 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V0      V1+FV3       * V0 = V1 + V3 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S1      S1+FS3  
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XADD_RUD

* XMAX_RUD
* XMAX_RUD: a reduce function (V-MAXF) (internal version)
        IDENT   XMAXRUD (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMAXRUD

        BASE    D       * base 10
XMAXRUD ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V4      V0-FV2  
        VM      V4,P    
        V1      V0!V2&VM     * V1 = max(V0,V2) 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V4      V1-FV3  
        VM      V4,P    
        V0      V1!V3&VM     * V0 = max(V1,V3) 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S0      S3-FS1  
        JSM     L7      
        S1      S3           * S1= max(S1,S3) 
L7      =       P.*     
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMAX_RUD

* XMIN_RUD
* XMIN_RUD: a reduce function (V-MINF) (internal version)
        IDENT   XMINRUD (VALUES, IDENTITY, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMINRUD

        BASE    D       * base 10
XMINRUD ENTER  NP=5    * # parameters
* int *VALUES;
* int *IDENTITY; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               VALUES
        ARGADD  S1,2,ARGPTR=A6
*                               IDENTITY
        ARGADD  A2,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,5,ARGPTR=A6
*                               REMAIN
        VL      A2      
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
* phase 1: generate the column sums
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A1+A4   
        V2      ,A0,A3       * Vector load V2 with stride A3 
        V4      V2-FV0  
        VM      V4,P    
        V1      V0!V2&VM     * V1 = min(V0,V2) 
* *** End 1st body ***
        S2      S2-S4   
        A4      A4+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        V3      ,A0,A3       * Vector load V3 with stride A3 
        V4      V3-FV1  
        VM      V4,P    
        V0      V1!V3&VM     * V0 = min(V1,V3) 
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
* phase 2: serial sum across row
        A5      A2      
        A4      0       
L6      =       P.*     
* execute body of serial loop
        S3      V0,A4   
        S0      S1-FS3  
        JSM     L7      
        S1      S3           * S1= min(S1,S3) 
L7      =       P.*     
        A4      A4+1    
        A5      A5-1    
        A0      A5      
        JAN     L6      
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMIN_RUD

* XMUL_SUZ
* XMUL_SUZ: An unsegmented scan function (V-*) (internal version)
        IDENT   XMULSUZ (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMULSUZ

        BASE    D       * base 10
XMULSUZ ENTER  NP=7    * # parameters
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
        S6      0.6          * Magic float-int conversion const. 
        V5      S6-V0   
        V3      S6-FV5       * Cast src 1 (V0) to float 
        V6      S6-V2   
        V4      S6-FV6       * Cast src 2 (V2) to float 
        V5      V3*FV4       * Float multiply 
        V6      S6-FV5       * Cast result to int 
        V1      S6-V6        * Whew... V1 = V0 * V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        S6      0.6          * Magic float-int conversion const. 
        V5      S6-V1   
        V3      S6-FV5       * Cast src 1 (V1) to float 
        V6      S6-V2   
        V4      S6-FV6       * Cast src 2 (V2) to float 
        V5      V3*FV4       * Float multiply 
        V6      S6-FV5       * Cast result to int 
        V0      S6-V6        * Whew... V0 = V1 * V2 
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
        S6      0.6     
        S5      S6-S1   
        S1      S6-S4   
        S7      S6-FS5  
        S5      S6-FS1  
        S1      S7*FS5  
        S7      S6-FS1  
        S1      S6-S7   
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
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        S6      0.6          * Magic float-int conversion const. 
        V5      S6-V0   
        V3      S6-FV5       * Cast src 1 (V0) to float 
        V6      S6-V2   
        V4      S6-FV6       * Cast src 2 (V2) to float 
        V5      V3*FV4       * Float multiply 
        V6      S6-FV5       * Cast result to int 
        V1      S6-V6        * Whew... V1 = V0 * V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L9      =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        S6      0.6          * Magic float-int conversion const. 
        V5      S6-V1   
        V3      S6-FV5       * Cast src 1 (V1) to float 
        V6      S6-V2   
        V4      S6-FV6       * Cast src 2 (V2) to float 
        V5      V3*FV4       * Float multiply 
        V6      S6-FV5       * Cast result to int 
        V0      S6-V6        * Whew... V0 = V1 * V2 
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
* END XMUL_SUZ

* XADD_SUZ
* XADD_SUZ: An unsegmented scan function (V-+) (internal version)
        IDENT   XADDSUZ (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XADDSUZ

        BASE    D       * base 10
XADDSUZ ENTER  NP=7    * # parameters
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
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V1      V0+V2        * V1 = V0 + V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L9      =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1+V3        * V0 = V1 + V3 
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
* END XADD_SUZ

* XMAX_SUZ
* XMAX_SUZ: An unsegmented scan function (V-MAX) (internal version)
        IDENT   XMAXSUZ (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMAXSUZ

        BASE    D       * base 10
XMAXSUZ ENTER  NP=7    * # parameters
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
        V4      V0-V2   
        VM      V4,P    
        V1      V0!V2&VM     * V1 = max(V0,V2) 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V4      V1-V3   
        VM      V4,P    
        V0      V1!V3&VM     * V0 = max(V1,V3) 
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
        S0      S4-S1   
        JSM     L7      
        S1      S4           * S1= max(S1,S4) 
L7      =       P.*     
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
        JAZ     L12     
L8      =       P.*     
        S4      S3&S5        * logical and 
* Jump to start if the number of iterations is even
        A0      S4      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V4      V0-V2   
        VM      V4,P    
        V1      V0!V2&VM     * V1 = max(V0,V2) 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L10     =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V4      V1-V3   
        VM      V4,P    
        V0      V1!V3&VM     * V0 = max(V1,V3) 
* *** End 2nd body ***
        S3      S3-S5   
        A5      A5+1    
        A0      S3      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S4      T70     
        A0      S4      
        JAZ     L11     
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
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S3      T71     
        A5      B71     
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMAX_SUZ

* XMIN_SUZ
* XMIN_SUZ: An unsegmented scan function (V-MIN) (internal version)
        IDENT   XMINSUZ (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMINSUZ

        BASE    D       * base 10
XMINSUZ ENTER  NP=7    * # parameters
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
        V4      V2-V0   
        VM      V4,P    
        V1      V0!V2&VM     * V1 = min(V0,V2) 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V4      V3-V1   
        VM      V4,P    
        V0      V1!V3&VM     * V0 = min(V1,V3) 
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
        S0      S1-S4   
        JSM     L7      
        S1      S4           * S1= min(S1,S4) 
L7      =       P.*     
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
        JAZ     L12     
L8      =       P.*     
        S4      S3&S5        * logical and 
* Jump to start if the number of iterations is even
        A0      S4      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V4      V2-V0   
        VM      V4,P    
        V1      V0!V2&VM     * V1 = min(V0,V2) 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L10     =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V4      V3-V1   
        VM      V4,P    
        V0      V1!V3&VM     * V0 = min(V1,V3) 
* *** End 2nd body ***
        S3      S3-S5   
        A5      A5+1    
        A0      S3      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S4      T70     
        A0      S4      
        JAZ     L11     
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
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S3      T71     
        A5      B71     
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMIN_SUZ

* XAND_SUZ
* XAND_SUZ: An unsegmented scan function (V-AND) (internal version)
        IDENT   XANDSUZ (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XANDSUZ

        BASE    D       * base 10
XANDSUZ ENTER  NP=7    * # parameters
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
        V1      V0&V2        * V1 = V0 AND V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1&V3        * V0 = V1 AND V3 
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
        S1      S1&S4        * logical and 
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
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V1      V0&V2        * V1 = V0 AND V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L9      =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1&V3        * V0 = V1 AND V3 
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
* END XAND_SUZ

* XIOR_SUZ
* XIOR_SUZ: An unsegmented scan function (V-OR) (internal version)
        IDENT   XIORSUZ (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XIORSUZ

        BASE    D       * base 10
XIORSUZ ENTER  NP=7    * # parameters
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
        V1      V0!V2        * V1 = V0 OR V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1!V3        * V0 = V1 OR V3 
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
        S1      S1!S4        * logical or 
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
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V1      V0!V2        * V1 = V0 OR V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L9      =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1!V3        * V0 = V1 OR V3 
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
* END XIOR_SUZ

* XXOR_SUZ
* XXOR_SUZ: An unsegmented scan function (V-XOR) (internal version)
        IDENT   XXORSUZ (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XXORSUZ

        BASE    D       * base 10
XXORSUZ ENTER  NP=7    * # parameters
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
        V1      V0\V2        * V1 = V0 XOR V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1\V3        * V0 = V1 XOR V3 
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
        S1      S1\S4        * logical xor 
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
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V1      V0\V2        * V1 = V0 XOR V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L9      =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1\V3        * V0 = V1 XOR V3 
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
* END XXOR_SUZ

* XMUL_SUD
* XMUL_SUD: An unsegmented scan function (V-*F) (internal version)
        IDENT   XMULSUD (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMULSUD

        BASE    D       * base 10
XMULSUD ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* double IDENTITY; 
* double ROWIDENT; 
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
        V1      V0*FV2       * V1 = V0 * V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1*FV3       * V0 = V1 * V3 
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
        S1      S1*FS4  
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
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V1      V0*FV2       * V1 = V0 * V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L9      =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1*FV3       * V0 = V1 * V3 
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
* END XMUL_SUD

* XADD_SUD
* XADD_SUD: An unsegmented scan function (V-+F) (internal version)
        IDENT   XADDSUD (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XADDSUD

        BASE    D       * base 10
XADDSUD ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* double IDENTITY; 
* double ROWIDENT; 
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
        V1      V0+FV2       * V1 = V0 + V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1+FV3       * V0 = V1 + V3 
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
        S1      S1+FS4  
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
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V1      V0+FV2       * V1 = V0 + V2 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L9      =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V0      V1+FV3       * V0 = V1 + V3 
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
* END XADD_SUD

* XMAX_SUD
* XMAX_SUD: An unsegmented scan function (V-MAXF) (internal version)
        IDENT   XMAXSUD (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMAXSUD

        BASE    D       * base 10
XMAXSUD ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* double IDENTITY; 
* double ROWIDENT; 
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
        V4      V0-FV2  
        VM      V4,P    
        V1      V0!V2&VM     * V1 = max(V0,V2) 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V4      V1-FV3  
        VM      V4,P    
        V0      V1!V3&VM     * V0 = max(V1,V3) 
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
        S0      S4-FS1  
        JSM     L7      
        S1      S4           * S1= max(S1,S4) 
L7      =       P.*     
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
        JAZ     L12     
L8      =       P.*     
        S4      S3&S5        * logical and 
* Jump to start if the number of iterations is even
        A0      S4      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V4      V0-FV2  
        VM      V4,P    
        V1      V0!V2&VM     * V1 = max(V0,V2) 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L10     =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V4      V1-FV3  
        VM      V4,P    
        V0      V1!V3&VM     * V0 = max(V1,V3) 
* *** End 2nd body ***
        S3      S3-S5   
        A5      A5+1    
        A0      S3      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S4      T70     
        A0      S4      
        JAZ     L11     
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
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S3      T71     
        A5      B71     
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMAX_SUD

* XMIN_SUD
* XMIN_SUD: An unsegmented scan function (V-MINF) (internal version)
        IDENT   XMINSUD (D, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMINSUD

        BASE    D       * base 10
XMINSUD ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* double IDENTITY; 
* double ROWIDENT; 
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
        V4      V2-FV0  
        VM      V4,P    
        V1      V0!V2&VM     * V1 = min(V0,V2) 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V4      V3-FV1  
        VM      V4,P    
        V0      V1!V3&VM     * V0 = min(V1,V3) 
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
        S0      S1-FS4  
        JSM     L7      
        S1      S4           * S1= min(S1,S4) 
L7      =       P.*     
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
        JAZ     L12     
L8      =       P.*     
        S4      S3&S5        * logical and 
* Jump to start if the number of iterations is even
        A0      S4      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        A0      A1+A5   
        ,A0,A4  V0           * Vector store V0 with stride A4 
        A0      A2+A5   
        V2      ,A0,A4       * Vector load V2 with stride A4 
        V4      V2-FV0  
        VM      V4,P    
        V1      V0!V2&VM     * V1 = min(V0,V2) 
* *** End 1st body ***
        S3      S3-S5   
        A5      A5+1    
L10     =       P.*     
* ***   2nd body   ***
        A0      A1+A5   
        ,A0,A4  V1           * Vector store V1 with stride A4 
        A0      A2+A5   
        V3      ,A0,A4       * Vector load V3 with stride A4 
        V4      V3-FV1  
        VM      V4,P    
        V0      V1!V3&VM     * V0 = min(V1,V3) 
* *** End 2nd body ***
        S3      S3-S5   
        A5      A5+1    
        A0      S3      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S4      T70     
        A0      S4      
        JAZ     L11     
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
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S3      T71     
        A5      B71     
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END XMIN_SUD

* XMUL_SEZ
* XMUL_SEZ: A segmented scan function (V-*) (internal version)
        IDENT   XMULSEZ (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMULSEZ

        BASE    D       * base 10
XMULSEZ ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V2      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
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
        V0      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S7      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S7      
        VM1     S1      
CRAY16  ELSE            
        VM      S7      
CRAY16  ENDIF           
        V3      V2!V0&VM     * If mask V2 else V0 
        S3      S3!S7        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
        S7      0.6          * Magic float-int conversion const. 
        V6      S7-V3   
        V4      S7-FV6       * Cast src 1 (V3) to float 
        V7      S7-V1   
        V5      S7-FV7       * Cast src 2 (V1) to float 
        V6      V4*FV5       * Float multiply 
        V7      S7-FV6       * Cast result to int 
        V0      S7-V7        * Whew... V0 = V3 * V1 
* *** End 1st body ***
        S2      S2-S6   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S7      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S7      
        VM1     S1      
CRAY16  ELSE            
        VM      S7      
CRAY16  ENDIF           
        V3      V2!V0&VM     * If mask V2 else V0 
        S3      S3!S7        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
        S7      0.6          * Magic float-int conversion const. 
        V6      S7-V3   
        V4      S7-FV6       * Cast src 1 (V3) to float 
        V7      S7-V1   
        V5      S7-FV7       * Cast src 2 (V1) to float 
        V6      V4*FV5       * Float multiply 
        V7      S7-FV6       * Cast result to int 
        V0      S7-V7        * Whew... V0 = V3 * V1 
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V3      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V4      S7!V3&VM     * If mask S7 else V3 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V4,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        T70     S7      
        T71     S3      
        T72     S4      
        S3      0.6     
        S7      S3-S6   
        S6      S3-S5   
        S4      S3-FS7  
        S7      S3-FS6  
        S6      S4*FS7  
        S4      S3-FS6  
        S6      S3-S4   
        S7      T70     
        S3      T71     
        S4      T72     
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S5      1       
        T70     S5      
        S6      1       
        A0      S2      
        JAZ     L12     
L8      =       P.*     
        S5      S2&S6        * logical and 
* Jump to start if the number of iterations is even
        A0      S5      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V0      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        S7      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S7      
        VM1     S1      
CRAY16  ELSE            
        VM      S7      
CRAY16  ENDIF           
        V3      V2!V0&VM     * If mask V2 else V0 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
        S7      0.6          * Magic float-int conversion const. 
        V6      S7-V3   
        V4      S7-FV6       * Cast src 1 (V3) to float 
        V7      S7-V1   
        V5      S7-FV7       * Cast src 2 (V1) to float 
        V6      V4*FV5       * Float multiply 
        V7      S7-FV6       * Cast result to int 
        V0      S7-V7        * Whew... V0 = V3 * V1 
* *** End 1st body ***
        S2      S2-S6   
        A7      A7+1    
L10     =       P.*     
* ***   2nd body   ***
        S7      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S7      
        VM1     S1      
CRAY16  ELSE            
        VM      S7      
CRAY16  ENDIF           
        V3      V2!V0&VM     * If mask V2 else V0 
        A0      A1+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
        S7      0.6          * Magic float-int conversion const. 
        V6      S7-V3   
        V4      S7-FV6       * Cast src 1 (V3) to float 
        V7      S7-V1   
        V5      S7-FV7       * Cast src 2 (V1) to float 
        V6      V4*FV5       * Float multiply 
        V7      S7-FV6       * Cast result to int 
        V0      S7-V7        * Whew... V0 = V3 * V1 
* *** End 2nd body ***
        S2      S2-S6   
        A7      A7+1    
        A0      S2      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S5      T70     
        A0      S5      
        JAZ     L11     
* Clear flag to indicate done with 'full' part
        S5      0       
        T70     S5      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S5      A5      
        S2      T71     
        S2      S5-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XMUL_SEZ

* XADD_SEZ
* XADD_SEZ: A segmented scan function (V-+) (internal version)
        IDENT   XADDSEZ (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XADDSEZ

        BASE    D       * base 10
XADDSEZ ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V2      V5+V1        * V2 = V5 + V1 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V0      V6+V3        * V0 = V6 + V3 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S6      S6+S5   
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L12     
L8      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V2      V5+V1        * V2 = V5 + V1 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L10     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V0      V6+V3        * V0 = V6 + V3 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L11     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XADD_SEZ

* XMAX_SEZ
* XMAX_SEZ: A segmented scan function (V-MAX) (internal version)
        IDENT   XMAXSEZ (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMAXSEZ

        BASE    D       * base 10
XMAXSEZ ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V7      V5-V1   
        VM      V7,P    
        V2      V5!V1&VM     * V2 = max(V5,V1) 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V7      V6-V3   
        VM      V7,P    
        V0      V6!V3&VM     * V0 = max(V6,V3) 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S0      S5-S6   
        JSM     L8      
        S6      S5           * S6= max(S6,S5) 
L8      =       P.*     
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L13     
L9      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L10     
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L11     
L10     =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V7      V5-V1   
        VM      V7,P    
        V2      V5!V1&VM     * V2 = max(V5,V1) 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L11     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V7      V6-V3   
        VM      V7,P    
        V0      V6!V3&VM     * V0 = max(V6,V3) 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L10     
L13     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L12     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L12     
* Jump back up to handle the remainder
        J       L9      
L12     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XMAX_SEZ

* XMIN_SEZ
* XMIN_SEZ: A segmented scan function (V-MIN) (internal version)
        IDENT   XMINSEZ (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMINSEZ

        BASE    D       * base 10
XMINSEZ ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V7      V1-V5   
        VM      V7,P    
        V2      V5!V1&VM     * V2 = min(V5,V1) 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V7      V3-V6   
        VM      V7,P    
        V0      V6!V3&VM     * V0 = min(V6,V3) 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S0      S6-S5   
        JSM     L8      
        S6      S5           * S6= min(S6,S5) 
L8      =       P.*     
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L13     
L9      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L10     
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L11     
L10     =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V7      V1-V5   
        VM      V7,P    
        V2      V5!V1&VM     * V2 = min(V5,V1) 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L11     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V7      V3-V6   
        VM      V7,P    
        V0      V6!V3&VM     * V0 = min(V6,V3) 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L10     
L13     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L12     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L12     
* Jump back up to handle the remainder
        J       L9      
L12     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XMIN_SEZ

* XAND_SEZ
* XAND_SEZ: A segmented scan function (V-AND) (internal version)
        IDENT   XANDSEZ (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XANDSEZ

        BASE    D       * base 10
XANDSEZ ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V2      V5&V1        * V2 = V5 AND V1 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V0      V6&V3        * V0 = V6 AND V3 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S6      S6&S5        * logical and 
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L12     
L8      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V2      V5&V1        * V2 = V5 AND V1 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L10     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V0      V6&V3        * V0 = V6 AND V3 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L11     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XAND_SEZ

* XIOR_SEZ
* XIOR_SEZ: A segmented scan function (V-OR) (internal version)
        IDENT   XIORSEZ (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XIORSEZ

        BASE    D       * base 10
XIORSEZ ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V2      V5!V1        * V2 = V5 OR V1 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V0      V6!V3        * V0 = V6 OR V3 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S6      S6!S5        * logical or 
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L12     
L8      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V2      V5!V1        * V2 = V5 OR V1 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L10     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V0      V6!V3        * V0 = V6 OR V3 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L11     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XIOR_SEZ

* XXOR_SEZ
* XXOR_SEZ: A segmented scan function (V-XOR) (internal version)
        IDENT   XXORSEZ (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XXORSEZ

        BASE    D       * base 10
XXORSEZ ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V2      V5\V1        * V2 = V5 XOR V1 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V0      V6\V3        * V0 = V6 XOR V3 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S6      S6\S5        * logical xor 
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L12     
L8      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V2      V5\V1        * V2 = V5 XOR V1 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L10     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V0      V6\V3        * V0 = V6 XOR V3 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L11     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XXOR_SEZ

* XMUL_SED
* XMUL_SED: A segmented scan function (V-*F) (internal version)
        IDENT   XMULSED (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMULSED

        BASE    D       * base 10
XMULSED ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* double ROWIDENT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VALUES
        ARGADD  A3,3,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V2      V5*FV1       * V2 = V5 * V1 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V0      V6*FV3       * V0 = V6 * V3 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S6      S6*FS5  
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L12     
L8      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V2      V5*FV1       * V2 = V5 * V1 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L10     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V0      V6*FV3       * V0 = V6 * V3 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L11     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XMUL_SED

* XADD_SED
* XADD_SED: A segmented scan function (V-+F) (internal version)
        IDENT   XADDSED (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XADDSED

        BASE    D       * base 10
XADDSED ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* double ROWIDENT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VALUES
        ARGADD  A3,3,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V2      V5+FV1       * V2 = V5 + V1 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V0      V6+FV3       * V0 = V6 + V3 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S6      S6+FS5  
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L12     
L8      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V2      V5+FV1       * V2 = V5 + V1 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L10     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V0      V6+FV3       * V0 = V6 + V3 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L11     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XADD_SED

* XMAX_SED
* XMAX_SED: A segmented scan function (V-MAXF) (internal version)
        IDENT   XMAXSED (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMAXSED

        BASE    D       * base 10
XMAXSED ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* double ROWIDENT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VALUES
        ARGADD  A3,3,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V7      V5-FV1  
        VM      V7,P    
        V2      V5!V1&VM     * V2 = max(V5,V1) 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V7      V6-FV3  
        VM      V7,P    
        V0      V6!V3&VM     * V0 = max(V6,V3) 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S0      S5-FS6  
        JSM     L8      
        S6      S5           * S6= max(S6,S5) 
L8      =       P.*     
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L13     
L9      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L10     
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L11     
L10     =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V7      V5-FV1  
        VM      V7,P    
        V2      V5!V1&VM     * V2 = max(V5,V1) 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L11     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V7      V6-FV3  
        VM      V7,P    
        V0      V6!V3&VM     * V0 = max(V6,V3) 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L10     
L13     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L12     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L12     
* Jump back up to handle the remainder
        J       L9      
L12     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XMAX_SED

* XMIN_SED
* XMIN_SED: A segmented scan function (V-MINF) (internal version)
        IDENT   XMINSED (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMINSED

        BASE    D       * base 10
XMINSED ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* double ROWIDENT; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               VALUES
        ARGADD  A3,3,ARGPTR=A6
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V7      V1-FV5  
        VM      V7,P    
        V2      V5!V1&VM     * V2 = min(V5,V1) 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V7      V3-FV6  
        VM      V7,P    
        V0      V6!V3&VM     * V0 = min(V6,V3) 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S0      S6-FS5  
        JSM     L8      
        S6      S5           * S6= min(S6,S5) 
L8      =       P.*     
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L13     
L9      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L10     
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L11     
L10     =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
        V7      V1-FV5  
        VM      V7,P    
        V2      V5!V1&VM     * V2 = min(V5,V1) 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L11     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        A0      A1+A7   
        ,A0,A5  V6           * Vector store V6 with stride A5 
        V7      V3-FV6  
        VM      V7,P    
        V0      V6!V3&VM     * V0 = min(V6,V3) 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L10     
L13     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L12     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L12     
* Jump back up to handle the remainder
        J       L9      
L12     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END XMIN_SED

* IADD_SEZ
* IADD_SEZ: A segmented inclusive scan function (V-+) (internal version)
        IDENT   IADDSEZ (D, VALUES, FLAGS, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   IADDSEZ

        BASE    D       * base 10
IADDSEZ ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
* Initialize sum and identity
        V0      0       
        V0      S1+V0        * V0 = S1 + V0 
        V4      V0      
        B71     A3      
        T70     S1      
        S3      0       
        S4      0       
* Phase 1: generate the column sums
        A7      0       
        B72     A7      
        T72     S2      
        VL      A4      
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
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V2      V5+V1        * V2 = V5 + V1 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V0      V6+V3        * V0 = V6 + V3 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A7      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial sum across row
*   expand flags
        S6      S1      
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V6      S7!V5&VM     * If mask S7 else V5 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V6,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S1      
L6      =       P.*     
        S6      S6+S5   
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S2      
        VL      A4      
        S6      1       
        T70     S6      
        S7      1       
        A0      S2      
        JAZ     L12     
L8      =       P.*     
        S6      S2&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V1      ,A0,A5       * Vector load V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V2      V5+V1        * V2 = V5 + V1 
        A0      A1+A7   
        ,A0,A5  V2           * Vector store V2 with stride A5 
* *** End 1st body ***
        S2      S2-S7   
        A7      A7+1    
L10     =       P.*     
* ***   2nd body   ***
        S5      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S1      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V6      V4!V2&VM     * If mask V4 else V2 
        V0      V6+V3        * V0 = V6 + V3 
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
* *** End 2nd body ***
        S2      S2-S7   
        A7      A7+1    
        A0      S2      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L11     
* Clear flag to indicate done with 'full' part
        S6      0       
        T70     S6      
* Decrement vector length for remainder
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T71     
        S2      S6-S2   
* Jump to end if the second part is empty
        A0      S2      
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S2      T71     
        A7      B71     
        A6      B70     
        EXIT
        END
* END IADD_SEZ

* XMUL_REZ
* XMUL_REZ: A segmented reduce function (V-*-PASS) (internal version)
        IDENT   XMULREZ (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMULREZ

        BASE    D       * base 10
XMULREZ ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V1      V0      
        T70     S1      
        T71     S2      
        S1      0       
        S2      0       
        S4      0       
        S5      0       
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V0      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V2      ,A0,1        * Vector load V2 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V3      V1!V0&VM     * V3 = If mask V1 else V0 
        S1      V2,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V2,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        T74     S5      
        S5      0.6          * Magic float-int conversion const. 
        V6      S5-V3   
        V4      S5-FV6       * Cast src 1 (V3) to float 
        V7      S5-V2   
        V5      S5-FV7       * Cast src 2 (V2) to float 
        V6      V4*FV5       * Float multiply 
        V7      S5-FV6       * Cast result to int 
        V0      S5-V7        * Whew... V0 = V3 * V2 
        S5      T74     
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V2      ,A0,1        * Vector load V2 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V3      V1!V0&VM     * V3 = If mask V1 else V0 
        S1      V2,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V2,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        T74     S5      
        S5      0.6          * Magic float-int conversion const. 
        V6      S5-V3   
        V4      S5-FV6       * Cast src 1 (V3) to float 
        V7      S5-V2   
        V5      S5-FV7       * Cast src 2 (V2) to float 
        V6      V4*FV5       * Float multiply 
        V7      S5-FV6       * Cast result to int 
        V0      S5-V7        * Whew... V0 = V3 * V2 
        S5      T74     
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V1!V0&VM     * V0 = If mask V1 else V0 
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   load lastindex vector
        A0      S1      
        V5      ,A0,1        * Vector load V5 
*   expand flags
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V6      0       
        T70     S5      
        S5      1       
        V4      S5!V6&VM     * If mask S5 else V6 
        S5      T70     
        A2      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A2      A2+A3   
CRAY16  ENDIF           
        S7      S2      
        T70     S5      
        A7      A4      
L7      =       P.*     
        A7      A7-1    
* execute body of serial loop
        S5      V4,A7   
* Reset running sum if flag is set
        S1      V0,A7   
        A0      S5      
        JAZ     L6      
        A2      A2-1    
        S6      V5,A7   
        V3,A2   S7      
        S7      S2      
        V2,A2   S6      
L6      =       P.*     
        T71     S5      
        T72     S2      
        S2      0.6     
        S5      S2-S7   
        S7      S2-S1   
        S6      S2-FS5  
        S5      S2-FS7  
        S7      S6*FS5  
        S6      S2-FS7  
        S7      S2-S6   
        S5      T71     
        S2      T72     
        A0      A7      
        JAN     L7      
        S5      T70     
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A7      A7+A3   
CRAY16  ENDIF           
        A0      A7      
        JAZ     L8      
        VL      A7      
        A0      A1      
        V1      ,A0,V2       * Gather into V1 using indices in V2 
        T70     S5      
        S5      0.6          * Magic float-int conversion const. 
        V6      S5-V1   
        V4      S5-FV6       * Cast src 1 (V1) to float 
        V7      S5-V3   
        V5      S5-FV7       * Cast src 2 (V3) to float 
        V6      V4*FV5       * Float multiply 
        V7      S5-FV6       * Cast result to int 
        V0      S5-V7        * Whew... V0 = V1 * V3 
        S5      T70     
        A0      A1      
        ,A0,V2  V0           * Scatter V0 using indices in V2 
L8      =       P.*     
        A6      B70     
        EXIT
        END
* END XMUL_REZ

* XADD_REZ
* XADD_REZ: A segmented reduce function (V-+) (internal version)
        IDENT   XADDREZ (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XADDREZ

        BASE    D       * base 10
XADDREZ ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V2      V0      
        T70     S1      
        T71     S2      
        S1      0       
        S2      0       
        S4      0       
        S5      0       
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V6      V2!V0&VM     * V6 = If mask V2 else V0 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V1      V6+V3        * V1 = V6 + V3 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V7      V2!V1&VM     * V7 = If mask V2 else V1 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V0      V7+V4        * V0 = V7 + V4 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V2!V0&VM     * V0 = If mask V2 else V0 
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   load lastindex vector
        A0      S1      
        V6      ,A0,1        * Vector load V6 
*   expand flags
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V7      0       
        T70     S5      
        S5      1       
        V5      S5!V7&VM     * If mask S5 else V7 
        S5      T70     
        A2      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A2      A2+A3   
CRAY16  ENDIF           
        S7      S2      
        T70     S5      
        A7      A4      
L7      =       P.*     
        A7      A7-1    
* execute body of serial loop
        S5      V5,A7   
* Reset running sum if flag is set
        S1      V0,A7   
        A0      S5      
        JAZ     L6      
        A2      A2-1    
        S6      V6,A7   
        V4,A2   S7      
        S7      S2      
        V3,A2   S6      
L6      =       P.*     
        S7      S7+S1   
        A0      A7      
        JAN     L7      
        S5      T70     
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A7      A7+A3   
CRAY16  ENDIF           
        A0      A7      
        JAZ     L8      
        VL      A7      
        A0      A1      
        V5      ,A0,V3       * Gather into V5 using indices in V3 
        V6      V5+V4        * V6 = V5 + V4 
        A0      A1      
        ,A0,V3  V6           * Scatter V6 using indices in V3 
L8      =       P.*     
        A6      B70     
        EXIT
        END
* END XADD_REZ

* XAND_REZ
* XAND_REZ: A segmented reduce function (V-AND) (internal version)
        IDENT   XANDREZ (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XANDREZ

        BASE    D       * base 10
XANDREZ ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V2      V0      
        T70     S1      
        T71     S2      
        S1      0       
        S2      0       
        S4      0       
        S5      0       
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V6      V2!V0&VM     * V6 = If mask V2 else V0 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V1      V6&V3        * V1 = V6 AND V3 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V7      V2!V1&VM     * V7 = If mask V2 else V1 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V0      V7&V4        * V0 = V7 AND V4 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V2!V0&VM     * V0 = If mask V2 else V0 
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   load lastindex vector
        A0      S1      
        V6      ,A0,1        * Vector load V6 
*   expand flags
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V7      0       
        T70     S5      
        S5      1       
        V5      S5!V7&VM     * If mask S5 else V7 
        S5      T70     
        A2      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A2      A2+A3   
CRAY16  ENDIF           
        S7      S2      
        T70     S5      
        A7      A4      
L7      =       P.*     
        A7      A7-1    
* execute body of serial loop
        S5      V5,A7   
* Reset running sum if flag is set
        S1      V0,A7   
        A0      S5      
        JAZ     L6      
        A2      A2-1    
        S6      V6,A7   
        V4,A2   S7      
        S7      S2      
        V3,A2   S6      
L6      =       P.*     
        S7      S7&S1        * logical and 
        A0      A7      
        JAN     L7      
        S5      T70     
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A7      A7+A3   
CRAY16  ENDIF           
        A0      A7      
        JAZ     L8      
        VL      A7      
        A0      A1      
        V5      ,A0,V3       * Gather into V5 using indices in V3 
        V6      V5&V4        * V6 = V5 AND V4 
        A0      A1      
        ,A0,V3  V6           * Scatter V6 using indices in V3 
L8      =       P.*     
        A6      B70     
        EXIT
        END
* END XAND_REZ

* XIOR_REZ
* XIOR_REZ: A segmented reduce function (V-OR) (internal version)
        IDENT   XIORREZ (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XIORREZ

        BASE    D       * base 10
XIORREZ ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V2      V0      
        T70     S1      
        T71     S2      
        S1      0       
        S2      0       
        S4      0       
        S5      0       
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V6      V2!V0&VM     * V6 = If mask V2 else V0 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V1      V6!V3        * V1 = V6 OR V3 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V7      V2!V1&VM     * V7 = If mask V2 else V1 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V0      V7!V4        * V0 = V7 OR V4 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V2!V0&VM     * V0 = If mask V2 else V0 
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   load lastindex vector
        A0      S1      
        V6      ,A0,1        * Vector load V6 
*   expand flags
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V7      0       
        T70     S5      
        S5      1       
        V5      S5!V7&VM     * If mask S5 else V7 
        S5      T70     
        A2      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A2      A2+A3   
CRAY16  ENDIF           
        S7      S2      
        T70     S5      
        A7      A4      
L7      =       P.*     
        A7      A7-1    
* execute body of serial loop
        S5      V5,A7   
* Reset running sum if flag is set
        S1      V0,A7   
        A0      S5      
        JAZ     L6      
        A2      A2-1    
        S6      V6,A7   
        V4,A2   S7      
        S7      S2      
        V3,A2   S6      
L6      =       P.*     
        S7      S7!S1        * logical or 
        A0      A7      
        JAN     L7      
        S5      T70     
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A7      A7+A3   
CRAY16  ENDIF           
        A0      A7      
        JAZ     L8      
        VL      A7      
        A0      A1      
        V5      ,A0,V3       * Gather into V5 using indices in V3 
        V6      V5!V4        * V6 = V5 OR V4 
        A0      A1      
        ,A0,V3  V6           * Scatter V6 using indices in V3 
L8      =       P.*     
        A6      B70     
        EXIT
        END
* END XIOR_REZ

* XXOR_REZ
* XXOR_REZ: A segmented reduce function (V-XOR) (internal version)
        IDENT   XXORREZ (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XXORREZ

        BASE    D       * base 10
XXORREZ ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V2      V0      
        T70     S1      
        T71     S2      
        S1      0       
        S2      0       
        S4      0       
        S5      0       
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V6      V2!V0&VM     * V6 = If mask V2 else V0 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V1      V6\V3        * V1 = V6 XOR V3 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V7      V2!V1&VM     * V7 = If mask V2 else V1 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V0      V7\V4        * V0 = V7 XOR V4 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V2!V0&VM     * V0 = If mask V2 else V0 
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   load lastindex vector
        A0      S1      
        V6      ,A0,1        * Vector load V6 
*   expand flags
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V7      0       
        T70     S5      
        S5      1       
        V5      S5!V7&VM     * If mask S5 else V7 
        S5      T70     
        A2      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A2      A2+A3   
CRAY16  ENDIF           
        S7      S2      
        T70     S5      
        A7      A4      
L7      =       P.*     
        A7      A7-1    
* execute body of serial loop
        S5      V5,A7   
* Reset running sum if flag is set
        S1      V0,A7   
        A0      S5      
        JAZ     L6      
        A2      A2-1    
        S6      V6,A7   
        V4,A2   S7      
        S7      S2      
        V3,A2   S6      
L6      =       P.*     
        S7      S7\S1        * logical xor 
        A0      A7      
        JAN     L7      
        S5      T70     
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A7      A7+A3   
CRAY16  ENDIF           
        A0      A7      
        JAZ     L8      
        VL      A7      
        A0      A1      
        V5      ,A0,V3       * Gather into V5 using indices in V3 
        V6      V5\V4        * V6 = V5 XOR V4 
        A0      A1      
        ,A0,V3  V6           * Scatter V6 using indices in V3 
L8      =       P.*     
        A6      B70     
        EXIT
        END
* END XXOR_REZ

* XMAX_REZ
* XMAX_REZ: A segmented reduce function (V-MAX) (internal version)
        IDENT   XMAXREZ (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMAXREZ

        BASE    D       * base 10
XMAXREZ ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V6      V0      
        S4      0       
        S5      0       
        T70     S1      
        T71     S2      
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      ,A3     
        A3      A3-1    
CRAY16  ENDIF           
        S1      ,A3     
        A3      A3-1    
        A0      A2+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V7      V0-V2   
        VM      V7,P    
        V4      V0!V2&VM     * V4 = max(V0,V2) 
        A0      A1+A7   
        ,A0,A5  V4           * Vector store V4 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V1      V6!V4&VM     * If mask V6 else V4 
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      ,A3     
        A3      A3-1    
CRAY16  ENDIF           
        S1      ,A3     
        A3      A3-1    
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V7      V1-V3   
        VM      V7,P    
        V5      V1!V3&VM     * V5 = max(V1,V3) 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V6!V5&VM     * If mask V6 else V5 
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   expand flags
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V3      S7!V5&VM     * If mask S7 else V5 
*   load lastindex vector
        A0      S1      
        V4      ,A0,1        * Vector load V4 
        S6      S2      
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A2      PS5          * Population count (of 1 bits) 
        A7      A7+A2   
CRAY16  ENDIF           
        A6      A4      
L7      =       P.*     
        A6      A6-1    
* execute body of serial loop
        S7      V3,A6   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        A7      A7-1    
        V2,A7   S6      
        S6      S2      
        S1      V4,A6   
        V1,A7   S1      
L6      =       P.*     
        S1      V0,A6   
        S0      S1-S6   
        JSM     L8      
        S6      S1           * S6= max(S6,S1) 
L8      =       P.*     
        A0      A6      
        JAN     L7      
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A2      PS5          * Population count (of 1 bits) 
        A7      A7+A2   
CRAY16  ENDIF           
        VL      A7      
        A0      A1      
        V3      ,A0,V1       * Gather into V3 using indices in V1 
        V5      V3-V2   
        VM      V5,P    
        V4      V3!V2&VM     * V4 = max(V3,V2) 
        A0      A1      
        ,A0,V1  V4           * Scatter V4 using indices in V1 
        A6      B70     
        EXIT
        END
* END XMAX_REZ

* XMIN_REZ
* XMIN_REZ: A segmented reduce function (V-MIN) (internal version)
        IDENT   XMINREZ (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMINREZ

        BASE    D       * base 10
XMINREZ ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V6      V0      
        S4      0       
        S5      0       
        T70     S1      
        T71     S2      
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      ,A3     
        A3      A3-1    
CRAY16  ENDIF           
        S1      ,A3     
        A3      A3-1    
        A0      A2+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V7      V2-V0   
        VM      V7,P    
        V4      V0!V2&VM     * V4 = min(V0,V2) 
        A0      A1+A7   
        ,A0,A5  V4           * Vector store V4 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V1      V6!V4&VM     * If mask V6 else V4 
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      ,A3     
        A3      A3-1    
CRAY16  ENDIF           
        S1      ,A3     
        A3      A3-1    
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V7      V3-V1   
        VM      V7,P    
        V5      V1!V3&VM     * V5 = min(V1,V3) 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V6!V5&VM     * If mask V6 else V5 
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   expand flags
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V3      S7!V5&VM     * If mask S7 else V5 
*   load lastindex vector
        A0      S1      
        V4      ,A0,1        * Vector load V4 
        S6      S2      
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A2      PS5          * Population count (of 1 bits) 
        A7      A7+A2   
CRAY16  ENDIF           
        A6      A4      
L7      =       P.*     
        A6      A6-1    
* execute body of serial loop
        S7      V3,A6   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        A7      A7-1    
        V2,A7   S6      
        S6      S2      
        S1      V4,A6   
        V1,A7   S1      
L6      =       P.*     
        S1      V0,A6   
        S0      S6-S1   
        JSM     L8      
        S6      S1           * S6= min(S6,S1) 
L8      =       P.*     
        A0      A6      
        JAN     L7      
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A2      PS5          * Population count (of 1 bits) 
        A7      A7+A2   
CRAY16  ENDIF           
        VL      A7      
        A0      A1      
        V3      ,A0,V1       * Gather into V3 using indices in V1 
        V5      V2-V3   
        VM      V5,P    
        V4      V3!V2&VM     * V4 = min(V3,V2) 
        A0      A1      
        ,A0,V1  V4           * Scatter V4 using indices in V1 
        A6      B70     
        EXIT
        END
* END XMIN_REZ

* XADD_RED
* XADD_RED: A segmented reduce function (V-+F) (internal version)
        IDENT   XADDRED (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XADDRED

        BASE    D       * base 10
XADDRED ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V2      V0      
        T70     S1      
        T71     S2      
        S1      0       
        S2      0       
        S4      0       
        S5      0       
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V6      V2!V0&VM     * V6 = If mask V2 else V0 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V1      V6+FV3       * V1 = V6 + V3 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V7      V2!V1&VM     * V7 = If mask V2 else V1 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V0      V7+FV4       * V0 = V7 + V4 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V2!V0&VM     * V0 = If mask V2 else V0 
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   load lastindex vector
        A0      S1      
        V6      ,A0,1        * Vector load V6 
*   expand flags
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V7      0       
        T70     S5      
        S5      1       
        V5      S5!V7&VM     * If mask S5 else V7 
        S5      T70     
        A2      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A2      A2+A3   
CRAY16  ENDIF           
        S7      S2      
        T70     S5      
        A7      A4      
L7      =       P.*     
        A7      A7-1    
* execute body of serial loop
        S5      V5,A7   
* Reset running sum if flag is set
        S1      V0,A7   
        A0      S5      
        JAZ     L6      
        A2      A2-1    
        S6      V6,A7   
        V4,A2   S7      
        S7      S2      
        V3,A2   S6      
L6      =       P.*     
        S7      S7+FS1  
        A0      A7      
        JAN     L7      
        S5      T70     
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A7      A7+A3   
CRAY16  ENDIF           
        A0      A7      
        JAZ     L8      
        VL      A7      
        A0      A1      
        V5      ,A0,V3       * Gather into V5 using indices in V3 
        V6      V5+FV4       * V6 = V5 + V4 
        A0      A1      
        ,A0,V3  V6           * Scatter V6 using indices in V3 
L8      =       P.*     
        A6      B70     
        EXIT
        END
* END XADD_RED

* XMUL_RED
* XMUL_RED: A segmented reduce function (V-*F) (internal version)
        IDENT   XMULRED (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMULRED

        BASE    D       * base 10
XMULRED ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V2      V0      
        T70     S1      
        T71     S2      
        S1      0       
        S2      0       
        S4      0       
        S5      0       
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V6      V2!V0&VM     * V6 = If mask V2 else V0 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V1      V6*FV3       * V1 = V6 * V3 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        B72     A1      
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A1      2       
CRAY16  ELSE            
        A1      1       
CRAY16  ENDIF           
        VL      A1      
        A0      A3      
        V5      ,A0,1        * Vector load V5 
        A1      B72     
        VL      A4      
        B72     A3      
        A3      0       
        V7      V2!V1&VM     * V7 = If mask V2 else V1 
        S1      V5,A3   
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      V5,A0   
CRAY16  ENDIF           
        A3      B72     
        A0      A2+A7   
        V4      ,A0,A5       * Vector load V4 with stride A5 
        V0      V7*FV4       * V0 = V7 * V4 
        A3      A3-1    
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      A3-1    
CRAY16  ENDIF           
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V2!V0&VM     * V0 = If mask V2 else V0 
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   load lastindex vector
        A0      S1      
        V6      ,A0,1        * Vector load V6 
*   expand flags
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V7      0       
        T70     S5      
        S5      1       
        V5      S5!V7&VM     * If mask S5 else V7 
        S5      T70     
        A2      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A2      A2+A3   
CRAY16  ENDIF           
        S7      S2      
        T70     S5      
        A7      A4      
L7      =       P.*     
        A7      A7-1    
* execute body of serial loop
        S5      V5,A7   
* Reset running sum if flag is set
        S1      V0,A7   
        A0      S5      
        JAZ     L6      
        A2      A2-1    
        S6      V6,A7   
        V4,A2   S7      
        S7      S2      
        V3,A2   S6      
L6      =       P.*     
        S7      S7*FS1  
        A0      A7      
        JAN     L7      
        S5      T70     
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A3      PS5          * Population count (of 1 bits) 
        A7      A7+A3   
CRAY16  ENDIF           
        A0      A7      
        JAZ     L8      
        VL      A7      
        A0      A1      
        V5      ,A0,V3       * Gather into V5 using indices in V3 
        V6      V5*FV4       * V6 = V5 * V4 
        A0      A1      
        ,A0,V3  V6           * Scatter V6 using indices in V3 
L8      =       P.*     
        A6      B70     
        EXIT
        END
* END XMUL_RED

* XMAX_RED
* XMAX_RED: A segmented reduce function (V-MAXF) (internal version)
        IDENT   XMAXRED (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMAXRED

        BASE    D       * base 10
XMAXRED ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V6      V0      
        S4      0       
        S5      0       
        T70     S1      
        T71     S2      
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      ,A3     
        A3      A3-1    
CRAY16  ENDIF           
        S1      ,A3     
        A3      A3-1    
        A0      A2+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V7      V0-FV2  
        VM      V7,P    
        V4      V0!V2&VM     * V4 = max(V0,V2) 
        A0      A1+A7   
        ,A0,A5  V4           * Vector store V4 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V1      V6!V4&VM     * If mask V6 else V4 
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      ,A3     
        A3      A3-1    
CRAY16  ENDIF           
        S1      ,A3     
        A3      A3-1    
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V7      V1-FV3  
        VM      V7,P    
        V5      V1!V3&VM     * V5 = max(V1,V3) 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V6!V5&VM     * If mask V6 else V5 
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   expand flags
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V3      S7!V5&VM     * If mask S7 else V5 
*   load lastindex vector
        A0      S1      
        V4      ,A0,1        * Vector load V4 
        S6      S2      
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A2      PS5          * Population count (of 1 bits) 
        A7      A7+A2   
CRAY16  ENDIF           
        A6      A4      
L7      =       P.*     
        A6      A6-1    
* execute body of serial loop
        S7      V3,A6   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        A7      A7-1    
        V2,A7   S6      
        S6      S2      
        S1      V4,A6   
        V1,A7   S1      
L6      =       P.*     
        S1      V0,A6   
        S0      S1-FS6  
        JSM     L8      
        S6      S1           * S6= max(S6,S1) 
L8      =       P.*     
        A0      A6      
        JAN     L7      
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A2      PS5          * Population count (of 1 bits) 
        A7      A7+A2   
CRAY16  ENDIF           
        VL      A7      
        A0      A1      
        V3      ,A0,V1       * Gather into V3 using indices in V1 
        V5      V3-FV2  
        VM      V5,P    
        V4      V3!V2&VM     * V4 = max(V3,V2) 
        A0      A1      
        ,A0,V1  V4           * Scatter V4 using indices in V1 
        A6      B70     
        EXIT
        END
* END XMAX_RED

* XMIN_RED
* XMIN_RED: A segmented reduce function (V-MINF) (internal version)
        IDENT   XMINRED (D, VALUES, FLAGS, LASTIND, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   XMINRED

        BASE    D       * base 10
XMINRED ENTER  NP=8    * # parameters
* int *D;
* int *VALUES;
* int *FLAGS;
* int *LASTIND; 
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
*                               FLAGS
        ARGADD  S1,4,ARGPTR=A6
*                               LASTIND
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        V6      V0      
        S4      0       
        S5      0       
        T70     S1      
        T71     S2      
        A7      A5      
        A7      A7-1    
        B71     A7      
        A4      A4-1    
        S6      A5      
        S3      S6-S3   
        T73     S3      
        VL      A4      
        S6      1       
        T72     S6      
        S7      1       
        A0      S3      
        JAZ     L5      
L1      =       P.*     
        S6      S3&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      ,A3     
        A3      A3-1    
CRAY16  ENDIF           
        S1      ,A3     
        A3      A3-1    
        A0      A2+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V7      V2-FV0  
        VM      V7,P    
        V4      V0!V2&VM     * V4 = min(V0,V2) 
        A0      A1+A7   
        ,A0,A5  V4           * Vector store V4 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V1      V6!V4&VM     * If mask V6 else V4 
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 1st body ***
        S3      S3-S7   
        A7      A7-1    
L3      =       P.*     
* ***   2nd body   ***
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S2      ,A3     
        A3      A3-1    
CRAY16  ENDIF           
        S1      ,A3     
        A3      A3-1    
        A0      A2+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V7      V3-FV1  
        VM      V7,P    
        V5      V1!V3&VM     * V5 = min(V1,V3) 
        A0      A1+A7   
        ,A0,A5  V5           * Vector store V5 with stride A5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S1      
        VM1     S2      
CRAY16  ELSE            
        VM      S1      
CRAY16  ENDIF           
        V0      V6!V5&VM     * If mask V6 else V5 
        S4      S4!S1        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      S5!S2   
CRAY16  ENDIF           
* *** End 2nd body ***
        S3      S3-S7   
        A7      A7-1    
        A0      S3      
        JAN     L2      
L5      =       P.*     
* Bail out if finished both parts
        S6      T72     
        A0      S6      
        JAZ     L4      
* Clear flag to indicate done with 'full' part
        S6      0       
        T72     S6      
* Increment vector length for remainder
        A4      A4+1    
        VL      A4      
        S6      A5      
        S3      T73     
        S3      S6-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L4      
* Jump back up to handle the remainder
        J       L1      
L4      =       P.*     
        VL      A4      
        S3      T73     
        A7      B71     
        S1      T70     
        S2      T71     
* Phase 2: sum across row compressing adjustment vectors
*   expand flags
        V5      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V3      S7!V5&VM     * If mask S7 else V5 
*   load lastindex vector
        A0      S1      
        V4      ,A0,1        * Vector load V4 
        S6      S2      
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A2      PS5          * Population count (of 1 bits) 
        A7      A7+A2   
CRAY16  ENDIF           
        A6      A4      
L7      =       P.*     
        A6      A6-1    
* execute body of serial loop
        S7      V3,A6   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        A7      A7-1    
        V2,A7   S6      
        S6      S2      
        S1      V4,A6   
        V1,A7   S1      
L6      =       P.*     
        S1      V0,A6   
        S0      S6-FS1  
        JSM     L8      
        S6      S1           * S6= min(S6,S1) 
L8      =       P.*     
        A0      A6      
        JAN     L7      
*  gather from lastindex, add adjustment, and scatter back
        A7      PS4          * Population count (of 1 bits) 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        A2      PS5          * Population count (of 1 bits) 
        A7      A7+A2   
CRAY16  ENDIF           
        VL      A7      
        A0      A1      
        V3      ,A0,V1       * Gather into V3 using indices in V1 
        V5      V2-FV3  
        VM      V5,P    
        V4      V3!V2&VM     * V4 = min(V3,V2) 
        A0      A1      
        ,A0,V1  V4           * Scatter V4 using indices in V1 
        A6      B70     
        EXIT
        END
* END XMIN_RED

* XCPYSCAN
* XCPYSCAN: A segmented copy-scan function (internal version)
        IDENT   XCPYSCA (D, VALUES, FLAGS, LASTIND, VLENGTH, STRIDE, REMAIN)
        ENTRY   XCPYSCA

        BASE    D       * base 10
XCPYSCA ENTER  NP=7    * # parameters
* int *D;
* int *VALUES;
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
*                               FLAGS
        ARGADD  A4,4,ARGPTR=A6
*                               LASTIND
        ARGADD  A5,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A7,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,7,ARGPTR=A6
*                               REMAIN
        VL      A5      
* Phases 1 and 2: result loaded from last index vector
        A0      A4      
        V4      ,A0,1        * Vector load V4 
        A0      A2      
        V0      ,A0,V4       * Gather into V0 using indices in V4 
* Phase 3: generate the final sums
        A6      0       
        B71     A6      
        T71     S1      
        VL      A5      
        S4      1       
        T70     S4      
        S5      1       
        A0      S1      
        JAZ     L5      
L1      =       P.*     
        S4      S1&S5        * logical and 
* Jump to start if the number of iterations is even
        A0      S4      
        JAZ     L2      
* Fix and Jump to middle if the number of iterations is odd
        V2      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S2      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S3      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A6   
        V1      ,A0,A7       * Vector load V1 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S2      
        VM1     S3      
CRAY16  ELSE            
        VM      S2      
CRAY16  ENDIF           
        V2      V1!V0&VM     * If mask V1 else V0 
        A0      A1+A6   
        ,A0,A7  V2           * Vector store V2 with stride A7 
* *** End 1st body ***
        S1      S1-S5   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        S2      ,A3     
        A3      A3+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S3      ,A3     
        A3      A3+1    
CRAY16  ENDIF           
        A0      A2+A6   
        V3      ,A0,A7       * Vector load V3 with stride A7 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S2      
        VM1     S3      
CRAY16  ELSE            
        VM      S2      
CRAY16  ENDIF           
        V0      V3!V2&VM     * If mask V3 else V2 
        A0      A1+A6   
        ,A0,A7  V0           * Vector store V0 with stride A7 
* *** End 2nd body ***
        S1      S1-S5   
        A6      A6+1    
        A0      S1      
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
        A5      A5-1    
        VL      A5      
        S4      A7      
        S1      T71     
        S1      S4-S1   
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
* END XCPYSCAN

* ADDSUZCPYWUZ
* An unsegmented add scan function folded with a copy (internal version)
        IDENT   ADDSUZC (D, COPYD, VALUES, IDENTITY, ROWIDENT, VLENGTH, STRIDE, REMAIN)
        ENTRY   ADDSUZC

        BASE    D       * base 10
ADDSUZC ENTER  NP=8    * # parameters
* int *D;
* int *COPYD;
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
*                               COPYD
        ARGADD  A3,3,ARGPTR=A6
*                               VALUES
        ARGADD  S1,4,ARGPTR=A6
*                               IDENTITY
        ARGADD  S2,5,ARGPTR=A6
*                               ROWIDENT
        ARGADD  A4,6,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,7,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,8,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
* Phase 1: generate the column sums
        A7      0       
        B71     A7      
        T71     S3      
        VL      A4      
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
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V1      V0+V2        * V1 = V0 + V2 
        A0      A2+A7   
        ,A0,A5  V2           * Vector store V2 with stride A5 
* *** End 1st body ***
        S3      S3-S5   
        A7      A7+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A3+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V0      V1+V3        * V0 = V1 + V3 
        A0      A2+A7   
        ,A0,A5  V3           * Vector store V3 with stride A5 
* *** End 2nd body ***
        S3      S3-S5   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S4      A5      
        S3      T71     
        S3      S4-S3   
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
        A7      B71     
* Phase 2: serial sum across row
        A6      A4      
        A7      0       
L6      =       P.*     
* execute body of serial loop
        S4      V0,A7   
        V0,A7   S1      
        S1      S1+S4   
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L6      
* Phase 3: generate the final sums
        A7      0       
        B71     A7      
        T71     S3      
        VL      A4      
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
        A0      A1+A7   
        ,A0,A5  V0           * Vector store V0 with stride A5 
        A0      A3+A7   
        V2      ,A0,A5       * Vector load V2 with stride A5 
        V1      V0+V2        * V1 = V0 + V2 
* *** End 1st body ***
        S3      S3-S5   
        A7      A7+1    
L9      =       P.*     
* ***   2nd body   ***
        A0      A1+A7   
        ,A0,A5  V1           * Vector store V1 with stride A5 
        A0      A3+A7   
        V3      ,A0,A5       * Vector load V3 with stride A5 
        V0      V1+V3        * V0 = V1 + V3 
* *** End 2nd body ***
        S3      S3-S5   
        A7      A7+1    
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
        A4      A4-1    
        VL      A4      
        S4      A5      
        S3      T71     
        S3      S4-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L10     
* Jump back up to handle the remainder
        J       L7      
L10     =       P.*     
* readjust the vector length
        A4      A4+1    
        VL      A4      
        S3      T71     
        A7      B71     
        S1      S1           *  return S1 
        A6      B70     
        EXIT
        END
* END ADDSUZCPYWUZ

* IND_LUZ
* Creates vector of strided indices starting from init (internal version)
        IDENT   INDLUZ (D, INIT, STR, VLENGTH, STRIDE, REMAIN)
        ENTRY   INDLUZ

        BASE    D       * base 10
INDLUZ  ENTER  NP=6    * # parameters
* int *D;
* int INIT; 
* int STR; 
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  S1,2,ARGPTR=A6
*                               INIT
        ARGADD  S2,3,ARGPTR=A6
*                               STR
        ARGADD  A2,4,ARGPTR=A6
*                               VLENGTH
        ARGADD  A3,5,ARGPTR=A6
*                               STRIDE
        ARGADD  S3,6,ARGPTR=A6
*                               REMAIN
        VL      A2      
        A4      S2      
        A4      A4*A3   
        S5      A4      
        S4      S1      
        V0      0       
        V0      S2+V0        * V0 = S2 + V0 
        A7      A2      
        A5      0       
L1      =       P.*     
* execute body of serial loop
        V1,A5   S4      
        S4      S4+S5   
        A5      A5+1    
        A7      A7-1    
        A0      A7      
        JAN     L1      
        A4      0       
        B71     A4      
        T71     S3      
        VL      A2      
        S4      1       
        T70     S4      
        S5      1       
        A0      S3      
        JAZ     L6      
L2      =       P.*     
        S4      S3&S5        * logical and 
* Jump to start if the number of iterations is even
        A0      S4      
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
        S3      S3-S5   
        A4      A4+1    
L4      =       P.*     
* ***   2nd body   ***
        A0      A1+A4   
        ,A0,A3  V2           * Vector store V2 with stride A3 
        V1      V2+V0        * V1 = V2 + V0 
* *** End 2nd body ***
        S3      S3-S5   
        A4      A4+1    
        A0      S3      
        JAN     L3      
L6      =       P.*     
* Bail out if finished both parts
        S4      T70     
        A0      S4      
        JAZ     L5      
* Clear flag to indicate done with 'full' part
        S4      0       
        T70     S4      
* Decrement vector length for remainder
        A2      A2-1    
        VL      A2      
        S4      A3      
        S3      T71     
        S3      S4-S3   
* Jump to end if the second part is empty
        A0      S3      
        JAZ     L5      
* Jump back up to handle the remainder
        J       L2      
L5      =       P.*     
* readjust the vector length
        A2      A2+1    
        VL      A2      
        S3      T71     
        A4      B71     
        A6      B70     
        EXIT
        END
* END IND_LUZ

* XINDLEZ
* Segmented index function (segmented scan on 1's) (internal version)
        IDENT   XINDLEZ (D, FLAGS, VLENGTH, STRIDE, REMAIN)
        ENTRY   XINDLEZ

        BASE    D       * base 10
XINDLEZ ENTER  NP=5    * # parameters
* int *D;
* int *FLAGS;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  A2,2,ARGPTR=A6
*                               FLAGS
        ARGADD  A3,3,ARGPTR=A6
*                               VLENGTH
        ARGADD  A4,4,ARGPTR=A6
*                               STRIDE
        ARGADD  S1,5,ARGPTR=A6
*                               REMAIN
        VL      A3      
        V0      0       
        V4      0       
        S4      1       
        V2      0       
        V2      S4+V2        * V2 = S4 + V2 
        V3      V2      
        B71     A2      
        S2      0       
* Phase 1: generate the column sums
        A5      0       
        B72     A5      
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
        V1      V0      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        S4      ,A2     
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A2     
        A2      A2+1    
CRAY16  ENDIF           
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        V1      V5+V2        * V1 = V5 + V2 
        S2      S2!S4        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S3      S3!S5   
CRAY16  ENDIF           
* *** End 1st body ***
        S1      S1-S7   
        A5      A5+1    
L3      =       P.*     
* ***   2nd body   ***
        S4      ,A2     
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A2     
        A2      A2+1    
CRAY16  ENDIF           
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V6      V4!V1&VM     * If mask V4 else V1 
        V0      V6+V3        * V0 = V6 + V3 
        S2      S2!S4        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S3      S3!S5   
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
        A5      B72     
        A2      B71     
* Phase 2: serial sum across row
*   expand flags
        S5      0       
        V5      0       
        S6      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S2      
        VM1     S3      
CRAY16  ELSE            
        VM      S2      
CRAY16  ENDIF           
        V6      S6!V5&VM     * If mask S6 else V5 
        A7      A3      
        A5      0       
L7      =       P.*     
* execute body of serial loop
        S4      V0,A5   
        V0,A5   S5      
        S6      V6,A5   
* Reset running sum if flag is set
        A0      S6      
        JAZ     L6      
        S5      0       
L6      =       P.*     
        S5      S5+S4   
        A5      A5+1    
        A7      A7-1    
        A0      A7      
        JAN     L7      
* Phase 3: generate the final sums
        A5      0       
        B71     A5      
        T71     S1      
        VL      A3      
        S6      1       
        T70     S6      
        S7      1       
        A0      S1      
        JAZ     L12     
L8      =       P.*     
        S6      S1&S7        * logical and 
* Jump to start if the number of iterations is even
        A0      S6      
        JAZ     L9      
* Fix and Jump to middle if the number of iterations is odd
        V1      V0      
        J       L10     
L9      =       P.*     
* ***   1st body   ***
        S4      ,A2     
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A2     
        A2      A2+1    
CRAY16  ENDIF           
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V5      V4!V0&VM     * If mask V4 else V0 
        A0      A1+A5   
        ,A0,A4  V5           * Vector store V5 with stride A4 
        V1      V5+V2        * V1 = V5 + V2 
* *** End 1st body ***
        S1      S1-S7   
        A5      A5+1    
L10     =       P.*     
* ***   2nd body   ***
        S4      ,A2     
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      ,A2     
        A2      A2+1    
CRAY16  ENDIF           
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S4      
        VM1     S5      
CRAY16  ELSE            
        VM      S4      
CRAY16  ENDIF           
        V6      V4!V1&VM     * If mask V4 else V1 
        A0      A1+A5   
        ,A0,A4  V6           * Vector store V6 with stride A4 
        V0      V6+V3        * V0 = V6 + V3 
* *** End 2nd body ***
        S1      S1-S7   
        A5      A5+1    
        A0      S1      
        JAN     L9      
L12     =       P.*     
* Bail out if finished both parts
        S6      T70     
        A0      S6      
        JAZ     L11     
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
        JAZ     L11     
* Jump back up to handle the remainder
        J       L8      
L11     =       P.*     
* readjust the vector length
        A3      A3+1    
        VL      A3      
        S1      T71     
        A5      B71     
        A6      B70     
        EXIT
        END
* END XINDLEZ

* CLASTINDEX
* Finds index of last seg start and compresses flags to flagd  (internal version)
        IDENT   CLASTIN (D, INCD, FLAGD, FLAGS, VLENGTH, STRIDE, REMAIN)
        ENTRY   CLASTIN

        BASE    D       * base 10
CLASTIN ENTER  NP=7    * # parameters
* int *D;
* int *INCD; 
* int *FLAGD;
* int *FLAGS;
* int VLENGTH; 
* int STRIDE; 
* int REMAIN; 
        B70     A6      
        ARGADD  A1,1,ARGPTR=A6
*                               D
        ARGADD  S1,2,ARGPTR=A6
*                               INCD
        ARGADD  A2,3,ARGPTR=A6
*                               FLAGD
        ARGADD  A3,4,ARGPTR=A6
*                               FLAGS
        ARGADD  A4,5,ARGPTR=A6
*                               VLENGTH
        ARGADD  A5,6,ARGPTR=A6
*                               STRIDE
        ARGADD  S2,7,ARGPTR=A6
*                               REMAIN
        VL      A4      
        V0      0       
        S3      0       
        S4      0       
* set v-last-a[i] = i*stride
        V4      0       
        V3,VM   V4,Z         * Compress Index Where V4 Zero 
        S5      16      
        A7      S5      
        S5      A5      
        S5      S5<31        * S5 = S5 left shift 31 
        V3      V3<A7        * V3 = V3 left shift A7 
        V3      S5*V3        * V3 = S5 * V3 
        S5      1       
        V2      0       
        V2      S5+V2        * V2 = S5 + V2 
        V3      V3-V2        * V3 = V3 - V2 
        B71     A3      
        T70     S1      
* Phase 1: generate the column sums
        A6      0       
        B72     A6      
        T72     S2      
        VL      A4      
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
        V1      V0      
        V4      V3      
        J       L3      
L2      =       P.*     
* ***   1st body   ***
        A0      A3+A6   
        V5      ,A0,A5       * Vector load V5 with stride A5 
        VM      V5,N         * Set mask for non-zero elements in V5 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      VM0     
        S1      VM1     
CRAY16  ELSE            
        S5      VM      
CRAY16  ENDIF           
        V4      V2+V3        * V4 = V2 + V3 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V1      V4!V0&VM     * If mask V4 else V0 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
        ,A2     S5           * Scalar store S5 
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        ,A2     S1           * Scalar store S1 
        A2      A2+1    
CRAY16  ENDIF           
* *** End 1st body ***
        S2      S2-S7   
        A6      A6+1    
L3      =       P.*     
* ***   2nd body   ***
        A0      A3+A6   
        V6      ,A0,A5       * Vector load V6 with stride A5 
        VM      V6,N         * Set mask for non-zero elements in V6 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S5      VM0     
        S1      VM1     
CRAY16  ELSE            
        S5      VM      
CRAY16  ENDIF           
        V3      V2+V4        * V3 = V2 + V4 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S5      
        VM1     S1      
CRAY16  ELSE            
        VM      S5      
CRAY16  ENDIF           
        V0      V3!V1&VM     * If mask V3 else V1 
        S3      S3!S5        * logical or packed mask 
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        S4      S4!S1   
CRAY16  ENDIF           
        ,A2     S5           * Scalar store S5 
        A2      A2+1    
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        ,A2     S1           * Scalar store S1 
        A2      A2+1    
CRAY16  ENDIF           
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
        A4      A4-1    
        VL      A4      
        S6      A5      
        S2      T72     
        S2      S6-S2   
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
        A6      B72     
        A3      B71     
        S1      T70     
* Phase 2: serial copy-scan across row
        S6      0       
*   expand flags
        V3      0       
        S7      1       
CRAY16  IFC     /"$CPU"/,EQ,/CRAY C90/
        VM0     S3      
        VM1     S4      
CRAY16  ELSE            
        VM      S3      
CRAY16  ENDIF           
        V4      S7!V3&VM     * If mask S7 else V3 
        A6      A4      
        A7      0       
L7      =       P.*     
* execute body of serial loop
        S5      V0,A7   
        V0,A7   S6      
        S7      V4,A7   
* Reset running sum if flag is set
        A0      S7      
        JAZ     L6      
        S6      S5      
L6      =       P.*     
        V5,A7   S6      
        A7      A7+1    
        A6      A6-1    
        A0      A6      
        JAN     L7      
        A0      A1      
        ,A0,1   V0           * Vector store V0 
        A7      S1      
        A0      A7      
        ,A0,1   V5           * Vector store V5 
        A6      B70     
        EXIT
        END
* END CLASTINDEX

