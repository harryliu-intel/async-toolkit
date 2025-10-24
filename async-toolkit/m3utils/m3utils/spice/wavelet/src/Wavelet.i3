(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Wavelet;
IMPORT SparseLR;

(*

C       1-D WAVELET TRANSFORM

        SUBROUTINE wavelet (x,n,direction)
        INTEGER n, direction            ! n is length of x, power of two
        REAL x(n)                       ! x() is input and output
        INTEGER m, i
        REAL y(2048)                    ! y() is permutation buffer

C       FORWARD
        IF (direction .GT. 0) THEN
C               POWER OF 2 PASSES, EACH HALF AS LONG
                m = n
C               BUTTERFLY
10              continue
                DO i=1,m,2
                        y(i) = (x(i) + x(i+1)) / 2
                        y(i+1) = (x(i) - x(i+1)) / 2
                ENDDO
C               PERMUTATION
                m = m / 2
                DO i=1,m
                        x(i) = y(i+i-1)
                        x(m+i) = y(i+i)
                ENDDO
                if (m .GT. 1) GOTO 10
C       INVERSE
        ELSE
C               POWER OF 2 PASSES, EACH TWICE AS LONG
                m = 1
20              continue
C               PERMUTATION
                DO i=1,m
                        y(i+i-1) = x(i)
                        y(i+i) = x(m+i)
                ENDDO
                m = m + m
C               BUTTERFLY
                DO i=1,m,2
                        x(i) = (y(i) + y(i+1))
                        x(i+1) = (y(i) - y(i+1))
                ENDDO
                if (m .LT. n) GOTO 20
        ENDIF
        END

*)

PROCEDURE Wavelet(VAR x, buff : ARRAY OF LONGREAL;
                  forward     : BOOLEAN);

PROCEDURE ToSparse(READONLY x : ARRAY OF LONGREAL;
                   VAR s : ARRAY OF SparseLR.T);

PROCEDURE FromSparse(READONLY s : ARRAY OF SparseLR.T;
                     VAR x : ARRAY OF LONGREAL); 
    
END Wavelet.
