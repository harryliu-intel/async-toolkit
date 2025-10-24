; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(require-modules "display")

;; parameters

(define *nstages* 10)
(define *dist-R* 36)
(define *total-C* 0.195e-12)
(define *dist-C-ratio* 1.0)
(define *dist-L* 0.5e-9) ;; what we believe from extract
;;(define *dist-L* 2e-9) ;; worst-case if extract is wrong

;; compute other params

(define (do-put-dist-model n wr)
  (let* ((dist-C (* *total-C* *dist-C-ratio*))
         (lump-C (- *total-C* dist-C))
         (partial-C (/ dist-C n))
         (partial-R (/ *dist-R* n))
         (partial-L (/ *dist-L* n)))

    (let loop ((i 0))
      (if (= i n)
          'ok
          (let ((k (* i 2)))
            (dis "Rd"k" x"k" x"(+ k 1)" "partial-R dnl wr)
            (dis "Cd"k" x"(+ k 1)" 0 "partial-C dnl wr)
            (dis "Ld"k" x"(+ k 1)" x"(+ k 2)" "partial-L dnl wr)
            (loop (+ i 1))
            )))))

(define (do-put-model n wr)
  (dis ".subckt dut_model x0 x"(* n 2) dnl dnl wr)
  (do-put-dist-model n wr)
  (dis dnl
;;       "Cl999 x"(* n 2)" 0 "(* *total-C* *dist-C-ratio*) dnl
       dnl
       ".ends" dnl
       wr )
  'ok
 )
  

        

(define wr (FileWr.Open "dut_model.inc"))

(do-put-model *nstages* wr)
(Wr.Close wr)


