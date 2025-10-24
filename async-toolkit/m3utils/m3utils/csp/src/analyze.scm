; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;; early-stage analysis to clean up the program
;; has to be run initially and after a few transformations
;; (such as after function inlining)

(define *last-anal* #f)

(define (analyze-program prog cell-info initvars)
  (let* ((ports      (get-ports    cell-info))
         (portids    (get-port-ids cell-info))
         (textids    (find-referenced-vars prog))
         (globalids  (set-intersection textids initvars))
         (dummies    (get-all-dummies prog))
         (unused-globalids
                     (set-diff initvars globalids))
         (undeclared (set-diff
                      (find-undeclared-vars prog portids) globalids))
         (missing    (set-diff undeclared dummies))
         (declnames  (find-declaration-vars prog))
         (multiples  (multi declnames))
         )

    (set! *last-anal* prog)

    ;; we should consider *the-inits* here.  We don't need to declare
    ;; anything that *the-inits* declares.
    (dis "portids    : " portids dnl)
    (dis "textids    : " textids dnl)
    (dis "globalids  : " globalids dnl)
    (dis "undeclared : " undeclared dnl) (set! *undeclared* undeclared)
    (dis "missing    : " missing dnl)
    (dis "decls      : " declnames dnl)
    (dis "multiples  : " multiples dnl)

    (cond ((not (null? multiples))
           (dis dnl "uniquifying..." dnl dnl)
           (analyze-program (uniquify-stmt prog)
                            cell-info
                            initvars))

          ((not (null? missing))
           (dis dnl "un-undeclaring missing..." dnl dnl)
           (analyze-program (predeclare prog missing)
                            cell-info
                            initvars)
           )

          (else (set! *analyze-result* prog)
                (set! *unused-globals* unused-globalids)
                *analyze-result*)
          )
    )
  )

(define (find-referenced-vars stmt)
  (find-stmt-ids stmt))

(define (find-undeclared-vars stmt portids)
  (let* ((used (find-referenced-vars stmt))
         (declared (find-declared-vars stmt)))
    (set-diff (set-diff used declared) portids)))
               
(define (find-declared-vars lisp) ;; set of declared vars
  (uniq eq?
        (append (find-loop-indices lisp)
                (map cadr (map cadadr (find-var1-stmts lisp))))))

(define (find-declaration-vars lisp) ;; multiset of declarations
  (map cadr (map cadadr (find-var1-stmts lisp))))

(define (count-declarations of stmt)
  (count-in of (find-declaration-vars stmt)))

(define (get-all-dummies prog)
  (uniq eq? (append (get-waiting-if-dummies prog)
                    (get-loop-dummies prog)
                    (get-loopex-dummies prog))))

(define (get-global-stmts prog initvars)
  (let* ((textids    (find-referenced-vars prog))
         (globalids  (set-intersection textids initvars))
         (var1s      (filter (compose (yrruc member globalids) get-var1-id)
                             (find-stmts 'var1 *the-inits*)))
         (assigns    (filter
                      (compose (yrruc member globalids)
                               (compose cadr
                                        (compose
                                         array-access-base
                                         get-assign-lhs)))
                      (find-stmts 'assign *the-inits*)))
         )

    (append var1s assigns)
    )
  )
