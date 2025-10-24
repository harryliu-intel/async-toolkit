; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define rdr #f)

(define (stringify-if-not-string x)
  (if (or (char? x) (string? x)) x (stringify x)))

(define (do-dbg . x)
;;  (apply dis  (map stringify-if-not-string x))
  )

(define (remove-do the-inits prog func-tbl struct-tbl cell-info)
  (let ((tg (make-name-generator "remove-do")))

    (define (visitor s)
      (do-dbg "remove-do::visitor s = " (identity s) dnl)
      (let ((kw (get-stmt-type s)))
        (if       (and (member kw '(do nondet-do))
                       ;;                       (nonsimple-guards? s))
                       #t)
            
            ;; get rid of it
            (let* ((gcs   (cdr s))
                   (ndone  (tg 'next 'do-not-done-))
                   
                   (vars  (map (lambda (gc) (tg 'next)) gcs))
                   (grds  (map car gcs))  ;; guards
                   (cmds  (map cadr gcs)) ;; commands

                   (decls
                    (map (lambda(nm)(make-var1-decl nm *default-boolean-type*)) vars))
                   
                   (assigns
                    (map (lambda(v x)(make-assign `(id ,v) x))
                         vars
                         grds))

                   (g-list  (map make-ident vars))
                   
                   (or-expr (make-binop '| g-list)) ; |))

                   (ndone-id     (make-ident ndone))
                   (ndone-decl   (make-var1-decl ndone *default-boolean-type*))
                   (ndone-init   (make-assign ndone-id #t))
                   (ndone-assign (make-assign ndone-id or-expr))

                   (if-grds
                    (cons
                     (list `(not ,(make-ident ndone)) 'skip)
                     (map list g-list cmds)))

                   (the-if (cons 'if if-grds))

                   (the-body
                    `(sequence  ,the-if ,@assigns ,ndone-assign))

                   (the-loop
                    `(while ,ndone-id ,the-body))

                   (res
                    `(sequence ,ndone-decl
                               ,@decls
                               ,@assigns
                               ,ndone-assign
                               ,the-loop
                               ))
                   )

              ;; we could insert a check for two guards for
              ;; deterministic dos.

              (do-dbg "remove-do : " (identity s) dnl)
              (do-dbg "remove-do : gcs   : " (identity gcs) dnl)
              (do-dbg "remove-do : ndone : " (identity ndone) dnl)
              (do-dbg "remove-do : vars  : " (identity vars) dnl)
              (do-dbg "remove-do : grds  : " (identity grds) dnl)
              (do-dbg "remove-do : cmds  : " (identity cmds) dnl dnl)
              (do-dbg "remove-do : ndone-assign : " (identity ndone-assign) dnl)
              (do-dbg "remove-do : the-if       : " (identity the-if) dnl)
              (do-dbg "remove-do : the-body     : " (identity the-body) dnl)

              (dis    "remove-do : the-loop     : " (identity the-loop) dnl)

              (set! rdr res)

;;              (error)
              
              res
              )
            s
            ) ;; fi
        ) ;; tel
      )

    (visit-stmt prog visitor identity identity)
    )
  )

