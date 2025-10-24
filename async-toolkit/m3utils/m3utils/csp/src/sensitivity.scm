; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (get-process-intf-objects cell-info)
  (let* ((ports (caddddr cell-info))
         (pnms  (map cadr ports)))
    pnms))
    

(define (make-process-intf-set cell-info)
  (let ((set  (make-set (lambda()(make-symbol-hash-table 100)))))
    (map (lambda(sym)(set 'insert! sym))
         (get-process-intf-objects cell-info))
  set))

(define (expr-sensitivity expr func-tbl cell-info)
  
  (define set (make-set (lambda()(make-symbol-hash-table 100))))

  (define intf-syms (make-process-intf-set cell-info))

  (expr-sensitivity-set! expr func-tbl intf-syms set)
  (set 'keys)
  )

(define (expr-sensitivity-set! expr func-tbl intf-syms set)

  ;; what external objects (channels, nodes) is x sensitive to

  (define (visitor x)
    (if (pair? x)
        (let ((kw   (car x))
              (args (cdr x)))
          (case kw
            ((id) (if (intf-syms 'member? (car args))
                      (set 'insert! (car args))))

            ((apply)
             (let* ((fnam (cadar args))
                    (fdef (func-tbl 'retrieve fnam)))
               (if (not (eq? '*hash-table-search-failed* fdef))
                   (func-sensitivity-set! fdef func-tbl intf-syms set)
                   );;fi
               )
             )
            
            );;esac
          );;tel
        x
        );; fi
    );;enifed

  (visit-expr expr identity visitor identity)
  (set 'keys)
  )

(define (stmt-sensitivity-set! stmt func-tbl intf-syms set)

  ;; what external objects (channels, nodes) can change the execution of s

  (define (s-visitor s)
    s
    )

  (define (x-visitor x)
    (expr-sensitivity-set! x func-tbl intf-syms set)
    x
    )

  (visit-stmt stmt s-visitor x-visitor identity)
  (set 'keys)
  )

(define (func-sensitivity-set! func-def func-tbl intf-syms set)

  ;; what external objects can change the execution or result of f

  (stmt-sensitivity-set! (get-function-text func-def)
                         func-tbl
                         intf-syms
                         set)
)

