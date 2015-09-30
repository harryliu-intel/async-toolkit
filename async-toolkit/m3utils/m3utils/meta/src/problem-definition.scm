;; sample problem definition

(load "dual-arbiter-verifications.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define routed-super 
;;  '("chip.alta.scheduler.alloc.S_ALLOC_80_48k_16_16.1001"  
;;     ;; routed
;;
;;    "alloc"                                                
;;    ;; instance name of routed
;;
;;    "freelist.fifo_in.arb.darb"
;;    ;; instance within routed
;;
;;    "lib.metastable.primitive.dual_arbiter.PRIMITIVE_DUAL_ARBITER.1001" 
;;    ;; target type
;;
;;    ))
;;
;;(define the-verification x1a1-verification-0)

(load (string-append "../scripts/" problem-definition-file))

(define the-early-node (cadr (assoc 'early-transition the-verification)))
(define the-early-tcnt (caddr (assoc 'early-transition the-verification)))

(define the-late-node (cadr (assoc 'late-transition the-verification)))
(define the-late-tcnt (caddr (assoc 'late-transition the-verification)))

(define the-current-block-list
  (cond ((assoc 'current-cycle-block-list the-verification) => cdr)
        (else '())))


(define the-valid-scenario 
  (cdr (assoc 'previous-cycle-scenario the-verification)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define full-instance-name (Name.Append (Name.ParseText (cadr routed-super))
                                        (Name.ParseText (caddr routed-super))))

(define full-instance-text (Name.Format full-instance-name))

(define full-instance-type (get-instance-type full-instance-text))

;;(define full-instance-rules (modula-type-op 'Dsim.DsimBody 'get-field (modula-type-op 'Dsim.Define 'get-field (get-dsim-type full-instance-type) 'dsimBody) 'rules))

(define (get-target-components)
  (let ((early 
         (get-nth-transition-expr 
          (string-append full-instance-text "." the-early-node)
          the-early-tcnt))
        (late
         (get-nth-transition-expr 
          (string-append full-instance-text "." the-late-node)
          the-late-tcnt)))
    (cons early late)))

(define (*get-target-expression*)
  (let ((early 
         (get-nth-transition-expr 
          (string-append full-instance-text "." the-early-node)
          the-early-tcnt))
        (late
         (get-nth-transition-expr 
          (string-append full-instance-text "." the-late-node)
          the-late-tcnt)))
    (simplify-arith
     `(- ,(get-time late) ,(get-time early) ,(get-slew early)))))

(define (close-environment)
  (add-channel-return-path (Name.Format full-instance-name) "L[0]" prs)

  (add-channel-return-path (Name.Format full-instance-name) "L[1]" prs)
  
  (add-arbitrary-delay-bitbucket 
   (Name.Format full-instance-name) '("A.0" "A.1" "A.2") "A.e" prs tm)
)

(define (make-scenarios) '())

  ;; scenarios based on the previous cycles results:

(define a-neutral-scenario
  `((await . ,(make-neutral-cond "A" 3))
    (block)))

(define block-2nd-arbiters 
  '( ("_y[0].0 . #f) ("_y[0].1 . #f)("_y[1].0 . #f) ("_y[1].1 . #f) )
)

(define arbiters-ready-scenario
  `((await . (and ,(make-valid-cond "x" 2)
                  ("L[0].0" . #t)
                  ("L[1].0" . #t)))
    ;; dont let circuit proceed past (either second-level arbiter)
    (block . ,(append the-current-block-list block-2nd-arbiters))
    )
)

(set! make-scenarios 
      (lambda()
        (make-scenario-sequence full-instance-name 
                                (list the-valid-scenario
                                      a-neutral-scenario
                                      arbiters-ready-scenario
                                      )
                                )))


