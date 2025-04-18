


(define (stmt-may-block? stmt)

  (define result #f)

  (define (yes) (set! result #t) 'cut)
  
  (define (s-visitor s)
    ;; statements that on their own introduce blocking
;;7    (dis "s-visitor " (stringify s) dnl)
    (if result
        'cut
        (case (get-stmt-type s)
          ((recv send)               (yes))
          ((if nondet-if waiting-if) (yes))
          (else                          s)
          )
        )
    )

  (define (x-visitor x)
    ;; expressions that introduce blocking
;;    (dis "x-visitor " (stringify x) dnl)
    (if result
        'cut
        (case (get-expr-type x)
          ((apply)                    (yes)) ;; shouldnt happen in compiled

          ((call-intrinsic)
           (if (eq? (cadr x) 'wait)   (yes) x))

          ((recv-expression peek)     (yes))

          (else x))))

;;  (dis "smb? : " (stringify stmt) dnl)

  (visit-stmt stmt s-visitor x-visitor identity)

  result
  )

(define (sequentialize-nonblocking-parallels stmt)

  (define (s-visitor s)
    (if (not (stmt-may-block? s))
        (case (get-stmt-type s)
          ((parallel)
           (dis "sequentialize-nonblocking-parallels : sequentializing : "
                s dnl)
           (cons 'sequence (cdr s)) ;; convert parallel to sequence
           )
          
          ((parallel-loop)
           (dis "sequentialize-nonblocking-parallels : sequentializing : "
                s dnl)
           (cons 'sequential-loop (cdr s)) ;; convert parallel to sequence
           )

          (else s))
        s);;fi
    )
  
  (visit-stmt stmt s-visitor identity identity)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insert-block-labels prog)
  (define tg (make-name-generator "L"))
  
  (define (s-visitor s)

    (define (mark side)
      (let* ((suffix (case side
                       ((both after)
                        `((label ,(tg 'next))))
                       ((before -before)
                        '())))

             (prefix (case side
                       ((-before) ;; - labels are optional
                        `((label ,(symbol-append '- (tg 'next)))))
                       ((before) ;; - labels are optional
                        `((label ,(tg 'next))))
                       ((both)
                        `((label ,(tg 'next))))
                       ((after)
                        '())))
             
             (res (append '(sequence) prefix (list s) suffix))
             )
        res
        )
      )

    
    (case (get-stmt-type s)
      ((recv send) (mark 'before))

      ((waiting-if) (mark 'before))

      ((if nondet-if do nondet-do) (error))

      ((parallel parallel-loop) (mark 'both))

      ((while)
       (if (stmt-may-block? s) (mark '-before) s))

      ((eval)
       (if (stmt-may-block? s) (mark 'before) s))
      
      ((assign)
       (let ((rhs (get-assign-rhs s)))
         (if (or (peek? rhs) (recv-expression? rhs))
             (mark 'before)
             s))
       )

      (else s)))


  ;; label the entry point also
  `(sequence (label START) ,(visit-stmt prog s-visitor identity identity))
  )

(define (unblock-loops stmt syms vals tg func-tbl struct-tbl cell-info)

  ;; sequential-loop with a blocking statement have to be desugared
  ;; to regular while loops
  
  (define tg (make-name-generator "unblock-loops"))

    (define (s-visitor s)
      (dis "s-visitor s : " s dnl)
      (if (and (eq? 'sequential-loop (get-stmt-type s))
               (stmt-may-block? s))
          
          (let* ((loopr    (get-loop-range s))
                 (lmin     (cadr loopr))
                 (lmax     (caddr loopr))
                 
                 (nam      (get-loop-dummy s))
                 ;; reuse the dummy
                 
                 (newtype  (derive-type lmax syms func-tbl struct-tbl cell-info))
                 ;; we can only count upward... so lmax is a safe type
                 
                 (newddecl (make-var1-decl nam newtype))
                 
                 (newdinit  (make-assign `(id ,nam) lmin))
                 
                 (incstmt   (make-assign `(id ,nam) `(+ (id, nam) ,*big1*)))
                 
                 (newbnam  (tg 'next))
                 
                 (newbdecl (make-var1-decl newbnam *default-boolean-type*))
                 
                 (bupdate  (make-assign `(id ,newbnam) `(<= (id ,newbnam ,lmax))))
                 
                 (the-loop `(while (id ,newbnam) (sequence ,(get-loop-stmt s)
                                                           ,incstmt
                                                           ,bupdate)))
                 )
            
            (define-var! syms nam newtype)
            (define-var! syms newbnam *default-boolean-type*)
            
            `(sequence ,newddecl ,newdinit ,newbdecl ,bupdate , the-loop))
          s
          )
      )

    (visit-stmt stmt s-visitor identity identity)
    )

(define (label? x) (and (pair? x) (eq? 'label (car x))))
(define (goto? x) (and (pair? x) (eq? 'goto (car x))))
(define (goto? x) (and (pair? x) (eq? 'goto (car x))))
(define (sequence? x) (and (pair? x) (eq? 'sequence (car x))))
(define (parallel? x) (and (pair? x) (eq? 'parallel (car x))))

(define (add-prefix pfx) (lambda(lst)(cons pfx lst)))

(define (scan-sequence sequence)

  (let loop ((p               (cdr sequence))
             ;; remaining statements in sequence
             ;; -- we've already skipped the "sequence"
             
             (cur             '())
             ;; text of current block  -- first should be a label

             (result          '())
             ;; list of blocks generated

             (proc-label        #t)
             )

    (dis "loop p      : " p dnl)
    (dis "loop cur    : " cur dnl)
    (dis "loop result : " result dnl)
    (dis "==============" dnl)

    (define (done)
      ;; take the result and format it correctly
      (reverse
       (map (add-prefix 'sequence)
            (cons (reverse cur) result))))

    (cond ((null?  p)       (done))
           
          ((label? (car p))
           (let ((revseq (cons (label->goto (car p)) cur)))
             ;; revseq is the subsequence, it should start in a label
             ;; and end in a goto or have a "loose end"
             ;; (but it's reversed)

             (if proc-label
                 (loop (cdr p)
                       ;; next statement
                       
                       (list (car p))
                       ;; lead with label
                       
                       (if (not (null? cur))
                           (cons (reverse revseq) result)
                           ;; and cons on the result, unless it's empty
                           )

                       #t
                       );;pool

                 (loop '()
                       '()
                       (if (not (null? cur))
                           (cons (reverse revseq) result)
                           ;; and cons on the result, unless it's empty
                           )
                       #f
                       )

                 );;fi
             );;tel
           );;?lebal

          
          (else ;; just add the statement to the current block
           (let* ((next         (car p))
                  (next-blocks  (scan-stmt (car p))))
             
             (dis "next        : " next dnl)
             (dis "next-blocks : " next-blocks dnl)

             ;; to the extent that the blocks aren't closed, we
             ;; map the rest of the parsing across all the blocks
             ;; those that *are* closed, we just add to result
             ;;
             ;; but we need to make sure that only one of the recursions
             ;; survives the next label.  Hmm...!
             (apply append
                    (map (lambda(blk)
                           (if (loose-end? blk)
                               (loop (cdr p) (cons blk cur) result #f)
                               (loop (cdr p) '() (cons blk result) #t)
                               );; fi
                           )
                         next-blocks))
                    

;;             (error)
             )
           )

          );; dnoc

    );;tel
  )

;; idea... blocking a statement results in a set of blocks.
;; any blocks that aren't closed should be closed with the coda within
;; the enclosing statement


(define (label->goto x) `(goto ,(cadr x)))
         

(define ts '(sequence (label L0)
                      (assign X Y)
                      (assign Y Z)
                      (label L1)
                      (recv L X)
                      (assign Y X)
                      (label L2)
                      (send R Y)
                      (assign Y Z)
                      )
  )

(define tp '(parallel
                      (assign X Y)
                      (assign Y Z)
                      (recv L X)
                      (assign Y X)
                      (send R Y)
                      (assign Y Z)
                      )
  )

(define (loose-end? seq) ;; a block has a loose end if it doesn't end in goto
  (not (and (sequence? seq) (goto? (last seq)))))

(define (labelled-sequence? seq) ;; a sequence that starts with a label
  (and (sequence? seq) (not (null? (cdr seq))) (label? (cadr seq))))

(define (dis1 . x)
  (apply dis x)
  (dis dnl)
  'ok
  )

(define *scan-stmts* '(parallel sequence))

(define (scan-stmt stmt)
  (let* ((kw      (get-stmt-type stmt))
         (callsym (symbol-append 'scan- kw))
         )
    (if (member kw *scan-stmts*)
        ((eval callsym) stmt)
        (list stmt)
        )
    )
  )

(define (scan-parallel stmt)
  (cdr stmt))

        
        
