


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
  ;;
  ;; insert block labels around blocking statements as required
  ;;
  ;; ALSO converts while to a local-if and gotos when the while may block
  ;;
  
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
       (if (stmt-may-block? s)
           (let* ((lab (tg 'next))
                  (G   (cadr s))
                  (S   (caddr s))
                  (true-clause `(sequence ,S (goto ,lab)))
                  (false-clause 'skip)
                  
                  (res
                   `(sequence
                      (label ,lab)
                      ,(cond ((eq? G #t) true-clause)
                             ((eq? G #f) false-clause)
                             (else 
                              `(local-if (,G   ,true-clause )
                                         (else ,false-clause))))))
                  )
             (dis "while may block : lab = " lab dnl)
             (dis "while may block : " res dnl)
             res
             )
           s))

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
  (and (not (goto? seq)) (not (and (sequence? seq) (goto? (last seq))))))

(define (loose-beg? seq) ;; a block has a loose beginning if it doesn't start
                         ;; in a label
  (not (and (sequence? seq) (label? (cadr seq)))))

(define (labelled-sequence? seq) ;; a sequence that starts with a label
  (and (sequence? seq) (not (null? (cdr seq))) (label? (cadr seq))))

(define (dis1 . x)
  (apply dis x)
  (dis dnl)
  'ok
  )

(define *scan-stmts* '(parallel sequence local-if))

(define (scan-stmt stmt)
  ;; return a list of statements (may be a singleton)
  ;; where the first statement is the entry point
  ;; and the remainder are blocks for compilation,
  ;; reachable via goto
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
  (apply append (map scan-stmt (cdr stmt))))

        
        
(define (label? x) (and (pair? x) (eq? 'label (car x))))
(define (goto? x) (and (pair? x) (eq? 'goto (car x))))
(define (skip? x) (eq? 'skip x))
(define (sequence? x) (and (pair? x) (eq? 'sequence (car x))))
(define (parallel? x) (and (pair? x) (eq? 'parallel (car x))))
(define (local-if? x) (and (pair? x) (eq? 'local-if (car x))))

(define (add-prefix pfx) (lambda(lst)(cons pfx lst)))

(define (extend-open-end stmt with)
  (if (loose-end? stmt)
      (list 'sequence stmt with)
      stmt))

(define (stuff-open-lif-clauses lif coda)
  (let* ((guards   (map car (cdr lif)))
         (stmts    (map cadr (cdr lif)))
         (mstmts   (map (lambda(cmd)(extend-open-end cmd coda))
                        stmts)))
    (dis "stuff-open-lif-clauses lif  = " lif dnl)
    (dis "stuff-open-lif-clauses coda = " coda dnl)

    (cons 'local-if (map list guards mstmts))
    
    )
  )
 

  
   

(define (combine-seqs a b)
  (dis "combine-seqs a : " a " ")
  (dis "combine-seqs b : " b dnl)

  (let ((res (cons b a)))
    (dis "combine-seqs res : " res dnl)

    (cond ((local-if? (car a))
           (cons (stuff-open-lif-clauses (car a) b) (cdr a)))
          
          (else
           res))

    )

  )

(define (combine-block-sets as bs)
  (dis "combine-block-sets as : " as dnl)
  (dis "combine-block-sets bs : " bs dnl)
  
  (apply append
         (map (lambda(a)(map
                         (lambda(b)(combine-seqs a b))
                         bs)
                     )
              as)
         )
  )

(define (scan-sequence sequence)

  (define first-result #f)
  
  (define result '()) ;; hold finished blocks here

  (define (add-result! x)
    (dis "add-result : x : " x dnl)

    (if first-result
        (set! result (cons (reverse x) result))
        (set! first-result (reverse x))
        )
    
    )
  
  (define (recurse sofar  ;; current block so far, list of list of statements,
                          ;; each in reverse order
                   
                   p      ;; remaining statements
                   )

    (cond ((null? p) (map add-result! sofar))

          ((label? (car p))
           (map
            (lambda(seq) (add-result! (cons (label->goto (car p)) seq)))
            sofar
            )
           ;; label : close out all current blocks

           (dis "starting from label : " (car p) dnl)
           
           (recurse (list (list (car p) 'sequence))
                    (cdr p))
           ;; start fresh
           )
          
          (else

           ;; a statement that is not a label
           
           (let* ((carblks    (scan-stmt (car p)))

                  ;; note that what comes out here is in FORWARD order:
                  
                  (open       (filter
                               (filter-and loose-beg? loose-end?) carblks))

                  (openbeg    (filter
                               (filter-and loose-beg?
                                           (filter-not loose-end?)) carblks))
                  (openend    (filter
                               (filter-and (filter-not loose-beg?)
                                           loose-end?) carblks))

                  (closed     (filter
                               (filter-and (filter-not loose-beg?)
                                           (filter-not loose-end?)) carblks))

                  )

             ;; 4 cases:
             ;; closed blocks : add to result
             ;; openbeg : append input sofar, then add to result
             ;; openend : add to output sofar
             ;; open    : append input sofar, then add to output sofar
             

             (dis "recurse -----> " dnl)
             (dis "sofar   = " sofar dnl)
             (dis "p       = " p dnl)
             (dis "open      " open      dnl)
             (dis "openbeg   " openbeg   dnl)
             (dis "openend   " openend   dnl)
             (dis "closed    " closed    dnl)
             
             (recurse (append 
                              (combine-block-sets sofar (reverse open))
                              (map reverse openend))
                      (cdr p))

             (map add-result! (map reverse closed)) ;; remember the closed ends

             (map add-result! (combine-block-sets sofar openbeg))
             
             )
            );;esle
           
          );;dnoc     
    );;enifed

  (recurse '((sequence)) (cdr sequence))

  (dis "after recursion, first-result : " first-result dnl)
  (dis "after recursion, result       : " result dnl)

  (cons first-result (reverse result)) ;; the reverse here is just for
                                       ;; readability
  
  );;enifed

(define (scan-local-if lif)
  (let* ((guards   (map car (cdr lif)))
         (stmts    (map cadr (cdr lif)))
         (stmt-bls (map scan-stmt stmts))

         (lif-cls  (cons 'local-if
                         (map list guards (map car stmt-bls))))
         (res
          (cons lif-cls  (apply append (map cdr stmt-bls))))
         )

         (dis "scan-local-if returns : " res dnl)
         res
    )
  )

;; idea... blocking a statement results in a set of blocks.
;; any blocks that aren't closed should be closed with the coda within
;; the enclosing statement


(define (label->goto x) `(goto ,(cadr x)))
         
