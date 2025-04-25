(define (blocking-dbg . x)
  ;; (apply dis x)
  )

(define (stmt-may-block? stmt)

  (define result #f)

  (define (yes) (set! result #t) 'cut)
  
  (define (s-visitor s)
    ;; statements that on their own introduce blocking
;;7    (blocking-dbg "s-visitor " (stringify s) dnl)
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
;;    (blocking-dbg "x-visitor " (stringify x) dnl)
    (if result
        'cut
        (case (get-expr-type x)
          ((apply)                    (yes)) ;; shouldnt happen in compiled

          ((call-intrinsic)
           (if (eq? (cadr x) 'wait)   (yes) x))

          ((recv-expression peek)     (yes))

          (else x))))

;;  (blocking-dbg "smb? : " (stringify stmt) dnl)

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
      (dis "mark " side dnl)

      (define (lab . x) `((label ,(tg 'next) ,@x)))
      
      (let* (
             (fork-lab (apply lab '(fork)))

             (prefix   (case side
                         ((before)        (lab))
                         ((both)          fork-lab)
                         );;esac
                       );;xiferp

             (suffix   (case side
                         ((both)      (apply lab `(join ,(cadar fork-lab))))
                         ((before)          '())
                         );;esac
                       );;xiffus
             
             (res (append '(sequence) prefix (list s) suffix))
             )
        res
        )
      )
    
    (case (get-stmt-type s)
      ((recv send) (mark 'before))

      ((waiting-if) (mark 'before))

      ((if nondet-if do nondet-do) (error))

      ((parallel) (mark 'both))

      ((parallel-loop)

       
       ;;
       ;; We can't support parallel loops without way more work!
       ;;
       ;; The main issue is that we would have to have a call stack
       ;; and this would affect function calls and many other things.
       ;;
       ;; It would also slow down the resulting code.
       ;;
       
       (error "this back-end does not support parallel loops")

       (mark 'both
             (get-loop-dummy s)
             (get-loop-range s)))
      
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

      (else s)
      );;esac

    );;enifed
    
    ;; label the entry and exit point also
    `(sequence (label START) ,(visit-stmt prog s-visitor identity identity)
               (label END))
  )

(define upl-result #f)

(define (unroll-parallel-loops loop-stmt syms vals tg func-tbl struct-tbl cell-info)

  ;; parallel-loop has to be unrolled

  (define tg (make-name-generator "unroll-parallel-loops"))

  (let* ((dummy         (get-loop-dummy loop-stmt))
         (range         (get-loop-range loop-stmt))
         (lo            (cadr  range))
         (hi            (caddr range))
         (literal-range (and (bigint? lo) (bigint? hi)))
         (stmt          (get-loop-stmt  loop-stmt))
         )

    (dis "unroll-parallel-loops : dummy         : " dummy dnl)
    (dis "unroll-parallel-loops : loop-stmt     : " loop-stmt dnl)
    (dis "unroll-parallel-loops : literal-range : " literal-range dnl)

    (define (one-iteration i)
      (define (replace-dummy expr)
        (if (and (ident? expr) (eq? dummy (cadr expr))) i expr))
      
      (visit-stmt stmt identity replace-dummy identity)
      )
    
    (if literal-range
        (if (xnum-< hi lo)
            'skip
            (let loop ((sofar '())
                       (i      lo))
              (if (xnum-= i hi)
                  (let* ((unrolled (cons
                                    'parallel
                                    (reverse (cons
                                              (one-iteration i)
                                              sofar))))

                         (result (uniquify-stmt unrolled))
                         )
                    (dis "unroll-parallel-loops : " (stringify result) dnl)
                    (set! upl-result result)
                    result
                    )
                         
                  (loop (cons (one-iteration i) sofar) (xnum-+ i *big1*)))
              );;tel
            );;fi
        loop-stmt
        );;fi
    );;*tel
  )
  
(define (unblock-loops stmt syms vals tg func-tbl struct-tbl cell-info)

  ;; sequential-loop with a blocking statement have to be desugared
  ;; to regular while loops
  
  (define tg (make-name-generator "unblock-loops"))

    (define (s-visitor s)
      (blocking-dbg "s-visitor s : " s dnl)
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
            
            `(sequence ,newddecl ,newdinit ,newbdecl ,bupdate ,the-loop))
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

(define *scan-stmts* '(seq parallel sequence local-if parallel-loop))

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

(define (sequence? x) (and (pair? x) (member (car x) '(seq sequence))))

(define (parallel? x) (and (pair? x) (member (car x) '(parallel pll))))

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
  (blocking-dbg "combine-seqs a : " a " ")
  (blocking-dbg "combine-seqs b : " b dnl)

  (let ((res (cons b a)))
    (blocking-dbg "combine-seqs res : " res dnl)

    (cond ((local-if? (car a))
           (cons (stuff-open-lif-clauses (car a) b) (cdr a)))
          
          (else
           res))
    )
  )

(define (combine-block-sets as bs)
  (blocking-dbg "combine-block-sets as : " as dnl)
  (blocking-dbg "combine-block-sets bs : " bs dnl)
  
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
    (blocking-dbg "add-result : x : " x dnl)

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

           (blocking-dbg "starting from label : " (car p) dnl)
           
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
             

             (blocking-dbg "recurse -----> " dnl)
             (blocking-dbg "sofar   = " sofar dnl)
             (blocking-dbg "p       = " p dnl)
             (blocking-dbg "open      " open      dnl)
             (blocking-dbg "openbeg   " openbeg   dnl)
             (blocking-dbg "openend   " openend   dnl)
             (blocking-dbg "closed    " closed    dnl)
             
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

  (blocking-dbg "after recursion, first-result : " first-result dnl)
  (blocking-dbg "after recursion, result       : " result dnl)

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
    
    (blocking-dbg "scan-local-if guards  : " guards dnl)
    (blocking-dbg "scan-local-if stmts   : " stmts dnl)
    (blocking-dbg "scan-local-if lif-cls : " lif-cls dnl)
    
    (blocking-dbg "scan-local-if returns : " res dnl)
    res
    )
  )

;; idea... blocking a statement results in a set of blocks.
;; any blocks that aren't closed should be closed with the coda within
;; the enclosing statement


(define (label->goto x) `(goto ,(cadr x) ,@(cddr x)))

(define (scan-parallel-loop ploop)
  ;; all the work for handling this has already been done by
  ;; the label/goto insertion mechanism
  (scan-stmt (cadddr ploop))
  )

(define scan-pll           scan-parallel)  ;; shorthand
(define scan-seq           scan-sequence)  ;; shorthand
        

(define (sequence-length seq) (length (cdr seq)))

(define (empty-block? blk)

  ;; an empty block is either
  ;; 1. skip
  ;; 2. label X ; goto Y
  
  (or (eq? blk 'skip)
      (and (sequence? blk)
           (= 2 (sequence-length blk))
;;           (or
            (simple-label? (first (cdr blk)))
;;            (fork-label? (first (cdr blk)))
;;            )
           (simple-goto? (last blk)))))

(define (simple-label? x) (and (label? x) (null?   (cddr x))))

(define (fork-label? x) (and (label? x) (equal?  (caddr x) 'fork)))
                      
(define (simple-goto? x) (and (goto? x) (null? (cddr x))))
                      
(define (find-empty-blocks blk-lst) (filter empty-block? blk-lst))

(define label-label cdr)
(define goto-label cdr)

(define (find-empty-label-remap blk-lst)
  ;; find the remapping map to get rid of empty blocks
  (map (lambda(blk)
         (if (sequence? blk)
             (cons (label-label (cadr blk))
                   (goto-label  (last blk)))
             #f
             )
         )
       (filter identity (find-empty-blocks blk-lst))
       )
  )

(define (remap-label blk from to)
  ;; remap a single label from from to to
  (define (visitor s)
    (case (get-stmt-kw s)
      ((goto)    (if (equal? from (goto-label  s)) `(goto ,@to) s))
      ((label)   (if (equal? from (label-label s)) `(label ,@to) s))
      (else s))
    )
  (visit-stmt blk visitor identity identity)
  )

(define (remap-labels remap blk)
  ;; remap all the labels according to the remapping map remap;
  ;; remap is a list of conses (from . to)
  (if (null? remap)
      blk
      (remap-labels (cdr remap)
                    (remap-label blk (cdar remap) (caar remap))))
  )

(define (remove-empty-blocks blk-lst)

  ;; remove all the empty blocks from the blk-lst,
  ;; and patching the labels so control flow remains
  
  (let ((remap     (find-empty-label-remap blk-lst))
        (short-lst (filter (filter-not empty-block?) blk-lst))
        )
    (map (lambda(blk)(remap-labels remap blk)) short-lst)))

                    
