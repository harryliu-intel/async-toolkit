(require-modules "basic-defs" "display" "hashtable" "m3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helpers for error messages and the like
;;

(define (symbol-string-append . x)
   (apply string-append
          (map (lambda (s) 
                 (cond ((symbol? s) (symbol->string s))
                       ((string? s) s)
                       (else (error (string-append 
                                     "not a string or symbol : " s)))))
               x)
          )
   )
  

(define (symbol-append . x)
  (string->symbol (apply symbol-string-append x))
  )

(define (error-append . x)
  (let ((txt (apply symbol-string-append x)))
    (if (> (Text.Length txt) 1024)
        (string-append (Text.Sub txt 0 1024) "...")
        txt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; examples for test & debug
;;

(define some-field '(field INITIAL_W0_OFFSET 56 8))

(define some-reg
  '(reg parser_port_cfg_r
        (field INITIAL_W0_OFFSET 56 8)
        (field INITIAL_W1_OFFSET 48 8)
        (field INITIAL_W2_OFFSET 40 8)
        (field INITIAL_PTR 32 8)
        (field INITIAL_STATE 16 16)
        (field INITIAL_OP_MASK 4 12)
        (field INITIAL_OP_ROT 0 4)
        )
  )

(define some-array `(array 16 ,some-reg))

(define some-2d-array `(array 256 ,some-array))

(define some-child ' (child EM_HASH_LOOKUP
                          (array 32768 
                            (reg em_hash_lookup_r
                              (field PTR 64 20)
                              (field RSVD1_ 52 12)
                              (field SELECT_4 48 4)
                              (field SELECT_3 44 4)
                              (field SELECT_2 40 4)
                              (field SELECT_1 36 4)
                              (field SELECT_0 32 4)
                              (field MASK 0 32)
                            )
                          )
                          )
  )

(define some-container
  '(container addrmap mby_ppe_cgrp_em_map
              (child A
                     (reg xxx
                          (field STUFF 0 64)
                          )
                     )
              (child B
                     (array 32 
                            (container regfile b_rf
                                       (child BB
                                              (array 8 
                                                     (reg bb_r
                                                          (field DATA 0 64)
                                                          )
                                                     )
                                              )
                                       )
                            )
                     )
              )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; consistency is not the hobgoblin of little minds when it is not foolish
;; we tag every object
;;

(define (get-tag obj) (car obj))

(define *tag-assert-failed* '())

;; make an asserter
(define (make-asserter tag)
  (lambda (x)
    ;; assert that x has the tag tag
    (if (not (eq? (get-tag x) tag))
        (begin
          (set! *tag-assert-failed* x)
          (error (error-append "not a " tag " : " (stringify x))))
        #t)))

(define assert-child     (make-asserter 'child    ))
(define assert-container (make-asserter 'container))
(define assert-array     (make-asserter 'array    ))
(define assert-field     (make-asserter 'field    ))
(define assert-reg       (make-asserter 'reg      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HO functions
;;

(define (get-unique proc lst tag)
  ;; get the unique member of the list lst s.t.
  ;; (eq? (proc member) tag)
  (let loop ((p lst))
    (cond ((null? p)
           (error
            (error-append "No member matching : " tag " : " (stringify lst))))
          ((eq? tag (proc (car p))) (car p))
          (else (loop (cdr p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; a (generic) container type (regfile or addrmap)
;;

(define (container-get-type c)
  (assert-container c)
  (cadr c))

(define (container-get-children c)
  (assert-container c)
  (cdddr c))

(define (container-get-children-names c)
  ;;(assert-container c)
  (map get-child-name (container-get-children c)))

(define (container-sum what c)
  ;;(assert-container c)
  (accumulate + 0 (map (lambda(ch)(child-sum what ch))
                       (container-get-children c))))

(define (container-get-child c cn)
  (let ((children (container-get-children c)))
    (child-get-contents (get-unique child-get-name children cn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; children of a container type
;;

(define (child-get-name c)
  (assert-child c)
  (cadr c))

(define (child-get-contents c)
  (assert-child c)
  (caddr c))

(define (child-get-children c) (list (child-get-contents c)))

(define (child-sum what c)
  ;;(assert-child c)
  (gen-sum what (child-get-contents c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; fields (children of a reg)
;;

(define (field-get-name f)
  (assert-field f)
  (cadr f))

(define (field-get-nfields f)
  (assert-field f)
  1)

(define (field-get-nbits f)
  (assert-field f)
  (cadddr f))

(define (field-get what f)
  ;;(assert-field f)
  ((eval (symbol-append 'field-get- what)) f))

(define field-sum field-get)

(define (field-get-children f) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; reg handling (regs can exist in containers or arrays and contain only fields)
;;

(define (reg-get-type r)
  (assert-reg r)
  (cadr r))

(define (reg-get-fields r)
  (assert-reg r)
  (cddr r))

(define reg-get-children reg-get-fields)

(define (reg-get-fields-before r fn)
  ;; get fields before a named field, unless fn is null, in which case get all
  (define (helper rest)
    (cond ((null? rest) (error
                         (error-append "No such field : "
                                        fn
                                        " : in reg : "
                                        (stringify r))))
          ((eq? fn (field-get-name (car rest))) '())
          (else (cons (car rest) (helper (cdr rest))))))

  (let ((fields (reg-get-fields r)))
    (if (null? fn) fields (helper fields))))

(define (reg-sum what r)
  ;;(assert-reg r)
  (accumulate + 0 (map
                   (lambda(f)(field-get what f))
                   (reg-get-fields r)))
  )

(define (reg-sum-before what r fn)
  (accumulate + 0 (map
                   (lambda(f)(field-get what f))
                   (reg-get-fields-before r fn)))
  )

(define (reg-get-field r fn)
  (let ((fields (reg-get-fields r)))
    (get-unique field-get-name fields fn)))

(define (reg-get-child r fn)
  ;; this is a bit weird but fully consistent
  ;; the point here is that we don't want the field, because every other
  ;; get-child consumes the tagged part.  Since we are at the leaf we
  ;; are left with '()
  ;; but we still want the side-effect of asserting the field exists...
  (cdr (list (reg-get-field r fn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; arrays : can contain containers or regs
;;

(define (array-get-length a)
  (assert-array a)
  (cadr a))

(define (array-get-elem a)
  (assert-array a)
  (caddr a))

(define (array-get-children a) (list (array-get-elem a)))

(define (array-sum what a)
  ;;(assert-array a)
  (* (array-get-length a) (gen-sum what (array-get-elem a))))

(define (array-get-child a idx)
  (assert-array a)
  (cond ((not (number? idx))
         (error (error-append "index not a number : " (stringify idx))))
        ((or (< idx 0) (>= idx (array-get-length a)))
         (error (error-append "index out of range : " (stringify idx) " : " (stringify a))))
        (else (array-get-elem a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; absolutely generic functions (generic over both query and type)
;;

(define (gen-sum what obj)
  (let* ((tag    (get-tag obj))
         (summer (eval (symbol-append tag '-sum))))
    (summer what obj)))

(define (get-child obj cn)
  (let* ((tag (get-tag obj))
         (finder (eval (symbol-append tag '-get-child))))
    (finder obj cn)))

(define *last-get-children-arg* '())

(define (get-children obj)
  (set! *last-get-children-arg* obj)
  (let* ((tag (get-tag obj))
         (getter (eval (symbol-append tag '-get-children))))
    (getter obj)))
  
        
(define (treesum what t)
  (let ((children (get-children t)))
    (cond ((null? children) (list (gen-sum what t)))

          (else (let ((csum (map (lambda(c)(treesum what c)) children)))
                  (list

                   (*
                    (if (eq? 'array (car t))
                        (array-get-length t)
                        1 ;; not an array -- a plain sum
                        )
                    (apply + (map car csum)))
                   
                   csum)))
          
          )))
