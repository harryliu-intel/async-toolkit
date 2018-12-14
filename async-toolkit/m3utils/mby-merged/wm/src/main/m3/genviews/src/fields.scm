;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scheme code for computing addresses for C API
;;
;; Author : mika.nystroem@intel.com
;; December, 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;; consistency is not the hobgoblin of little minds when it is not foolish
;; --->we tag every object
;;

(define (get-tag obj)
  (case (caar obj)
    ((cont) 'container)
    (else (caar obj))))

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

(define assert-container (make-asserter 'container))
(define assert-array     (make-asserter 'array    ))
(define assert-field     (make-asserter 'field    ))

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
  (cdr c))

(define (container-get-name c)
  (assert-container c)
  (cadar c))

(define (children-get-children-names children)
  ;; the skipArc rule is here...
  (cond ((and (not (null? children))
              (eq? (get-tag (car children)) 'array))
         (array-get-name (car children)))
        
        (else (map get-name children))
        )
  )

(define (container-get-children-names c)
  ;;(assert-container c)
  (children-get-children-names (container-get-children c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; reg header
;;

(define (regheader-get-children r) '())

(define (regheader-sum what r) 0)

(define (isnt-regheader? x) (not (eq? 'regheader (get-tag x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; fields (children of a reg)
;;

(define (field-get-name f)
  (assert-field f)
  (cadar f))

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
;; arrays 
;;

(define (array-get-length a)
  (assert-array a)
  (cadar a))

(define (array-get-elem a) ;; returns a list
  (assert-array a)
  (cdr a))

(define (array-get-children a)
  (array-get-elem a)
)

(define (array-get-children-names a)
  (children-get-children-names (array-get-children a)))

(define (array-child-get-child a idx)
  (assert-array a)
  (cond ((not (number? idx))
         (error (error-append "index not a number : " (stringify idx))))
        ((or (< idx 0) (>= idx (array-get-length a)))
         (error (error-append "index out of range : " (stringify idx) " : " (stringify a))))
        (else (array-get-elem a))))

(define (array-get-name a)
  (assert-array a)
  (array-get-length a) ;; not sure this entirely makes sense
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; child handling
;;

(define (array-child? children)
  (and (not (null? children))
       (eq? (get-tag (car children)) 'array)))

(define (get-child obj cn)
  (let ((children (get-children obj)))
    (if (array-child? children)

        ;; handle the array case
        (cond ((not (number? cn))
               (error (error-append "index not a number : " (stringify cn))))
              ((or (< cn 0) (>= cn (array-get-length (car children))))
               (error (error-append "index out of range : " (stringify cn) " : " (stringify (car children)))))
              (else
               (cons '(cont *elem*) (array-get-elem (car children)))))

          (get-unique get-name children cn))))
 
(define (get-child-by-cnt obj i)
  (let ((children (get-children obj)))
    (if (array-child? children)
        (get-child obj i)
        (nth children i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; absolutely generic functions (generic over both query and type)
;;

(define (get-name obj)
  (let* ((tag (get-tag obj))
         (getter (eval (symbol-append tag '-get-name))))
    (getter obj)))

(define (get-children-names obj)
  ;; this function has a funny spec
  ;; for a container, it returns the list of names of children
  ;; for an array, it returns a number, the size of the array
  ;; (all arrays here are zero-based)
  (let* ((tag (get-tag obj))
         (lister (eval (symbol-append tag '-get-children-names))))
    (lister obj)))

(define (gen-sum what obj)
  (let* ((tag (get-tag obj))
         (summer (eval (symbol-append tag '-sum))))
    (summer what obj)))
              

(define *last-get-children-arg* '())

(define (get-children obj)
  (set! *last-get-children-arg* obj)
  (let* ((tag (get-tag obj))
         (getter (eval (symbol-append tag '-get-children))))
    (getter obj)))

(define get-children cdr)
  
(define (treesum what t)
  (let ((children (get-children t)))
    (cond ((null? children) (list (gen-sum what t)))

          (else (let ((csum (map (lambda(c)(treesum what c)) children)))
                  (cons

                   (*
                    (if (eq? 'array (get-tag t))
                        (array-get-length t)
                        1 ;; not an array -- a plain sum
                        )
                    (apply + (map car csum)))
                   
                   csum)))
          )))

(define (treemap op t) ;; could generalize to multiple trees...
  (let ((children (get-children t)))
    (cons (op t) (map (lambda(c)(treemap op c)) children))))

(define (vertex-op op t)
  (let ((children (get-children t)))
    (cond ((null? children) (list #f))

          (else
           (let ((down (map (lambda(c)(vertex-op op c)) children)))
             (cons (op t) down)))
          )))

(define (zip a-lst b-lst) (map cons a-lst b-lst))

(define get-tree-children cadr)

(define i (lambda (x) x))

(define (vertex-op2 op t u)
  (let ((t-children (get-children t))
        (u-children (get-tree-children u))
        )
    (cond ((null? t-children) (list #f))

          (else
           (let ((down (map (lambda(c)(vertex-op2 op (car c) (cadr c)))
                            (zip t-children u-children))))
             (list (op t) down)))
          )))

(define (make-array-size-tree t)
  (vertex-op
   (lambda(c) (if (eq? 'array (get-tag c)) (array-get-length c) #f))
   t))

;; first child of map
(define (fc m)(car (get-children m)))

;; first child of address tree
(define tl cadr)

;; iterate HOF
(define (iter f n x)
  (if (= 0 n) x (iter f (- n 1) (f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-accum t)
  ;; sum to the right at every level of tree
 (define (helper p n)
    (cond ((null? p) '())
          
          ((null? (cdr p))
           (list n))
          
          (else
           (cons n
                 (let loop ((pp   (cdr p))
                            (i    n))
                   (if (null? pp)
                       '()
                       (cons (helper (car pp) i)
                             (loop (cdr pp) (+ i (caar pp))))))))
          )
    )
 (helper t 0))

(define (aux-treemap-old op t)
  (cond ((null? t) '())

        ((pair? t) (cons (aux-treemap op (car t))
                         (aux-treemap op (cdr t))))

        (else (op t))))

(define (aux-treemap op t)
  (cond ((null? t) '())
        (else (cons (op (car t)) (map (lambda(u)(aux-treemap op u)) (cdr t))))))

(define (make-tagger tag)
  (lambda (x)(cons tag x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                           
(define (atom? x) (and (not (pair? x)) (not (null? x))))

(define (tree-iso? a b)
  ;; we now ignore the car, that is where node data is stored
  ;; all that matters is that the cdr is (recursively) isomorphic
  (cond ((and (null? a) (null? b)) #t)

        ((and (pair? a) (pair? b))
         (tree-iso? (cdr a) (cdr b))) 

        ((and (atom? a) (atom? b)) #t)

        (else #f)

        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-by-sequence getter in seq)
  (let loop ((z     in)
             (p     seq)
             (last '()))
    (if (null? p)
        last
        (let ((this (getter z (car p))))
          (loop this (cdr p) this)))))

(define (get-by-cnt-sequence tree seq)  ;; for struct tree
  (get-by-sequence get-child-by-cnt tree seq))

(define (get-aux-child-by-cnt x n)
  (nth (cdr x) n))

(define (get-aux-children-by-cnt x n)
  (head n (cdr x)))

(define (get-aux-by-cnt-sequence auxtree seq) ;; for aux tree
  (car (get-by-sequence get-aux-child-by-cnt auxtree seq)))
   
(define (get-index x lst)
  (if (number? lst)
      (if (number? x) x (error (error-append "not a number : " (stringify x))))
      
      (let loop ((p lst)
                 (i 0))
        (cond ((null? p) (error (error-append
                                 "not found : "
                                 (stringify x) " : " (stringify lst))))
              ((eq? (car p) x) i)
              (else (loop (cdr p) (+ i 1)))))))

(define (cnt-sequence-by-name in names)
  (let loop ((p in)
             (n names))
    (if (null? n)
        '()
        (begin
;;          (dis (error-append "p : " (stringify p)) dnl)
        
          (let* ((q (get-children-names p))
                 (i (get-index (car n) q)))
            (cons i (loop (get-child-by-cnt p i) (cdr n))))))))
        
(define (name-by-cnt-sequence in seq)
  (let loop ((p in)
             (n seq))
    (if (null? n)
        '()
        (let* ((i (car n))
               (q (get-children-names p)))
          (cons
           (if (number? q) i (nth q i))
           (loop (get-child-by-cnt p i) (cdr n)))))))

(load "examples.scm")
