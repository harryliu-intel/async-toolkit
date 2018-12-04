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
  (map child-get-name (container-get-children c)))

(define (container-sum what c)
  ;;(assert-container c)
  (accumulate + 0 (map (lambda(ch)(child-sum what ch))
                       (container-get-children c))))

(define (container-get-child c cn)
  (let ((children (container-get-children c)))
    (child-get-contents (get-unique child-get-name children cn))))

(define (container-get-child-by-cnt c i)
  (let ((children (container-get-children c)))
    (child-get-contents (nth children i))))

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

(define (reg-get-children-names r) (map cadr (reg-get-children r)))

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

(define (reg-get-child-by-cnt r i)
  ;; as above
  (let ((children (reg-get-children r)))
    (cdr (list (nth children i)))))

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

(define (array-get-children-names a) (array-get-length a))

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

(define array-get-child-by-cnt array-get-child)

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

(define (get-children-names obj)
  ;; this function has a funny spec
  ;; for a container, it returns the list of names of children
  ;; for an array, it returns a number, the size of the array
  ;; (all arrays here are zero-based)
  (let* ((tag (get-tag obj))
         (lister (eval (symbol-append tag '-get-children-names))))
    (lister obj)))

(define (get-child-by-cnt obj cn)
  (let* ((tag (get-tag obj))
         (finder (eval (symbol-append tag '-get-child-by-cnt))))
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
                  (cons

                   (*
                    (if (eq? 'array (car t))
                        (array-get-length t)
                        1 ;; not an array -- a plain sum
                        )
                    (apply + (map car csum)))
                   
                   csum)))
          )))

(define (vertex-op op t)
  (let ((children (get-children t)))
    (cond ((null? children) (list #f))

          (else
           (let ((down (map (lambda(c)(vertex-op op c)) children)))
             (cons (op t) down)))
          )))

(define (zip a-lst b-lst) (map cons a-lst b-lst))

(define get-tree-children cadr)

(define i (lambda(x)x))

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

;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                           
(define (atom? x) (and (not (pair? x)) (not (null? x))))

(define (tree-iso? a b)
  (cond ((and (null? a) (null? b)) #t)

        ((and (pair? a) (pair? b))
         (and (tree-iso? (car a) (car b))
              (tree-iso? (cdr a) (cdr b))))

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
        (let* ((q (get-children-names p))
               (i (get-index (car n) q)))
          (cons i (loop (get-child-by-cnt p i) (cdr n)))))))
        
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
