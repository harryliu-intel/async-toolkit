(require-modules "basic-defs" "m3" "display" "hashtable" "struct" "set")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; COMMON HELPERS
;;

(define (fromhex x) (Scan.Int (stringify x) 16))

(define (symbol-append . x) ;; permissive form, allow both symbol and string
  (string->symbol
   (eval
    (cons 'string-append
          (map (lambda (s)
                 (cond ((symbol? s) (symbol->string s))
                       ((string? s) s)
                       (else (error (string-append
                                     "not a string or symbol : " s)))))
               x)))))

(define sa string-append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GLOBALS

(define constants    'JUNK)
(define enum         'JUNK)
(define header       'JUNK)
(define bitstruct    'JUNK)

(define (clear-shared-globals!)
  (set! constants      '())
  (set! enum           '())
  (set! header         '())
  (set! bitstruct      '())
  )

(define clear-globals! clear-shared-globals!) ;; can override

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FILE HANDLING

(define (cmp-files-safely fn1 fn2)
  (let ((res #f))
    (unwind-protect
     (set! res (cmp-files fn1 fn2)) #f #f)
    res))

(define (rename-file-if-different fn1 fn2)
  (if (not (cmp-files-safely fn1 fn2))
      (fs-rename fn1 fn2) ;; copy the scratch over the target
      (FS.DeleteFile fn1) ;; else delete the scratch
      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HEADER FIELD TYPES

(define (array-type? type) (and (list? type) (eq? 'array (car type))))

(define (get-elem-type type)
  (cond ((symbol? type)                                    type)
        ((array-type? type)                          (cadr type))
        (else
         (error (sa "Illegal type descriptor " (stringify type))))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTRAINT COMPILER SUPPORT

(define (digit? c)
  (and
   (>= (char->integer c) (char->integer #\0))
   (<= (char->integer c) (char->integer #\9))
   ))
       
(define (mynumber? expr)
  (or (number? expr)
      (and (symbol? expr)
           (let ((lst (string->list (symbol->string expr))))
             (and (not (null? lst))
                  (digit? (car lst)))))))

(define (intersperse lst sep)
  ;; this routine MUST BE tail-recursive, or we shall definitely
  ;; run out of stack space!
  (define (helper lst so-far)
    (cond ((null? lst) so-far)
          ((null? (cdr lst)) (cons (car lst) so-far))

          (else 
           (helper (cdr lst) 
                   (cons sep  (cons (car lst) so-far))))))
  (reverse (helper lst '()))
  )

(define (infixize string-list sep)
  (if (null? string-list) ""
      (apply sa 
             (intersperse string-list sep))))

(define (unary-op? op)
  ;; recognize unary operators
  (member? op '(- not)))

(define (binary-op? op)
  ;; recognize binary (or higher) operators, assumed left-associative
  (member? op '(+ - * / mod > >= < <= = and or)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAIN FUNCTIONS : COMPILE THE DATA STRUCTURES

(define (compile-one! x)
  (let ((category (car x))
        (nm       (cadr x)))
    (eval `(set! ,category (cons (cdr x) ,category)))
    (eval `(,(symbol-append 'compile- category `!) nm (cddr x)))
    #t
    ))
