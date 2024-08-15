(require-modules "display" "hashtable")

;;
;; generic optimization in the Unix environment
;;
;; mika.nystroem@intel.com
;; August, 2024
;;

(dis "**********  Setting up genopt environment  **********" dnl)

(define *param-vars* '())
(define (def-paramvar nm val)
  (set! *param-vars* (cons (list nm val) *param-vars*)))

(define *opt-vars* '())
(define (def-optvar nm defval defstep)
  (GenOpt.DefOptVar nm defval defstep)
  (set! *opt-vars* (cons (list nm defval defstep) *opt-vars*)))

(define *rhobeg* 1.0)
(define (def-rhobeg val)
  (set! *rhobeg* val))

(define *rhoend* 0.001)
(define (def-rhoend val)
  (set! *rhoend* val))

(define *compute-command* #f)
(define (def-compute-command proc)
  (set! *compute-command* proc))

(define *schema-path* #f)
(define (def-schema-path path)
  (set! *schema-path* path))


(define *load-scm* '())
(define (def-load-scm scm-path)
  (set! *load-scm* (cons scm-path *load-scm*)))

(define *data-filename* #f)
(define (def-data-filename fnm)
  (set! *data-filename* fnm))


(define *eval-lisp* #f)
(define (def-eval lisp-code)
  (set! *eval-lisp* lisp-code))

(define *x* #f)       ;; the abstract point
(define *p* #f)       ;; the concrete point
(define *factors* #f) 

(define (do-setup!)
  (set! *x* (map cadr *opt-vars*))
  (set! *factors* (map caddr *opt-vars*))
  (update-*p*!))

(define (update-*p*!)
  (set! *p* (map / *x* *factors*)))

(define (expand-command)
  ;; this expands into the command that we need to run to perform
  ;; the execution part of the evaluation
  (let ((param-defs
         (map (lambda (param)
                `(define ,(car param) ,(cadr param)))
              *param-vars*))
        (opt-defs
         (map (lambda(optvar pc)
                (list 'define (car optvar) pc))
              *opt-vars* *x*))
        (compute-defs (list *compute-command*)))
    (eval (append '(begin) param-defs opt-defs compute-defs))))
  

(dis "**********  Done setting up genopt environment  **********" dnl)


