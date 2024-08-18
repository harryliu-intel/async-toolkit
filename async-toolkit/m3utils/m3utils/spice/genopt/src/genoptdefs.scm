(require-modules "display" "hashtable")

;;
;; generic optimization in the Unix environment
;;
;; mika.nystroem@intel.com
;; August, 2024
;;

;; these two are suspicious: why are they here?
(define *param-vars* '())

(define *opt-vars* '())


(define (def-paramvar nm val)
  (set! *param-vars* (cons (list nm val) *param-vars*)))

(define (set-param! param to-val)
  (set-cdr! (assoc param *param-vars*) (list to-val)))

;; note that last variable gets to be first in the list
(define (def-optvar nm defval defstep)
  (GenOpt.DefOptVar nm defval defstep)
  (set! *opt-vars* (cons (list nm defval defstep) *opt-vars*)))


(define (def-rhobeg val)
  (GenOpt.SetRhoBeg val))

(define (def-rhoend val)
  (GenOpt.SetRhoEnd val))

(define (set-netbatch val)
  (GenOpt.SetNetbatch val))

(define *compute-command* #f)
(define (def-compute-command proc)
  (set! *compute-command* proc))

(define (def-schema-path path)
  (GenOpt.DefSchemaPath path))

(define (def-load-scm scm-path)
  (GenOpt.DefLoadScm scm-path))

(define (def-data-filename fnm)
  (GenOpt.DefDataFilename fnm))

(define (def-eval lisp-code)
  (GenOpt.DefEval lisp-code))

;; *p*[i] = *x*[i] / *factors*[i]
;;(define *x* #f)       ;; the abstract point (problem variables)
;;(define *p* #f)       ;; the concrete point (optimization variables)
;;(define *factors* #f) 

;;(define (do-setup!)
;;  (set! *x* (map cadr *opt-vars*))
;;  (set! *factors* (map caddr *opt-vars*))
;;  (update-*p*!)
;;  #t)
;;
;;(define (update-*p*!)
;;  (set! *p* (map / *x* *factors*)))

(define (get-param-string-converter param)
  (let ((av (cadr (assoc param *param-vars*))))
    (cond ((string? av) (lambda(x) x))
          ((number? av) string->number)
          ((symbol? av) string->symbol)
          (else "Unknown type of param " param " : " av))))
        
(define (get-param-val param)
  (let ((av (cadr (assoc param *param-vars*)))
        (mv (GenOpt.GetParam param)))
    (if mv ((get-param-string-converter param) mv) av)))

(define (print-defines)
  (let ((param-defs
         (map (lambda (param)
                `(define ,(car param) ,(get-param-val (car param))))
              *param-vars*))
        (opt-defs
         (map (lambda(optvar pc)
                (list 'define (car optvar) pc))
              *opt-vars* (get-*x*))))
    (append param-defs opt-defs)))
        
(define (eval-in-env cmd)
  ;; this expands into the command that we need to run to perform
  ;; the execution part of the evaluation
  (let ((to-eval (append '(begin) (print-defines) (list cmd))))
    (dis "eval-in-env : will eval : " to-eval dnl)
    (eval to-eval)
    ))

(define (expand-command)
  ;; this expands into the command that we need to run to perform
  ;; the execution part of the evaluation
  (eval-in-env *compute-command*))

(define (run-once) (run-command (expand-command)))

(define (make-cb-obj)
  ;; this is the callback through which the Modula code gets the
  ;; command to run to perform an evaluation
  (let* ((func (lambda(*unused*)(expand-command)))
         (cb-obj (new-modula-object 'OptCallback.T `(command . ,func))))
    cb-obj))

(define (downrange n)
  (if (= 0 n)
      '()
      (cons (- n 1) (downrange (- n 1)))))

(define (uprange n)
  (reverse (downrange n)))

(define (get-*p*)
  (let ((coords (obj-method-wrap (GenOpt.GetCoords) 'LongRealSeq.T)))
    (map (lambda(i)(coords 'get i)) (uprange (coords 'size)))))

(define (get-*x*)
  (let ((factors (map caddr *opt-vars*)))
    (map * (get-*p*) factors)))


