(require-modules "basic-defs" "m3" "display" "hashtable" "struct" "set")
(load "../../wm_net/src/structgen_m3.scm")
(load "../../wm_net/src/structgen_shared.scm")

(define defs
  `((constant ffu-n-key32 16)
    (constant ffu-n-key16 32)
    (constant ffu-n-key8  64)

    (constant ffu-n-act24 16)
    (constant ffu-n-act4  23)
    (constant ffu-n-act1  24)

    (constant n-parser-keys  84)
    (constant n-parser-ptrs  8)
    (constant n-parser-flags  4)
    (constant n-realign-keys (- n-parser-keys 4))

    (typedef parser-key 16)
    (typedef pk-alias parser-key)

    (typedef ffu-key32  32)
    (typedef ffu-key16  16)
    (typedef ffu-key8    8)

    (typedef pkt-meta (array 32 8))

    (typedef mac-to-parser
             (struct
              ((rx-data       8)
               (rx-length    32)
               (rx-port       8)
               (pkt-meta      pkt-meta))))
    
    (typedef parser-to-mapper
             (struct
              ((rx-port             8)
               (pkt-meta            pkt-meta)
               (rx-flags            8)
               (pa-keys             (array  n-parser-keys     parser-key))
               (pa-keys-valid       (array  n-parser-keys     boolean))
               (pa-flags            (array  n-parser-flags    boolean))
               (pa-ptrs             (array  n-parser-ptrs     8))
               (pa-ptrs-valid       (array  n-parser-ptrs     boolean))
               (pa-csum-ok          2)
               (pa-ex-stage         8)
               (pa-ex-depth-exceed  boolean)
               (pa-ex-trunc-header  boolean)
               (pa-drop             boolean)
               (pa-l3len-err        boolean)
               (pa-packet-type      8))))
    
    (typedef classifier-keys
             (struct 
              ((key32 (array ffu-n-key32 ffu-key32))
               (key16 (array ffu-n-key16 ffu-key16))
               (key8  (array ffu-n-key8  ffu-key8)))))
             
    (typedef prec-val
             (struct
              ((prev 3)
               (val 24))))

    (typedef classifier-actions
             (struct 
              ((act24 (array ffu-n-act24 prec-val))
               (act4  (array ffu-n-act4 prec-val))
               (act1  (array ffu-n-act1 prec-val)))))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lang-name kind lang obj)
  (let* ((db (caddr obj))
         (sym (cadr obj))
         (c-mode (case kind
                   ((typedef) (list 'LCamel 'None))
                   ((constant) (list 'Upper 'Underscore))))
         )
    (case lang
      ((scm) sym)
      ((m3) (IdStyles.Convert (symbol->string sym)
                              'Lower 'Camel
                              'Hyphen 'None))
      ((c) (IdStyles.Convert (symbol->string sym)
                             'Lower (car c-mode)
                             'Hyphen (cadr c-mode))))))


(define *builtins*
  '((boolean (typedef (boolean) ((m3 "BOOLEAN") (c "fm_bool") (scm boolean)))))
  )

(define *languages* '(m3 c scm))

(define *m3-proj* "Mby")
(define *m3-uint-intf* "UInt")
(define *m3-const-intf* "StructConst")
(define *c-proj* "mby")
(define *c-const-pfx* "mbyStruct_")

(define (sym-lookup sym defs)

  (define (make-result rec)
    (let* ((tag (car rec))
           (names (map
                   (lambda (lang)
                     (list lang
                           (lang-name tag lang rec)))
                   *languages*)))
      (list tag rec names)
      ))

  (define (helper p)
    (cond ((null? p) #f)
          ((eq? sym (cadar p)) (make-result (car p)))
          (else (helper (cdr p)))))

  (helper defs))

(define (get-m3-name rec)
  (cadr (assoc 'm3 (caddr rec))))

(define (scheme-mem->m3 sym)
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'LCamel
                    'Hyphen 'None))

(define (get-type-field-cnt x defs)
  (cond ((or (number? x)
             (and (symbol? x)
                  (assoc x *builtins*))) 1)

        ((and (symbol? x)
              (let ((r-test (sym-lookup x defs)))
                (if (and r-test (eq? 'typedef (car r-test)))
                    (get-type-field-cnt (caddr (cadr r-test)) defs)))))

        ((not (pair? x)) (error "get-type-field-cnt failed on " x))

        ((eq? (car x) 'bits) 1)

        ((eq? (car x) 'array) (* (force-value (cadr x) defs) (get-type-field-cnt (caddr x) defs)))

        ((eq? (car x) 'struct)
         (let loop ((cnt 0)
                    (p (cadr x))) ;; the list of fields
           (if (null? p)
               cnt
               (loop (+ cnt (get-type-field-cnt (cadar p) defs)) (cdr p)))))
               
        ))
 
         
(define (gen-m3-type-use x defs imports)

  (cond ((number? x) 
         (imports 'insert! "UInt")
         (sa *m3-uint-intf* "." "UInt" (number->string x)))
        
        ((and (symbol? x)
              (let ((b-test (assoc x *builtins*)))
                (if b-test
                    (get-m3-name (cadr b-test))
                    #f))))

        ((and (symbol? x)
              (let ((r-test (sym-lookup x defs)))
                (if (and r-test (eq? 'typedef (car r-test)))
                    (let ((intf (sa *m3-proj* (get-m3-name r-test))))
                      (imports 'insert! intf)
                      (sa intf ".T"))
                    #f))))

        ;; must be type expression
        ((not (pair? x)) '*not-found*)

        ((eq? (car x) 'bits)
               (sa "[0..WM(LS(1," (gen-m3-val-use (cadr x) defs imports) "),1)]")) ;; WM = Word.Minus, LS = Word.LeftShift
          
        ((eq? (car x) 'array)
         (sa "ARRAY [0.." (gen-m3-val-use (cadr x) defs imports) "-1] OF " (gen-m3-type-use (caddr x) defs imports) ""))

        ((eq? (car x) 'struct)
         (apply sa
                (append
                (list "RECORD" dnl)
                (map
                 (lambda (fspec) (sa "  " (scheme-mem->m3 (car fspec)) " : " (gen-m3-type-use (cadr fspec) defs imports) ";" dnl))
                 (cadr x))
                (list "END" dnl))))

        (else '*not-found*)))

(define mod modulo) ;; for Scheme!

(define *binops*
  '((mod ((m3 "MOD") (c "%")))
    (div ((m3 "DIV") (c "/")))))

(define *multiops*
  '(+ - *))

(define (pow2 n) (if (= n 0) 1 (* 2 (pow2 (- n 1)))))

(define (force-type-number x defs)
  (cond ((number? x) (pow2 x))

        ((and (pair? x) (eq? (car x) 'bits)) (pow2 (force-value (cadr x) defs)))

        ((and (pair? x) (eq? (car x) 'array)) (force-value (cadr x) defs))

        (else (error "force-type-number on " x))))

(define (force-value x defs)
  (cond ((number? x) x)

        ((and (symbol? x)
              (let ((r-test (sym-lookup x defs)))
                (if (and r-test (eq? 'constant (car r-test)))
                    (force-value (caddr (cadr r-test)) defs)))))

        ((not (pair? x)) '*not-found*)

        ((or (memq (car x) *multiops*) (assoc (car x) *binops*))
         (apply (eval (car x)) (map (lambda(z)(force-value z defs)) (cdr x))))))

(define (gen-m3-val-use x defs imports)

  (define (recurse z) (gen-m3-val-use z defs imports))

  (cond ((number? x) (number->string x))

        ((and (symbol? x)
              (let ((r-test (sym-lookup x defs)))
                (if (and r-test (eq? 'constant (car r-test)))
                    (let ((intf (sa *m3-proj* *m3-const-intf* )))
                      (imports 'insert! intf)
                      (sa intf "." (get-m3-name  r-test) ))
                    #f))))
        
        ;; must be value expression
        ((not (pair? x)) '*not-found*)

        ((eq? (car x) 'number)
         (sa "NUMBER(" (gen-m3-type-use (cadr x) defs imports) ")"))

        ((memq (car x) *multiops*)
         (sa "(" (infixize (map recurse (cdr x)) (car x)) ")"))

        ((let ((br (assoc (car x) *binops*)))
           (if br
               (sa "(" (recurse (cadr x)) " " (cadr (assoc 'm3 (cadr br))) " " (recurse (caddr x)) ")")
               #f)))
             
        (else '*not-found*)))

;;
;; In M3, the various compile routines will tag the output with the filename whither to place each
;; generated fragment.
;;
;; syntax of top-level output is
;; ((<fn> <fragment>) (<fn> <fragment>) ...)
;;

(define (m3-intf-nm typedef)
  (sa *m3-proj*  (scheme->m3 (cadr typedef)) ".i3"))

(define (m3-modu-nm typedef)
  (sa *m3-proj*  (scheme->m3 (cadr typedef)) ".i3"))
  
(define (compile-m3-constant x defs)
  (if (not (eq? (car x) 'constant)) (error "not a constant : " x))
  (let* ((imports (make-string-set 10))
         (code (sa "CONST " (scheme->m3 (cadr x)) " = " (gen-m3-val-use (caddr x) defs imports) ";")))
    (list
     (list (sa *m3-proj* *m3-const-intf* ".i3")
           imports
           code)
     )
    )
  )

(define (compile-m3-typedef-def x defs)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (let* ((imports (make-string-set 10))
         (code (sa "TYPE T = " (gen-m3-type-use (caddr x) defs imports) ";")))
    (list (m3-intf-nm x) imports code)
    )
  )

(define *empty-set* (make-string-set 0))

(define (compile-m3-typedef-serial-size x defs)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (list (m3-intf-nm x) *empty-set* (sa "CONST SerialSize = " (get-type-field-cnt (caddr x) defs) ";")))
  
(define (compile-m3-typedef x defs)
  (list (compile-m3-typedef-def x defs)
        (compile-m3-typedef-serial-size x defs)
        ))

;;
;; in C, the tags will be used only to be topologically sorted before generation into a single
;; flat file
;;
;; syntax of top-level output is
;; ((<tag> <fragment>) (<tag> <fragment>) ...)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *fn* '())

(define (make-compiler lang)
  (lambda (rec defs)
    (let* ((fn (symbol-append 'compile- lang '- (car rec)))
          (f (eval fn)))
      (set! *fn* fn)
      (f rec defs))))
              
                            
(define (compile lang)
  (let* ((compiler (make-compiler lang))
         (lsts (map (lambda (def) (compiler def defs)) defs)))
    (apply append lsts)))
