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
    (constant n-realign-keys (- n-parser-keys 4))

    (typedef parser-key 16)

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
              (rx-port             8)
              (pkt-meta            pkt-meta)
              (rx-flags            8)
              (pa-keys             (array  n-parser-keys parser-key))
              (pa-keys-valid       (array  n-parser-keys boolean))
              (pa-flags            (array             48 boolean))
              (pa-ptrs             (array              8 8))
              (pa-ptrs-valid       (array              8 boolean))
              (pa-csum-ok          2)
              (pa-ex-stage         8)
              (pa-ex-depth-exceed  boolean)
              (pa-ex-trunch-header boolean)
              (pa-drop             boolean)
              (pa-l3len-err        boolean)
              (pa-packet-type      8)))

    (typedef classifier-keys
             (struct 
              (key32 (array ffu-n-key32 ffu-key32))
              (key16 (array ffu-n-key16 ffu-key16))
              (key8  (array ffu-n-key8  ffu-key8))))

    (typedef prec-val
             (struct
              (prev 3)
              (val 24)))

    (typedef classifier-actions
             (struct 
              (act24 (array ffu-n-act24 prev-val))
              (act4  (array ffu-n-act4 prev-val))
              (act1  (array ffu-n-act1 prev-val))))

    ))

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
  '((boolean (type (boolean) ((m3 "BOOLEAN") (c "fm_bool") (scm boolean)))))
  )

(define *languages* '(m3 c scm))

(define *m3-proj* "Mby")
(define *m3-uint-intf* "UInt")
(define *m3-const-intf* "StructConst")
(define *c-proj* "mby")
(define *c-const-pfx* "mbyStruct_")


;; kind is 'member or 'type

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

(define (gen-m3-type-use x defs)

  (cond ((number? x) 
         (sa *m3-uint-intf* "." "UInt" (number->string x)))
        
        ((and (symbol? x)
              (let ((b-test (assoc x *builtins*)))
                (if b-test
                    (get-m3-name (cadr b-test))
                    #f))))

        ((and (symbol? x)
              (let ((r-test (sym-lookup x defs)))
                (if (and r-test (eq? 'type (car r-test)))
                    (sa *m3-proj* (get-m3-name  r-test) ".T")
                    #f))))

        ;; must be type expression
        ((not (pair? x)) '*not-found*)

        ((eq? (car x) 'bits)
               (sa "[0..WM(LS(1," (gen-m3-val-use (cadr x) defs) "),1)]")) ;; WM = Word.Minus, LS = Word.LeftShift
          
        ((eq? (car x) 'array)
         (sa "ARRAY [0.." (gen-m3-val-use (cadr x) defs) "-1] OF (" (gen-m3-type-use (caddr x) defs) ")"))

        ((eq? (car x) 'struct)
         (apply sa
                (append
                (list "RECORD" dnl)
                (map (lambda (fspec) (sa "  " (scheme-mem->m3 (car fspec)) " : (" (gen-m3-type-use (cadr fspec) defs) ");" dnl)) (cdr x))
                (list "END" dnl))))

        (else '*not-found*)))

(define mod modulo) ;; for Scheme!

(define *binops*
  '((mod ((m3 "MOD") (c "%")))
    (div ((m3 "DIV") (c "/")))))

(define *multiops*
  '(+ - *))
  
(define (gen-m3-val-use x defs)

  (define (recurse z) (gen-m3-val-use z defs))

  (cond ((number? x) (number->string x))

        ((and (symbol? x)
              (let ((r-test (sym-lookup x defs)))
                (if (and r-test (eq? 'constant (car r-test)))
                    (sa *m3-proj* *m3-const-intf* "." (get-m3-name  r-test) )
                    #f))))
        
        ;; must be value expression
        ((not (pair? x)) '*not-found*)

        ((eq? (car x) 'number)
         (sa "NUMBER(" (gen-m3-type-use (cadr x) defs) ")"))

        ((memq (car x) *multiops*)
         (sa "(" (infixize (map recurse (cdr x)) (car x)) ")"))

        ((let ((br (assoc (car x) *binops*)))
           (if br
               (sa "(" (recurse (cadr x)) " " (cadr (assoc 'm3 (cadr br))) " " (recurse (caddr x)) ")")
               #f)))
             
        (else '*not-found*)))
         
(define (compile-m3-typedef x defs)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (sa "TYPE " (scheme->m3 (cadr x)) " = " (gen-m3-type-use (caddr x) defs) ";"))
                
(define (compile-interlang! defs)
  
  (map compile-one-il! defs))
             
                            
