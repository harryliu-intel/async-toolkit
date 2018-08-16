(require-modules "basic-defs" "m3" "display" "hashtable" "struct" "set" "mergesort")
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
(define *m3-lib-name* "mby_struct")
(define *m3-common-output-dir* "wm_meta_common")

(define *c-proj* "mby")
(define *c-const-pfx* "mbyStruct_")
(define *c-builtin-serdes-fn* "common_serdes")

(define (make-sym-def-data rec)
  (let* ((tag (car rec))
         (names (map
                 (lambda (lang)
                   (list lang
                         (lang-name tag lang rec)))
                 *languages*)))
    (list tag rec names)
    ))

(define (sym-lookup sym defs)

  (define (helper p)
    (cond ((null? p) #f)
          ((eq? sym (cadar p)) (make-sym-def-data (car p)))
          (else (helper (cdr p)))))

  (helper defs))

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

        ((eq? (car x) 'number) (force-type-number (cadr x) defs))

        ((or (memq (car x) *multiops*) (assoc (car x) *binops*))
         (apply (eval (car x)) (map (lambda(z)(force-value z defs)) (cdr x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         
(define (gen-m3-type-use x defs imports in-intf)

  (cond ((number? x) 
         (imports 'insert! "UInt")
         (fmt-m3-sym (cons *m3-uint-intf* (sa "UInt" (number->string x))) in-intf imports))
        
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
                      (fmt-m3-sym (cons intf "T") in-intf imports))
                    #f))))

        ;; must be type expression
        ((not (pair? x)) '*not-found*)

        ((eq? (car x) 'bits)
               (sa "[0..Word.Minus(Word.LeftShift(1," (gen-m3-val-use (cadr x) defs imports in-intf) "),1)]"))
          
        ((eq? (car x) 'array)
         (sa "ARRAY [0.." (gen-m3-val-use (cadr x) defs imports in-intf) "-1] OF " (gen-m3-type-use (caddr x) defs imports in-intf) ""))

        ((eq? (car x) 'struct)
         (apply sa
                (append
                (list "RECORD" dnl)
                (map
                 (lambda (fspec) (sa "  " (scheme-mem->m3 (car fspec)) " : " (gen-m3-type-use (cadr fspec) defs imports in-intf) ";" dnl))
                 (cadr x))
                (list "END" dnl))))

        (else '*not-found*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fmt-m3-sym intf-sym in-intf imports)
  (let ((intf (car intf-sym))
        (sym  (cdr intf-sym)))
    (imports 'insert! intf)
    (sa (if (equal? intf in-intf) "" (sa intf "."))
        sym)
    )
  )
      
(define (gen-m3-val-use x defs imports in-intf)

  (define (recurse z) (gen-m3-val-use z defs imports in-intf))

  (cond ((number? x) (number->string x))

        ((and (symbol? x)
              (let ((r-test (sym-lookup x defs)))
                (if (and r-test (eq? 'constant (car r-test)))
                    (let ((intf (sa *m3-proj* *m3-const-intf* )))
                      (imports 'insert! intf)
                      (fmt-m3-sym (cons intf (get-m3-name r-test)) in-intf imports))
                    #f))))
        
        ;; must be value expression
        ((not (pair? x)) '*not-found*)

        ((eq? (car x) 'number)
         (sa "NUMBER(" (gen-m3-type-use (cadr x) defs imports in-intf) ")"))

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
  (sa *m3-proj*  (scheme->m3 (cadr typedef))))

(define (m3-intf-fnm typedef)
  (sa (m3-intf-nm typedef) ".i3"))

(define (m3-modu-fnm typedef)
  (sa (m3-intf-nm typedef) ".m3"))
  
(define (compile-m3-typedef-def x defs)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (let* ((imports (make-string-set 10))
         (code (sa "TYPE T = " (gen-m3-type-use (caddr x) defs imports (m3-intf-nm x)) ";")))
    (list (m3-intf-fnm x) imports code)
    )
  )

(define *empty-set* (make-string-set 1))

(define (compile-m3-typedef-serial-size x defs)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (list (m3-intf-fnm x) *empty-set* (sa "CONST SerialSize = " (get-type-field-cnt (caddr x) defs) ";")))

(define (m3-deser-name whch)
  (case whch
    ((ser)   "Serialize")
    ((deser) "Deserialize")
    ((fmt)   "FormatWx")
    (else (error))))

(define (m3-deser-proto whch nm serial-size semi type)
  (sa "PROCEDURE " nm "("
      (case whch
        ((ser)   (sa "VAR s : ARRAY[0.."serial-size"-1] OF Word.T; READONLY z : "type))
        ((deser) (sa "READONLY s : ARRAY[0.."serial-size"-1] OF Word.T; VAR z : "type))
        ((fmt)   (sa "wx : Wx.T; READONLY z : "type))
        )
      ")" semi)
  )

(define (m3-deser-uint-pname b whch)
  (cons *m3-uint-intf*  (sa (m3-deser-name whch) (number->string b))))

(define (m3-deser-builtin-pname x whch)
   (let ((b-test (assoc x *builtins*)))
                   (if b-test
                       (cons "WmDeSer" (sa (m3-deser-name whch) (get-m3-name (cadr b-test))))
                       #f)))
           
(define (m3-deser-typedef-pname x whch)
  (let ((r-test (sym-lookup x defs)))
    (if (and r-test (eq? 'typedef (car r-test)))
        (let ((intf (sa *m3-proj* (get-m3-name r-test))))
          (cons intf (m3-deser-name whch)))
        #f)))

(define (compile-m3-typedef-deser-proto td defs whch semi)
  (if (not (eq? (car td) 'typedef)) (error "not a typedef : " td))
  (let ((x          (caddr td))
        (in-intf    (m3-intf-nm td))
        (imports    (make-string-set 10))
        (proc-name  (m3-deser-name whch)))
    ;; depending what the typedef is, we do different things
    ;; if its just an alias, make it a CONST def, relating to the base type
    ;; else provide the standard proto
    (let ((code
     (cond ((number? x) 
            (sa "CONST " proc-name " = " (fmt-m3-sym (m3-deser-uint-pname x whch) in-intf imports) semi))

           ((let ((builtin-name (m3-deser-builtin-pname x whch)))
              (if builtin-name (sa "CONST "proc-name" = " (fmt-m3-sym builtin-name in-intf imports) ";") #f)))

           ((let ((td-name (m3-deser-typedef-pname x whch)))
              (if td-name (sa "CONST "proc-name" = " (fmt-m3-sym td-name in-intf imports) ";") #f)))
           
           ;; must be type expression
           ((not (pair? x)) '*not-found*)
           
           ((eq? (car x) 'bits)
            (let ((rv (compile-m3-typedef-deser-proto (force-value (cadr x) defs) defs whch semi)))
              (set! imports (cadr rv))
              (caddr rv)))

           (else ;; a compound type of some kind (struct or array)
            (m3-deser-proto whch (m3-deser-name whch) "SerialSize" semi "T"))
            ) ;;dnoc
     ))
           
      (list (m3-intf-fnm td) imports code)
      )
    ) ;;tel
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ser/deser/fmt
;;;

(define (call-m3-deser-code whch obj start-strm-idx len)
  (case whch
    ((ser deser) (sa "(SUBARRAY(s," start-strm-idx"," len "), " obj ")"))
    ((fmt)       (sa "(wx, "obj")"))))

(define (m3-tag-deser-code whch ind tag)
  (if (eq? tag 'fmt)
      (sa
       ind "Wx.PutChar(wx, '\\n');" dnl
       ind "Wx.PutText(wx, \""ind"\");" dnl
       (if tag (sa
                ind "Wx.PutText(wx, "tag");" dnl) "")
       ind "Wx.PutChar(wx, ':');" dnl
       )
      ""
      ))

(define (make-m3-deser-code whch x defs lhs lev ind p tag imports in-intf)
  (cond
   ((number? x) (sa (m3-tag-deser-code whch ind tag)
                    ind
                    (fmt-m3-sym (m3-deser-uint-pname x whch) in-intf imports)
                    (call-m3-deser-code whch lhs p 1)))

   ((let ((builtin-name (m3-deser-builtin-pname x whch)))
      (if builtin-name (sa (m3-tag-deser-code whch ind tag)
                           ind
                           (fmt-m3-sym builtin-name in-intf imports)
                           (call-m3-deser-code whch lhs p 1))
          #f)))
   
   ((let ((td-name (m3-deser-typedef-pname x whch)))
      (if td-name (sa (m3-tag-deser-code whch ind tag)
                      ind
                      (fmt-m3-sym td-name in-intf imports)
                      (call-m3-deser-code whch lhs p (fmt-m3-sym (cons (car td-name) "SerialSize") in-intf imports)))
          #f)))
   
   ((not (pair? x)) (error "cant de/ser " x))
    
   ((eq? (car x) 'bits)
    (make-m3-deser-code whch (force-value (cadr x) defs) lhs lev ind p tag imports in-intf))
   
   ((eq? (car x) 'array)
    (sa
     ind "FOR i"lev" := 0 TO " (force-value (cadr x) defs) "-1 DO" dnl
     ind "  WITH t"lev " = " lhs "[i"lev"], " dnl

     (case whch
       ((ser deser) (sa
     ind "       o"lev " = " p" + i"lev" * " (get-type-field-cnt (caddr x) defs) " DO" dnl))
     
       ((fmt) (sa
     ind "       tag"lev " = Fmt.F(\"%s[%s]\","(if (= lev 0) (if tag tag "\"\"") (sa "tag"(- lev 1)))", Fmt.Int(i"lev")) DO" dnl)))

          (make-m3-deser-code whch (caddr x) defs (sa "t"lev) (+ lev 1) (sa ind "    ") (sa "o"lev) (sa "tag" lev) imports in-intf) dnl

          ind "  END" dnl
          ind "END" 
          ))

   ((eq? (car x) 'struct)
    (let loop ((ptr (cadr x))
               (outp "")
               (idx p)
               )
      (if (null? ptr)
          outp
          (loop (cdr ptr)
                (sa outp
                    (make-m3-deser-code whch
                                        (cadar ptr)
                                        defs
                                        (sa lhs "." (scheme-mem->m3 (caar ptr)))
                                        lev
                                        (sa ind "  ")
                                        idx
                                        (sa "\"" (caar ptr) "\"")
                                        imports
                                        in-intf
                                        )
                    ";"
                    dnl
                    ) ;; as
                (+ idx (get-type-field-cnt (cadar ptr) defs))
                )
          ) ;; fi
      ) ;; tel
    )))

(define (compile-m3-typedef-deser-code td defs whch)
  (if (not (eq? (car td) 'typedef)) (error "not a typedef : " td))
  (let* ((imports    (make-string-set 10))
        (x          (caddr td))
        (proc-name  (m3-deser-name whch))
        (tdp (compile-m3-typedef-deser-proto td defs whch " ="))
        (code
         (cond ((not (pair? x)) "") ;; base type, no need for code generation
               ((or (eq? (car x) 'array)
                    (eq? (car x) 'struct))
                (sa (caddr tdp) dnl
                    "  BEGIN" dnl
                    (make-m3-deser-code whch x defs "z" 0 "  " 0 #f imports (m3-intf-nm td))
                    "  END " proc-name ";" dnl
                    ) ;; as
                )
               (else "")
               );; dnoc
         )
        )
    (set! imports (imports 'union (cadr tdp)))
    (list (m3-modu-fnm td) imports code)
    );;*tel
  )

;; main entry points for generating M3 below ...

(define (compile-m3-typedef x defs)
  (list (compile-m3-typedef-def x defs)
        (compile-m3-typedef-serial-size x defs)
        (compile-m3-typedef-deser-proto x defs 'ser ";")
        (compile-m3-typedef-deser-proto x defs 'deser ";")
        (compile-m3-typedef-deser-proto x defs 'fmt ";")
        (compile-m3-typedef-deser-code x defs 'ser)
        (compile-m3-typedef-deser-code x defs 'deser)
        (compile-m3-typedef-deser-code x defs 'fmt)
        ))

(define (compile-m3-constant x defs)
  (if (not (eq? (car x) 'constant)) (error "not a constant : " x))
  (let* ((imports (make-string-set 10))
         (intf-nm (sa *m3-proj* *m3-const-intf*))
         (code (sa "CONST " (scheme->m3 (cadr x)) " = " (gen-m3-val-use (caddr x) defs imports intf-nm) ";")))
    (list
     (list (sa intf-nm ".i3") imports code)
     )
    )
  )

(define (compile-m3-prepare)  #t)

;;
;; in C, the tags will be used only to be topologically sorted before generation into a single
;; flat file
;;
;; syntax of top-level output is
;; ((<tag> <fragment>) (<tag> <fragment>) ...)
;;

(define (c-deser-name whch)
  (case whch
    ((ser)   "serialize")
    ((deser) "deserialize")
    (else (error))))

(define (get-c-name rec)
  (cadr (assoc 'c (caddr rec))))

(define (gen-c-val-use x defs)

  (define (recurse z) (gen-c-val-use z defs))

  (cond ((number? x) (number->string x))

        ((and (symbol? x)
              (let ((r-test (sym-lookup x defs)))
                (if (and r-test (eq? 'constant (car r-test)))
                    (sa *c-const-pfx* (get-c-name r-test))
                    #f))))
        
        ;; must be value expression
        ((not (pair? x)) '*not-found*)

        ((eq? (car x) 'number) (force-value x defs))

        ((memq (car x) *multiops*)
         (sa "(" (infixize (map recurse (cdr x)) (car x)) ")"))

        ((let ((br (assoc (car x) *binops*)))
           (if br
               (sa "(" (recurse (cadr x)) " " (cadr (assoc 'c (cadr br))) " " (recurse (caddr x)) ")")
               #f)))
             
        (else '*not-found*)))

(define (gen-c-val-use x defs)

  (define (recurse z) (gen-c-val-use z defs))

  (cond ((number? x) (number->string x))

        ((and (symbol? x)
              (let ((r-test (sym-lookup x defs)))
                (if (and r-test (eq? 'constant (car r-test)))
                    (sa *c-const-pfx* (get-c-name  r-test) )
                    #f))))
        
        ;; must be value expression
        ((not (pair? x)) '*not-found*)

        ((eq? (car x) 'number)
         (number->string (force-type-number (cadr x) defs)))

        ((memq (car x) *multiops*)
         (sa "(" (infixize (map recurse (cdr x)) (car x)) ")"))

        ((let ((br (assoc (car x) *binops*)))
           (if br
               (sa "(" (recurse (cadr x)) " " (cadr (assoc 'c (cadr br))) " " (recurse (caddr x)) ")")
               #f)))
             
        (else '*not-found*)))

(define (compile-c-constant x defs)
  (let ((header-fn (sa *c-proj* "_struct.h")))
    (if (not (eq? (car x) 'constant)) (error "not a constant : " x))
    
    (let* ((nm         (sa *c-const-pfx* (get-c-name (sym-lookup (cadr x) defs))))
           (imports    (make-string-set 10))
           (code       (sa "#define " nm "    " (force-value (caddr x) defs))))
      (list (list header-fn #f code) )
      )))
    
(define (scheme-mem->c sym)
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'LCamel
                    'Hyphen 'None))

(define (compile-c-typedef-def-from-data x nm defs dep-recorder)
  (let ((def-sfx (sa " " nm)))
    (cond ((number? x) (sa "uint" (number->string x) def-sfx))
          
          ((and (symbol? x)
                (let ((b-test (assoc x *builtins*)))
                  (if b-test
                      (sa (get-c-name (cadr b-test)) def-sfx)
                      #f))))

          
          ((and (symbol? x)
                (let ((r-test (sym-lookup x defs)))
                  (if (and r-test (eq? 'typedef (car r-test)))
                      (let ((tn (sa *c-proj* "_" (get-c-name r-test))))
                        (dis "... depends on " tn dnl)
                        (dep-recorder tn)
                        (sa tn def-sfx))
                      #f))))

          ;; must be type expression
          ((not (pair? x)) '*not-found*)

          ((eq? (car x) 'bits)
           (compile-c-typedef-def-from-data (force-value (cadr x) defs) nm defs dep-recorder))

          ((eq? (car x) 'array)
           (compile-c-typedef-def-from-data (caddr x) (sa nm "[" (force-value (cadr x) defs) "]") defs dep-recorder))
                
          ((eq? (car x) 'struct)
           (apply sa
                  (append
                   (list "struct {" dnl)
                   (map
                    (lambda (fspec)
                      (sa "  "
                          (compile-c-typedef-def-from-data (cadr fspec) (scheme-mem->c (car fspec)) defs dep-recorder)
                          ";"
                          dnl)
                      );;adbmal
                    (cadr x))
                   (list "} " nm))))
          
          (else '*not-found*)))
  )

(define (compile-c-typedef-def x defs topo-sorter)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (let* ((sym-nm                           (sa *c-proj* "_" (get-c-name (sym-lookup (cadr x) defs))))
         (dep-recorder (lambda(pred)(dis pred " <- " sym-nm dnl)(topo-sorter 'addDependency pred sym-nm)))
         (code
          (begin (dis "c-typedef " sym-nm dnl)
                 (sa "typedef "
                     (compile-c-typedef-def-from-data (caddr x) sym-nm defs dep-recorder)
                     ";"
                     ))))

    (list sym-nm code)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c-deser-uint-pname b whch)
  (sa "uint" (number->string b) "_" (c-deser-name whch)  ))

(define (c-deser-builtin-pname x whch)
   (let ((b-test (assoc x *builtins*)))
                   (if b-test
                       (sa (get-c-name (cadr b-test)) "_" (c-deser-name whch))
                       #f)))
           
(define (c-deser-typedef-pname x whch defs)
  (let ((r-test (sym-lookup x defs)))
    (if (and r-test (eq? 'typedef (car r-test)))
        (sa *c-proj* "_" (get-c-name r-test) "_" (c-deser-name whch))
        #f)))

(define (c-deser-proto-for-type whch tn formal)
  (sa "void "  tn "_" (c-deser-name whch) "("
      (case whch
        ((ser)   (sa "uint64 *s, const " tn " " formal))
        ((deser) (sa "const uint64 *s, " tn " " formal))
        )
      ")"))

(define (make-pointer-formal of type defs)
  (if (array-typedef? type defs) of (sa "*" of)))


(define (compile-c-typedef-deser-proto td defs whch semi)
  (if (not (eq? (car td) 'typedef)) (error "not a typedef : " td))
  (let ((tn (sa *c-proj* "_" (get-c-name (sym-lookup (cadr td) defs)))))
    ;; this is a bit different from the M3 since there are no const
    ;; functions in C
    (list
     #f
     (sa (c-deser-proto-for-type whch tn (make-pointer-formal "t" (cadr td) defs)) semi)
     ) ;;tsil
    ) ;;tel
  )

(define (call-c-deser-code whch obj strm-idx)
  (case whch
    ((ser deser) (sa "(&(s[" strm-idx "]), " obj ")")) ;; deref hack :-(
    ))

(define (take-address obj) (sa "&(" obj ")"))

(define *of* '())
(define *tn* '())

(define (array-typedef? tn defs)
  (let ((t-rec (sym-lookup tn defs)))
    (and t-rec
         (equal? (car t-rec) 'typedef)         ;; its a typedef
         (pair? (caddr (cadr t-rec)))          ;; referencing a compound type...
         (equal? (caar (cddadr t-rec)) 'array))))

(define (make-pointer of tn defs)
  (cond ((equal? of "t") "t")         ;; t itself ALWAYS a pointer

        ((array-typedef? tn defs) of) ;; array already a pointer in C

        (else (take-address of))      ;; all others
        ))

(define (make-c-deser-code whch x defs lhs lev ind p)
  (cond
   ((number? x)
    (sa ind (c-deser-uint-pname x whch) (call-c-deser-code whch (make-pointer lhs x defs) p) ";" dnl))

   ((let ((dsname (c-deser-builtin-pname x whch)))
      (if dsname (sa ind dsname (call-c-deser-code whch (make-pointer lhs x defs) p) ";" dnl) #f)))
   
   ((let ((dsname (c-deser-typedef-pname x whch defs)))
      (if dsname (sa ind dsname (call-c-deser-code whch (make-pointer lhs x defs) p) ";" dnl) #f)))
   
   ((not (pair? x)) (error "cant de/ser " x))
    
   ((eq? (car x) 'bits)
    (make-c-deser-code whch (force-value (cadr x) defs) lhs lev ind p))
   
   ((eq? (car x) 'array)
    (sa
     ind "for (int i"lev" = 0; i"lev" < " (force-value (cadr x) defs) "; ++i"lev") {" dnl
     ind "  uint o"lev " = " p" + i"lev" * " (get-type-field-cnt (caddr x) defs) ";" dnl
     (make-c-deser-code whch (caddr x) defs (sa lhs "[i"lev"]") (+ lev 1) (sa ind "  ") (sa "o"lev) ) dnl
          ind "}" 
          ))

   ((eq? (car x) 'struct)
    (let loop ((ptr (cadr x))
               (outp "")
               (idx p)
               )
      (if (null? ptr)
          outp
          (loop (cdr ptr)
                (sa outp
                    (make-c-deser-code whch
                                       (cadar ptr)
                                       defs
                                       (sa lhs "->" (scheme-mem->c (caar ptr)))
                                       lev
                                       (sa ind "  ")
                                       idx
                                       )
                    dnl
                    ) ;; as
                (+ idx (get-type-field-cnt (cadar ptr) defs))
                )
          ) ;; fi
      ) ;; tel
    )))

(define (compile-c-typedef-deser-code td defs whch)
  (if (not (eq? (car td) 'typedef)) (error "not a typedef : " td))
  (let ((x          (caddr td)))
    (list
     #f
     (sa
      (cadr (compile-c-typedef-deser-proto td defs whch "")) dnl
      "{" dnl
      (make-c-deser-code whch x defs "t" 0 "  " 0)
      "}" dnl
      )
     )
    )
  )

(define (compile-c-typedef-deser-size x defs)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (list #f (sa "#define " (sa *c-proj* "_" (get-c-name (sym-lookup (cadr x) defs))) "_deser_qwords " (get-type-field-cnt (caddr x) defs) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; just test code, to make sure it all works
;;

(define topo-sorter (obj-method-wrap (new-modula-object 'TextTopoSort.T ) 'TextTopoSort.T))
(topo-sorter 'init)
(topo-sorter 'addDependency "a" "b")
(topo-sorter 'addDependency "a" "c")
(topo-sorter 'addDependency "b" "d")

(define s (obj-method-wrap (topo-sorter 'sort) 'TextSeq.T))
(s 'size)
(s 'get 0)

(define (new-topo-sorter)
  (let ((res (obj-method-wrap (new-modula-object 'TextTopoSort.T ) 'TextTopoSort.T)))
    (obj-method-wrap (res 'init) 'TextTopoSort.T)
    ))
                                                                    ;;
                                                                    ;;
                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-c-typedef x defs)
  (let ((header-fn (sa *c-proj* "_struct.h"))
        (c-code-fn (sa *c-proj* "_struct.c")))

    (append
     ;; these should be directed to a header file
     (map (lambda(c) (cons header-fn c))
          (list
           (compile-c-typedef-def           x defs *topo-sorter*)
           (compile-c-typedef-deser-proto   x defs 'ser ";")
           (compile-c-typedef-deser-proto   x defs 'deser ";")
           (compile-c-typedef-deser-size    x defs)
           ))

     
     ;; and these to an implementation
     (map (lambda(c) (cons c-code-fn c))
          (list
           (compile-c-typedef-deser-code    x defs 'ser)
           (compile-c-typedef-deser-code    x defs 'deser)
           ))
     );;dneppa
   ))

(define *topo-sorter* '())

(define (compile-c-prepare)
  (set! *topo-sorter* (new-topo-sorter)) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-compiler lang)
  (lambda (rec defs)
    (let* ((fn (symbol-append 'compile- lang '- (car rec)))
          (f (eval fn)))
      (f rec defs))))
                            
(define (compile lang)
  ((eval (symbol-append 'compile- lang '-prepare)))
  (let* ((compiler (make-compiler lang))
         (lsts (map (lambda (def) (compiler def defs)) defs)))
    (apply append lsts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-lt t0 t1) (= -1 (Text.Compare t0 t1)))

(define (sort-string-list L) (mergesort L string-lt))

(define (uniq-string-list L) (uniq equal? (sort-string-list L)))

(define (compile-all) (map compile '(m3 c)))

(define (seq-to-list s)
  (let loop ((i (s 'size))
             (res '()))
    (if (= 0 i) res (loop (- i 1) (cons (s 'get (- i 1)) res)))))

(define (rearrange-list lst order)
  (if (not (= (length lst) (length order))) (error "length mismatch " (length lst) " " (length order)))
  (map (lambda (o)(assoc o lst)) order))

(define (do-c-output lst odir)
  (let* ((files    (uniq-string-list (map car lst))) ;; uniq the filenames
         (wrs      (map (lambda(fn) (list fn (FileWr.Open (sa odir "/src/" fn)))) files))
         (untagged (filter (lambda(x)(not (cadr x))) lst))
         (seq      (obj-method-wrap (*topo-sorter* 'sort) 'TextSeq.T))
         (tagged   (filter cadr lst))
         (ordered  (map cdr (rearrange-list (map (lambda (t) (cons (cadr t) t)) tagged) (seq-to-list seq))))
         )
    (dis (length untagged) " untagged" dnl)
    (dis (length tagged) " tagged" dnl)
    
    (map (lambda(w)
           (let* ((fn (car w))
                  (wr (cadr w)))

             (do-c-header fn wr)
             (define (output-matching L)
               (map (lambda (code) (dis code dnl dnl wr)) (map caddr (filter (lambda(q)(equal? fn (car q))) L))))
             
             ;; do tagged and ordered
             (output-matching ordered)

             ;; do untagged 
             (output-matching untagged)

             (do-c-trailer fn wr)
             
             ))
         wrs)
    files
    ))

(define (do-c-header fn wr)
  (let* ((bn (TextUtils.RemoveSuffixes fn '(".c" ".h")))
         (sfx (string->symbol
               (TextUtils.RemovePrefix   fn (sa bn ".")))))
    (case sfx
      ((c)
       (dis "#include \"" "uint" ".h\"" dnl wr)
       (dis "#include \"" bn ".h\"" dnl dnl wr)
       (dis "#include \"../../" *m3-common-output-dir* "/src/" *c-builtin-serdes-fn* ".h\"" dnl dnl wr))
      ((h)
       (dis "#ifndef _"bn"_H" dnl
            "#define _"bn"_H" dnl
            dnl
            "#include \"" "uint" ".h\"" dnl 
            dnl
            wr)))))

(define (do-c-trailer fn wr)
  (let* ((bn (TextUtils.RemoveSuffixes fn '(".c" ".h")))
         (sfx (string->symbol
               (TextUtils.RemovePrefix   fn (sa bn ".")))))
    (case sfx
      ((c) #t)
      ((h)
       (dis "#endif /* !_"bn"_H */" dnl
            wr)))))

(define (do-c-meta-output lst)
  (do-c-output lst *m3-lib-name*))

(define (do-m3-output lst odir libnm c-files)
  (let* ((files (uniq-string-list (map car lst))) ;; uniq the filenames
         (wrs   (map (lambda(fn) (list fn (FileWr.Open (sa odir "/src/" fn)) (make-string-set 10))) files))
         (m3mwr (FileWr.Open (sa odir "/src/m3makefile")))
         (m3owr (FileWr.Open (sa odir "/src/m3overrides")))
         )
    (define (dis-import i-l-nm)
      (if (not (equal? i-l-nm libnm)) (dis "import(\"" i-l-nm "\")" dnl m3mwr)))

    (dis-import "libm3")
    (dis-import "libbuf")
    (dis-import *m3-common-output-dir*)
    (dis "SYSTEM_CC = SYSTEM_CC & \" -I../../../../../c -std=gnu99\"" dnl m3mwr)

    (dis "override(\"" *m3-common-output-dir* "\",\"../..\")" dnl m3owr)

    ;; side effects :-(
    (map (lambda (o)
           (let* ((fn (car o))
                  (im (cadr o))
                  (x (assoc fn wrs)))
             (set-cdr! (cdr x) (list ((caddr x) 'union im)))))
         lst)

    (map (lambda(w)
           ;; do one output file at a time ---
           (let* ((fn  (car w))
                  (wr  (cadr w))
                  (im  (caddr w))
                  (mn  (TextUtils.RemoveSuffixes fn '(".m3" ".i3") )) ;; compute module name
                  (sfx (string->symbol
                        (TextUtils.RemovePrefix   fn (sa mn "."))))) ;; and suffix :-)
             (dis (case sfx
                    ((i3) (dis "Interface(\"" mn "\")" dnl m3mwr)      "INTERFACE ")
                    ((m3) (dis "implementation(\"" mn "\")" dnl m3mwr) "MODULE "))
                  mn ";" dnl dnl
                  wr)

             (map (lambda (i) (if (not (equal? i mn)) (dis "<*NOWARN*>IMPORT " i ";" dnl wr)))
                  (append '("Fmt" "Word" "Wx") (im 'keys)))

             (dis dnl wr)

             (let ((matches (filter (lambda (q)(equal? (car q) fn)) lst)))
               (map (lambda (c) (dis c dnl dnl wr)) (map caddr matches))
               )
             
             (dis (case sfx ((i3) "END ") ((m3) "BEGIN END ")) mn "." dnl ;; EOM
                  wr)
             ))
        wrs)

    (map Wr.Close (map cadr wrs))

    (map (lambda(fn)
           (let* ((bn (TextUtils.RemoveSuffixes fn '(".c" ".h")))
                  (sfx (string->symbol
                        (TextUtils.RemovePrefix   fn (sa bn ".")))))
             (dis sfx "_source(\"" bn "\")"dnl m3mwr)))
         c-files)
             


    (dis "library(\"" libnm "\")" dnl m3mwr)
    (Wr.Close m3mwr)
    (Wr.Close m3owr)
    wrs
    ))

(define (do-m3-meta-output lst c-files)
  (do-m3-output lst *m3-lib-name* *m3-lib-name* c-files))

(define (do-m3-common-output lst c-files)
  ;;(dis "c-files : " c-files dnl)
  (do-m3-output lst *m3-common-output-dir* *m3-common-output-dir* c-files))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *uint-high* 64)

(define (m3-uint-type n)
  (if (= 64 n) "Word.T"
      (sa "[ 0..Word.Minus(Word.LeftShift(1,"n"),1) ]")))

(define (c-uint-type n)
  (cond ((> 32 n) "unsigned long int")
        ((> 16 n) "unsigned int")
        ((> 8 n) "unsigned short int")
        (else "unsigned char")))
          
(define (make-1-uint n)
  (let* ((tn (sa "UInt"n))
         (ser-proto  (m3-deser-proto 'ser (sa "Serialize"n) 1 "" tn))
         (deser-proto (m3-deser-proto 'deser (sa "Deserialize"n) 1 "" tn))
         (fmt-proto (m3-deser-proto 'fmt (sa "FormatWx"n) 1 "" tn))
         (i-import-set (make-string-set 10)))
    (list 
     (list "UInt.i3" i-import-set
           (sa
            "TYPE UInt"n" = "(m3-uint-type n) ";" dnl
            ser-proto ";" dnl
            deser-proto ";" dnl
            fmt-proto ";" dnl
            dnl
            )
           )
     (list "UInt.m3" i-import-set
           (sa
            ser-proto "=" dnl
            "  BEGIN s[0] := z END Serialize"n";" dnl
            dnl
            deser-proto "=" dnl
            "  BEGIN z := s[0] END Deserialize"n";" dnl
            dnl
            fmt-proto "=" dnl
            "  BEGIN Wx.PutText(wx, \"16_\" & Fmt.Unsigned(z)) END FormatWx"n";" dnl
            dnl
            )
          )
     )
    )
  )

(define (make-uints)
  (let loop ((i     *uint-high*)
             (res   '()))
    (if (= i 0)
        res
        (loop (- i 1) (append res (make-1-uint i))))))

(define (make-builtins)
  (let* ((n "BOOLEAN")
         (tn n)
         (ser-proto  (m3-deser-proto 'ser (sa "Serialize"n) 1 "" tn))
         (deser-proto (m3-deser-proto 'deser (sa "Deserialize"n) 1 "" tn))
         (fmt-proto (m3-deser-proto 'fmt (sa "FormatWx"n) 1 "" tn)))
  (list
   (list "WmDeSer.i3"
         *empty-set*
         (sa
          ser-proto ";" dnl
          deser-proto ";" dnl
          fmt-proto ";" dnl
          dnl
          )
         )
   (list "WmDeSer.m3"
         *empty-set*
         (sa
            ser-proto "=" dnl
            "  BEGIN s[0] := ORD(z) END Serialize"n";" dnl
            dnl
            deser-proto "=" dnl
            "  BEGIN z := VAL(s[0],"tn") END Deserialize"n";" dnl
            dnl
            fmt-proto "=" dnl
            "  BEGIN Wx.PutText(wx, Fmt.Bool(z)) END FormatWx"n";" dnl
            dnl
            )
          )
     )
    )
  )
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-uint-type-names)
  (let loop ((i 64)
             (res '()))
    (if (= i 0)
        res
        (loop (- i 1) (cons (sa "uint" i) res)))))

(define *a* '())
(define *p* '())
(define *c* '())

(define (make-c-common-protos type)
  (map (lambda(whch)(sa (c-deser-proto-for-type whch type "*t") ";"))
       '(ser deser)))

(define (make-c-common-code type)
  (map (lambda(whch)
         (sa (c-deser-proto-for-type whch type "*t") dnl
             "{" dnl
             (case whch ((ser)   "  *s = *t;") ((deser) "  *t = *s;")) dnl
             "}" dnl
             )) '(ser deser)))

(define (do-c-common-output)
  (let* ((all-types (cons "fm_bool" (make-uint-type-names)))
         (protos (map (lambda(txt)(list (sa *c-builtin-serdes-fn* ".h") #f txt))
                      (apply append (map make-c-common-protos all-types))))
         (codes (map (lambda(txt)(list (sa *c-builtin-serdes-fn* ".c") #f txt))
                     (apply append (map make-c-common-code all-types)))))
                     
    (do-c-output (append codes protos) *m3-common-output-dir*)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-it)
  (let ((c-files (do-c-meta-output (compile 'c))))
    (do-m3-meta-output (compile 'm3) c-files)
    )
  (let ((c-files (do-c-common-output)))
    ;;(dis "do-it c-files : " c-files dnl)
    (do-m3-common-output (append (make-uints) (make-builtins)) c-files)
    )
  )
