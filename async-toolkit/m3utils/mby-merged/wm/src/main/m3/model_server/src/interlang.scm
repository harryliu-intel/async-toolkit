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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (sa *m3-proj*  (scheme->m3 (cadr typedef)) ".m3"))
  
(define (compile-m3-typedef-def x defs)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (let* ((imports (make-string-set 10))
         (code (sa "TYPE T = " (gen-m3-type-use (caddr x) defs imports) ";")))
    (list (m3-intf-nm x) imports code)
    )
  )

(define *empty-set* (make-string-set 1))

(define (compile-m3-typedef-serial-size x defs)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (list (m3-intf-nm x) *empty-set* (sa "CONST SerialSize = " (get-type-field-cnt (caddr x) defs) ";")))

(define (m3-deser-name whch)
  (case whch
    ((ser)   "Serialize")
    ((deser) "Deserialize")
    ((fmt)   "FormatWx")
    (else (error))))

(define (m3-deser-uint-pname b whch)
  (sa *m3-uint-intf* "." (m3-deser-name whch) (number->string b)))

(define (m3-deser-builtin-pname x whch)
   (let ((b-test (assoc x *builtins*)))
                   (if b-test
                       (sa "WmDeSer." (m3-deser-name whch) (get-m3-name (cadr b-test)))
                       #f)))
           
(define (m3-deser-typedef-pname x whch)
  (let ((r-test (sym-lookup x defs)))
    (if (and r-test (eq? 'typedef (car r-test)))
        (let ((intf (sa *m3-proj* (get-m3-name r-test))))
          (sa intf "." (m3-deser-name whch)))
        #f)))

(define (compile-m3-typedef-deser-proto td defs whch semi)
  (if (not (eq? (car td) 'typedef)) (error "not a typedef : " td))
  (let ((x          (caddr td))
        (proc-name  (m3-deser-name whch)))
    ;; depending what the typedef is, we do different things
    ;; if its just an alias, make it a CONST def, relating to the base type
    ;; else provide the standard proto
    (list
     (m3-intf-nm td)
     *empty-set* 
     (cond ((number? x) 
            (sa "CONST " proc-name " = " (m3-deser-uint-pname x whch) semi))

           ((let ((builtin-name (m3-deser-builtin-pname x whch)))
              (if builtin-name (sa "CONST "proc-name" = " builtin-name ";") #f)))

           ((let ((td-name (m3-deser-typedef-pname x whch)))
              (if td-name (sa "CONST "proc-name" = " td-name ";") #f)))
           
           ;; must be type expression
           ((not (pair? x)) '*not-found*)
           
           ((eq? (car x) 'bits)
            (compile-m3-typedef-deser-proto (force-value (cadr x) defs) defs whch))

           (else ;; a compound type of some kind (struct or array)
            (sa "PROCEDURE " (m3-deser-name whch) "("
                (case whch
                  ((ser)   "VAR s : ARRAY[0..SerialSize-1] OF Word.T; READONLY t : T")
                  ((deser) "READONLY s : ARRAY[0..SerialSize-1] OF Word.T; VAR t : T")
                  ((fmt)   "wx : Wx.; READONLY t : T")
                  )
                ")" semi))
           ) ;;dnoc
     ) ;;tsil
    ) ;;tel
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ser/deser/fmt
;;;

(define (call-m3-deser-code whch obj strm-idx)
  (case whch
    ((ser deser) (sa "(s[" strm-idx "], " obj ")"))
    ((fmt)       (sa "(wx, "obj")"))
    ))

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

(define (make-m3-deser-code whch x defs lhs lev ind p tag)
  (cond
   ((number? x) (sa (m3-tag-deser-code whch ind tag) ind (m3-deser-uint-pname x whch) (call-m3-deser-code whch lhs p)))

   ((let ((builtin-name (m3-deser-builtin-pname x whch)))
      (if builtin-name (sa (m3-tag-deser-code whch ind tag) ind builtin-name (call-m3-deser-code whch lhs p)) #f)))
   
   ((let ((td-name (m3-deser-typedef-pname x whch)))
      (if td-name (sa (m3-tag-deser-code whch ind tag) ind td-name (call-m3-deser-code whch lhs p)) #f)))
   
   ((not (pair? x)) (error "cant de/ser " x))
    
   ((eq? (car x) 'bits)
    (make-m3-deser-code whch (force-value (cadr x) defs) lhs lev ind p tag))
   
   ((eq? (car x) 'array)
    (sa
     ind "FOR i"lev" := 0 TO " (force-value (cadr x) defs) "-1 DO" dnl
     ind "  WITH t"lev " = " lhs "[i"lev"], " dnl

     (case whch
       ((ser deser) (sa
     ind "       o"lev " = " p" + i"lev" * " (get-type-field-cnt (caddr x) defs) " DO" dnl))
     
       ((fmt) (sa
     ind "       tag"lev " = F(\"%s[%s]\","(if (= lev 0) (if tag tag "\"\"") (sa "tag"(- lev 1)))", Int(i"lev")) DO" dnl)))

          (make-m3-deser-code whch (caddr x) defs (sa "t"lev) (+ lev 1) (sa ind "    ") (sa "o"lev) (sa "tag" lev)) dnl

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
  (let ((imports    (make-string-set 10))
        (x          (caddr td))
        (proc-name  (m3-deser-name whch)))
    (list
     (m3-modu-nm td)
     imports
     (cond ((not (pair? x)) "") ;; base type, no need for code generation

           ((or (eq? (car x) 'array)
                (eq? (car x) 'struct))
            (sa (caddr (compile-m3-typedef-deser-proto td defs whch " =")) dnl
                "  BEGIN" dnl

                (make-m3-deser-code whch x defs "t" 0 "  " 0 "")
                
                "  END " proc-name ";" dnl
                ) ;; as
            )

           (else "")

           );; dnoc
          );;tsil
    );;tel
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
         (code (sa "CONST " (scheme->m3 (cadr x)) " = " (gen-m3-val-use (caddr x) defs imports) ";")))
    (list
     (list (sa *m3-proj* *m3-const-intf* ".i3")
           imports
           code)
     )
    )
  )

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
  (if (not (eq? (car x) 'constant)) (error "not a constant : " x))

  (let* ((nm         (sa *c-const-pfx* (get-c-name (sym-lookup (cadr x) defs))))
         (imports    (make-string-set 10))
         (code       (sa "#define " nm "    " (gen-c-val-use (caddr x) defs))))
    (list (list nm code) )
    ))

(define (scheme-mem->c sym)
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'LCamel
                    'Hyphen 'None))

(define (compile-c-typedef-def-from-data x nm defs)
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
                      (sa *c-proj* "_" (get-c-name r-test) def-sfx)
                      #f))))

          ;; must be type expression
          ((not (pair? x)) '*not-found*)

          ((eq? (car x) 'bits)
           (compile-c-typedef-def-from-data (force-value (cadr x) defs) nm defs))

          ((eq? (car x) 'array)
           (compile-c-typedef-def-from-data (caddr x) (sa nm "[" (force-value (cadr x) defs) "]") defs))
                
          ((eq? (car x) 'struct)
           (apply sa
                  (append
                   (list "struct {" dnl)
                   (map
                    (lambda (fspec) (sa "  " (compile-c-typedef-def-from-data (cadr fspec) (scheme-mem->c (car fspec)) defs) ";" dnl))
                    (cadr x))
                   (list "} " nm))))
          
          (else '*not-found*)))
  )

(define (compile-c-typedef-def x defs)
  (if (not (eq? (car x) 'typedef)) (error "not a typedef : " x))
  (sa "typedef " (compile-c-typedef-def-from-data
   (caddr x)
   (sa *c-proj* "_" (get-c-name (sym-lookup (cadr x) defs)))
   defs) ";"
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c-deser-name whch)
  (case whch
    ((ser) "serialize")
    ((deser) "deserialize")
    (else (error))))

(define (c-deser-uint-pname b whch)
  (sa (c-deser-name whch) "_uint" (number->string b)))

(define (c-deser-builtin-pname x whch)
   (let ((b-test (assoc x *builtins*)))
                   (if b-test
                       (sa (c-deser-name whch) "_" (get-c-name (cadr b-test)))
                       #f)))
           
(define (c-deser-typedef-pname x whch)
  (let ((r-test (sym-lookup x defs)))
    (if (and r-test (eq? 'typedef (car r-test)))
        (sa *c-proj* "_" (get-c-name r-test) "_" (c-deser-name whch))
        #f)))

(define (compile-c-typedef-deser-proto td defs whch semi)
  (if (not (eq? (car td) 'typedef)) (error "not a typedef : " td))
  (let ((tn (sa *c-proj* "_" (get-c-name (sym-lookup (cadr td) defs)))))
    ;; this is a bit different from the M3 since there are no const
    ;; functions in C
    (list
     ""
     (sa "void " (c-deser-name whch) "_" tn "("
         (case whch
           ((ser)   (sa "uint64 *w, const " tn " *t"))
           ((deser) (sa "const uint64 *w, " tn " *t"))
           )
         ")" semi)
     ) ;;tsil
    ) ;;tel
  )

(define (call-c-deser-code whch obj strm-idx)
  (case whch
    ((ser deser) (sa "(&(s[" strm-idx "]), &(" obj "))"))
    ))

(define (make-c-deser-code whch x defs lhs lev ind p)
  (cond
   ((number? x)
    (sa ind (c-deser-uint-pname x whch) (call-c-deser-code whch lhs p) ";" dnl))

   ((let ((dsname (c-deser-builtin-pname x whch)))
      (if dsname (sa ind dsname (call-c-deser-code whch lhs p) ";" dnl) #f)))
   
   ((let ((dsname (c-deser-typedef-pname x whch)))
      (if dsname (sa ind dsname (call-c-deser-code whch lhs p) ";" dnl) #f)))
   
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
     ""
     (sa
      (cadr (compile-c-typedef-deser-proto td defs whch "")) dnl
      "{" dnl
      (make-c-deser-code whch x defs "t" 0 "  " 0)
      "}" dnl
      )
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-c-typedef x defs)
  (list
   ;; these should be directed to a header file
   (compile-c-typedef-def x defs)
   (compile-c-typedef-deser-proto x defs 'ser ";")
   (compile-c-typedef-deser-proto x defs 'deser ";")

   ;; and these to an implementation
   (compile-c-typedef-deser-code x defs 'ser)
   (compile-c-typedef-deser-code x defs 'deser)
   
   ))

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
