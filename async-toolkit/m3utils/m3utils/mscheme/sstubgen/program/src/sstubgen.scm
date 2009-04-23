;;
;; $Id$
;;

;;
;; Process Modula-3 interfaces in Scheme
;;

(require-modules "basic-defs" "display" "hashtable" "struct" "set" 
                 "pregexp/pregexp")

(define (make-type-name interface name) (cons interface name))

(define (extract-field  field-name type)
  (let ((ass (assoc field-name type)))
    (if (not (pair? ass))
        (error "No assoc for " field-name " in " type)
        (cdr ass))))

(define (have-field? field-name type)
  (assoc field-name type))

(define (type-type type) (car type))

(define (get-alias type) 
  (if (not (have-field? 'alias type)) '()
      (let ((qid (extract-field 'alias type)))
        (if (null? qid) 
            '()
            (cleanup-qid qid)))))

(define (get-name type) 
  (if (not (have-field? 'name type)) '()
      (let ((qid (extract-field 'name type)))
        (if (null? qid) 
            '()
            (cleanup-qid qid)))))

(define (cleanup-qid qid)
  (if (= 0 (string-length (symbol->string (extract-field 'intf qid))))
      (cons '() (extract-field 'item qid))
      (cons (extract-field 'intf qid)
            (extract-field 'item qid))))

(define (string-type-alias type sep env)
  (let ((tn (get-alias type)))
    (stringify-qid tn sep env)))

(define (string-type-name type sep env)
  (let ((tn (get-name type)))
    (stringify-qid tn sep env)))

(define (stringify-qid tn sep env)
  (if (not (pair? tn))
      (error "stringify-qid : not a pair : " tn))

  (if (not (or (null? (car tn)) (symbol? (car tn))))
      (error "stringify-qid : car is not a symbol or null : " tn))

  (if (not (symbol? (cdr tn)))
      (error "stringify-qid : cdr is not a symbol : " tn))

  (if (or (null? (car tn))
          ((env 'get 'exports) 'member? (car tn)))
      (symbol->string (cdr tn))
      (begin
        ((env 'get 'imports) 'insert! (car tn))
        (string-append (symbol->string (car tn))
                       sep
                       (symbol->string (cdr tn))))))

(define (find-type-alias named types)
  (car 
   (filter (lambda (t) (equal? (get-alias t) named))
           types)))

(define (find-named-element named lst)
  (let ((matches (filter 
                  (lambda (t) (equal? (extract-field 'name t) named))
                  lst)))
    (if (null? matches) 
        (error "find-named-element: no matches : " named " " lst))
    (if (not (null? (cdr matches)))
        (error "find-named-element: multiple matches : " named " " lst))
    (car matches)))

;; subranges
(define (base-type-type type) (type-type (base-type type)))

(define (base-type type)
  (if (not (eq? (car type) 'Subrange))
      (error "cant get base type of " (car type))
      (let ((base (extract-field 'base type)))
        (cond ((eq? (type-type base) 'UserDefined) base)
              ((eq? type base)       base) ;; integer
              (else (base-type base))))))

(define (infixize string-list sep)
  (if (null? string-list) ""
      (accumulate 
       (lambda (x y) (string-append y sep x))
       (car string-list)
       (reverse (cdr string-list)))))


(define (formals-from-sig sig)
  (filter (lambda(f) (and (pair? f) (eq? (car f) 'Formal)))
          (assoc 'formals sig)
          ))

(define (format-formal-param f)
  (symbol->string (extract-field 'name f)))

(define (format-formal-decl f env)
  (let ((name (extract-field 'name f))
        (mode (extract-field 'mode f))
        (type (extract-field 'type f)))
    (string-append
     (case mode
       ((Mode.Var) "VAR ")
       ((Mode.Value) "VALUE ")
       ((Mode.Readonly) "READONLY ")
       (else (error "unknown mode " mode)))
     (symbol->string name)
     " : "
     (format-type type env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                            ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;       FORMAT TYPES         ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                            ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bad-type '())

(define (format-object-super t env)
  (let ((super (extract-field 'super t)))
    (if (null? super) "" (format-type super env))))

(define (format-branding t)
  (if (extract-field 'branded t)
      (string-append " BRANDED " 
                     (let ((brand (extract-field 'brand t)))
                       (if (null? brand) "" (string-append "\"" brand "\""))))
      ""))

(define (format-field f env)
  (let ((type (extract-field 'type f)))
    (string-append
     (extract-field 'name f)
     " : "
     (format-type type env)
     (let ((default (extract-field 'default f)))
       (if (not (null? default))
           (string-append " := " (format-type-value type default env))
           "")))))

(define (format-fields t env)
  (apply string-append 
         (map 
          (lambda(fld) (string-append "    " (format-field fld env) ";" dnl))
          (extract-field 'fields t))))

(define (extract-and-format-qid d env)
  (let* ((qid (extract-field 'qid d))
         (intf (extract-field 'intf qid))
         (item (extract-field 'item qid)))
    (stringify-qid (cons intf item) "." env)))

(define (format-formal f env) 
  (let ( (mode (extract-field 'mode f)) )
    (string-append
     (case mode
       ((Mode.Var) "VAR ")
       ((Mode.Value) "VALUE ")
       ((Mode.Readonly) "READONLY ")
       (else (error "unknown mode " mode)))
		 (format-field f env))))

(define (format-exceptions x env)
  (infixize (map (lambda(xx)(extract-and-format-qid xx env)) x) ", "))

(define (format-parenthesized-signature sig env . optional-me)

  ;; format a procedure (or method) signature
  (string-append 
   "("
   (infixize  (append optional-me 
                      (map (lambda(x)(format-formal x env)) 
                           (extract-field 'formals sig))) "; ")
   ")"

   (let ((result (extract-field 'result sig)))
     (if (null? result) ""
         (string-append " : " (format-type result env))))

   (let ((raises (extract-field 'raises sig)))
     ;; note the distinction here.
     ;; raises null : raises ANY
     ;; a null list : raises {}
     ;; else.. the exceptions listed
     (cond ((null? raises) " RAISES ANY")
           ((and (pair? raises) (null? (car raises))) "")
           (else (string-append " RAISES { "
                                (format-exceptions (car raises) env)
                                " }"))))))

(define (format-method m env)
  (let ((name (extract-field 'name m))
        (sig (extract-field 'sig m))
        (default (extract-field 'default m)))
    (string-append "    " name (format-parenthesized-signature sig env)
                   (if (not (null? default)) 
                       (string-append " := " (extract-and-format-qid default env) )
                       ""))
    ))

(define (format-object-methods t env)
  (apply string-append 
         (map 
          (lambda(f) (string-append (format-method f env) ";" dnl))
          (extract-field 'methods t))))

(define (format-override m env)
  (let ((name (extract-field 'name m))
        (default (extract-field 'default m)))
    (string-append "    " name " := " (extract-and-format-qid default env) )

    ))

(define (format-object-overrides t env)
  (apply string-append 
         (map 
          (lambda(f) (string-append (format-override f env) ";" dnl))
          (extract-field 'overrides t))))

(define (unprotected-format-type t env)
  (cond ((is-basetype t) => 
                         (lambda(x)(string-append " " (symbol->string x) " ")))
        ((have-field? 'alias t) (string-type-alias t "." env))
        ((and (have-field? 'name t)(not (null? (extract-field 'name t)))) 
         (string-type-name t "." env))
        (else 
         (case (car t)
           ((Ref)
            (string-append " REF " (type-formatter 
                                    (extract-field 'target t)
                                    env
                                    )))

           ((Subrange)
            (let ((base (type-formatter (extract-field 'base t) env)))
              (string-append "[ VAL("(cdr (extract-field 'min t))  "," base ")"
                             ".."
                             "VAL("(cdr (extract-field 'max t))  "," base ") ]")
              ))

           ((OpenArray)
            (string-append 
             " ARRAY OF " 
             (type-formatter (extract-field 'element t) env)
             ))
           
           
           ((Array)
            (string-append
             " ARRAY " 
             (type-formatter (extract-field 'index t) env)
             " OF "
             (type-formatter (extract-field 'element t) env)
             )
            )

           ((Set)
            (string-append
             " SET OF " (type-formatter (extract-field 'range t) env)))

           ;; do we really want CHAR and WIDECHAR here?
           ((Char) " CHAR")
           
           ((WideChar) " WIDECHAR")

           ((Object)
            (string-append 
             (format-object-super t env)
             (format-branding t)
             " OBJECT" dnl
             (format-fields t env)
             " METHODS" dnl
             (format-object-methods t env)
             " OVERRIDES" dnl
             (format-object-overrides t env)
             " END" dnl))

           ((UserDefined) 
            (string-append " { " 
                           (infixize (extract-field 'elts t) ", ")
                           " } " dnl))

           ((Record)
            (string-append
             " RECORD " dnl
             (format-fields t env)
             " END" dnl))

           ((Procedure)
            (string-append
             " PROCEDURE"
             (format-parenthesized-signature (extract-field 'sig t) env)))

           ((Opaque)
            (string-append
             " (* <: *) " (format-branding t) dnl
             (type-formatter (extract-field 'revealedSuperType t) env) dnl))

           (else (error "format-type: dont know how to format " t)))))
  )

(define (strip-names type)
  (filter-out '(alias name) type))

(define (format-type t env)
  (unwind-protect
   (unprotected-format-type t env)
   '() ;; finally-clause
   (begin
     (set! bad-type t) ;; remember for debugging
     (error "format-type caught error formatting " t dnl dnl))))

(define type-formatter format-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                            ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;       FORMAT VALUES        ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                            ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define bad-values '())

(define (is-reference-type? type)
	(member? (car type) '(Opaque Ref Procedure Object)))

(define (format-type-value type value env)
  (case (car value)

    ((Ordinal) 
		 ;; special case for NIL: (Ordinal . 0)
		 
		 (if (is-reference-type? type)
				 (if (not (= (cdr value) 0))
						 (error "Ordinal initializer for Ref, non-zero value : " type 
										", " value)
						 " NIL")
				 (string-append 
					" VAL(" 
					(number->string (cdr value))", "  (type-formatter type env)
					")")))
    
    ((LongFloat)
     (string-append
      " FLOAT("
      (number->LONGREAL (cdr value))
      ", "
      (type-formatter type env)
      ")"
      ))

    ((Proc) (stringify-qid (cleanup-qid (cdr value))
                           "."
                           env))

    ((Set)
     (string-append
      " " (type-formatter type env)
      " { "

      (infixize 
       (map (lambda(m)(format-set-member m type env)) (cdr value))
       ", ")

      " } "))
    
    ((Txt)
     (string-append "\"" (cdr value) "\""))

    ((ArrayOrRecord)

     (string-append
      " " (type-formatter type env)
      " { "
      (infixize
       (let loop ((i 0)
                  (elements (cdr value)))
         (if (null? elements) 
             '()
             (cons (format-element (car elements) type i env)
                   (loop (+ i 1) (cdr elements) ))))
       ", ")
      " } " dnl))

    (else
     (set! bad-values (cons (cons type value) bad-values))
     "**NOTYET**")
    ))

(define (format-set-member m set-type env)
  ;; format the initializer for a set member
  (format-type-value (extract-field 'range set-type) m env))

(define (format-element e container-type index env)
  ;; format the initializer of a single item in an ARRAY or RECORD
  ;; dispatches on the container type
  (case (car container-type)

    ((Record) 
     (format-record-element e container-type index env))

    ((OpenArray Array) 
     (format-array-element e container-type index env))
    
    (else (error "cant format element of " (car container-type)))))

(define (format-array-element e container-type index env)
  ;; format the initializer for an array element
  (case (car e)
    ((Propagate) " .. ")
    
    ((Range) 
     (let* ((elem-valu (cdr e))
            (elem-type (extract-field 'element container-type)))
       (string-append 
        " "
        (format-type-value elem-type elem-valu env)
        )))
    
    (else (error "unknown element type " (car e)))
    ))


(define (format-record-element e container-type index env)
  ;; format the initializer for a record member
  (case (car e)
    ((Actual) 
     (let* ((field-name (cadr e))
            (field-valu (caddr e))
            (field-desc (find-named-element 
                         field-name (extract-field 'fields container-type)))
            (field-type (extract-field 'type field-desc)))
       (string-append
        " "
        field-name
        " := " 
        (format-type-value field-type field-valu env)
        
        )
       
       ))
    
    ((Range) 
     (let* ((field-valu (cdr e))
            (field-desc (nth (extract-field 'fields container-type) index))
            (field-type (extract-field 'type field-desc)))
       (string-append 
        " "
        (format-type-value field-type field-valu env)
        )))
    
    (else (error "unknown element type " (car e)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filter-out keys struct)
  (filter
   (lambda (x) (or (not (pair? x))(not (memq (car x) keys))))
   struct))

(define (lp xx)
  ;; "line printing"

  (begin 
    (map (lambda(x) (dis x dnl)) xx) 
    #t))

(define (lp2 xx)
  (begin
    (map (lambda(x) (begin (lp x)(dis "----------------------" dnl)) #t) xx)
    #t
    ))

(define (reload) (load "/home/mika/t/mscheme/sstubgen/program/src/sstubgen.scm"))

(set-warnings-are-errors! #t)

(define (make-pair-hash-table size) ;; silly
  (define (pair-hash p) 0)
  (make-hash-table size pair-hash))

(define env-type 
  (make-struct-type
   'env-type
   `((exports       ,(lambda()(make-symbol-set 100))) 
     ;; interfaces we export

     (imports       ,(lambda()(make-symbol-set 100))) 
     ;; interfaces we need to import

     (convert->m3-req          ,(lambda()(make-symbol-hash-table 100))) 
     ;; converters->m3 requested

     (convert->scm-req          ,(lambda()(make-symbol-hash-table 100))) 
     ;; converters->scm requested

     (convert-blt          ,(lambda()(make-symbol-set 100))) 
     ;; converters built

     (procedure-call-stubs ,(lambda()(make-pair-hash-table 100))) 
     ;; procedure call stubs built

     )))

(define global-env (env-type 'new))

(define (global-format t)
  (begin
    (set! global-env (env-type 'new))
    (format-type t global-env)
    ))

(define (init-symbol-set . syms)
  (let ((res (make-symbol-set 100)))
    (map (lambda(x)(res 'insert! x)) syms)
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-const-value c env) ;; format constant from global env 
  (format-type-value
   (extract-field 'type c)
   (extract-field 'value c)
   env
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test code
(set! type-formatter unprotected-format-type)
;;(map global-format (map strip-names the-types))

(define (m3type->m3identifier s)
	;;
	;; turn a legal Modula-3 type specifier into a legal Modula-3 identifier
	;; (by hook or by crook)
	;;
  (list->string
   (filter 
    (lambda (c) (or (char=? c #\_)
                    (char-alphabetic? c)
                    (char-numeric? c)))
    (map 
     (lambda (c) (cond ((char=? c #\.) #\_)
                       (else c)))
     (string->list s)))))

(define (to-scheme-proc-name type env)
	;;
  ;; if it's a base type, we call the base type conversion library
  ;; else we make the converter ourselves.
	;;
  (if (is-basetype type)
      (begin
        (stringify-qid
         (cons 'SchemeModula3Types
               (string->symbol (string-append "ToScheme_" (is-basetype type))))
         "."
         env))
      (let ((convert-req (env 'get 'convert->scm-req))
            (res 
             (string-append "ToScheme_" 
                            (m3type->m3identifier (type-formatter type env)))))
        (convert-req 'update-entry! (string->symbol res) type)
        res)))

(define (is-basetype type)
  ;; returns string name of basetype or #f

	(define (is-opaque-basetype)
		;; special handling for opaques, by name
		(let loop ((p the-basetypes))
			(cond ((null? p) #f)
						((equal? (cdar p) type) (caar p))
						(else (loop (cdr p))))))

	(if (eq? 'Opaque (car type))
			(is-opaque-basetype)
			(let ((unnamed-type (strip-names type)))
				(let loop ((p the-basetypes))
					(cond ((null? p) #f)
								((equal? (strip-names (cdar p)) unnamed-type) (caar p))
								(else (loop (cdr p))))))))
  
(define (string-quote s) (string-append "\"" s "\""))

(define (make-modules name              ;; name of interface
                      interface-decls   ;; stuff for .i3
                      module-decls      ;; stuff for .m3
                      module-code       ;; initialization block
                      env)
  (if (not (env 'get 'exports) 'member? name)
      (error "not member of exports : " name))
  (string-append
   "MODULE " name ";" dnl
   (apply string-append
          (map (lambda (i) 
                 (string-append 
                  "IMPORT " i ";" dnl))
                 ((env 'get 'imports) 'keys)))
   dnl
   module-decls
   dnl
   "BEGIN" dnl
   module-code 
   "END " name "." dnl))
      
  
  

(define (make-to-scheme type env)

  (define (push-make type)(to-scheme-proc-name type env))

  (let* ((m3tn                       ;; type name
          (type-formatter type env))

         (m3ti                       ;; identifier mangled from tn
          (m3type->m3identifier (type-formatter type env)))

         (pname (to-scheme-proc-name type env))

         (imports (env 'get 'imports))

         (proto (string-append
                 "PROCEDURE " pname "(READONLY x : " m3tn 
								 ") : SchemeObject.T RAISES {"
								 (if (member? (car type) '(Ref Procedure Object Opaque))
										 ""
										 " Scheme.E")
								 " }"
                       )
                )
         )

    ((env 'get 'convert-blt) 'insert! (string->symbol pname))
    ;; remember that we're building pname, so we dont try to
    ;; build it recursively
    

    (imports 'insert! 'SchemeObject) 
    (imports 'insert! 'Scheme)

    (define (make-intf) (string-append proto ";"))

    (define (make-impl)
      (string-append
       proto " = " dnl
       "  BEGIN" dnl

       "    "
       (case (car type)

         ((Ref)
          ;; canonical rep. of a Modula-3 ref is just the ref itself
          ;;        (let* ((target (extract-field 'target type))
          ;;               (target-pname (push-make target)))
          ;;          (string-append 
          ;;           "RETURN " target-pname "(x^)")))
          "RETURN x")
         
         
         ((Record)
          (map 
           (lambda(i)(imports 'insert! i))
           '(SchemeUtils SchemePair SchemeUtils))

          (let ((fields (extract-field 'fields type)))

            (define (format-field f)
              (let* ((field-name (extract-field 'name f))
                     (field-type (extract-field 'type f))
                     (field-pname (push-make field-type)))
                (string-append 
                 "      res := SchemeUtils.Cons(SchemeUtils.Cons(SchemeSymbol.FromText(\"" field-name "\")," field-pname "(x." field-name ")))")))

            (string-append
             "VAR res : SchemePair.T:=NIL; BEGIN" dnl 
             (infixize (map format-field fields) (string-append ";" dnl)) ";" dnl
             "      RETURN res" dnl
             "    END" 
             )

            )
          )

         ((UserDefined) 
          (let ((elts (extract-field 'elts type)))
            (define (elts-names) 
              (infixize (map string-quote (map symbol->string elts)) ", ")
              )

            (imports 'insert! 'SchemeSymbol)
            (string-append 
             "CONST" dnl
             "      Name = ARRAY " m3tn " OF TEXT { " (elts-names) " }; " dnl
             "    BEGIN" dnl
             "      RETURN SchemeSymbol.FromText(Name[x])" dnl
             "    END" 
             )
            )
          )

         ((Subrange)
          (let* ((base (extract-field 'base type))
                 (base-pname (push-make base)))
            (string-append
             "RETURN " base-pname "(x)" 
             )
            )
          )

         ((Array)
          (let* ((element (extract-field 'element type))
                 (element-pname (push-make element))
                 (index (extract-field 'index type))
                 (index-pname (push-make index))
                 )
            (imports 'insert! 'SchemePair)
            (imports 'insert! 'SchemeUtils)
            (string-append
             "VAR res : SchemePair.T:=NIL; BEGIN" dnl 
             
             "      FOR i := LAST(x) TO FIRST(x) BY -1 DO" dnl
             "        res := SchemeUtils.Cons(SchemeUtils.Cons(" index-pname "(i)," element-pname "(x[i])), res)" dnl
             "      END;" dnl
             "      RETURN res"
             "    END"))
          

          )

         ((OpenArray)
          (let* ((element (extract-field 'element type))
                 (element-pname (push-make element)))
            (imports 'insert! 'SchemePair)
            (imports 'insert! 'SchemeUtils)
            (string-append
             "VAR res : SchemePair.T:=NIL; BEGIN" dnl 
             "      FOR i := LAST(x) TO FIRST(x) BY -1 DO" dnl
             "        res := SchemeUtils.Cons(" element-pname "(x[i]), res)" dnl
             "      END;" dnl
             "      RETURN res" dnl
             "    END"))
          
          )

         ((Set)
          (let* ((range-type (extract-field 'range type))
                 (range-name (type-formatter range-type env))
                 (range-pname (push-make range-type)))
            (string-append
             "VAR res : SchemePair.T:=NIL; BEGIN" dnl
             "      FOR i := FIRST("range-name") TO LAST("range-name") DO" dnl
             "        IF i IN x THEN" dnl
             "          res := SchemeUtils.Cons(" range-pname "(i), res)" dnl
             "        END" dnl
             "      END;" dnl
             "      RETURN res" dnl
             "    END"
             )
            )
          )

        ;; ((Char) 
        ;;  (imports 'insert! 'SchemeChar)
        ;;  "RETURN SchemeChar.Character(x)" )

         ((Procedure) 
          "RETURN x" ;; wonder if this actually works
          )

         ((Object Opaque)
          "RETURN x" ;; just let it represent itself
          )

         (else (error "unknown type header " (car type)))

         )               
       dnl

       "  END " pname ";" dnl
       )
      )
    
    (cons (make-intf) (make-impl))
    ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (to-modula-proc-name type env)
  ;; if it's a base type, we call the base type conversion library
  ;; else we make the converter ourselves.
  (if (is-basetype type)
      (begin
        (stringify-qid
         (cons 'SchemeModula3Types
               (string->symbol (string-append "ToModula_" (is-basetype type))))
         "."
         env))
      (let ((convert-req (env 'get 'convert->m3-req))
            (res (string-append 
                  "ToModula_" 
                  (m3type->m3identifier (type-formatter type env)))))
        (convert-req 'update-entry! (string->symbol res) type)
        res)))

(define (make-to-modula type env)

  (define (push-make type) (to-modula-proc-name type env))

  (let* ((m3tn                       ;; type name
          (type-formatter type env))

         (m3ti                       ;; identifier mangled from tn
          (m3type->m3identifier (type-formatter type env)))

         (pname (to-modula-proc-name type env))

         (imports (env 'get 'imports))

         (proto (string-append
                 "PROCEDURE " pname "(x : SchemeObject.T) : " m3tn 
                       " RAISES { Scheme.E }"
                       )
                )
         )

    ((env 'get 'convert-blt) 'insert! (string->symbol pname))
    ;; remember that we're building pname, so we dont try to
    ;; build it recursively
    

    (imports 'insert! 'SchemeObject) 
    (imports 'insert! 'Scheme)

    (define (make-intf) (string-append proto ";"))

    (define (make-impl)
      (string-append
       proto " = " dnl
       "  BEGIN" dnl

       (case (car type)

         ((Ref)
          ;; several possibilities.  either we have the correct type
          ;; already, or else we may have to build it, if a Ref OpenArray
          (string-append
           "    IF ISTYPE(x, "m3tn") THEN RETURN x END;" dnl

           (let* ((target (extract-field 'target type)))
             (if (eq? (car target) 'OpenArray)
                 (let* ((element (extract-field 'element target))
                        (element-pname (push-make element)))
                   (string-append
                    "    VAR arr := NEW("m3tn",SchemeUtils.Length(x));" dnl
                    "        p := SchemePair.Pair(x);" dnl      
                    "        i := 0;" dnl
                    "    BEGIN" dnl
                    "      WHILE p # NIL DO" dnl
                    "        arr[i] := "element-pname"(p.first);" dnl
                    "        p := SchemePair.Pair(p.rest);" dnl
                    "        INC(i)" dnl
                    "      END;" dnl
                    "      RETURN arr" dnl
                    "    END"
                    ))
                 ""
                 ))
           )
          )
         
         ((Record)
          (let ((fields (extract-field 'fields type)))
        
            (define (format-field f)
              (let* ((field-name (extract-field 'name f))
                     (field-type (extract-field 'type f))
                     (field-pname (push-make field-type)))
                (string-append 
                 "        IF SchemeSymbol.FromText(\"" field-name "\") = p.first THEN" dnl
                 "          res."field-name" := "field-pname"(p.rest)" dnl
                 "        END")))
            
            (string-append
             "    VAR res : "m3tn";" dnl
             "        p := SchemePair.Pair(x);" dnl 
             "    BEGIN" dnl 
             "      WHILE p # NIL DO" dnl
             (infixize (map format-field fields) (string-append ";" dnl)) ";" dnl
             "        p := SchemePair.Pair(p.rest)" dnl
             "      END;" dnl
             "      RETURN res" dnl
             "    END" 
             )
            )
          
          )

         ((UserDefined) 
          (let ((elts (extract-field 'elts type)))
            (define (elts-names) 
              (infixize (map string-quote (map symbol->string elts)) ", ")
              )

            (imports 'insert! 'SchemeSymbol)
            (string-append 
             "CONST" dnl
             "      Name = ARRAY " m3tn " OF TEXT { " (elts-names) " }; " dnl
             "    BEGIN" dnl
             "      FOR i := FIRST(Name) TO LAST(Name) DO" dnl
             "        IF SchemeSymbol.FromText(Name[i]) = x THEN" dnl
             "          RETURN i" dnl
             "        END" dnl
             "      END;" dnl
             "      RAISE Scheme.E(\"Not a value of "m3tn" : \" & SchemeUtils.Stringify(x))" dnl
             "    END"
             )
            )
          )

         ((Subrange)
          (let* ((base (extract-field 'base type))
                 (base-pname (push-make base)))
            (string-append
             "    WITH baseVal = " base-pname "(x) DO"  dnl
             "      IF baseVal < FIRST("m3tn") OR baseVal > LAST("m3tn") THEN" dnl
             "        RAISE Scheme.E(\"Value out of range for "m3tn" :\" & SchemeUtils.Stringify(x))" dnl
             "      END;" dnl
             "      RETURN baseVal" dnl
             "    END"
             )
            )
          )

         ((Array)
          (let* ((element (extract-field 'element type))
                 (element-pname (push-make element))
                 (index (extract-field 'index type))
                 (index-pname (push-make index))
                 )
            (imports 'insert! 'SchemePair)
            (imports 'insert! 'SchemeUtils)
            (string-append
             "    VAR res : "m3tn"; p := SchemePair.Pair(x); BEGIN" dnl 
             "      WHILE p # NIL DO" dnl
             "        WITH desc = SchemePair.Pair(p.first) DO" dnl
             "          IF desc = NIL THEN " dnl
             "            RAISE Scheme.E(\"NIL pair in \" & SchemeUtils.Stringify(x))" dnl
             "          END;" dnl
             "          res["index-pname"(desc.first)] := "element-pname"(desc.rest)" dnl
             "        END;" dnl
             "        p := SchemePair.Pair(p.rest)" dnl
             "      END" dnl
             "    END"))
          )

         ((OpenArray)
          (error "Cant make open array directly from Scheme object, make a REF instead")
          )

         ((Set)
          (let* ((range-type (extract-field 'range type))
                 (range-name (type-formatter range-type env))
                 (range-pname (push-make range-type)))
            (imports 'insert! 'SchemePair)
            (string-append
             "    VAR res := "m3tn" {}; p := SchemePair.Pair(x); BEGIN" dnl 
             "      WHILE p # NIL DO" dnl
             "        res := res + "m3tn" { "range-pname"(p.first) };" dnl
             "        p := SchemePair.Pair(p.rest)" dnl
             "      END;" dnl
             "      RETURN res" dnl
             "    END"
             )
            )
          )

         ((Procedure Object Opaque) 
          (string-append
           "    IF NOT ISTYPE(x,"m3tn") THEN" dnl
           "      RAISE Scheme.E(\"Not of type "m3tn" : \" & SchemeUtils.Stringify(x))" dnl
           "    END;" dnl
           "    RETURN x" 
           )
          )

         (else (error "unknown type header " (car type)))

         )               
       dnl

       "  END " pname ";" dnl
       )
      )
    
    (cons (make-intf) (make-impl))
    ))

(define (make-conversion-routines type env)
  ;; make only the legal conversion routines
  ;; none for base types
  ;; to-scheme only for open arrays
  ;; to-scheme and to-modula for all others
  (if (is-basetype type) 
      #f
      (cons (make-to-scheme type env)
            (if (eq? (car type) 'OpenArray) 
                '()
                (list (make-to-modula type env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-exception-catcher x env)
  (let* ((qid (extract-field 'qid x))
         (xname (stringify-qid (cleanup-qid qid) "." env))
         (imports (env 'get 'imports))
         (arg (extract-field 'arg x)) 
         )

    (imports 'insert! 'SchemeApply)
    (imports 'insert! 'SchemeSymbol)

    (if (null? arg)
        (string-append 
         "      | " xname " => EVAL SchemeApply.OneArg(interp,excHandler,SchemeUtils.Cons(SchemeSymbol.FromText(\"" xname "\"), NIL))" dnl
         )
        (let ((xarg->scm (to-scheme-proc-name arg env)))

          (string-append
           "      | " xname "(xarg) => EVAL SchemeApply.OneArg(interp,excHandler,SchemeUtils.Cons(SchemeSymbol.FromText(\"" xname "\"), " xarg->scm "(xarg)))" dnl
           )
          )
        )
    )
  )

(define (filter-tree tree match-list converter)
	;; very generic routine to search a tree for a "path" and
	;; call converter on everything found down that path.
	;; see prefix-formals for an example
	(map 
	 (lambda (p) 
		 (if (and (pair? p) 
							(eq? (car match-list) (car p)))  ;; any match?
				 (if (null? (cdr match-list))          ;; final match?
						 (cons (car p) (converter (cdr p)));; yes - convert 
						 (cons (car p)                     ;; no - recurse
									 (filter-tree (cdr p) (cdr match-list) converter)))
				 p ;; anything else
				 ))
	 tree))

(define (prefix-formals prefix proc-type)
	;; add a prefix to all the formals of a procedure (or method?) 
	;; declaration
	(filter-tree proc-type 
							 '(sig formals Formal name)
							 (lambda(formal-name)
								 (string->symbol (string-append prefix formal-name)))))
							 

(define (make-procedure-call-stub proc env)
  (let* ((qid (car proc))
         (m3pn (stringify-qid (cleanup-qid qid) "." env))
         (u3pn (stringify-qid (cleanup-qid qid) "_" env))
         (stub-name (string-append "CallStub_" u3pn))
         (proc-type (prefix-formals 'formal_ (cdr proc)))
         (imports (env 'get 'imports))
         (sig (extract-field 'sig proc-type)))

    (define (formal-type-converter type)
      (case (car type)
        ((OpenArray) (formal-type-converter `(Ref (target . ,type))))
        (else (to-modula-proc-name type env))))

    (define (make-formal-temp f)
      (string-append
       (extract-field 'name f)
       " = "
       (formal-type-converter (extract-field 'type f))
       "(Next())"
       ))

    (define (format-call) 
      (let* ((formals (extract-field 'formals sig))
             (arg-list (infixize
                        (map (lambda (f) 
                               ;; open arrays we allocate as REF <type>
                               ;; and de-ref on call
                               (if (eq? (car (extract-field 'type f))
                                        'OpenArray) 
                                   (string-append
                                    (extract-field 'name f)
                                    "^" )
                                   (extract-field 'name f))
                               )
                             formals)
                        ", "))
             (result (extract-field 'result sig)))

    (define (format-nil-checks)
        (apply string-append
               (map (lambda(f)
                      (string-append
                       "        EVAL SchemeUtils.CheckNonNil("
                       (extract-field 'name f)
                       ");" dnl))
                    (filter 
                     (lambda (f) 
                       (eq? (car (extract-field 'type f)) 'OpenArray))
                     formals)
                    )))

        (define (unpack-var f)
          (let* ((arg-type (extract-field 'type f))
                 (arg-name (extract-field 'name f))
                 (deref-caret (if (eq? (car arg-type) 'OpenArray) "^" "")))
          (string-append
           (if (eq? (extract-field 'mode f) 'Mode.Var)
               (string-append
                "        EVAL SchemeUtils.SetFirst(p__," (to-scheme-proc-name arg-type env)"(" arg-name deref-caret"));" dnl
                )
               ""
               )
           "        EVAL Next();" dnl 
           )
          ))

        (define (unpack-var-params) 
          ;; fill this in
          (string-append
           "        p__ := SchemePair.Pair(args);" dnl
           (apply string-append (map unpack-var formals))
           )
          )

        (string-append 
         "      (* unpack formals *)" dnl
         "      WITH "
         (infixize (cons "<*NOWARN*>junk__ = 0" (map make-formal-temp formals))
                   (string-append "," dnl "           ")) " DO" dnl

         "        (* carry out NIL checks for open arrays *)" dnl
         (format-nil-checks) dnl

         "        (* make procedure call *)" dnl
         (if (null? result)
             (string-append
              "        " m3pn"("arg-list");" dnl
              "        (* unpack VAR params *)" dnl
              (unpack-var-params) dnl
              "        (* proper procedure : return TRUE *)" dnl
              "        RETURN SchemeBoolean.True()"
              )
             (string-append
              "        WITH res = " (to-scheme-proc-name result env) "("m3pn"("arg-list")) DO" dnl
              "        (* unpack VAR params *)" dnl
              (unpack-var-params) dnl
              "        (* return procedure result *)" dnl
              "        RETURN res" dnl
              "        END" 
              )
             ) dnl
         "      END(*WITH*)" dnl
         )))   
         
    (define (format-exception-handling) 
      (let ((exceptions (extract-field 'raises sig)))
        ;; exceptions null? RAISES ANY
        ;; exceptions '(()) RAISES {}
        ;; exceptions '((X)) RAISES { X }

        (if (null? exceptions)
            (begin
              (imports 'insert! 'SchemeApply)
              (imports 'insert! 'SchemeSymbol)
              (string-append
               "      ELSE" dnl
               "        SchemeApply.OneArg(interp,excHandler,SchemeUtils.Cons(SchemeSymbol.FromText(\"ANY\"),NIL))" dnl
               )
              )
            (apply string-append
                   (map (lambda(x)(format-exception-catcher x env))
                        (car exceptions))))))

    (imports 'insert! 'Scheme)
    (imports 'insert! 'SchemeObject)
    (imports 'insert! 'SchemeUtils)
    (imports 'insert! 'SchemePair)
    (imports 'insert! 'SchemeBoolean)

    (let ((stubs (env 'get 'procedure-call-stubs))
          (qualified-name (cleanup-qid qid)) 
          )
      (stubs 'delete-entry! qualified-name)
      (stubs 'add-entry! qualified-name stub-name))

    (string-append
     "PROCEDURE "stub-name"(interp : Scheme.T;" dnl
     "                      args : SchemeObject.T;" dnl
     "                      excHandler : SchemeObject.T) : SchemeObject.T" dnl
     "  RAISES { Scheme.E } = " dnl
     " " dnl
     "  PROCEDURE Next() : SchemeObject.T RAISES { Scheme.E } = " dnl
     "    BEGIN" dnl
     "      TRY RETURN SchemeUtils.First(p__) FINALLY p__ := SchemePair.Pair(SchemeUtils.Rest(p__)) END" dnl
     "    END Next;" dnl  
     " " dnl
     "  VAR p__ := SchemePair.Pair(args);" dnl
     "  BEGIN" dnl
     "    TRY" dnl
     (format-call)
     "    EXCEPT" dnl
     (format-exception-handling)
     "    END;" dnl
		 "    <*NOWARN*>RETURN SchemeBoolean.False()" dnl
     "  END " stub-name ";"  dnl
    )))


;; (make-procedure-call-stub (car the-procs) global-env)
;; (map (lambda(p)(make-procedure-call-stub p global-env)) the-procs)

(define dnldnl (string-append dnl dnl))

(define (close-conversions env)
  (let* ((built-sofar (env 'get 'convert-blt))
         (missing->m3  (env 'get 'convert->m3-req))
         (missing->scm (env 'get 'convert->scm-req)))

    (map (lambda(k)
           (missing->m3 'delete-entry! k)
           (missing->scm 'delete-entry! k))
         (built-sofar 'keys))

    (if (and (= 0 (missing->m3 'size)) (= 0 (missing->scm 'size)))
        '() ;; base case
        (let ((this
               (append
                (map (lambda (k)
                       (make-to-scheme (missing->scm 'retrieve k) env))
                     (missing->scm 'keys))
                (map (lambda (k)
                       (make-to-modula (missing->m3 'retrieve k) env))
                     (missing->m3 'keys)))))
          (append this (close-conversions env))))))

(define (spit-out intf-name proc-stubs converters types env)
  (cons
   (spit-out-intf intf-name proc-stubs (map car converters) env)
   (spit-out-impl intf-name proc-stubs (map cdr converters) types env)
   )
)

(define (spit-out-intf intf-name proc-stubs converter-intfs env)
  (let ((imports ((env 'get 'imports) 'copy)))
    (string-append
     "INTERFACE " intf-name ";" dnl
     "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl
     "IMPORT " (infixize (imports 'keys) ", ") ";" dnl
     dnl
     "PROCEDURE RegisterStubs();" dnl
     dnl
     (infixize converter-intfs dnldnl)
     dnl
     "CONST Brand = \"" intf-name "\";" dnl
     dnl
     "END " intf-name "." dnl
     )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-flatten-list lst)
  (define (helper x)
    (if (list? x) 
        (string-flatten-list x)
        x))
  (apply string-append (map helper lst)) )

(define (string-flatten . lst)
  (string-flatten-list lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-register-stubs input-registrations env)
  (string-flatten
   "PROCEDURE RegisterStubs() = " dnl
   "  BEGIN" dnl
	 input-registrations
   (let* ((stubs (env 'get 'procedure-call-stubs))
          (qids (stubs 'keys)))
     
     (define (formatter k)
       (string-append
        "    SchemeProcedureStubs.Register(NEW(SchemeProcedureStubs.Qid, intf := Atom.FromText(\"" (car k) "\"), item := Atom.FromText(\"" (cdr k) "\")), "(stubs 'retrieve k)");" 
        )
       )
     
     (infixize (map formatter qids) dnl)
     ) dnl
    "  END RegisterStubs;" dnl
   ))


(define (spit-out-impl intf-name proc-stubs converter-impls types env)
  (let* ((imports (env 'get 'imports))
         (object-types (filter (lambda(t)(eq? (car t) 'Object)) types))
         (object-stubs (map (lambda(t)(make-object-surrogate t env)) 
                            object-types))
         (object-new-registrations 
                       (map (lambda(t)(make-object-registrations t env)) 
                            object-types)))
      
    (imports 'insert! 'SchemeProcedureStubs)
    (imports 'insert! 'Atom)
    
    (string-append
     "MODULE " intf-name ";" dnl
     "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl
     "IMPORT " (infixize (imports 'keys) ", ") ";" dnl
     dnl
     (infixize proc-stubs dnl) dnl
     dnl
     
     (string-flatten
      (make-register-stubs object-new-registrations env)
      dnl
      object-stubs
      )
       
     dnl

     (infixize converter-impls dnldnl)
     dnl

     ;; more converters... hrmph shouldnt really be here
     (infixize (map cdr (close-conversions env)) dnldnl)
     dnl

     "BEGIN END " intf-name "." dnl
     )
    )
)

(define (make-object-surrogate type env)
  (let ((m3tn (type-formatter type env))
        (m3ti (m3type->m3identifier (type-formatter type env)))
        (imports (env 'get 'imports))
        (surrogate-type-name
         (string-append
          (m3type->m3identifier (type-formatter type env))
          "_Surrogate"))

        (methods ((visible-methods type) 'values)))

    (define (scheme-slot-name method)
      (string-append (extract-field 'name method) "_slot"))

    (define (override-name method)
      (string-append m3ti "_" (extract-field 'name method) "_default"))

    (define (format-scheme-slots)
      (imports 'insert! 'SchemeObject)
      (apply 
       string-append
       (map (lambda(m)
              (string-append 
               "    " (scheme-slot-name m) " : SchemeObject.T := NIL; " dnl)
              )
            methods)
       ))

    (define (format-default-overrides)
      (apply 
       string-append
       (map (lambda (m)
              (string-append
               "    " (extract-field 'name m) " := " (override-name m) ";" dnl)
              )
            methods)))

    (define (make-object-surrogate-decl)
      (imports 'insert! 'Scheme)

      (string-append
       "TYPE " surrogate-type-name " = " m3tn " OBJECT " dnl
       "    interp : Scheme.T;" dnl
       (format-scheme-slots)
       "  OVERRIDES" dnl
       (format-default-overrides)
       "  END;" dnl
       dnl
       )
      )

    (define (make-default-method m)
      (let* ((sig (extract-field 'sig (prefix-formals 'formal_ m)))
             (have-return 
              (if (null? (extract-field 'result sig)) 
                  #f
                  (extract-field 'result sig)))
             (name (extract-field 'name m)))

        (define (format-punt-call) 
          (let ((vars (map 
                       (lambda(f)(extract-field 'name f)) 
                       (extract-field 'formals sig))))
            (string-append
             "      (* punt to Modula-3 object type defn *)" dnl
             "      " (if have-return "RETURN " "") m3tn"."name"("
                       (infixize (cons 'object__ vars) ", ") ")" dnl
             
             )
            )
          )

        (define (format-arg-to-scheme f)
          (imports 'insert! 'SchemeUtils)

          (string-append
           "        args__ := SchemeUtils.Cons("
           (to-scheme-proc-name (extract-field 'type f) env)
           "(" (extract-field 'name f) ")"
            ",args__);" dnl
           )
          )

        (define (format-override-call) 
          (map 
           (lambda(i)(imports 'insert! i))
           '(SchemePair SchemeProcedureStubs SchemeUtils))

          (string-append
           "      (* method overriden by Scheme code *)" dnl
           "      VAR" dnl
           "        args__ : SchemePair.T := NEW(SchemePair.T, first := object__, rest := NIL);" dnl
           "      BEGIN" dnl
           (apply string-append
                  (map format-arg-to-scheme (extract-field 'formals sig)))
           "        args__ := SchemeUtils.Reverse(args__);" dnl

           "        " (if have-return 
                         (string-append "RETURN " (to-modula-proc-name have-return env ) "(")
                         "EVAL (")
                   "SchemeProcedureStubs.CallScheme(object__.interp, "
                   "object__."(scheme-slot-name m)", "
                   "args__))" dnl
           "        (* and this is where we need to unpack VAR params *)" dnl
           "      END" dnl
           )
          )

        (string-append
         "PROCEDURE " (override-name m) 
         (format-parenthesized-signature sig 
                                         env 
                                         (string-append
                                          "object__ : " surrogate-type-name)) 
         " = " dnl
         "  BEGIN" dnl
         "    IF object__." (scheme-slot-name m) " = NIL THEN" dnl
         (format-punt-call)
         "    ELSE" dnl
         (format-override-call)
         "    END" dnl
         "  END " (override-name m) ";" dnl dnl
       
         )
      ))

    (define (make-object-ops)

      (define (format-field-initializer f)
        (let ((type (extract-field 'type f))
              (name (extract-field 'name f)))
          (imports 'insert! 'SchemeSymbol)
          (string-append
           "            IF r.first = SchemeSymbol.FromText(\"" name "\") THEN" dnl
           "              res." name " := "(to-modula-proc-name type env)"(r.rest); gobbled := TRUE" dnl
           "            END;" dnl
           )))

          
      (define (format-field-initializers) 
        (apply string-append
               (map format-field-initializer (extract-field 'fields type))
               )
        )

      (define (format-method-override m)
        (imports 'insert! 'SchemeSymbol)
        (let ((name (extract-field 'name m)))
          (string-append
           "            IF r.first = SchemeSymbol.FromText(\"" name "\") THEN" dnl
           "               res." (scheme-slot-name m) " := r.rest; gobbled := TRUE" dnl
           "            END;" dnl
           )))

      (define (format-method-overrides) 
        (apply string-append
               (map format-method-override methods)
               )
        )

      (map 
       (lambda(i)(imports 'insert! i))
       '(SchemePair Scheme SchemeObject))
      
      (string-append
       "PROCEDURE New_" m3ti "(<*UNUSED*>interp : Scheme.T; inits : SchemeObject.T) : SchemeObject.T RAISES { Scheme.E } =" dnl
       "  VAR" dnl
       "    p := SchemePair.Pair(inits);" dnl
			 "    gobbled : BOOLEAN;" dnl
       "  BEGIN" dnl
       "    WITH res = NEW("surrogate-type-name") DO" dnl
       "      WHILE p # NIL DO" dnl
       "        WITH r = SchemePair.Pair(p.first) DO" dnl
			 "          gobbled := FALSE;" dnl
			 "          IF r # NIL THEN" dnl
       (format-field-initializers)
       (format-method-overrides)
			 "          END;" dnl
			 "          IF NOT gobbled THEN" dnl
			 "            RAISE Scheme.E(\"Unknown field/method in \" & SchemeUtils.Stringify(inits))" dnl
			 "          END" dnl
       "        END;" dnl
       "        p := SchemePair.Pair(p.rest)" dnl
       "      END;" dnl
       "      RETURN res" dnl
       "    END" dnl
       "  END New_" m3ti ";" dnl
			 dnl
       "PROCEDURE GenNew_" m3ti "(interp : Scheme.T; <*UNUSED*>obj : SchemeObject.T; inits : SchemeObject.T) : SchemeObject.T RAISES { Scheme.E } =" dnl
			 "  BEGIN RETURN New_" m3ti "(interp,inits) END GenNew_" m3ti ";" dnl
			 dnl
       ))
    
    (cons
     (make-object-ops)
     (cons
      (make-object-surrogate-decl)
      (map make-default-method methods)
      )
     )
    )
)

(define (make-an-op-registration type name proc-name env)
	((env 'get 'imports) 'insert! 'SchemeProcedureStubs)
	(string-append
	 "    SchemeProcedureStubs.RegisterOp(TYPECODE("
	 (type-formatter type env)
	 "),\"" name  "\"," proc-name");" dnl
	 ))

(define (make-a-tc-registration type name env)
	((env 'get 'imports) 'insert! 'SchemeProcedureStubs)
	(string-append
	 "    SchemeProcedureStubs.RegisterTC(TYPECODE("(type-formatter type env)
	 "),\"" name "\");" dnl
	 ))

(define (make-object-registrations obj-type env)
  ;; print those things that need to be registered for an object
  ;; type
  (let* ((m3tn (type-formatter obj-type env))
         (m3ti (m3type->m3identifier m3tn))
         (alias (cleanup-qid (extract-field 'alias obj-type)))
         (name (cleanup-qid (extract-field 'alias obj-type)))
         
         (new-name (string-append "New_" m3ti))
         (gen-new-name (string-append "GenNew_" m3ti))
				 )
    ((env 'get 'imports) 'insert! 'SchemeProcedureStubs)
    ((env 'get 'imports) 'insert! 'Atom)
    (string-flatten
     "    SchemeProcedureStubs.RegisterNew(NEW(SchemeProcedureStubs.Qid, intf := Atom.FromText(\""(car alias)"\"), item := Atom.FromText(\""(cdr alias)"\")), "new-name");" dnl
		 (make-an-op-registration obj-type 'new gen-new-name env)

		 (make-a-tc-registration obj-type (type-formatter obj-type env) env)
		 ;; we can introduce type aliases here as well

     )
    )
     
  )

(define (visible-methods obj-type)
  ;; a type can only have methods if its opaque or object
  (cond ((null? obj-type) (make-symbol-hash-table 100))
        ((eq? (car obj-type) 'Object)
         (let ((super-visible 
                (visible-methods (extract-field 'super obj-type))))
           (map (lambda(m)
                  (super-visible 'update-entry!
                                 (extract-field 'name m)
                                 m))
                (extract-field 'methods obj-type))
           super-visible))
        ((eq? (car obj-type) 'Opaque)
         (visible-methods (extract-field 'revealedSuperType obj-type)))
        (else (error "Cant get methods from " obj-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-procedure-call-stub (car the-procs) global-env)

(define (write-files intf-name procs types env)
  (let* ((proc-stubs (map (lambda(p)(make-procedure-call-stub p env)) procs))
         (converters (close-conversions env))
         (im (spit-out intf-name proc-stubs converters types env))
         (iwr (open-output-file (string-append intf-name ".i3")))
         (mwr (open-output-file (string-append intf-name ".m3"))))

    (display (car im) iwr)
    (display (cdr im) mwr)
    
    (close-output-port iwr) (close-output-port mwr)

    proc-stubs
    ))
        
    
(define (make-standard-stuff intf-name)
  ;; build all the Scheme interfaces for a given M3 interface
  (set! global-env (env-type 'new))
  (write-files intf-name the-procs the-types global-env)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;                           ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;   m3build/cm3  helpers    ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;                           ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-scheme-package-exports intfs intf-name)
  (let* ((iwr (open-output-file (string-append intf-name ".i3")))
         (mwr (open-output-file (string-append intf-name ".m3"))))

    (dis 
     (string-append
      "INTERFACE "intf-name";" dnl
      "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl
      "PROCEDURE RegisterStubs();" dnl
      "END " intf-name "." dnl
      )
     iwr
     )

    (dis 
     (string-append
      "MODULE "intf-name";" dnl
      "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl
      "IMPORT " (infixize intfs ", ") ";" dnl
      dnl
      "PROCEDURE RegisterStubs() =" dnl
      "  BEGIN" dnl
      (apply string-append
             (map (lambda(i)(string-append "    " i ".RegisterStubs();" dnl))
                  intfs)
             )
      "  END RegisterStubs;" dnl
      dnl
      "BEGIN END " intf-name "." dnl
      )
     mwr
     )

    (close-output-port iwr) (close-output-port mwr)
    ))
       
    
(define (make-global-scheme-stubs magic-string  ;; what to search for
                                  search-path   ;; path of IMPTAB
                                  output-path   ;; where to put intf/impl
                                  .
                                  local-exports ;; if were exporting from
                                                ;; current package
                                  )

  (let ((in (open-input-file search-path)))
    
    (define (get-matches)
      (let loop ((next (read in))
                 (res '()))
;;                 (res local-exports))
        (if (eof-object? next) 
            res
            (begin
              ;;(dis "matching against " next dnl)
              (let ((match (string-havesub? (symbol->string next)
                                            magic-string)))
                (if match 
                    (loop (read in) (cons (strip-extension next) res))
                    (loop (read in) res)))))))
                  
    (write-scheme-package-exports  (get-matches)
                                   output-path)

    (close-input-port in))
  
  )


(define (strip-extension sym)
  (string->symbol
   (car (pregexp-split "\\." 
                       (symbol->string sym)))))


;; pure testing code...
(define (make-list n . x) 

  (define (kar x)
    (if (null? x) '() (car x))) 

  (if (= 0 n) 
      (cons 0 (kar x)) 
      (make-list (- n 1) (cons n (kar x)))))
