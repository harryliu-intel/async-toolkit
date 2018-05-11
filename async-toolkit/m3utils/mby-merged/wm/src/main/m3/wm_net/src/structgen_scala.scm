(require-modules "basic-defs" "m3" "display" "hashtable" "struct" "set")

;; load structgen_shared.scm BEFORE this file

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;                                                ;;;;;;;;;;;
;;;;;;;;;;;         SCALA STRUCT COMPILER FOR MBY          ;;;;;;;;;;;
;;;;;;;;;;;                                                ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the data structure
;; compiler.
;;
;; header types are BIG-ENDIAN (IBM/Internet convention)
;; bitstruct types are LITTLE-ENDIAN (Intel convention)
;;
;; header and bitstruct types offer slightly different interfaces
;;
;;
;; The compiler itself currently depends (maybe) on the following
;; Modula-3 code:
;;
;;   FileWr.OpenAppend
;;   M3Support.Modula3Type
;;   M3Support.ReformatNumber
;;   IdStyles.Convert
;;   Scan.Int
;;
;; the m3makefile of the compiler hence has to contain (or somehow
;; import) SchemeStubs for these interfaces.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HELPER FUNCS

(define (scheme->scala sym)  ;; same as Modula-3 for now
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'Camel
                    'Hyphen 'None))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OUTPUT FILE HANDLING

(define (open-scala nm scala-super)
  (let ((wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".scala.tmp")))))

    (let ((mf-wr (FileWr.OpenAppend (sa deriv-dir "derived_scala.filelist"))))
      (dis nm dnl mf-wr)
      (wr-close mf-wr))
          
    (dis "package switch_wm" dnl wr)
    (dis "object " nm " extends " scala-super " {" dnl wr)

    ;; imports ?

    ;; return a list of useful info
    (list wr nm deriv-dir)
    )
  )

(define (open-scala-case-class nm)
  (let ((wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".scala.tmp")))))

    (let ((mf-wr (FileWr.OpenAppend (sa deriv-dir "derived_scala.filelist"))))
      (dis nm dnl mf-wr)
      (wr-close mf-wr))


    (dis "case class " nm " {" dnl wr)

    ;; imports ?

    ;; return a list of useful info
    (list wr nm deriv-dir)
    )
  )

(define (open-empty nm)
  (let ((wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".scala.tmp")))))

    (let ((mf-wr (FileWr.OpenAppend (sa deriv-dir "derived_scala.filelist"))))
      (dis nm dnl mf-wr)
      (wr-close mf-wr))

    ;; imports ?

    ;; return a list of useful info
    (list wr nm deriv-dir)
    )
  )



(define (close-scala wrs)
  (let ((wr        (car wrs))
        (nm        (cadr wrs))
        (deriv-dir (caddr wrs)))

    (wr-close wr)
    (rename-file-if-different (sa deriv-dir nm ".scala.tmp")
                              (sa deriv-dir nm ".scala"))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define e '()) ;; debugging slush bucket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROTOTYPES FOR Read AND Write AND OTHER FUNCTIONS EXPORTED
;;;


(define read-proc-name "Read")
(define read-proto "(rd : Rd.T) : T RAISES { Rd.Failure, NetError.OutOfRange, Rd.EndOfFile, Thread.Alerted }")

(define write-proc-name "Write")
(define write-proto "(wr : Wr.T; READONLY t : T) RAISES { Wr.Failure, Thread.Alerted }")

(define readc-proc-name "ReadC")
(define readc-proto "(rd : Rd.T; VAR cx : NetContext.T) : T RAISES { Rd.Failure, NetError.OutOfRange, Rd.EndOfFile, Thread.Alerted, NetContext.Short }")

(define reads-proc-name "ReadS")
(define reads-proto "(s : Pkt.T; VAR at : CARDINAL; VAR t : T) : BOOLEAN")

(define writec-proc-name "WriteC")
(define writec-proto "(wr : Wr.T; READONLY t : T; VAR cx : NetContext.T) RAISES { Wr.Failure, Thread.Alerted }")

(define writes-proc-name "WriteS")
(define writes-proto "(s : Pkt.T; at : CARDINAL; READONLY t : T)")

(define readsb-proc-name "ReadSB")
(define readsb-proto "(s : Pkt.T; at : CARDINAL; VAR res : T) : BOOLEAN")

(define readeb-proc-name "ReadEB")
(define readeb-proto "(s : Pkt.T; e : Pkt.End; VAR res : T) : BOOLEAN")

(define writee-proc-name "WriteE")
(define writee-proto "(s : Pkt.T; e : Pkt.End; READONLY t : T)")

(define format-proc-name "Format")
(define format-proto "(READONLY t : T) : TEXT")

(define check-proc-name "Check")
(define check-proto "(READONLY t : T) : BOOLEAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put-m3-proc whch .
                     wrs ;; i3 m3 ...
                     )
  (map (lambda(wr)
         (dis
          "PROCEDURE "
          (eval (symbol-append whch '-proc-name))
          (eval (symbol-append whch '-proto))
          wr))
       wrs)
  (dis ";" dnl (car wrs)) ;; i3
  (dis " =" dnl (cadr wrs)) ;; m3
)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANTS

(define (compile-scala-const-value v wr)
  (dis "val " (scheme->scala (car v)) " : W = " (cadr v) dnl wr)
  )


(define (compile-constants-scala! nm x)
  (dis "compiling constants :  " nm dnl)
  (let* ((wire-type    (car x))
         (scala-wire-type (scheme->scala wire-type))
         (names        (cadr x))
         (values       (caddr x))
         (scala-name      ( nm names))
         (scala-wrs       (open-scala scala-name "AnyObj"))
         (wr              (car scala-wrs))
         )
    (dis "scala-name " scala-name dnl)
    (add-tgt-type! nm scala-name)
    (dis "{" dnl wr)

    (dis "type W = " scala-wire-type dnl
         dnl
         wr)
    (map (lambda(x)(compile-scala-const-value x wr)) values)

    (close-scala scala-wrs)
    ) ;; *tel
  )


(define (get-scala-name nm lst)
  (let ((pair (assoc 'scala lst)))
    (if (null? pair)
        (error (sa "No Scala name for " nm))
        (cadr pair))))

(define compile-constants! compile-constants-scala!)

(define (compile-enum! nm x)
  (dis "compiling enum      :  " nm dnl)
  (let* ((wire-type        (car x))
         (scala-wire-type  (scheme->scala wire-type))
         (names            (cadr x))
         (values           (caddr x))
         (scala-name       (get-scala-name nm names))
         (scala-wrs        (open-scala scala-name "Enumeration"))
         (wr               (car scala-wrs))
         )

    ;;(dis "m3-name " m3-name dnl)
    (add-tgt-type! nm scala-name)

    ;; final IMPORTs
    

    (dis "  const Length = " (get-scala-type-size wire-type) ";" dnl
         dnl wr)
    
    (let loop ((i      0)
               (x      values)
               (v2t    ""))

      (if (null? x) ;; base case of iteration (done case)

          (begin
            (dis v2t
                 "//  def apply(is : InputStream) : Value = {" dnl
                 "//    " scala-name "(is.read" scala-wire-type "BE())" dnl
                 "//  }" dnl
                 dnl
                 wr)
            ) ;; nigeb

          ;; else -- iterate further
          
          (let* ((sym (scheme->scala (caar x)))
                 (comma   (if (null? (cdr x)) "" ", "))
                 (valspec (cdar x))
                 (val     (cond ((null? valspec) i)
                                ((< (car valspec) i)
                                 (error (sa
                                         "bad value for enum "
                                         (stringify (car valspec)))))
                                (else (car valspec)))))
            (loop (+ val 1)
                  (cdr x)
                  (sa v2t "  val " sym " = Value(" val ", \"" sym "\")" dnl)))
          ) ;; fi
      ) ;; pool
    (dis dnl wr)
    (dis "}" dnl wr)
    (dis dnl wr)
	
	(let* ((scala-name-is (string-append scala-name "InputStream"))
	       (scala-name-os (string-append scala-name "OutputStream")))
	  (dis "class " scala-name-os "(OutputStream os) extends DataOutputStream(os) {" dnl wr)
	  (dis " def write" scala-name "( x  :"  scala-name ") = { writeByte(x.id) }" dnl wr)
	
      (dis "}" dnl wr)
	  (dis "class " scala-name-is "(InputStream os) extends DataInputStream(is) {" dnl wr)
	  (dis " def read" scala-name "() : " scala-name " = " scala-name "(readByte())" dnl wr)
	
	  (dis "}" dnl wr)
      (dis "implicit isTo" scala-name-is "(InputStream is) : " scala-name-is " = " scala-name-is "(is)" dnl wr)
	  (dis "implicit osTo" scala-name-os "(OutputStream os) : " scala-name-os "= " scala-name-is "(os)" dnl wr)
	)
    (close-scala scala-wrs)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HEADER FIELD TYPES

(define (get-m3-type type)
  (cond ((member? type '(u8 u16 u32 u64)) (scheme->scala type))
        ((array-type? type)
         (sa "ARRAY [0.." (caddr type) "-1] OF "
                        (get-m3-type (cadr type))))
        (else
         (symbol->string (symbol-append (get-tgt-typemapping type) ".T")))))

(define (get-scala-type type)
  (cond ((member? type '(u8 u16 u32 u64)) (scheme->scala type))
        ((array-type? type)
         (string-append "ARRAY [0.." (caddr type) "-1] OF "
                        (get-scala-type (cadr type))))
        (else
         (symbol->string (symbol-append (get-tgt-typemapping type) ".Value")))))


(define (make-dotted-reference intf member)
  (symbol->string (symbol-append intf (symbol-append "." member))))

(define (get-scala-type-size type)
  (get-type-size type make-dotted-reference))

(define (emit-header-field-type f i-wr)
  (dis "    " (car f) " : " (get-scala-type (cadr f)) dnl i-wr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-m3-read-type whch type lhs lev ind)
  (cond ((member? type '(u8 u16 u32 u64))
         (case whch
           ((read)
            (sa ind lhs " := RdNet.Get"(scheme->scala type)"(rd)"))
           ((readc)
            (sa ind lhs " := RdNet.Get"(scheme->scala type)"C(rd,cx)"))
           ((reads)
            (sa ind "IF NOT RdNet.Get"(scheme->scala type)"S(s,p,"lhs") THEN RETURN FALSE END"))
           (else (error whch))))
           
        ((array-type? type)
         (sa
          ind "FOR i"lev" := 0 TO " (caddr type) "-1 DO" dnl

          (get-m3-read-type
           whch
           (cadr type)
           (sa lhs "[i"lev"]")
           (+ lev 1)
           (sa ind "  ")
           ) dnl
          ind "END"
          ))
        (else
         (case whch
           ((read)
            (sa ind lhs " := " (get-tgt-typemapping type) ".Read(rd)"))
           ((readc)
            (sa ind lhs " := " (get-tgt-typemapping type) ".ReadC(rd,cx)"))
           ((reads)
            (sa ind "IF NOT " (get-tgt-typemapping type) ".ReadS(s,p,"lhs") THEN RETURN FALSE END"))
            
           (else (error whch))))))
         
(define (emit-header-field-readx whch f m-wr updn)
  (dis (get-m3-read-type whch
                         (cadr f)
                         (sa "t." (car f))
                         0
                         "    ")
       ";" dnl m-wr))

(define (emit-header-field-readc . x)
  (apply emit-header-field-readx (cons 'readc x)))

(define (emit-header-field-reads . x)
  (apply emit-header-field-readx (cons 'reads x)))

(define (emit-header-field-read . x)
  (apply emit-header-field-readx (cons 'read x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-m3-format-type type rhs lev ind)
  ;;(dis "RHS " rhs dnl)
  (cond ((member? type '(u8 u16 u32 u64))
         (sa
          ind
          "Wx.PutText(wx,NetTypes.Format"(scheme->scala type)"("rhs"))"
          ))
          
        ((array-type? type)
         (sa
          ind "Wx.PutChar(wx,'{');" dnl
          ind "FOR i"lev" := 0 TO " (caddr type) "-1 DO" dnl
          ind "  Wx.PutChar(wx,' ');" dnl
              
          (get-m3-format-type
           (cadr type)
           (sa "  " rhs "[i"lev"]")
           (+ lev 1)
           (sa ind "  ")
           )
          dnl

          ind "END;" dnl
          ind "Wx.PutText(wx,\" }\")" 
          ))
        (else
         (sa
          ind
          "Wx.PutText(wx," (get-tgt-typemapping type) ".Format("rhs"))"))))

(define (emit-header-field-format f m-wr updn)
  (let ((field-name (car f))
        (field-type (cadr f)))
    
    (dis
     "    Wx.PutText(wx,\""field-name"=\");" dnl
     (get-m3-format-type field-type
                         (sa "t."field-name)
                         0
                         "    "
                         ) 
     ";" dnl
     "    Wx.PutText(wx,\" \");" dnl
     m-wr)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-m3-write-type whch type rhs lev ind updn)
  ;;(dis "RHS " rhs dnl)
  ;;(dis whch " " type dnl)

  (sa

   ;;
   ;; in case of the generic write 'writee , the fields are
   ;; processed twice: once in ascending, then once in descending
   ;; order.
   ;;
   ;; we need to ensure that only one copy is actually emitted!
   ;;

   (if (eq? whch 'writee)
       (sa ind                   ;; matches *** >>>
                      (case updn
                        ((up) "IF e=Pkt.End.Back THEN ")
                        ((dn) "IF e=Pkt.End.Front THEN ")
                        ) ;; esac
         )
       ""
       )

   ;; and now for your regularly scheduled presentation...
  (cond ((member? type '(u8 u16 u32 u64))
         (case whch
           ((write)
            (sa ind "WrNet.Put"(scheme->m3 type)"(wr,"rhs")"))
           ((writec)
            (sa ind "WrNet.Put"(scheme->m3 type)"C(wr,"rhs",cx)"))
           ((writes)
            (sa ind "WrNet.Put"(scheme->m3 type)"S(s,at,"rhs")"))
           ((writee)
            (sa ind "WrNet.Put"(scheme->m3 type)"G(s,e,"rhs")"))
           (else (error whch))))
         
        ((array-type? type)
         (sa
          ind
          (case updn
            ((up)
             (sa "FOR i"lev" := 0 TO " (caddr type) "-1 DO"))

            ((dn)
             (sa "FOR i"lev" := " (caddr type) "-1 TO 0 BY -1 DO"))
            )
          dnl

          (get-m3-write-type
           whch
           (cadr type)
           (sa "  " rhs "[i"lev"]")
           (+ lev 1)  ;; increase nesting depth
           (sa ind "  ")
           updn
           )
          dnl

          ind "END"
          ))

        (else
         (case whch
           ((write)
            (sa ind (get-tgt-typemapping type) ".Write(wr,"rhs")"))
           ((writec)
            (sa ind (get-tgt-typemapping type) ".WriteC(wr,"rhs",cx)"))
           ((writes)
            (sa ind (get-tgt-typemapping type) ".WriteS(s,at,"rhs")"))
           ((writee)
            (sa ind (get-tgt-typemapping type) ".WriteE(s,e,"rhs")"))
           (else (error whch)))))
  ;; dnoc
  
  (if (eq? whch 'writee) " END" "")  ;; matches *** <<<
  
  ) ;; sa
  )

(define (emit-header-field-writex whch f m-wr updn)
  (dis 
   (get-m3-write-type whch
                      (cadr f)
                      (sa "t."(car f))
                      0
                      "    "
                      updn)
   
   ";" dnl m-wr))

(define (emit-header-field-writee . x)
  (apply emit-header-field-writex (cons 'writee x)))

(define (emit-header-field-writes . x)
  (apply emit-header-field-writex (cons 'writes x)))

(define (emit-header-field-writec . x)
  (apply emit-header-field-writex (cons 'writec x)))

(define (emit-header-field-write . x)
  (apply emit-header-field-writex (cons 'write x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BITSTRUCTS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; THE CONSTRAINT COMPILER
;;
;; This compiles from s-expressions to Modula-3 code testing the
;; given expressions.
;;
;; e.g. given:
;;       (and (> (- x y) (+ a b)) (= 0 z))
;; result s.b.
;;       IF NOT (t.x - t.y) > (t.a + t.b) AND (z = 0) THEN RETURN FALSE END
;;
;; also constants:
;;       (f ... (constant 16_abcd))
;; result s.b.
;;       IF t.f # 16_abcd THEN RETURN FALSE END
;;
;; A constraint can refer to any field in the RECORD (not just the
;; field currently being parsed).  The current record is called "t".
;;

(define (symbol->m3integer x)
  (M3Support.ReformatNumber (symbol->string x))
  )
   
(define (get-bitstruct-constraint-check nm t constraint)
  ;;(dis "CONSTRAINT: " (stringify constraint) dnl)
  (case (car constraint)
    ((constant)
     (sa "    IF t."nm" # " (symbol->m3integer (cadr constraint))
                    " THEN RETURN FALSE END;" dnl))
    
    ((constraint)
     (sa "    IF NOT "
                    (format-m3-expr (lambda(nm)(sa "t." nm)) (cadr constraint))
                    " THEN RETURN FALSE END;" dnl)
      )

    (else (error (sa "Unknown constraint " (stringify constraint))))

    ) ;; esac
  )


(define (mynumber->m3integer x)
  (cond ((symbol? x) (symbol->m3integer x))
        ((number? x) (number->string x))
        (else (error "unknown number " (stringify x)))))


;;
;; anything not a binary or unary operator is assumed to use functional
;; notation and invocation!
;;

(define (op->m3op op) ;; operators we recognize
  (case op
    ((not)   " NOT ")
    ((/)     " DIV ")
    ((mod)   " MOD ")
    ((and)   " AND ")
    ((or)    " OR ")
    (else (symbol->string op))))

(define (format-m3-expr sym-formatter expr)
  (cond ((mynumber? expr) ;; a number
         (mynumber->m3integer expr)) 

        ((symbol? expr)   ;; the name of a field (same or other)
         (sym-formatter (symbol->string expr)))

        ((list? expr)     ;; an expression
         (let ((me     (lambda(x)(format-m3-expr sym-formatter x)))
               (op     (car expr))
               (args   (cdr expr)))
           (cond

            ;; check for unary ops, binary ops, or default to function call
            
            ((and (= (length args) 1) (unary-op? op))
             (sa (op->m3op op) (me (car args))))
            
            ((and (> (length args) 1) (binary-op? op))
             (sa "(" (infixize (map me args) (op->m3op op)) ")"))

            (else  ;; function call
             (sa (op->m3op op) "(" (infixize (map me args) ",") ")"))
            
            );;dnoc
           );;tel
         )
        
        (else
         (error (sa "cant format expression " (stringify expr))))
        );;dnoc
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; exported routines for bitstructs
;;

(define (get-m3-bitstype n)
  (M3Support.Modula3Type n))

(define (emit-bitstruct-field-type f i-wr)
  (let* ((field-name (car f))
         (field-type (cadr f))
         (tail       (cddr f))
         (defval     (if (or (null? tail)
                             (not (eq? (caar tail) 'constant)))
                         ""
                         (sa " := " (symbol->m3integer (cadar tail)))))
          
        )
    (dis "    " field-name " : Int" dnl i-wr)))

(define (emit-bitstruct-field-format f wr dummy)
  (let ((field-name (car f))
        (field-type (cadr f))
        )
    (dis
     "    Wx.PutText(wx,\""field-name"=\");" dnl
     "    Wx.PutText(wx,StructField.Format(t."field-name",wid:="field-type"));" dnl
     "    Wx.PutChar(wx, ' ');" dnl
     wr
     )
    )
  )

(define (emit-bitstruct-field-check f wr dummy)
  (let ((field-name (car f))
        (field-type (cadr f))
        (tail       (cddr f)))
    (if (null? tail)
        (dis "    (*no constraint "field-name" *)" dnl wr)
        (dis "    (*have constraint "field-name" " (stringify (car tail)) " *)" dnl
             (get-bitstruct-constraint-check field-name field-type (car tail))
             dnl wr))))

(define (emit-bitstruct-field-readsb f wr start-bit)
  (let ((field-name (car f))
        (field-type (cadr f)))
    (dis "    (* "field-name" @ "start-bit":+"field-type" *)" dnl
         wr)
    (dis "    IF NOT Pkt.ExtractBits(s, at, "start-bit", "field-type", w) THEN RETURN FALSE END;" dnl wr)
    (dis "    t."field-name" := w;" dnl wr)
    (+ start-bit field-type)))

(define (emit-bitstruct-field-writee f wr start-bit)
  (let ((field-name (car f))
        (field-type (cadr f)))
    (dis "    (* "field-name" @ "start-bit":+"field-type" *)" dnl
         wr)
    (dis "    Pkt.ArrPut(a, "start-bit", t."field-name", "field-type");" dnl wr)
    (+ start-bit field-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAIN COMPILER

(define (compile-bitstruct! nm x)
  ;; concept here
  ;; generate a case class with typed 'machine-sized' fields
  ;; generate a matching object class with an apply method to create the case
  ;;  class from an array of bytes
  ;; generate input/outputstream classes to match the above
  (dis "compiling bitstruct :  " nm dnl)

  (let* (
         (names (car x))
         (scala-name      (cadr (assoc 'scala names)))
         (wrs       (open-empty scala-name))
         (wr        (car wrs))
         (fields       (cadr x))
         )

    (set! e fields)

    (define (emit-m3-t)
      (dis dnl
           "case class " scala-name " extends Bitstruct" dnl
           "  (" dnl
           wr)
      (map (lambda(f)(emit-bitstruct-field-type f wr)) fields)
      (dis " )" wr)
      )

    (add-tgt-type! nm scala-name)
	(dis "package switch_wm" dnl wr)

    ;; the matching Modula-3 declaration
    (emit-m3-t)
    (dis " = { " wr)
    (dis dnl "val LengthBits = " (accumulate + 0 (map cadr fields)) dnl wr)
    (dis dnl "def toByteArray : Array[Byte] = { " dnl 
	   " val a = Array[Byte].ofDim(LenthBits / 8)" dnl
       " // modify A for each field, with insert operations" dnl	
	   " a " dnl	
	   
	   " }" dnl wr)

    (dis "} " wr)

	(dis dnl "object " scala-name " = { " dnl wr)
	(dis dnl " def apply(a : Array[Byte]) : " scala-name " = {" wr)
	(dis dnl "  require(a.size == LengthBits / 8)" wr)
    (dis dnl "  // return a new object from applying the array provided" dnl wr)
	(dis dnl " }" wr)

	(dis dnl "}" dnl wr)
	(let* ((scala-name-is (string-append scala-name "InputStream"))
	       (scala-name-os (string-append scala-name "OutputStream")))
	  (dis "class " scala-name-os "(OutputStream os) extends DataOutputStream(os) {" dnl wr)
	  (dis " def write" scala-name "( x  :"  scala-name ") = { write(x.toByteArray,0,x.LengthsBits/8 }" dnl wr)
	
      (dis "}" dnl wr)
	  (dis "class " scala-name-is "(InputStream os) extends DataInputStream(is) {" dnl wr)
	  (dis " def read" scala-name "() : " scala-name " = {" dnl "  val a = Array[Byte].ofDim(a.size / 8)" dnl "  readFully(a) " dnl "  " scala-name "(a)" dnl wr)
	
	  (dis " }" dnl wr)
	  (dis "}" dnl wr)

      (dis "implicit isTo" scala-name-is "(InputStream is) : " scala-name-is " = " scala-name-is "(is)" dnl wr)
	  (dis "implicit osTo" scala-name-os "(OutputStream os) : " scala-name-os "= " scala-name-is "(os)" dnl wr)
	)

    (close-scala wrs)
)
)

(define (compile-header! nm x)
  (let* ((names (car x))
         (scala-name      (cadr (assoc 'scala names)))
         (scala-wrs       (open-empty scala-name))
         (wr        (car scala-wrs))
         (fields       (cadr x))
         (types        (uniq eq? (map cadr fields)))
    )
    (dis "compiling scala header    :  " nm dnl)
    (define (emit-scala-t)
      (dis dnl "(" dnl wr)
      ;; need comma, after every except last entry
      (map (lambda(f)(emit-header-field-type f wr)) fields)
      (dis ") //" dnl dnl wr))


    (define (emit-length)
      (dis "val length = 0" wr)
      (map
       (lambda(f)(dis
                  (string-append " + " (get-scala-type-size (cadr f)))
                  wr))
      (dis dnl wr)
      )
    )
    (add-tgt-type! nm scala-name)
    ;; (set! e import-intfs)
    ;; the matching Modula-3 declaration
    (dis "package switch_wm" dnl wr)

    (dis "case class " scala-name dnl wr)
    (emit-scala-t)
    (dis "{" dnl wr)
    ;; a symbol called Length, denoting the wire size of the record
    ;; (emit-length)
    (dis "}" dnl wr)
    (dis dnl wr)
	
	(let* ((scala-name-is (string-append scala-name "InputStream"))
	       (scala-name-os (string-append scala-name "OutputStream")))
	  (dis "class " scala-name-os "(OutputStream os) extends DataOutputStream(os) {" dnl wr)
	  (dis " def write" scala-name "( x  :"  scala-name ") = { " dnl
	     " // write out the case class, using appropriate portable writeShort, writeInt, etc. operations" dnl 
		 " }" dnl wr)
	
      (dis "}" dnl wr)
	  (dis "class " scala-name-is "(InputStream os) extends DataInputStream(is) {" dnl wr)
	  (dis " def read" scala-name "() : " scala-name " = {" dnl 
	       "  // read in, using portable readShort, readInt, etc., operations" dnl
		   " }" dnl wr)
	
	  (dis "}" dnl wr)
      (dis "implicit isTo" scala-name-is "(InputStream is) : " scala-name-is " = " scala-name-is "(is)" dnl wr)
	  (dis "implicit osTo" scala-name-os "(OutputStream os) : " scala-name-os "= " scala-name-is "(os)" dnl wr)
	)

    (close-scala scala-wrs)
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAIN FUNCTIONS : COMPILE THE DATA STRUCTURES

(define (compile! structs)
  (clear-globals!)
  (wr-close (filewr-open (sa deriv-dir "derived.m3m")))
  (map compile-one! structs)
  )

;; for now -- override with NOP
;;(set! compile-enum!       (lambda(nm x) #t))
(set! compile-constants!  (lambda(nm x) #t))
;;(set! compile-header!     (lambda(nm x) #t))
;;(set! compile-bitstruct!  (lambda(nm x) #t))
