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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HELPER FUNCS

(define (scheme->scala sym)  ;; same as Modula-3 for now
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'Camel
                    'Hyphen 'None))

(define (symbol->scalainteger x)
  (M3Support.ReformatNumberScala (symbol->string x))
 )

					
(define legalize-field scheme->scala)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OUTPUT FILE HANDLING

(define (open-scala nm scala-super)
  (let ((wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".scala.tmp")))))

    (let ((mf-wr (FileWr.OpenAppend (sa deriv-dir "derived_scala.filelist"))))
      (dis nm dnl mf-wr)
      (wr-close mf-wr))
          
    (dis "package switch_wm.model_server" dnl dnl wr)
	(dis "import java.io._" dnl dnl wr)
	(dis "import switch_wm.PrimitiveTypes._" dnl dnl wr)
	
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANTS

(define (compile-scala-const-value v wr)
  (dis "val " (scheme->scala (car v)) " : W = " (symbol->scalainteger (cadr v)) dnl wr)
  )


(define (compile-constants-scala! nm x)
  (dis "compiling constants :  " nm dnl)
  (let* ((wire-type    (car x))
         (scala-wire-type (scheme->scala wire-type))
         (names        (cadr x))
         (values       (caddr x))
         (scala-name      (get-scala-name nm names))
         (scala-wrs       (open-scala scala-name "WhiteModelConstants"))
         (wr              (car scala-wrs))
         )
    (add-tgt-type! nm scala-name)
    (dis "scala-name " scala-name dnl)
    
    (dis "type W = " scala-wire-type dnl
         dnl
         wr)
    (map (lambda(x)(compile-scala-const-value x wr)) values)   
    (dis "}" dnl wr)
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
    

    (dis "  val Length = " (get-scala-type-size wire-type) ";" dnl
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
	  (dis "class " scala-name-os "(os: OutputStream) extends DataOutputStream(os) {" dnl wr)
	  (dis " def write" scala-name "( x  :"  scala-name ".Value) = { this.write" scala-wire-type "(x.id) }" dnl wr)
	
      (dis "}" dnl wr)
	  (dis "class " scala-name-is "(is: InputStream) extends DataInputStream(is) {" dnl wr)
	  (dis " def read" scala-name "() : " scala-name ".Value = " scala-name "(unsigned" scala-wire-type "toInt(this.read" scala-wire-type "()))" dnl wr)
	
	  (dis "}" dnl wr)
	  (dis "object " scala-name-is "{" dnl wr)
      (dis " implicit def isTo" scala-name-is "(is: InputStream) : " scala-name-is " = new " scala-name-is "(is)" dnl wr)
	  (dis "}" dnl wr)
      
	  (dis "object " scala-name-os "{" dnl wr)
	  (dis " implicit def osTo" scala-name-os "(os: OutputStream) : " scala-name-os " = new " scala-name-os "(os)" dnl wr)
	  (dis "}" dnl wr)
      
	)
    (close-scala scala-wrs)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HEADER FIELD TYPES

(define (get-scala-type type)
  (cond ((member? type '(u8 u16 u32 u64)) (scheme->scala type))
        ((array-type? type)
         (string-append "Array[" (get-scala-type (cadr type)) "]"
                         " /* " (caddr type) " */" ))
        (else
         (symbol->string (symbol-append (get-tgt-typemapping type) ".Value")))))

(define (get-scala-type-2 type)
  (cond ((member? type '(u8 u16 u32 u64)) (scheme->scala type))
        ((array-type? type)
         (string-append "Array[" (get-scala-type (cadr type)) "]"
                         " /* " (caddr type) " */" ))
        (else
         (symbol->string (symbol-append (get-tgt-typemapping type))))))

(define (make-dotted-reference intf member)
  (symbol->string (symbol-append intf (symbol-append "." member))))

(define (get-scala-type-size type)
  (get-type-size type make-dotted-reference))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

         
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
                         (sa " = " (symbol->scalainteger (cadar tail)))))
          
        )
    (dis "    " field-name " : Long "  defval "," dnl i-wr)))

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
      (dis dnl "case class " scala-name " (" dnl wr)
      (map (lambda(f)(emit-bitstruct-field-type f wr)) fields)
      (dis " )" wr)
	  (dis " extends BitStruct" dnl wr)
      )

    (add-tgt-type! nm scala-name)
	(dis "package switch_wm.model_server" dnl dnl wr)
	(dis "import java.io._" dnl wr)

    ;; the matching Modula-3 declaration
    (emit-m3-t)
    (dis " { " wr)
	(define (emit-array-builder)
	;; if setField(a : Array[Byte], bitPos : Int, length : Int, value : Long) = {
	(define (emit-array-element f)
      (let* ((field-name (car f))
         (field-type (cadr f))
         (tail       (cddr f)))
         (dis "  BitStruct.setField(a, bitPos, " field-type  ", " field-name ")" dnl wr)  
         (dis "  bitPos += " field-type dnl wr)          
        ))
      (map (lambda(f)(emit-array-element f)) fields)
     )
    (dis dnl "def toByteArray : Array[Byte] = { " dnl wr)
	(dis " val a = Array.ofDim[Byte](" scala-name ".LengthBits / 8)" dnl wr)
	(dis " var bitPos = 0" dnl wr)
    (dis  " // modify A for each field, with insert operations" dnl wr)
	(emit-array-builder)
	(dis   " a " dnl wr)  
	(dis  " }" dnl wr)

    (dis "} " wr)

	(dis dnl "object " scala-name " { " dnl wr)
	(dis dnl "val LengthBits = " (accumulate + 0 (map cadr fields)) dnl wr)
    (define (emit-array-extractor)
	  
	;; if setField(a : Array[Byte], bitPos : Int, length : Int, value : Long) = {
	(define (emit-array-element f)
      (let* ((field-name (car f))
         (field-type (cadr f))
         (tail       (cddr f)))
         (dis "    " field-name " =  BitStruct.extractField(a, autoInc(" field-type "), " field-type  ")," dnl wr)
        ))
	  (dis "  var bitPos = 0" dnl wr)
	  (dis "  def autoInc(amt : Int) = { val tmp = bitPos ; bitPos += amt ; tmp }" dnl wr)
	  (dis "  new " scala-name "( " wr)
      (map (lambda(f)(emit-array-element f)) fields)
	  (dis "  )" wr)

     )
	(dis dnl " def apply(a : Array[Byte]) : " scala-name " = {" wr)
	(dis dnl "  require(a.size == LengthBits / 8)" dnl wr)
	(emit-array-extractor)
    (dis dnl "  // return a new object from applying the array provided" dnl wr)
	(dis dnl " }" wr)

	(dis dnl "}" dnl wr)
	(let* ((scala-name-is (string-append scala-name "InputStream"))
	       (scala-name-os (string-append scala-name "OutputStream")))
	  (dis "class " scala-name-os "(os : OutputStream) extends DataOutputStream(os) {" dnl wr)
	  (dis " def write" scala-name "( x  :"  scala-name ") = { write(x.toByteArray,0," scala-name ".LengthBits/8) }" dnl wr)
	
      (dis "}" dnl wr)
	  (dis "class " scala-name-is "(is : InputStream) extends DataInputStream(is) {" dnl wr)
	  (dis " def read" scala-name "() : " scala-name " = {" dnl "  val a = Array.ofDim[Byte](" scala-name ".LengthBits / 8)" dnl "  readFully(a) " dnl "  " scala-name "(a)" dnl wr)
	
	  (dis " }" dnl wr)
	  (dis "}" dnl wr)

      (dis "object " scala-name-is " {" dnl wr) 
	  (dis "  implicit def isTo" scala-name-is "(is: InputStream) : " scala-name-is " = new " scala-name-is "(is)" dnl wr)
	  (dis "}" dnl wr) 

	  (dis "object " scala-name-os " {" dnl wr) 
	  (dis " implicit def osTo" scala-name-os "(os: OutputStream) : " scala-name-os " = new " scala-name-os "(os)" dnl wr)
	  (dis "}" dnl wr) 

	)

    (close-scala wrs)
)
)

(define get-scala-field get-scala-type)

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
      (define (emit-header-field-type f)
         (dis "    " (legalize-field (car f)) " : " (get-scala-type (cadr f)) "," dnl wr))
      (dis " (" dnl wr)
      (for-each (lambda(f)(emit-header-field-type f)) fields)
      (dis ") //" dnl dnl wr))
	  
	  
	  (define (get-scala-type-2 type)
        (cond ((member? type '(u8 u16 u32 u64)) (scheme->scala type))
        ((array-type? type)
         (string-append "Array" (get-scala-type (cadr type)) "(" (caddr type) ")"))
        (else
         (symbol->string (symbol-append (get-tgt-typemapping type))))))
	  (define (get-scala-type-3 type)
        (cond ((member? type '(u8 u16 u32 u64)) (scheme->scala type))
        ((array-type? type)
         (string-append "Array" (get-scala-type (cadr type))))
        (else
         (symbol->string (symbol-append (get-tgt-typemapping type))))))

		 
	(define (emit-scala-read)
      (define (emit-field-read f)
         (dis "    " (legalize-field (car f)) " = this.read" (get-scala-type-2 (cadr f)) "," dnl wr))
      (for-each (lambda(f)(emit-field-read f)) fields))
	(define (emit-scala-write)
      (define (emit-field-write f)
		 (dis "    this.write" (get-scala-type-3 (cadr f)) "(x." (legalize-field (car f)) ")" dnl wr))
      (for-each (lambda(f)(emit-field-write f)) fields))

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
    (dis "package switch_wm.model_server" dnl dnl wr)
    (dis "import java.io._" dnl wr)
    (dis "import switch_wm.PrimitiveTypes._" dnl dnl wr)
    (dis "import Implicits._" dnl dnl wr)

    (dis "case class " scala-name dnl wr)
    (emit-scala-t)
    ;;(dis "{" dnl wr)
    ;; a symbol called Length, denoting the wire size of the record
    ;; (emit-length)
    ;;(dis "}" dnl wr)
    (dis dnl wr)
	
	(let* ((scala-name-is (string-append scala-name "InputStream"))
	       (scala-name-os (string-append scala-name "OutputStream")))
	  (dis "class " scala-name-os "(os: OutputStream) extends DataOutputStream(os) {" dnl wr)
	  (dis " def write" scala-name "( x  :"  scala-name ") = { " dnl wr)
		 (emit-scala-write)
	  (dis " }" dnl wr)
	
      (dis "}" dnl wr)
	  (dis "class " scala-name-is "(is : InputStream) extends DataInputStream(is) {" dnl wr)
	  (dis " def read" scala-name "() : " scala-name " = {" dnl wr)
	       (dis "   " scala-name "(" dnl wr)
		   (emit-scala-read)
		   (dis "  ) " dnl wr)
		   (dis " }" dnl wr)

	  (dis "}" dnl wr)
	  (dis "object " scala-name-is "{" dnl wr)
      (dis "implicit def isTo" scala-name-is "(is: InputStream) : " scala-name-is " = new " scala-name-is "(is)" dnl wr)
	  (dis "}" dnl wr)

	  (dis "object " scala-name-os "{" dnl wr)
	  (dis " implicit def osTo" scala-name-os "(os: OutputStream) : " scala-name-os " = new " scala-name-os "(os)" dnl wr)
	  (dis "}" dnl wr)
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
;;(set! compile-constants!  (lambda(nm x) #t))
;;(set! compile-header!     (lambda(nm x) #t))
;;(set! compile-bitstruct!  (lambda(nm x) #t))
