(require-modules "basic-defs" "m3" "display" "hashtable" "struct" "set")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;                                                ;;;;;;;;;;;
;;;;;;;;;;;        MODULA-3 STRUCT COMPILER FOR MBY        ;;;;;;;;;;;
;;;;;;;;;;;                                                ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the Modula-3 (and possibly other languages) data structure
;; compiler.
;;
;; Basic conventions for generated interfaces:
;;
;; Format: human-readable output (debugging)
;; Read  : read structure from a binary format
;; Write : write structure to a binary format
;; Check : pattern-match whether a bit pattern matches a bitstruct spec
;;
;; standard type exported by an interface is T
;; wire type, if given, is W
;;
;; Length, if given, is in BYTES (enum, const, header)
;; LengthBits, if given, is in BITS (only for bitstruct)
;;
;; header types are BIG-ENDIAN (IBM/Internet convention)
;; bitstruct types are LITTLE-ENDIAN (Intel convention)
;;
;; header and bitstruct types offer slightly different interfaces
;;
;; legend for optional versions
;;   C   : with Context (see NetContext.i3)
;;   S   : against ServerPacket (see ServerPacket.i3)
;;   E   : against ServerPacket, and at a ServerPacket.End
;;   ..B : with BOOLEAN result for the match
;; if nothing is stated, the default is to read/write the packet type
;; "hot" (zero-copy off/on the wire) 
;;
;; The compiler itself currently depends on the following Modula-3 code:
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
;; Author : Mika Nystrom <mika.nystroem@intel.com>
;; April, 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HELPER FUNCS

(define (scheme->m3 sym)
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'Camel
                    'Hyphen 'None))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GLOBAL HANDLING

(define m3typemap    'JUNK)

(set! clear-globals!              ;; override clear-globals!
      (lambda()
        (clear-shared-globals!)
        (set! m3typemap      '())
        ))


(define (add-m3-type! nm m3-name)
  (set! m3typemap
        (cons
         (cons nm m3-name)
         m3typemap)))

(define (get-m3-typemapping type)
  (let ((rec (assoc type m3typemap)))
    (if (not rec)
        (error (sa "No M3 type mapping for " (stringify type)))
        (cdr rec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OUTPUT FILE HANDLING

(define (open-m3 nm)
  (let ((i-wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".i3.tmp"))))
        (m-wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".m3.tmp")))))
    (let ((m3m-wr (FileWr.OpenAppend (sa deriv-dir "derived.m3m"))))
      (dis "derived_interface(\""nm"\",VISIBLE)" dnl
           "derived_implementation(\""nm"\")" dnl
           m3m-wr)
      (wr-close m3m-wr))
          
                  
    (dis "INTERFACE " nm ";" dnl i-wr)
    (put-m3-imports i-wr)
    
    (dis "MODULE " nm ";" dnl m-wr)
    (put-m3-imports m-wr)
    
    (list i-wr m-wr nm deriv-dir)))

(define (close-m3 wrs)
  (let ((i-wr      (car wrs))
        (m-wr      (cadr wrs))
        (nm        (caddr wrs))
        (deriv-dir (cadddr wrs)))

    (dis dnl
         "CONST Brand = \"" nm "\";" dnl i-wr)
    (dis dnl
         "END " nm "." dnl i-wr)
    (dis dnl
         "BEGIN END " nm "." dnl m-wr)
    (wr-close i-wr)
    (wr-close m-wr)
    (rename-file-if-different (sa deriv-dir nm ".i3.tmp")
                              (sa deriv-dir nm ".i3"))
    (rename-file-if-different (sa deriv-dir nm ".m3.tmp")
                              (sa deriv-dir nm ".m3"))
    ))

(define (put-m3-imports wr)
  (dis "<*NOWARN*>FROM NetTypes IMPORT U8, U16, U32, U64;" dnl
       "<*NOWARN*>IMPORT WrNet, RdNet, NetError, ServerPacket AS Pkt;" dnl
       "<*NOWARN*>IMPORT Wx, NetTypes, Fmt, NetContext;" dnl
       dnl
       wr))

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

(define (compile-m3-const-value v wr)
  (dis "CONST " (scheme->m3 (car v)) " = " (cadr v) ";" dnl wr)
  )

(define (get-m3-name nm lst)
  (let ((pair (assoc 'm3 lst)))
    (if (null? pair)
        (error (sa "No M3 name for " nm))
        (cadr pair))))

(define (compile-constants-m3! nm x)
  (dis "compiling constants :  " nm dnl)
  (let* ((wire-type    (car x))
         (m3-wire-type (scheme->m3 wire-type))
         (names        (cadr x))
         (values       (caddr x))
         (m3-name      (get-m3-name nm names))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         )
    ;;(dis "m3-name " m3-name dnl)
    (add-m3-type! nm m3-name)
    
    (dis "TYPE W = " m3-wire-type ";" dnl
         dnl
         i3-wr)
    (dis "CONST Write  = WrNet.Put" m3-wire-type ";"  dnl i3-wr)
    (dis "CONST WriteC = WrNet.Put" m3-wire-type "C;" dnl i3-wr)
    (dis "CONST WriteS = WrNet.Put" m3-wire-type "S;" dnl i3-wr)
    (dis "CONST WriteE = WrNet.Put" m3-wire-type "G;" dnl i3-wr)
    (dis "CONST Read   = RdNet.Get" m3-wire-type ";"  dnl i3-wr)
    (dis "CONST ReadS  = RdNet.Get" m3-wire-type "S;" dnl i3-wr)
    (dis "CONST ReadC  = RdNet.Get" m3-wire-type "C;" dnl
         dnl i3-wr)
    (map (lambda(x)(compile-m3-const-value x i3-wr)) values)

    (close-m3 m3-wrs)
    ) ;; *tel
  )

(define compile-constants! compile-constants-m3!)

(define (compile-enum! nm x)
  (dis "compiling enum      :  " nm dnl)
  (let* ((wire-type    (car x))
         (m3-wire-type (scheme->m3 wire-type))
         (names        (cadr x))
         (values       (caddr x))
         (m3-name      (get-m3-name nm names))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         )

    ;;(dis "m3-name " m3-name dnl)
    (add-m3-type! nm m3-name)

    ;; final IMPORTs
    
    (map (lambda(wr)
           (dis "IMPORT Rd, Wr, Thread;" dnl dnl wr)) (list i3-wr m3-wr))

    ;; declare wire type 

    (dis "TYPE W = " m3-wire-type ";" dnl
         dnl
         i3-wr)

    (dis "CONST Length = " (get-m3-type-size wire-type) ";" dnl
         dnl i3-wr)

    (put-m3-proc 'read i3-wr m3-wr)
    (dis
     "  BEGIN RETURN V2T(RdNet.Get"m3-wire-type"(rd)) END Read;" dnl
     dnl m3-wr)

    (put-m3-proc 'readc i3-wr m3-wr)
    (dis
     "  BEGIN RETURN V2T(RdNet.Get"m3-wire-type"C(rd,cx)) END ReadC;" dnl
     dnl m3-wr)

    (put-m3-proc 'reads i3-wr m3-wr)
    (dis
     "  VAR p := at; w : W;" dnl
     "  BEGIN" dnl
     "    WITH success = RdNet.Get"m3-wire-type"S(s,p,w) AND V2TB(w,t) DO" dnl
     "      IF success THEN at := p END;" dnl
     "      RETURN success" dnl
     "    END" dnl
     "   END ReadS;" dnl
     dnl m3-wr)

    (put-m3-proc 'write i3-wr m3-wr)
    (dis
    "  BEGIN WrNet.Put"m3-wire-type"(wr, Vals[t]) END Write;" dnl
         dnl m3-wr)

    (put-m3-proc 'writec i3-wr m3-wr)
    (dis
    "  BEGIN WrNet.Put"m3-wire-type"C(wr, Vals[t], cx) END WriteC;" dnl
         dnl m3-wr)

    (put-m3-proc 'writes i3-wr m3-wr)
    (dis
    "  BEGIN WrNet.Put"m3-wire-type"S(s, at, Vals[t]) END WriteS;" dnl
         dnl m3-wr)

    (put-m3-proc 'writee i3-wr m3-wr)
    (dis
    "  BEGIN WrNet.Put"m3-wire-type"G(s, e, Vals[t]) END WriteE;" dnl
         dnl m3-wr)

    (dis "PROCEDURE V2T(w : W) : T RAISES { NetError.OutOfRange };" dnl
         dnl i3-wr)

    (put-m3-proc 'format i3-wr m3-wr)
    (dis "BEGIN RETURN Names[t] END Format;" dnl
         dnl m3-wr)

    (dis "PROCEDURE V2T(w : W) : T RAISES { NetError.OutOfRange } =" dnl
         "  VAR res : T; BEGIN" dnl
         "    IF NOT V2TB(w,res) THEN RAISE NetError.OutOfRange(w) END;" dnl
         "    RETURN res" dnl
         "  END V2T;" dnl
         dnl m3-wr)
    
    (let loop ((i   0)
               (x   values)
               (t     "TYPE T = { ")
               (names "CONST Names = ARRAY T OF TEXT { ")
               (t2v   "CONST Vals = ARRAY T OF W { ")
               (v2t (sa
                      "PROCEDURE V2TB(w : W; VAR t : T) : BOOLEAN =" dnl
                      "  BEGIN" dnl
                      "    CASE w OF" dnl
                     )))

      (if (null? x) ;; base case of iteration (done case)

          (begin
            (dis t "};" dnl
                 dnl
                 i3-wr)
            (dis names "};" dnl
                 dnl
                 i3-wr)
            (dis t2v "};" dnl
                 dnl
                 i3-wr)
            (dis v2t
                 "    ELSE RETURN FALSE" dnl
                 "    END" dnl
                 "  END V2TB;" dnl
                 dnl
                 m3-wr)
            ) ;; nigeb

          ;; else -- iterate further
          
          (let* ((sym     (scheme->m3 (caar x)))
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
                  (sa t sym comma)
                  (sa names "\"" sym "\"" comma)
                  (sa t2v val comma)
                  (sa v2t "    | " val " => t := T." sym "; RETURN TRUE" dnl)))
          ) ;; fi
      ) ;; pool

    (close-m3 m3-wrs)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HEADER FIELD TYPES

(define (get-m3-type type)
  (cond ((member? type '(u8 u16 u32 u64)) (scheme->m3 type))
        ((array-type? type)
         (sa "ARRAY [0.." (caddr type) "-1] OF "
                        (get-m3-type (cadr type))))
        (else
         (symbol->string (symbol-append (get-m3-typemapping type) ".T")))))

(define (get-m3-type-size type)  ;; PACKED size! -- wire protos are packed!
  (cond ((eq? type 'u8)  "1")
        ((eq? type 'u16) "2")
        ((eq? type 'u32) "4")
        ((eq? type 'u64) "8")
        ((array-type? type) (sa (caddr type)
                                           "*("
                                           (get-m3-type-size (cadr type))
                                           ")"))
        (else
         (symbol->string (symbol-append (get-m3-typemapping type) ".Length")))))

(define (emit-header-field-type f i-wr)
  (dis "    " (car f) " : " (get-m3-type (cadr f)) ";" dnl i-wr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-m3-read-type whch type lhs lev ind)
  (cond ((member? type '(u8 u16 u32 u64))
         (case whch
           ((read)
            (sa ind lhs " := RdNet.Get"(scheme->m3 type)"(rd)"))
           ((readc)
            (sa ind lhs " := RdNet.Get"(scheme->m3 type)"C(rd,cx)"))
           ((reads)
            (sa ind "IF NOT RdNet.Get"(scheme->m3 type)"S(s,p,"lhs") THEN RETURN FALSE END"))
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
            (sa ind lhs " := " (get-m3-typemapping type) ".Read(rd)"))
           ((readc)
            (sa ind lhs " := " (get-m3-typemapping type) ".ReadC(rd,cx)"))
           ((reads)
            (sa ind "IF NOT " (get-m3-typemapping type) ".ReadS(s,p,"lhs") THEN RETURN FALSE END"))
            
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
          "Wx.PutText(wx,NetTypes.Format"(scheme->m3 type)"("rhs"))"
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
          "Wx.PutText(wx," (get-m3-typemapping type) ".Format("rhs"))"))))

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
            (sa ind (get-m3-typemapping type) ".Write(wr,"rhs")"))
           ((writec)
            (sa ind (get-m3-typemapping type) ".WriteC(wr,"rhs",cx)"))
           ((writes)
            (sa ind (get-m3-typemapping type) ".WriteS(s,at,"rhs")"))
           ((writee)
            (sa ind (get-m3-typemapping type) ".WriteE(s,e,"rhs")"))
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
    (dis "    " field-name " : " (get-m3-bitstype field-type) defval ";" dnl i-wr)))

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
  (dis "compiling bitstruct :  " nm dnl)

  (let* ((names (car x))
         (m3-name      (cadr (assoc 'm3 names)))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         (fields       (cadr x))
         )

    (set! e fields)

    (define (emit-m3-t)
      (dis dnl
           "TYPE" dnl
           "  T = RECORD" dnl
           i3-wr)
      (map (lambda(f)(emit-bitstruct-field-type f i3-wr)) fields)
      (dis "  END;" dnl
           dnl i3-wr)
      )

    (define (emit-proc whch decls pre-return return q)
      ;; emit a procedure to do "something" (in parameter whch)
      ;; return value provided in parameter return
      (let ((emitter (eval (symbol-append 'emit-bitstruct-field- whch))))
        (put-m3-proc whch i3-wr m3-wr)
        (dis "  VAR " decls dnl m3-wr)
        (dis "  BEGIN" dnl m3-wr)
        (map (lambda(f)(set! q (emitter f m3-wr q))) fields)

        (dis "    " pre-return dnl m3-wr)
        (dis "    RETURN " return dnl m3-wr)
        (dis "  END " (eval (symbol-append whch '-proc-name)) ";" dnl
             dnl m3-wr)))

    (add-m3-type! nm m3-name)
     
    ;; the obvious imports (should these really be here?) 
    (map (lambda(wr)
           (dis "<*NOWARN*>IMPORT Byte, Word, StructField, Rd, Wr, Thread;" dnl dnl wr)) (list i3-wr m3-wr))

    ;; the matching Modula-3 declaration
    (emit-m3-t)
    (emit-proc 'format
               "wx := Wx.New(); <*UNUSED*>VAR ok := Assert(Check(t));" ;; decls
               ""
               (sa "Fmt.F(\"<"m3-name">{ %s}\", Wx.ToText(wx))")
               #f)

    (emit-proc 'check
               ""        ;; decls
               ""        ;; pre-return
               "TRUE"    ;; return
               #f)

    (dis "PROCEDURE Check2(READONLY t : T; VAR u : T) : BOOLEAN = "dnl"  BEGIN" dnl"    WITH check = Check(t) DO IF check THEN u := t END; RETURN check END" dnl"  END Check2;" dnl dnl m3-wr)

    (dis "PROCEDURE Start(sz : CARDINAL; e : Pkt.End) : CARDINAL ="dnl
         "  BEGIN" dnl
         "    CASE e OF" dnl
         "      Pkt.End.Front => RETURN 0" dnl
         "    | " dnl
         "      Pkt.End.Back =>" dnl
         "      <*ASSERT LengthBits MOD 8 = 0*>" dnl
         "      RETURN sz - LengthBits DIV 8" dnl
         "    END" dnl
         "  END Start;" dnl
         dnl
         m3-wr)

    (emit-proc 'readsb
               "w : Word.T; t : T;"
               ""
               "Check2(t,res)"
               0) ;; read from ServerPacket at i

    (put-m3-proc 'readeb i3-wr m3-wr)
    (dis
     "  BEGIN" dnl
     "    WITH ok = ReadSB(s, Start(s.size(),e), res) DO" dnl
     "      IF ok THEN Pkt.Truncate(s, e, LengthBits DIV 8) END;" dnl
     "      RETURN ok" dnl
     "    END" dnl
     "  END " readeb-proc-name ";" dnl
     dnl
     m3-wr)

    (dis "PROCEDURE Assert(q : BOOLEAN) : BOOLEAN = BEGIN <*ASSERT q*>RETURN q END Assert;" dnl dnl m3-wr)

    (emit-proc 'writee
               "a : ARRAY [0..(LengthBits-1) DIV 8 + 1-1] OF Byte.T;<*UNUSED*>VAR ok := Assert(Check(t)); "
               "Pkt.PutA(s,e,a);"
               ""
               0)
    
    (dis dnl "CONST LengthBits = " (accumulate + 0 (map cadr fields)) ";" dnl
         dnl i3-wr)

    (close-m3 m3-wrs)

    ))

(define (compile-header! nm x)
  (dis "compiling header    :  " nm dnl)
  (let* ((names (car x))
         (m3-name      (cadr (assoc 'm3 names)))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         (fields       (cadr x))
         (types        (uniq eq? (map cadr fields)))

         (import-intfs
          (filter (lambda(x) (not (member? x '(u8 u16 u32 u64))))
                  (map get-elem-type types)))
         )

    (define (emit-m3-t)
      (dis dnl
           "TYPE" dnl
           "  T = RECORD" dnl
           i3-wr)
      (map (lambda(f)(emit-header-field-type f i3-wr)) fields)
      (dis "  END;" dnl
           dnl i3-wr)
      )

    (define (emit-length)
      (dis "CONST Length = 0" i3-wr)
      (map
       (lambda(f)(dis
                  (sa " + " (get-m3-type-size (cadr f)))
                  i3-wr))
       fields)
      (dis ";" dnl i3-wr)
      
      (dis dnl m3-wr)
      )

    (define (emit-proc whch decls return . opt-pre-ret)
      ;; emit a procedure to do "something" (in parameter whch)
      ;; return value provided in parameter return
      ;; pre-return is optional and allows a final fixup before
      ;; returning.
      (let ((emitter (eval (symbol-append 'emit-header-field- whch))))
        (put-m3-proc whch i3-wr m3-wr)
        (dis "  VAR " decls dnl m3-wr)
        (dis "  BEGIN" dnl m3-wr)
        (map (lambda(f)(emitter f m3-wr 'up)) fields)

        ;; special case for the generic write, can do backwards too
        (if (eq? whch 'writee)
            (map (lambda(f)(emitter f m3-wr 'dn)) (reverse fields)))
        (if (not (null? opt-pre-ret))
            (dis "    " (car opt-pre-ret) dnl m3-wr))
        (dis "    RETURN " return dnl m3-wr)
        (dis "  END " (eval (symbol-append whch '-proc-name)) ";" dnl
             dnl m3-wr)))
      
    ;;(dis "m3-name " m3-name dnl)
    (add-m3-type! nm m3-name)
    (set! e import-intfs)

    ;; the obvious imports (should these really be here?) 
    (map (lambda(wr)
           (dis "IMPORT Rd, Wr, Thread;" dnl dnl wr)) (list i3-wr m3-wr))

    ;; emit the imports for the interfaces needed
    (map (lambda (wr)
           (map (lambda(intf)
                  (dis "IMPORT " (get-m3-typemapping intf) ";" dnl wr))
                import-intfs))
         (list i3-wr m3-wr))

    ;; the matching Modula-3 declaration
    (emit-m3-t)

    ;; a symbol called Length, denoting the wire size of the record
    (emit-length)

    ;; procedures for reading the record off the wire
    ;;          whch    decls     return  opt-pre-ret
    (emit-proc 'read   "t : T;"   "t"              ) ;; raw read
    (emit-proc 'readc  "t : T;"   "t"              ) ;; read with context
    (emit-proc 'reads  "p := at;" "TRUE" "at := p;") ;; read from Pkt.T

    ;; various ways of writing the record to the wire
    (emit-proc 'write   "" "")         ;; raw write
    (emit-proc 'writec  "" "")         ;; write with context
    (emit-proc 'writes  "" "")         ;; write to ServerPacket at i
    (emit-proc 'writee  "" "")         ;; write to ServerPacket at frt/bck

    ;; procedures for human-readable formatting of packet
    (emit-proc 'format
               "wx := Wx.New();"
               (sa "Fmt.F(\"<"m3-name">{ %s }\", Wx.ToText(wx))"))
    
    (dis dnl m3-wr)
    (close-m3 m3-wrs)
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
