
(define (m3-expand-type type)
  (cond ((boolean-type? type) "BOOLEAN")
        ((string-type? type) "TEXT")
        ((integer-type? type) (m3-expand-integer-type type))
        ((array-type? type) (m3-expand-array-type type))
        ((struct-type? type) (m3-expand-struct-type type)) ;; hmm
        (else (error "Unknown type " type))
        )
  )

(define (m3-expand-array-type type)
  ;; this isnt right, this is just an open array
  ;; -- we need the range.
  
  (string-append "ARRAY OF " (m3-expand-type (caddr type))))

(define *big64* (bn 64))
(define *big63* (bn 64))

(define m3-word-min *big0*)
(define m3-word-max (xnum--(xnum-<< *big1* *big64*) *big1*))

(define m3-integer-min (xnum-- (xnum-<< *big1* *big63*)))
(define m3-integer-max (xnum-- (xnum-<< *big1* *big63*) *big1*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; map identifiers (we could do this so much better directly in M3)
;;

(define (string->integer str1)
  ;; single character symbol sym1
  (char->integer (car (string->list str1))))

(define (symbol->integer sym1)
  ;; single character symbol sym1
  (string->integer (symbol->string sym1)))


(define (make-char-in-range? lo hi)
  (lambda(ch)
    (and (>= (char->integer ch) (string->integer lo))
         (<= (char->integer ch) (string->integer hi)))))

(define alpha-checker
  (filter-or (make-char-in-range? "a" "z")
             (make-char-in-range? "A" "Z")))

(define digit-checker
  (make-char-in-range? "0" "9"))

(define *us* (char->integer #\_))

(define (map-char c)
  (cond ((alpha-checker c) (list (char->integer c)))
        ((digit-checker c) (list (char->integer c)))
        (else  `(,*us* 
                  ,@(map char->integer
                         (string->list (number->string (char->integer c))))

                  ,*us*))))
        
(define (m3-map-ident scm-ident) ;; Good grief
;;  (dis "m3-map-ident : " scm-ident dnl)
  (string-append "m3__"
                 (list->string
                  (map integer->char
                       (apply append
                              (map map-char
                                   (string->list
                                    (symbol->string scm-ident))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;

(define (m3-write-port-decl w pdef)
  (w "    " (pad 40 (m3-map-ident (get-port-id pdef))) " : REF "
     (m3-convert-port-type (cadddr pdef))
     ".T;" dnl
     )
  )

(define (m3-format-port-ass pdef)
  (let ((ident (m3-map-ident (get-port-id pdef))))
    (string-append ident " := " ident)
    )
  )
                  
(define (m3-map-decltype type)
  (cond ((string-type?  type) "TEXT")
        ((boolean-type? type) "BOOLEAN")
        ((array-type?   type) (error "not done") )
        ((struct-type?  type) (error "not done") )
        ((integer-type? type)
         (let ((width (cadddr type)))
           (cond ((not (bigint? width)) "DynamicInteger.T")
                 ((caddr type)
                  (string-append "SInt" (BigInt.Format width 10) ".T"))
                 (else
                  (string-append "UInt" (BigInt.Format width 10) ".T"))
                 );;dnoc
           );;tel
         )
        (else (error "m3-map-decltype : unknown type to map : " type))
        );;dnoc
  )

(define (m3-convert-vardecl v1)
  (let ((id (get-var1-id v1))
        (ty (get-decl1-type (get-var1-decl1 v1)))
        )
    (string-append (pad 40 (m3-map-ident id)) " : " (m3-map-decltype ty) ";")
    )
  )

(define (m3-write-shared-locals w the-blocks cell-info the-decls)
  (let* ((shared-local-ids (get-shared-variables the-blocks cell-info))
         (v1s              (map (curry find-decl the-decls) shared-local-ids))
         )
    (w "    (* shared locals list *)" dnl)
    (map (lambda(dt) (w "    " dt dnl))
         (map m3-convert-vardecl v1s))
    )
  )

(define (m3-write-process-closure-list w the-blocks)
  (w "    (* closures *)" dnl)

  (map (lambda(blk)
         (let* ((btag (m3-map-ident (cadr (get-block-label blk)))))
           (w "    " (pad 40 btag "_Cl") " : Closure;" dnl)
           );;*tel
         )
       (cdr the-blocks)
       );;pam
  )

(define (m3-write-proc-frame-decl w port-tbl the-blocks cell-info the-decls)
  (w dnl
     "TYPE" dnl)
  (w "  Frame = Process.Frame OBJECT" dnl)
  (m3-write-port-list w cell-info)
  (w dnl)
  (m3-write-shared-locals w the-blocks cell-info the-decls)
  (w dnl)
  (m3-write-process-closure-list w the-blocks)
  (w dnl)
  (w "  END;" dnl dnl)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m3-write-block-decl w)
  (w dnl
     "TYPE Block = PROCEDURE (cl : Closure) : BOOLEAN;" dnl
     dnl)
  )
  
(define (m3-write-closure-decl w)
  (w dnl
     "TYPE" dnl
     "  Closure = Process.Closure OBJECT" dnl
     "    frame  :  Frame;" dnl
     "    block  :  Block;" dnl
     "  OVERRIDES" dnl
     "    run := Run;" dnl
     "  END;" dnl
     dnl
     "PROCEDURE Run(cl : Closure) = BEGIN EVAL cl.block(cl) END Run;" dnl
     dnl
     )
  )

(define (m3-write-port-list w cell-info)
  (let ((plist (get-ports cell-info)))

    (w "    (* port list *)" dnl)
  
    (map (curry m3-write-port-decl w) plist) 
    )
  )

(define (m3-write-build-signature w cell-info)
  (w "PROCEDURE Build(" dnl)
  (m3-write-port-list w cell-info)
  (w ")")
  )

(define (m3-write-build-decl w cell-info)
  (m3-write-build-signature w cell-info)
  (w ";" dnl dnl)
  )

(define (indent-writer w by) (lambda x (apply w (cons by x))))

(define (m3-write-build-defn w cell-info the-blocks)
  (let ((proc-ports (get-ports cell-info)))
    (m3-write-build-signature w cell-info)
    (w " = " dnl)
    (w "  BEGIN" dnl)
    (w "    WITH frame = NEW(Frame," dnl
       "                     id := NextFrameId()," dnl)

    (let ((asslist (map m3-format-port-ass proc-ports)))
      (map (lambda(ass)
             (w "                     " ass ";" dnl))
           asslist)
      );;tel
    
    (w "      ) DO" dnl)

    ;; build body
    (let ((iw (indent-writer w "     ")))

      (let* ((inlist (filter input-port? proc-ports))
             (ids    (map m3-map-ident (map get-port-id inlist)))
             )
        (map (lambda(m3id)
               (iw "MarkReader(" m3id ", frame);" dnl))
             ids
             )
        )
      (w dnl)
      
      (let* ((outlist (filter output-port? proc-ports))
             (ids    (map m3-map-ident (map get-port-id outlist)))
             )
        (map (lambda(m3id)
               (iw "MarkWriter(" m3id ", frame);" dnl))
             ids
             )
        )

      (w dnl)

      (let* ((blk-labels (map m3-map-ident
                              (map cadr
                                   (map get-block-label (cdr the-blocks)))))
             (iiw (indent-writer iw (pad 45 "")))
             )
        (map (lambda(m3lab)
               (iw  (pad 45 "frame." m3lab "_Cl := NEW(") "Closure," dnl)
               (iiw        "id      := NextId()," dnl)
               (iiw        "frameId := frame.id," dnl)
               (iiw        "frame   := frame," dnl)
               (iiw        "block   := Block_" m3lab ");" dnl dnl)
               )
             blk-labels)
        );;*tel

      (iw "Schedule(frame." (m3-map-ident (cadar the-blocks))"_Cl);" dnl)
      );;tel (iw)
    
    (w "    END(*WITH*)" dnl)
    (w "  END Build;" dnl dnl)
    )
  )

(define (m3-write-text w the-blocks cell-info)

  )

(define (m3-write-modu-imports w)
  (w "IMPORT CspCompiledProcess AS Process;" dnl)

  )

(define (m3-write-intf-imports w)
  (w "IMPORT CspCompiledProcess AS Process;" dnl)
  
  )

(define (m3-convert-port-type ptype)
  ;; return string name of interface that defines the channel type
  ;; requested by the CSP code
  (let ((stype (port-type-short ptype)))
    (case (car stype)
      ((node) (string-append "Node" (BigInt.Format (cadr stype) 10)))
      ((bd)   (string-append "UInt" (BigInt.Format (cadr stype) 10) "Chan"))
      (else (error))
      )
    )
  )

(define (m3-gen-statement w cell-info stmt)
  (w "      (* " (stringify stmt) " *)" dnl dnl)
  )

(define (m3-write-block w cell-info blk)
  (dis (get-block-label blk) dnl)
  (dis "m3-write-block : " blk dnl)
  (let* ((btag (m3-map-ident (cadr (get-block-label blk))))
         (bnam (string-append "Block_" btag))
         (the-code (cddr (filter-out-var1s blk)))
         )
    ;; write decl.  We have to change this later for dynamic parallelism
    (w
     "PROCEDURE "bnam"(cl : Closure) : BOOLEAN =" dnl
     "  BEGIN" dnl
     "    WITH frame = cl.frame DO" dnl)
    
    ;; the block text goes here
    (map (curry m3-gen-statement w cell-info) the-code)

    (w 
     "    END(*WITH*)" dnl
     "  END " bnam ";" dnl
     dnl)
    )
  )

(define (m3-write-blocks w the-blocks cell-info)
  (map (curry m3-write-block w cell-info)
       (cdr the-blocks))
  )

(define (do-m3!)
  (let* ((the-blocks text9)
         (cell-info  *cellinfo*)
         (port-tbl   *the-prt-tbl*)
         (the-decls  (gen-decls the-blocks *proposed-types*))
         (root       (m3-map-ident (string->symbol *the-proc-type-name*)))
         (i3fn       (string-append "build/" root ".i3"))
         (i3wr       (FileWr.Open i3fn))
         (m3fn       (string-append "build/" root ".m3"))
         (m3wr       (FileWr.Open m3fn))
         )

    (define (intf . x) (Wr.PutText i3wr (apply string-append x)))
    (define (modu . x) (Wr.PutText m3wr (apply string-append x)))
    ;; interface file
    
    (intf "INTERFACE " root ";" dnl dnl)

    (m3-write-intf-imports intf)

    (m3-write-build-decl intf cell-info)
    

    (intf dnl "END " root "." dnl)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (modu "MODULE " root ";" dnl dnl)

    (m3-write-modu-imports    modu)
    (m3-write-proc-frame-decl modu port-tbl the-blocks cell-info the-decls)
    (m3-write-block-decl      modu)
    (m3-write-closure-decl    modu)
    (m3-write-build-defn      modu cell-info the-blocks)

    (m3-write-blocks          modu the-blocks cell-info)
    
    (modu dnl "BEGIN END " root "." dnl)
    
    (Wr.Close i3wr)
    (Wr.Close m3wr)
    )
  )

(define (find-decl decls id)
  (cond ((null? decls) #f)
        ((eq? id (get-var1-id (car decls))) (car decls))
        (else (find-decl (cdr decls) id)))
  )
