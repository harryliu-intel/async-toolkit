(require-modules "basic-defs" "display" "hashtable" "struct" "set" "m3")

(define types '((Orientation 
                 (N S W E FN FS FW FE)) ;; PV 5.7 p.267
                (Use 
                 (SIGNAL POWER GROUND CLOCK TIEOFF ANALOG SCAN RESET))
                (Source
                 (DIST NETLIST TEST TIMING USER))
                (Pattern
                 (BALANCED STEINER TRUNK WIREDLOGIC))
                (AntennaModel
                 (OXIDE1 OXIDE2 OXIDE3 OXIDE4))
                (Direction 
                 (INPUT OUTPUT INOUT FEEDTHRU)))
)

(define tokens '(DISTANCE MICRONS REAL STRING INTEGER 
                 RANGE END DO BY STEP MASK RECT VIA VIARULE WIREEXT
                 LAYER SAMEMASK SPACING HARDSPACING WIDTH MINCUTS
                 PROPERTY DIAGWIDTH TYPE FENCE GUIDE EEQMASTER SOURCE
                 NETLIST DIST USER TIMING FIXED COVER PLACED UNPLACED
                 HALO SOFT ROUTEHALO WEIGHT REGION VERSION DIVIDERCHAR
                 BUSBITCHARS DESIGN UNITS PROPERTYDEFINITIONS DIEAREA ROW
                 TRCKS GCELLGRID VIAS NONDEFAULTRULES REGIONS 
                 COMPONENTMASKSHIFT COMPONENTS PINS BLOCKAGES
                 FILLS SPECIALNETS NETS GROUPS SPECIALNET NET COMPONENTPIN
                 NONDEFAULTRULE COMPONENT DIRECTION USE))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; COMMON HELPERS
;;

(define deriv-dir "../AMD64_LINUX/")

(define (fromhex x) (Scan.Int (stringify x) 16))

(define (symbol-append . x) ;; permissive form, allow both symbol and string
  (string->symbol
   (eval
    (cons 'string-append
          (map (lambda (s)
                 (cond ((symbol? s) (symbol->string s))
                       ((string? s) s)
                       (else (error (string-append
                                     "not a string or symbol : " s)))))
               x)))))

(define sa string-append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FILE HANDLING

(define (cmp-files-safely fn1 fn2)
  (let ((res #f))
    (unwind-protect
     (set! res (cmp-files fn1 fn2)) #f #f)
    res))

(define (rename-file-if-different fn1 fn2)
  (if (not (cmp-files-safely fn1 fn2))
      (fs-rename fn1 fn2) ;; copy the scratch over the target
      (FS.DeleteFile fn1) ;; else delete the scratch
      ))

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

(define (put-m3-imports wr)
  (dis "(* IMPORTs *)" dnl
       dnl
       wr))

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
         "BEGIN Do() END " nm "." dnl m-wr)
    (wr-close i-wr)
    (wr-close m-wr)
    (rename-file-if-different (sa deriv-dir nm ".i3.tmp")
                              (sa deriv-dir nm ".i3"))
    (rename-file-if-different (sa deriv-dir nm ".m3.tmp")
                              (sa deriv-dir nm ".m3"))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (intersperse lst sep)
  ;; this routine MUST BE tail-recursive, or we shall definitely
  ;; run out of stack space!
  (define (helper lst so-far)
    (cond ((null? lst) so-far)
          ((null? (cdr lst)) (cons (car lst) so-far))

          (else
           (helper (cdr lst)
                   (cons sep  (cons (car lst) so-far))))))
  (reverse (helper lst '()))
  )

(define (infixize string-list sep)
  (if (null? string-list) ""
      (apply sa
             (intersperse string-list sep))))     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define e '()) ;; debugging slush bucket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(do-build-types types 'Def)

(define (single-quote str)
  (string-append "'" str "'"))

(define (double-quote str)
  (string-append "\"" str "\""))

(define (do-token-i3 tok wr)
  (let* ((str (symbol->string tok))
         (lst (string->list str))
         (slst (map single-quote (map Text.FromChar lst)))
         )
    (set! e slst)
    (dis "VAR "tok"a : REF ARRAY OF CHAR;" dnl
         "CONST T_"tok" = ARRAY [0.."(length slst)"-1] OF CHAR { "(infixize slst ",")" };" dnl
         dnl 
         wr)
  )
)

(define *lookup-name* "Lookup")   
                              
(define *lookup-proto* 
  (string-append
 "PROCEDURE "*lookup-name*"(READONLY a : ARRAY OF CHAR; VAR v : T) : BOOLEAN"
 )
  )

(define (T-prefix str) (string-append "T_" str))
(define (a-suffix str) (string-append str "a"))

(define (do-build-tokens tokens mod-name)
  (let* ((wrs (open-m3 mod-name))
         (i-wr (car  wrs))
         (m-wr (cadr wrs))
         (nm   (caddr wrs))
         )

    (map (lambda(tok)(do-token-i3 tok i-wr)) tokens)


    (dis
"TYPE T = { " (infixize (map T-prefix (map symbol->string tokens)) ", ") " };" dnl
dnl
"CONST N = ARRAY T OF TEXT { " (infixize (map double-quote (map symbol->string tokens)) ", ") " };" dnl
dnl
"VAR A : ARRAY T OF REF ARRAY OF CHAR;" dnl
dnl
*lookup-proto* ";" dnl
dnl
    i-wr)

    (dis 
"IMPORT Text;" dnl
"IMPORT CharCardTrie AS Trie;" dnl
dnl
"CONST NotFound = ORD(LAST(T)) + 1;" dnl
dnl
"PROCEDURE MakeCA(txt : TEXT) : REF ARRAY OF CHAR =" dnl
"  VAR" dnl
"    buff := NEW(REF ARRAY OF CHAR, Text.Length(txt));" dnl
"  BEGIN" dnl
"    Text.SetChars(buff^, txt);" dnl
"    RETURN buff" dnl
"  END MakeCA;" dnl
dnl
*lookup-proto* "=" dnl
"  BEGIN" dnl
"    WITH x = trie.get(a) DO"  dnl
"      IF x = NotFound THEN" dnl
"        RETURN FALSE" dnl
"      ELSE" dnl
"        v := VAL(x,T);"
"        RETURN TRUE" dnl
"      END" dnl
"    END" dnl
"  END " *lookup-name* ";" dnl
dnl
     m-wr)

     (dis
"VAR trie := NEW(Trie.T).init(NotFound);" dnl
dnl
"PROCEDURE Do()=" dnl
"  BEGIN" dnl
m-wr)

     (define (make-a tok)
       (let ((str (symbol->string tok)))

         (dis "    WITH x = trie.put(T_"str", ORD(T.T_"str")) DO <*ASSERT x = NotFound *> END;" dnl m-wr)
   
         (dis "    "str"a := MakeCA(\"" str "\");" dnl dnl m-wr)))

     (map make-a tokens)

     (dis
"    A := ARRAY T OF REF ARRAY OF CHAR { " (infixize (map a-suffix (map symbol->string tokens)) ", ") " };" dnl
"  END Do;" dnl
dnl
    m-wr)

    (close-m3 wrs)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-build-types types pfx)

  (define (do-build-type type)
    (let* ((mod-nm (symbol-append pfx (car type))))
           (do-build-tokens (cadr type) mod-nm)))

  (map do-build-type types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(do-build-tokens tokens 'DefTokens)

(do-build-types types 'Def)
