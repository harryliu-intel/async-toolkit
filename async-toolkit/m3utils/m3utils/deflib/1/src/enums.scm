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
                (Shape
                 (RING PADRING BLOCKRING STRIPE FOLLOWPIN IOWIRE COREWIRE 
                       BLOCKWIRE BLOCKAGEWIRE FILLWIRE FILLWIREOPC DRCFILL))
                (ObjectType
                 (COMPONENT COMPONENTPIN DESIGN GROUP NET NONDEFAULTRULE
                            REGION ROW SPECIALNET))
                (PropType
                 (INTEGER REAL STRING))
                
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
                 NONDEFAULTRULE COMPONENT DIRECTION USE SPECIAL
                 POLYGON PORT PLACEMENT SLOTS PUSHDOWN EXCEPTPGNET
                 DESIGNRULEWIDTH OPC SHAPE STYLE NEW ROUTED SHIELD
                 VOLTAGE FIXEDBUMP ORIGINAL PATTERN ESTCAP
                 TOKENS ANTENNAMODEL PIN SYNTHESIZED
                 SUBNET XTALK FREQUENCY TAPER TAPERRULE NOSHIELD VPIN
                 SHIELDNET MUSTJOIN VIRTUAL
                 
                 ORIENTATION OBJECTTYPE PROPTYPE ;; fake
                 ))


(load "../../../parsesupport/src/shared.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define e '()) ;; debugging slush bucket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(do-build-types types 'Def)

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

(define *get-name* "Get")
(define *peek-name* "Peek")
(define *getkeyed-name* "GetKeyed")
(define *mustbe-name* "MustBe")
(define *mustget-name* "MustGet")
                              
(define *lookup-proto* 
  (string-append
   "PROCEDURE "*lookup-name*"(READONLY a : ARRAY OF CHAR; VAR v : T) : BOOLEAN"
   )
  )

(define *get-proto*
  (string-append
   "PROCEDURE "*get-name*"(p : RecursiveParser.T; VAR t : T) : BOOLEAN RAISES { E }"
   )
  )

(define *peek-proto*
  (string-append
   "PROCEDURE "*peek-name*"(p : RecursiveParser.T; VAR t : T) : BOOLEAN"
   )
  )

(define *getkeyed-proto*
  (string-append
   "PROCEDURE "*getkeyed-name*"(p : RecursiveParser.T; VAR t : T) : BOOLEAN RAISES { E } "
   )
  )

(define *mustbe-proto*
  (string-append
   "PROCEDURE "*mustbe-name*"(p : RecursiveParser.T; VAR t : T) RAISES { E }"
   )
  )

(define *mustget-proto*
  (string-append
   "PROCEDURE "*mustget-name*"(p : RecursiveParser.T) : T RAISES { E }"
   )
  )


(define (T-prefix str) (string-append "T_" str))
(define (a-suffix str) (string-append str "a"))

(define (do-build-tokens tokens mod-name basenm)
  (let* ((wrs (open-m3 mod-name))
         (i-wr (car  wrs))
         (m-wr (cadr wrs))
         (nm   (caddr wrs))
         (unm  (TextUtils.ToUpper (symbol->string basenm)))
         )

    (dis "IMPORT RecursiveParser;" dnl
         "FROM ParseError IMPORT E;" dnl
         dnl
         i-wr)

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
*get-proto* ";" dnl
dnl
*peek-proto* ";" dnl
dnl
*getkeyed-proto* ";" dnl
dnl
*mustbe-proto* ";" dnl
dnl
*mustget-proto* ";" dnl
dnl
dnl
    i-wr)

    (dis 
"IMPORT Text;" dnl
"IMPORT CharCardTrie AS Trie;" dnl
"IMPORT DefTokens AS K;" dnl
"IMPORT RecursiveParser;" dnl
"IMPORT RecursiveParserRep;" dnl
"FROM RecursiveParser IMPORT S2T, Next;" dnl
"FROM ParseError IMPORT E;" dnl
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
"        v := VAL(x,T);" dnl
"        RETURN TRUE" dnl
"      END" dnl
"    END" dnl
"  END " *lookup-name* ";" dnl
dnl
*get-proto* "=" dnl
"  BEGIN" dnl
"    IF Lookup(SUBARRAY(p.buff, p.token.start, p.token.n), t) THEN" dnl
"      Next(p);" dnl
"      RETURN TRUE;" dnl
"    ELSE" dnl
"      RETURN FALSE" dnl
"    END" dnl
"  END " *get-name* ";" dnl
dnl
*peek-proto* "=" dnl
"  BEGIN" dnl
"    IF Lookup(SUBARRAY(p.buff, p.token.start, p.token.n), t) THEN" dnl
"      RETURN TRUE;" dnl
"    ELSE" dnl
"      RETURN FALSE" dnl
"    END" dnl
"  END " *peek-name* ";" dnl
dnl
*getkeyed-proto* "=" dnl
"  BEGIN" dnl
"    IF SUBARRAY(p.buff, p.token.start, p.token.n) # K.T_" unm " THEN" dnl
"      RETURN FALSE" dnl
"    END;" dnl
"    Next(p);" dnl
"    MustBe(p, t);" dnl
"    RETURN TRUE" dnl
"  END " *getkeyed-name* ";" dnl
dnl
*mustbe-proto* "=" dnl
"  BEGIN" dnl
"    IF NOT Get(p, t) THEN" dnl
"      RAISE E(\""*mustbe-name* ":" nm " expected value but got \" & S2T(p.buff, p.token) & \"\")" dnl
"    END" dnl
"  END "*mustbe-name*";" dnl
dnl 
*mustget-proto* "=" dnl
"  VAR" dnl
"    t : T;" dnl
"  BEGIN" dnl
"    IF Get(p, t) THEN" dnl
"      RETURN t" dnl
"    ELSE" dnl
"      RAISE E(\""*mustbe-name* ":" nm " expected value but got \" & S2T(p.buff, p.token) & \"\")" dnl
"    END" dnl
"  END "*mustget-name*";" dnl
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
           (do-build-tokens (cadr type) mod-nm (car type))))

  (map do-build-type types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(do-build-tokens tokens 'DefTokens 'Tokens)

(do-build-types types 'Def)
