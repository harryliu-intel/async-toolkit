(require-modules "time")

(define data (load-normal "mpz.scm"))

(define (convert-type cty)
  (case cty
    ((unsigned-long unsigned-long-int)     "Word.T")
    ((int signed-long-int)                 "INTEGER")
    ((mpz_ptr mpz_srcptr)                  "MpzPtrT")
    ((const-char-*)                        "Ctypes.const_char_star")
    ((double)                              "LONGREAL")
    ((size_t)                              "CARDINAL")
    ((mp_bitcnt_t)                         "Word.T")
    (else #f)
    )
  )

(define (convert-wrapped-type cty)
  (case cty
    ((mpz_ptr mpz_srcptr)   "T")
    ((const-char-*)         "TEXT")
    (else                   (convert-type cty))
    )
  )


(define *last-cardinal* (Word.Minus (Word.Shift 1 63) 1)) ;; not right, oh well

(define (void-type? cty) (eq? cty 'void))

(define (get-proto-nm p) (car p))
(define (get-proto-rtype p) (cadr p))
(define (get-proto-ftypes p) (cddr p))

(define (proto-understood? proto)
  (and
   (or (void-type? (get-proto-rtype proto))
       (convert-type (get-proto-rtype proto)))
   (not (member #f (map convert-type (get-proto-ftypes proto)))))
)

(define (m3-mpz-proto proto)
  (make-m3-proto "c_" convert-type proto))

(define (m3-mpz-wrap-proto proto)
  (make-m3-proto "" convert-wrapped-type proto))

(define (make-m3-proto pfx convert proto)
  (let ((wx (Wx.New)))

    (define (p . x)
      (map (curry Wx.PutText wx) x)
      )
    
    (p "PROCEDURE "
       pfx (remove-prefix "mpz_" (symbol->string (get-proto-nm proto)))
       " (")
    
    (let loop ((fp (get-proto-ftypes proto))
               (i 0))
      (if (null? fp)
          #t
          (begin
            (p "f" (number->string i) " : " (convert (car fp)))
            (p (if (null? (cdr fp)) "" "; "))
            (loop (cdr fp) (+ i 1))))
      );;tel
    
    (p ")")

    (let ((rty (get-proto-rtype proto)))
      (if (not (void-type? rty))
        (p " : " (convert rty))))
    
    (Wx.ToText wx)
    )
  )

(define (m3-mpz-proto-decl proto)
  (sa "<*EXTERNAL \"__g" (symbol->string (get-proto-nm proto)) "\" *>" dnl
      (m3-mpz-proto proto) ";" dnl
      dnl)
  )

(define (m3-mpz-wrap-proto-decl proto)
  (sa (m3-mpz-wrap-proto proto) ";" dnl
      dnl))

(define (m3-convert-call-arg nm type)
  (let ((m3type (convert-wrapped-type type)))
    (cond ((equal? m3type "T")     (sa "ADR(" nm ".val)"))
          ((equal? m3type "TEXT")  (sa "M3toC.CopyTtoS(" nm ")"))
          (else nm))
    )
  )

(define (remove-prefix pfx from)
  (if (CitTextUtils.HavePrefix from pfx)
      (CitTextUtils.RemovePrefix from pfx)
      from
      )
  )

(define (m3-mpz-wrap-proto-impl proto)
  (let* ((rtype  (get-proto-rtype proto))
         (nam    (remove-prefix "mpz_" (symbol->string (get-proto-nm proto))))
         (ftypes (get-proto-ftypes proto))
         (fnames (map
                  (curry sa "f")
                  (map number->string
                       (count-execute (length ftypes) identity))))
         
         )

    (dis "fnames : " fnames dnl)
    (dis "ftypes : " ftypes dnl)
    
    (sa (m3-mpz-wrap-proto proto) " =" dnl
        "  BEGIN" dnl
        "    "
        (if (void-type? rtype) "" "RETURN ")
        "P.c_" nam "("

        (infixize (map m3-convert-call-arg fnames ftypes) ",")
        
        ")" dnl
        "  END " nam ";" dnl
        dnl)
    )
  )


(define (force-string x)
  (cond ((boolean? x) (stringify x))
        ((number? x)  (stringify x))
        ((symbol? x)  (symbol->string x))
        ((string? x) x)
        ((char? x) (list->string (list x)))
        (else (error "can't force to string : " x))))

(define *m3-ident-pat* (RegEx.Compile "^[a-zA-Z][a-zA-Z_0-9]*$"))

(define (legal-m3-identifier? sym)
  (let ((str (symbol->string sym)))
    (= 0 (RegEx.Execute *m3-ident-pat* str 0 1e9 '()))
    )
  )

(define m3-proto-ok?
  ;; we can generate the m3 interface if the C identifier is legal
  ;; and we understand the type signature
  (filter-and proto-understood?
              (compose legal-m3-identifier? get-proto-nm)))

(define *m3-ban-list* '(_mpz_cmp_si _mpz_cmp_ui mpz_2fac_ui mpz_mfac_uiui mpz_primorial_ui mpz_powm_sec))

(define (m3-allowed-proto? sym)
  (not (member sym *m3-ban-list*)))

(define (m3-mpz-write-code!)
  (let ((the-procs (filter (filter-and
                            (compose m3-allowed-proto? get-proto-nm)
                            proto-understood?)
                           data)))
    
    (let ((pi3wr (FileWr.Open "MpzP.i3")))

      (define (pi . x)
        (map (curry Wr.PutText pi3wr) (map force-string x)))

      (pi "INTERFACE MpzP;" dnl
          "IMPORT Word;" dnl
          "IMPORT Ctypes;" dnl
          "IMPORT Cstddef;" dnl
          dnl)

      (pi "TYPE T       = MpzPtrT;" dnl
          "     MpzPtrT = ADDRESS;" dnl
          dnl)


      (pi
       "(***** hand-coded functions *****)" dnl
       "<*EXTERNAL mpz_format_octal*>" dnl
       "PROCEDURE format_octal(f0 : MpzPtrT) : Ctypes.const_char_star;" dnl
       dnl
       "<*EXTERNAL mpz_format_decimal*>" dnl
       "PROCEDURE format_decimal(f0 : MpzPtrT) : Ctypes.const_char_star;" dnl
       dnl
       "<*EXTERNAL mpz_format_hexadecimal*>" dnl
       "PROCEDURE format_hexadecimal(f0 : MpzPtrT) : Ctypes.const_char_star;" dnl
       dnl
       "<*EXTERNAL mpz_free_formatted*>" dnl
       "PROCEDURE free_formatted(f0 : Ctypes.char_star);" dnl
       dnl
       "<*EXTERNAL \"__gmpz_import\"*>" dnl
       "PROCEDURE import(rop : MpzPtrT; count : Cstddef.size_t; order : Ctypes.int; size : Cstddef.size_t; endian : Ctypes.int; nails : Cstddef.size_t; op : ADDRESS);" dnl
       dnl
       "<*EXTERNAL \"__gmpz_export\"*>" dnl
       "PROCEDURE export(rop : ADDRESS; count : ADDRESS; order : Ctypes.int; size : Cstddef.size_t; endian : Ctypes.int; nails : Cstddef.size_t; op : MpzPtrT);" dnl
       dnl

       
       dnl)

      (pi
       "(***** auto-generated functions *****)" dnl
       dnl)
      
      (map pi (map m3-mpz-proto-decl the-procs))
      
      (pi "END MpzP." dnl)
      
      
      (Wr.Close pi3wr)
      )

    (let ((i3wr (FileWr.Open "Mpz.i3")))

      (define (i . x)
        (map (curry Wr.PutText i3wr) (map force-string x)))

      (i
       "(**** AUTOMATICALLY GENERATED -- DO NOT EDIT ****)" dnl
       dnl
       "INTERFACE Mpz;" dnl
       "" dnl
       "IMPORT Word;" dnl
       dnl
       "TYPE" dnl
       "  T <: REFANY;" dnl
       "" dnl
       "PROCEDURE New() : T;" dnl
       "" dnl
       "CONST Brand = \"Mpz\";" dnl
       "" dnl
       "" dnl
       "TYPE FormatBase = { Binary, Octal, Decimal, Hexadecimal };" dnl
       "     " dnl
       "PROCEDURE Format(t : T; base := FormatBase.Decimal) : TEXT;" dnl
       "  " dnl
       "PROCEDURE FormatDecimal(t : T) : TEXT;" dnl
       "" dnl
       "PROCEDURE FormatHexadecimal(t : T) : TEXT;" dnl
       "" dnl
       "PROCEDURE FormatOctal(t : T) : TEXT;" dnl
       "" dnl
       "PROCEDURE Import(t : T; READONLY data : ARRAY OF Word.T);" dnl
       "" dnl
       "PROCEDURE Export(VAR data : ARRAY OF Word.T; t : T);" dnl
       "" dnl
       dnl)
        
      (i
       "(***** auto-generated functions *****)" dnl
       dnl)
      

      (map i (map m3-mpz-wrap-proto-decl the-procs))
      
      (i "END Mpz." dnl)
      
      
      (Wr.Close i3wr)
      )

    (let ((m3wr (FileWr.Open "MpzOps.m3")))

      (define (m . x)
        (map (curry Wr.PutText m3wr) (map force-string x)))

      (m "UNSAFE MODULE MpzOps EXPORTS Mpz;" dnl
;;         "IMPORT Mpz;" dnl
         "IMPORT MpzRep;" dnl
         "IMPORT MpzP AS P;" dnl
;;         "IMPORT Ctypes;" dnl
         "IMPORT Word;" dnl
         "IMPORT M3toC;" dnl
         dnl)

      (map m (map m3-mpz-wrap-proto-impl the-procs))
      
      (m "BEGIN END MpzOps." dnl)
      
      
      (Wr.Close m3wr)
      )
    )
  )
