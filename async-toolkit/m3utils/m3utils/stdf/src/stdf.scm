(require-modules "basic-defs" "m3" "display" "hashtable" "struct" "set")
(load "../src/common.scm")

;;
;; types cf and c12 are not used
;; n1 only appears in an array
;; 

(define record-header ;; not sure whether we need this defn
  '((rec-len u2)
    (rec-typ u1)
    (rec-sub u1)))

(define file-attributes-record
  '(FAR
    ((cpu-type u1)
     (stdf-ver u1))))

(define audit-trail-record
  '(ATR
    ((mod-time u4)
     (cmd-line cn))))

(define master-information-record
  '(MIR
  ((setup-t u4)
    (start-t u4)
    (stat-num u1)
    (mode-cod c1)
    (rtst-cod c1)
    (prot-cod c1)
    (burn-tim u1)
    (cmod-cod c1)
    (lot-id cn)
    (part-typ cn)
    (node-nam cn)
    (tstr-typ cn)
    (job-nam cn)
    (job-rev cn)
    (sblot-id cn)
    (oper-nam cn)
    (exec-typ cn)
    (exec-ver cn)
    (test-cod cn)
    (tst-temp cn)
    (user-txt cn)
    (aux-file cn)
    (pkg-typ cn)
    (famly-id cn)
    (date-cod cn)
    (facil-id cn)
    (floor-id cn)
    (proc-id cn)
    (oper-frq cn)
    (spec-nam cn)
    (spec-ver cn)
    (flow-id cn)
    (setup-id cn)
    (dsgn-rev cn)
    (eng-id cn)
    (rom-cod cn)
    (serl-num cn)
    (supr-nam cn))))

(define master-results-record
  '(MRR
  ((finish-t u4)
    (disp-cod c1)
    (usr-desc cn)
    (exc-desc cn))))

(define part-count-record
  '(PCR
  ((head-num u1)
    (site-num u1)
    (part-cnt u4)
    (rtst-cnt u4)
    (abrt-cnt u4)
    (good-cnt u4)
    (func-cnt u4))))

(define hardware-bin-record
  '(HBR
  ((head-num u1)
    (site-num u1)
    (hbin-num u2)
    (hbin-cnt u4)
    (hbin-pf c1)
    (hbin-nam cn))))

(define software-bin-record
  '(SBR
  ((head-num u1)
    (site-num u1)
    (sbin-num u2)
    (sbin-cnt u4)
    (sbin-pf c1)
    (sbin-nam cn))))

(define pin-map-record
  '(PMR
  ((pmr-indx u2)
    (chan-typ u2)
    (chan-nam cn)
    (phy-nam cn)
    (log-nam cn)
    (head-num u1)
    (site-num u1))))

(define pin-group-record
  '(PGR
  ((grp-indx u2)
    (grp-nam cn)
    (indx-cnt u2)
    (pmr-indx (array indx-cnt u2)))))

(define pin-list-record
  '(PLR
  ((grp-cnt u2)
    (grp-indx (array grp-cnt u2))
    (grp-mode (array grp-cnt u2))
    (grp-radx (array grp-cnt u1))
    (grp-char (array grp-cnt cn))
    (rtn-char (array grp-cnt cn))
    (pgm-chal (array grp-cnt cn))
    (rtn-chal (array grp-cnt cn)))))

(define retest-data-record
  '(RDR
  ((num-bins u2)
    (rtst-bin (array num-bins u2)))))

(define site-description-record
  '(SDR
  ((head-num u1)
    (site-grp u1)
    (site-cnt u1)
    (site-num (array site-cnt u1))
    (hand-typ c1)
    (hand-id c1)
    (card-typ cn)
    (load-typ cn)
    (dib-typ cn)
    (dib-id cn)
    (cabl-typ cn)
    (cont-typ cn)
    (cont-id cn)
    (lasr-typ cn)
    (lasr-id cn)
    (extr-typ cn)
    (extr-id cn))))

(define wafer-information-record
  '(WIR
  ((head-num u1)
    (site-grp u1)
    (start-t u4)
    (wafer-id cn))))

(define wafer-results-record
  '(WRR
  ((head-num u1)
    (site-grp u1)
    (finish-t u4)
    (part-cnt u4)
    (rtst-cnt u4)
    (abrt-cnt u4)
    (good-cnt u4)
    (func-cnt u4)
    (wafer-id cn)
    (fabwf-id cn)
    (frame-id cn)
    (mask-id cn)
    (usr-desc cn)
    (exc-desc cn))))

(define wafer-configuration-record
  '(WCR
  ((wafr-siz r4)
    (die-ht r4)
    (die-wid r4)
    (wf-units u1)
    (wf-flat c1)
    (center-x i2)
    (center-y i2)
    (pos-x c1)
    (pos-y c1))))

(define part-information-record
  '(PIR
  ((head-num u1)
    (site-num u1))))

(define part-results-record
  '(PRR
  ((head-num u1)
    (site-num u1)
    (part-flg b1)
    (num-test u2)
    (hard-bin u2)
    (soft-bin u2)
    (x-coord i2)
    (y-coord i2)
    (test-t u4)
    (part-id cn)
    (part-txt cn)
    (part-fix bn))))

(define test-synopsis-record
  '(TSR
  ((head-num u1)
    (site-num u1)
    (test-typ c1)
    (test-num u4)
    (exec-cnt u4)
    (fail-cnt u4)
    (alrm-cnt u4)
    (test-nam cn)
    (seq-name cn)
    (test-lbl cn)
    (opt-flag b1)
    (test-tim r4)
    (test-min r4)
    (test-max r4)
    (tst-sums r4)
    (tst-sqrs r4))))

(define parametric-test-record
  '(PTR
  ((test-num u4)
    (head-num u1)
    (site-num u1)
    (test-flg b1)
    (parm-flg b1)
    (result r4)
    (test-txt cn)
    (alarm-id cn)
    (opt-flag b1)
    (res-scal i1)
    (llm-scal i1)
    (hlm-scal i1)
    (lo-limit r4)
    (hi-limit r4)
    (units cn)
    (c-resfmt cn)
    (c-llmfmt cn)
    (c-hlmfmt cn)
    (lo-spec r4)
    (hi-spec r4))))

(define multiple-result-parametric-record
  '(MPR
  ((test-num u4)
    (head-num u1)
    (site-num u1)
    (test-flg b1)
    (parm-flg b1)
    (rtn-icnt u2)
    (rslt-cnt u2)
    (rtn-stat (array rtn-icnt n1))
    (rtn-rslt (array rslt-cnt r4))
    (test-txt cn)
    (alarm-id cn)
    (opt-flag b1)
    (res-scal i1)
    (llm-scal i1)
    (hlm-scal i1)
    (lo-limit r4)
    (hi-limit r4)
    (start-in r4)
    (incr-in r4)
    (rtn-indx (array rtn-icnt u2))
    (units cn)
    (units-in cn)
    (c-resfmt cn)
    (c-llmfmt cn)
    (c-hlmfmt cn)
    (lo-spec r4)
    (hi-spec r4))))

(define functional-test-record
  '(FTR
  ((test-num u4)
    (head-num u1)
    (site-num u1)
    (test-flg b1)
    (opt-flag b1)
    (cycl-cnt u4)
    (rel-vadr u4)
    (rept-cnt u4)
    (num-fail u4)
    (xfail-ad i4)
    (yfail-ad i4)
    (vect-off i2)
    (rtn-icnt u2)
    (pgm-icnt u2)
    (rtn-indx (array rtn-icnt u2))
    (rtn-stat (array rtn-icnt n1))
    (pgm-indx (array pgm-icnt u2))
    (pgm-stat (array pgm-icnt n1))
    (fail-pin dn)
    (vect-nam cn)
    (time-set cn)
    (op-code cn)
    (test-txt cn)
    (alrarm-id cn)
    (prog-txt cn)
    (rslt-txt cn)
    (patg-num u1)
    (spin-map dn))))

(define begin-program-section-record
  '(BPS
  ((seq-name cn))))

(define end-program-section-record
  '(EPS
  ()))

(define generic-data-record
  '(GDR
  ((fld-cnt u2)
    (gen-data vn))))

(define datalog-text-record
  '(DTR
  ((text-dat cn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stdf-record-types
  '(
    ;;                              REC_TYP REC_SUB
    (file-attributes-record               0      10)
    (audit-trail-record                   0      20)

    (master-information-record            1      10)
    (master-results-record                1      20)
    (part-count-record                    1      30)
    (hardware-bin-record                  1      40)
    (software-bin-record                  1      50)
    (pin-map-record                       1      60)
    (pin-group-record                     1      62)
    (pin-list-record                      1      63)
    (retest-data-record                   1      70)
    (site-description-record              1      80)

    (wafer-information-record             2      10)
    (wafer-results-record                 2      20)
    (wafer-configuration-record           2      30)

    (part-information-record              5      10)
    (part-results-record                  5      20)

    (test-synopsis-record                10      30)

    (parametric-test-record              15      10)
    (multiple-result-parametric-record   15      15)
    (functional-test-record              15      20)

    (begin-program-section-record        20      10)
    (end-program-section-record          20      20)

    (generic-data-record                 50      10)
    (datalog-text-record                 50      30)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m3-field-type f)
  (if (list? f)
      (let* ((idx (scheme->m3l (cadr f)))
             (m3tn (scheme->m3  (caddr f))))
        (string-append "REF ARRAY OF Stdf" m3tn ".T"))
      (string-append "Stdf" (scheme->m3 f) ".T" )))

(define (m3-field-type-intf f)
  (if (list? f)
      (let* ((idx (scheme->m3l (cadr f)))
             (m3tn (scheme->m3  (caddr f))))
        (string-append "REF ARRAY OF StdfTypes." m3tn))
      (string-append "Stdf" (scheme->m3 f))))

(define (produce-field-decl fieldspec)
  ;; field is 2-entry list: scm-nm scm-typ-nm
  (let* ((m3fn (scheme->m3l (car fieldspec)))
         (m3tn (m3-field-type (cadr fieldspec))))
    (string-append m3fn " : " m3tn ";")
    )
  )

(define (caddadr x) (car (cddadr x)))

(define (produce-field-parse fieldspec)
  ;; field is 2-entry list: scm-nm scm-typ-nm
  (let* ((m3fn (scheme->m3l (car fieldspec)))
         (m3in (m3-field-type-intf (cadr fieldspec))))
    (if (list? (cadr fieldspec))
        (if (eq? (caddadr fieldspec) 'n1)
            ;; array of nibbles


            ;; array of non-nibbles
            (string-append
             "x." m3fn " := NEW(" m3tn ", x." (scheme->m3l (cadadr fieldspec))");" dnl
             "FOR i := FIRST(x." m3fn "^) TO LAST(x." m3fn "^) DO" dnl
             "  " m3in ".Parse(rd, len, x."m3fn"[i])" dnl
             "END")
            )
        ;; non-array
        (string-append
         m3in ".Parse(rd, len, x." m3fn ")" )
        )
    )
  )

(define (produce-record-decl field-list wr)
  (begin
    (dis "RECORD" dnl wr)
    (let loop ((lst field-list))
      (if (null? lst)
          #t
          (begin
            (dis "  " (produce-field-decl (car lst)) dnl wr)
            (loop (cdr lst)))
          )
      )
    (dis "END;" dnl wr)
    #t
    );;nigeb
  )
    
 
;;(produce-record-decl stdf-record-header "" '())
;;(produce-record-decl (cadr file-attributes-record) (string-append "  hdr : StdfRecordHeader.T;" dnl) '())

(define (put-m3-imports wr)
  (dis "<*NOWARN*>IMPORT StdfU1, StdfU2, StdfU4, StdfN1, StdfCn;" dnl
       "<*NOWARN*>IMPORT StdfI2, StdfB1, StdfC1, StdfDn, StdfVn;" dnl
       "<*NOWARN*>IMPORT StdfI1, StdfR4, StdfI4, StdfBn;" dnl
       "<*NOWARN*>IMPORT StdfE, Rd, Thread, Wx, Fmt;" dnl
       dnl
       wr))

(define deriv-dir "../AMD64_LINUX/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-proc-name "Parse")
(define parse-proto "(rd : Rd.T; VAR len : CARDINAL; VAR t : T) RAISES { StdfE.E , Rd.EndOfFile, Rd.Failure, Thread.Alerted}")

(define parseobj-proc-name "ParseObject")
(define parseobj-proto "(rd : Rd.T; VAR len : CARDINAL) : StdfRecordObject.T RAISES { StdfE.E, Rd.EndOfFile, Rd.Failure, Thread.Alerted }")

(define formatwx-proc-name "FormatWx")
(define formatwx-proto "(wx : Wx.T; READONLY t : T)")

(define format-proc-name "Format")
(define format-proto "(READONLY t : T) : TEXT")

(define formatobj-proc-name "FormatObject")
(define formatobj-proto "(x : StdfRecordObject.T) : TEXT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (make-header-code rec-nam)
  (let* ((rec (eval rec-nam))
         (wrs (open-m3 (string-append "Stdf" (scheme->m3 rec-nam))))
         (i-wr (car wrs))
         (m-wr (cadr wrs))
         (field-lens (map StdfTypeName.GetByteLength
                          (map symbol->string
                               (map cadr rec))))
         )

    (dis "TYPE T = " dnl i-wr)
    (produce-record-decl rec i-wr)
    (dis dnl i-wr)
    
    ;; following leads to circular imports
    ;;  (dis "TYPE O = StdfRecordObject.T OBJECT rec : T END;" dnl
    ;;       dnl i-wr)
    
    (if (not (member? -1 field-lens))
        (dis "CONST Length = "
             (number->string (eval (cons '+ field-lens)))
             ";" dnl
             i-wr))
    
    (put-m3-proc 'parse i-wr m-wr)
    (dis "  VAR x : T; BEGIN" dnl m-wr)
    (let loop ((lst rec))
      (if (null? lst)
          ""
          (let* ((rec (car lst))
                 (t (cadr rec))
                 (m3t (scheme->m3 t))
                 (m3f (scheme->m3l (car rec))))
            
            (dis "    Stdf" m3t ".Parse(rd, len, x." m3f ");" dnl m-wr)
            (loop (cdr lst))
            )
          
          )
      )
    (dis  "  END Parse;" dnl
          dnl
          m-wr)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (put-m3-proc 'format i-wr m-wr)
    (dis "  VAR wx := Wx.New(); BEGIN" dnl 
         "    FormatWx(wx, t);" dnl
         "    RETURN Wx.ToText(wx)" dnl
         "  END Format;" dnl
         dnl
         m-wr)

    (put-m3-proc 'formatwx i-wr m-wr)
    (dis "  BEGIN" dnl m-wr) 
    (let loop ((lst rec))
      (if (null? lst)
          ""
          (let* ((rec (car lst)))
            (emit-field-formatwx rec m-wr)
            (loop (cdr lst))
            )
          
          )
      );;tel
    (dis "  END FormatWx;" dnl
         dnl
         m-wr)
    
    (close-m3 wrs)
    )
  )

(define (get-type-len tdef)
  (if (symbol? tdef)
      (StdfTypeName.GetByteLength (symbol->string tdef))
      -1
      )
  )

(define (make-record-code rec-nam)
  (let* ((rec (cadr (eval rec-nam)))
         (wrs (open-m3 (string-append "Stdf" (scheme->m3 rec-nam))))
         (i-wr (car wrs))
         (m-wr (cadr wrs))
         (field-lens (map get-type-len
                          (map cadr rec)))
         )
    
    (dis "IMPORT StdfRecordObject;" dnl dnl i-wr)
    (dis "IMPORT StdfRecordObject;" dnl dnl m-wr)
    
    (dis "TYPE T = " dnl i-wr)
    (produce-record-decl rec i-wr)
    (dis dnl i-wr)
    
    (if (not (member? -1 field-lens))
        (dis "CONST Length = "
             (number->string (eval (cons '+ field-lens)))
             ";" dnl
             i-wr))
    
    (put-m3-proc 'parse i-wr m-wr)
    (dis "  BEGIN" dnl m-wr)
    (let loop ((lst rec))
      (if (null? lst)
          ""
          (let* ((rec (car lst)))
            (set! *e* rec)
            (emit-field-parse rec m-wr)
            (loop (cdr lst))
            )
          
          )
      )
    (dis  "  END Parse;" dnl
          dnl
          m-wr)

    (put-m3-proc 'parseobj i-wr m-wr)
    (dis "TYPE O = StdfRecordObject.T OBJECT rec : T; END;" dnl dnl i-wr)
    (dis "  VAR res := NEW(O); BEGIN" dnl
         "    Parse(rd, len, res.rec);" dnl
         "    RETURN res" dnl
         "  END ParseObject;" dnl
         dnl
         m-wr)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (put-m3-proc 'format i-wr m-wr)
    (dis "  VAR wx := Wx.New(); BEGIN" dnl 
         "    FormatWx(wx, t);" dnl
         "    RETURN Wx.ToText(wx)" dnl
         "  END Format;" dnl
         dnl
         m-wr)

    (put-m3-proc 'formatwx i-wr m-wr)
    (dis "  BEGIN" dnl m-wr)
    (let loop ((lst rec))
      (if (null? lst)
          ""
          (let* ((rec (car lst)))
            (set! *e* rec)
            (emit-field-formatwx rec m-wr)
            (loop (cdr lst))
            )
          
          )
      )
    (dis   "  END FormatWx;" dnl
         dnl
         m-wr)

    (put-m3-proc 'formatobj i-wr m-wr)
    (dis "  BEGIN" dnl 
         "    RETURN Format(NARROW(x, O).rec)" dnl
         "  END FormatObject;" dnl
         dnl
         m-wr)

    (close-m3 wrs)
    )
  )

(define *e* '())

(define (emit-field-formatwx rec wr)
  (let ((m3f (scheme->m3l (car rec)))
        (t (cadr rec)))
    (dis "    Wx.PutText(wx, \"" m3f ": \");" dnl wr)
    (if (symbol? t)
        (let ((m3t (scheme->m3 t)))
          (dis "    Wx.PutText(wx, Stdf" m3t ".Format(t." m3f "));" dnl wr)
          )
        (let ((a   (car t))
              (idx (cadr t))
              (m3t (scheme->m3 (caddr t))))
          (if (not (eq? a 'array)) (error "not an array spec : " t))
          (dis "    FOR i := FIRST(t." m3f "^) TO LAST(t." m3f "^) DO" dnl
               "      Wx.PutChar(wx, '\\n');" dnl
               "      Wx.PutChar(wx, '[');" dnl
               "      Wx.PutText(wx, Fmt.Int(i));" dnl
               "      Wx.PutText(wx, \"] : \");" dnl
               "      Wx.PutText(wx, Stdf" m3t ".Format(t." m3f "[i]));" dnl
               "    END;" dnl wr)
          )
        )
    (dis "    Wx.PutChar(wx, '\\n');" dnl wr)
    )
  )
               

(define (emit-field-parse rec wr)
  (let ((m3f (scheme->m3l (car rec)))
         (t (cadr rec)))
    (if (symbol? t)
        (let ((m3t (scheme->m3 t)))
          (dis "    Stdf" m3t ".Parse(rd, len, t." m3f ");" dnl wr)
          )
        (let ((a   (car t))
              (idx (cadr t))
              (m3t (scheme->m3 (caddr t))))
          (if (not (eq? a 'array)) (error "not an array spec : " t))
          (if (number? idx)
              (dis "    t."m3f" := NEW(REF ARRAY OF Stdf"m3t".T, " (number->string idx)");" dnl wr)
              (dis "    t."m3f" := NEW(REF ARRAY OF Stdf"m3t".T, t." (scheme->m3l idx)");" dnl wr)
              )
          (if (eq? (caddr t) 'n1)
              (dis     "    StdfN1.ParseArray(rd, len, t."m3f"^);" dnl wr)
              
              (dis     "    FOR i := FIRST(t."m3f"^) TO LAST(t."m3f"^) DO" dnl
                       "      Stdf" m3t ".Parse(rd, len, t." m3f "[i])" dnl 
                       "    END;" dnl wr)
              )
          )
        ))
  'ok
  )

(define (make-record-types)
  (let* ((wrs (open-m3 "StdfRecordTypes"))
         (i-wr (car wrs))
         (m-wr (cadr wrs)))

    (dis "IMPORT StdfRecordObject;" dnl dnl i-wr)
    (dis "IMPORT "
         (infixize (map (lambda(s)(string-append "Stdf" s)) (map scheme->m3 (map car stdf-record-types))) ", ")
         ";" dnl
         i-wr)
              
    (dis dnl
         "TYPE" dnl
         "  PF = PROCEDURE" parseobj-proto ";" dnl
         dnl
         "  FF = PROCEDURE" formatobj-proto ";" dnl
         dnl
         "  T = RECORD" dnl
         "    nm      : TEXT;" dnl
         "    enum    : Enum;" dnl
         "    recTyp  : CARDINAL;" dnl
         "    recSub  : CARDINAL;" dnl
         "    parser  : PF;" dnl
         "  END;" dnl
         dnl
         "  Enum = { " (infixize (map car (map eval (map car stdf-record-types))) ", ") " };" dnl
         dnl
         "CONST" dnl
         "  Names = ARRAY Enum OF TEXT { " (infixize (map double-quote (map car (map eval (map car stdf-record-types)))) ", ") " };" dnl
         dnl
         "  Types = ARRAY Enum OF T { " (infixize (map make-type-desc stdf-record-types) ", "  ) " };" dnl
         dnl
         "  Formatters = ARRAY Enum OF FF { " (infixize (map make-format-desc stdf-record-types) ", "  ) " };" dnl
         dnl
         i-wr)
  
  (close-m3 wrs)
  )
)

(define (make-type-desc rtyp)
  (let ((m3tn (scheme->m3 (car rtyp)))
      (rec (eval (car rtyp))))
    (string-append
     dnl "    T { \"Stdf"m3tn"\", "
     "Enum."(symbol->string (car rec))", "
     (number->string (cadr rtyp))", "
     (number->string (caddr rtyp))", "
     "Stdf"m3tn".ParseObject }")
    )
)
  
(define (make-format-desc rtyp)
  (let ((m3tn (scheme->m3 (car rtyp))))
    (string-append "Stdf"m3tn".FormatObject")
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dis ">>>>>>>>>>>>>>>>>>>>  building STDF modules  >>>>>>>>>>>>>>>>>>>>" dnl '())

(map make-record-code (map car stdf-record-types))
(make-header-code 'record-header)
(make-record-types)

(dis "<<<<<<<<<<<<<<<<<<  done building STDF modules  <<<<<<<<<<<<<<<<<" dnl '())
