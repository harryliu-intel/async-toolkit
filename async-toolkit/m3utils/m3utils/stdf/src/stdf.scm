(require-modules "basic-defs" "m3" "display" "hashtable" "struct" "set")
(load "common.scm")

;; types cf and c12 are not used
;; n1 only appears in an array
;; 

(define stdf-record-header ;; not sure whether we need this defn
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
    (sblot-id c)
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
        (string-append "REF ARRAY OF StdfTypes." m3tn))
      (string-append "StdfTypes." (scheme->m3 f))))

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


