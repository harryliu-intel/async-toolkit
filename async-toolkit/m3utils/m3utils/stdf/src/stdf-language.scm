;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Standard Test Data Format (STDF) 
;; Version 4
;;
;; Author : Mika Nystrom <mika.nystroem@intel.com>
;; Inspired by Michael Wrighton <michael.wrighton@intel.com>
;;
;; November, 2020
;;
;; Reference: Standard Test Data Format (STDF) Specification, Version 4.
;;            Teradyne Corporation, 2007.
;;
;; This file documents the STDF format exactly in line with the Teradyne
;; spec.  It does not include any information regarding field meanings or
;; their optionality.  The generated parser is responsible for implementing
;; "reasonable behavior" regarding such optionality.
;;
;; The definition is plainly embedded in Scheme.
;;
;; This file consists of three separate parts:
;;
;; 1. def'n of record-header.  This header appears implicitly at the
;;    beginning of *every* STDF record.
;;
;; 2. def'ns of all the STDF record types, in succession from the
;;    Teradyne spec.
;;
;; 3. a structure called stdf-record-types, which documents the tags
;;    of each of the record types, again from the Teradyne spec.
;;    This structure is also used for the builder to know what record
;;    types are available.
;;
;; The record-header and stdf-record-types definitions must be named thus,
;; as these names are used by the code generator to find all the other
;; definitions.  The types are also matching the expectation of the
;; code generator and the associated Modula-3 library.  These types
;; are named analogously to the types defined in the Teradyne spec (but
;; not identically).
;;
;; A few field types require special mention.
;;
;; Teradyne type c<x> for <x> an integer are simply special cases of a more
;; vague definition in the Teradyne spec.
;;
;; The type "n1" (Teradyne: "N*1") only makes sense in the context of an
;; array (if you read the spec carefully this comes out).  The generator
;; handles this type specially for this reason (you cannot have a field
;; of type "n1" in the record types without wrapping it in an array).
;;
;; If we wish to add Intel-specific types, we define the record as desired
;; and add it to stdf-record-types, ideally in a way that makes it obvious
;; which types are from Teradyne and which types are added by us for
;; internal use.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define record-header ;; shared by ALL record types
  '((rec-len u2)
    (rec-typ u1)
    (rec-sub u1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STDF record type definitions follow.
;; The format of a record type definition is as follows:
;;
;; (define <type-name> '(<short-tag> ((<field-name> <field-type>) ..)) )
;;
;; After defining a record of type <type-name> here, the <type-name> must
;; be added to the stdf-record-types structure elsewhere in the file, with
;; appropriate tag values.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (burn-tim u2)
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
    (hand-typ cn)
    (hand-id cn)
    (card-typ cn)
    (card-id cn)
    (load-typ cn)
    (load-id cn)
    (dib-typ cn)
    (dib-id cn)
    (cabl-typ cn)
    (cabl-id cn)
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
;;
;; control structure stdf-record-types follows
;;
;; To add a type, add it in the format
;;
;; (<scheme-name> <REC_TYP> <REC_SUB>)
;;
;; where <scheme-name> is the name following Scheme conventions.
;; This name is automatically converted to Modula-3 or C conventions
;; as needed by the code generator.
;;
;; REC_TYP and REC_SUB are the tag values, which allow the generated
;; parser to discover the record of the type in question.
;;
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
