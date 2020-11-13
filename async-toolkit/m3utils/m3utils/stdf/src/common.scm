;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; COMMON HELPERS
;;

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

;; the below from structgen_m3.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HELPER FUNCS

(define (scheme->m3 sym)
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'Camel
                    'Hyphen 'None))

(define (scheme->m3l sym)
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'LCamel
                    'Hyphen 'None))

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
