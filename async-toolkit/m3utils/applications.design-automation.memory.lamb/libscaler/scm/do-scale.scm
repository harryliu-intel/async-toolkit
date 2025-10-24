;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; do-scale.scm
;;
;; This is the command-line front-end for liberty-scaler.scm
;;
;; Parse command-line arguments and call gen-lib from liberty-scaler.scm
;;
;;
;; Author : mika.nystroem@intel.com
;;          March, 2022
;;
;; requires use of editliberty program from m3utils/liberty area of 
;; m3utils repo.
;;
;;

(set! *do-debug* #t)

(define *gtr-home* (Env.Get "GTR_HOME"))

(debug "GTR_HOME is " *gtr-home* dnl)

(set! *area-program-path*
      ;; this is the path to the python program that computes the area
      (string-append *gtr-home* "/../python/pyarea/area.py"))

(define pp
  (obj-method-wrap (LibertyUtils.DoParseParams) 'ParseParams.T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (pp 'keywordPresent "-template"))
    (error "must specify -template <path>"))
(define *template-path* (pp 'getNext))

(if (not (pp 'keywordPresent "-name"))
    (error "must specify -name <name>"))
(define *name* (pp 'getNext))

(if (not (pp 'keywordPresent "-cellname"))
    (error "must specify -cellname <cell name>"))
(define *cell-name* (pp 'getNext))

(if (not (pp 'keywordPresent "-path"))
    (error "must specify -path <path>"))
(define *path* (pp 'getNext))

(if (not (pp 'keywordPresent "-w"))
    (error "must specify -w <width in bits>"))
(define *width* (string->number (pp 'getNext)))

(if (not (pp 'keywordPresent "-d"))
    (error "must specify -d <depth in words>"))
(define *depth* (string->number (pp 'getNext)))

(if (not (pp 'keywordPresent "-tech"))
    (error "must specify -tech <technology name>"))
(define *tech-name* (pp 'getNext))

(if (not (pp 'keywordPresent "-sitemp"))
    (error "must specify -sitemp <temperature in degrees Celsius>"))
(define *sitemp-c* (string->number (pp 'getNext)))

(if (not (pp 'keywordPresent "-mttemp"))
    (error "must specify -mttemp <temperature in degrees Celsius>"))
(define *mttemp-c* (string->number (pp 'getNext)))

(if (not (pp 'keywordPresent "-v"))
    (error "must specify -v <voltage>"))
(define *volt* (string->number (pp 'getNext)))

(if (not (pp 'keywordPresent "-sicorner"))
    (error "must specify -sicorner"))
(define *sicorner* (string->number (pp 'getNext)))

(if (not (pp 'keywordPresent "-rcorner"))
    (error "must specify -rcorner"))
(define *rcorner* (string->number (pp 'getNext)))

(if (not (pp 'keywordPresent "-ccorner"))
    (error "must specify -ccorner"))
(define *ccorner* (string->number (pp 'getNext)))

(if (not (pp 'keywordPresent "-pvtname"))
    (error "must specify -pvtname"))
(define *pvt-name* (pp 'getNext))

(define *pow-opts*
  (let loop ((res '()))
    (if (pp 'keywordPresent "-pow")
        (loop (cons (list (pp 'getNext) (pp 'getNext)) res))
        res)))

(define *pinpow2-opts*
  (let loop ((res '()))
    (if (pp 'keywordPresent "-pinpow2")
        (loop (cons (list (pp 'getNext) (pp 'getNext) (pp 'getNext)) res))
        res)))

(define *comb-read*
  (pp 'keywordPresent "-comb_read"))
                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *the-tech*
  (eval (string->symbol (string-append *tech-name* "-tech-constants"))))

(define (do-it)
  (gen-lib *template-path*
           *cell-name*
           *name*
           *path*
           *width*
           *depth*
           *the-tech*
           *volt*
           *sitemp-c*
           ;; *mttemp-c* ;; future add.
           *sicorner*
           *rcorner*
           *ccorner*
           *pvt-name*
           *pow-opts*
           *pinpow2-opts*
           *comb-read*
           ))

(do-it)

(exit)

