(set! *area-program-path*
      (string-append
       (Env.Get "GTR_HOME")
       "/../python/pyarea/area.py"))

(define pp (obj-method-wrap (LibertyUtils.DoParseParams) 'ParseParams.T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (pp 'keywordPresent "-template"))
    (error "must specify -template <path>"))
(define *template-path* (pp 'getNext))

(if (not (pp 'keywordPresent "-name"))
    (error "must specify -name <name>"))
(define *name* (pp 'getNext))

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

(if (not (pp 'keywordPresent "-temp"))
    (error "must specify -temp <temperature in degrees Celsius>"))
(define *temp-c* (string->number (pp 'getNext)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *the-tech*
  (eval (string->symbol (string-append *tech-name* "-tech-constants"))))

(define (do-it)
  (set! *do-debug* #t)
  (gen-lib *template-path*
           *name*
           *path*
           *width*
           *depth*
           *the-tech*
           *volt*
           *temp-c*
           *sicorner*
           *rcorner*
           *ccorner*
           *pvt-name*))

(do-it)

(exit)

