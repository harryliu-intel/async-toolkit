(define *min-mult* 0.9) ;; multiply quick paths by this on the min side

(define (lve-pns type)  ;; where to find timing data
  (list (string-append
         "/mnt/fulcrum/alta/lve/lve/" 
         (replace-char type #\. #\/) 
         "/layout_pg/extracted/alint/tt/0.9V/125C/directives.0")
   
        (string-append
         "/mnt/fulcrum/alta/lve/lve/" 
         (replace-char type #\. #\/) 
         "/layout/extracted/alint/tt/0.9V/125C/directives.0") ) 
)
   
(define default-slew 40) ;; slew of environment transitions


;; minimization parameters

(define *initial-parametric-value*  0.5   )
(define *initial-free-value*       50     ) ;; in picoseconds
(define *major-step-value*          1e6   ) ;; in picoseconds
(define *the-epoch-value*           0     ) ;; in picoseconds

(define *epsmach* 1e-8)

(define *quit-derivative* (sqrt *epsmach*))
;;(define *quit-derivative* 1.00)

(define *standard-bracketing-value* 3)

