(require-modules "m3" "display")

;;
;; Modify transistor thresholds using the "secret" delvth parameter
;;
;; Author : mika.nystroem@intel.com
;;
;; March, 2024
;;

;;(+ 1 1)

(define (modify-mos-vth del-nvth del-pvth)
  (begin

    (let*((name     (modula-type-op 'SpiceObject.T 'get-field
                                    the-spice-object 'name))
          (type     (modula-type-op 'SpiceObject.M 'get-field
                                    the-spice-object 'type)  dnl '())

          (data-obj (modula-type-op 'SpiceObject.M 'get-field
                                    the-spice-object 'data)  dnl '())

          (data     (obj-method-wrap data-obj 'TextSeq.T))

          
          )

      (let ((delvth 
             (case (car (string->list type))
               ((#\p) ;;(dis "p-transistor" dnl '())
                (string-append "delvth=" (stringify del-pvth)))
               ((#\n) ;;(dis "n-transistor" dnl '())
                (string-append "delvth=" (stringify del-nvth)))
               (else  (dis "unknown transistor" dnl '()) #f))))

        ;;(dis "ADDON : " delvth dnl '())

        (if delvth
            (data 'addhi delvth)
            )
        
        )

    )
    
    the-spice-object)
  )
)
