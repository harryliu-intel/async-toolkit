;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the driver
;;
;; This is what the user would normally run.
;;

(define tpt #f)

(define (drive! fn . x)

  (if (and (not (null? x)) (eq? 'force (car x)))
      (m3-clear-build-area!))
  
  (define the-driver (obj-method-wrap
                      (new-modula-object 'CspCompilerDriver.T)
                      'CspCompilerDriver.T))

  (the-driver 'init fn)

  (let* ((proc-type-seq (obj-method-wrap (the-driver 'getProcTypes)
                                         'TextSeq.T))
         (the-modules   (count-execute
                         (proc-type-seq 'size)
                         (curry proc-type-seq 'get)))
         (the-scms      (map (lambda(m)(sa m ".scm")) the-modules))
         (the-port-tbl  (m3-make-module-intf-tbl the-modules))
         );;*tel
         
    (set! tpt the-port-tbl)
    
    (dis "the-modules  : " (stringify the-modules) dnl)
    (dis "the-port-tbl : " (stringify the-port-tbl) dnl)

    (apply compile-csp! the-scms)
    
    (the-driver 'setProcessPorts (the-port-tbl '*m3*))

    (m3-write-main! (the-driver 'genBuilder "BuildSimulation" *default-slack*))
    
    'ok
    )

  )

