(require-modules "basic-defs" "m3" "display" "hashtable" "struct" "set")

(define deriv-dir "../AMD64_LINUX/")
;;(define deriv-dir "./out/")

(define structs
  `((constants iosf-op       
                    u8                         ;; type
                    ((c IOSF)                  ;; names
                     (m3 IosfOp))              
                    ((reg-read           16_0) ;; values
                     (reg-write          16_1)
                     (reg-wr-priv-ctrl   16_7)
                     (reg-blk-read       16_10)
                     (reg-blk-write      16_11)
                     (comp-no-data       16_20)
                     (comp-data          16_21)
                     (fuse-req           16_45)
                     (ip-ready           16_d0)))

    (enum fm-socket-type
               u8 ;; wire type
               ((c fm_socketType FM_SOCKET_TYPE)
                (m3 FmSocketType))
               ((closed)
                (tcp)
                (udp)
                (pipe)
                (max))
               )

    (struct fm-model-sideband-data
                 ((c fm_modelSidebandData)
                  (m3 FmModelSideBandData))
                 ((idTag          u32)
                  (tc              u8)
                  (pktMeta (array  u8 32))))

    (enum fm-model-msg-type
               u16
               ((c fm_modelMsgType FM_MODEL_MSG)
                (m3 FmModelMsgType))
               ((packet 0)
                (link-state)
                (switch-state)
                (set-egress-info)
                (enable-alternative-data-path)
                (packet-loopback)
                (packet-eot)
                (mgmt)
                (attr)
                (get-info)
                (error)
                (iosf)
                (ctrl)
                (version-info)
                (nvm-read))
               )
    
    (struct fm-model-message-hdr
                 ((c fm_modelMessageHdr)
                  (m3 FmModelMessageHdr))
                 ((msgLength       u32)
                  (version         u16)
                  (type            fm-model-msg-type)
                  (sw              u16)
                  (port            u16)
                  ;; payload is left out here
                  ))

    (enum fm-model-attr-type
               u8
               ((c fm_modelAttrType FM_MODEL_ATTR)
                (m3 FmModelAttrType))
               ((get-request 1)
                (get-response)
                (set)
                (set_ack))
               )

    (enum fm-model-mgmt-type
               u8
               ((c fm_modelMgmtType FM_MODEL_MGMT)
                (m3 FmModelMgmtType))
               ((read-request 1)
                (read-response)
                (write)
                (write-ack)
                (read64-request)
                (read64-response)
                (write64)
                (write64-ack))
               )

    )
  )

(define (scheme->m3 sym)
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'Camel
                    'Hyphen 'None))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (symbol-append . x) ;; permissive form
  (string->symbol
   (eval
    (cons 'string-append
          (map (lambda (s)
                 (cond ((symbol? s) (symbol->string s))
                       ((string? s) s)
                       (else (error (string-append
                                     "not a string or symbol : " s)))))
               x)))))

(define constants '())
(define enum '())
(define struct '())
(define m3typemap '())

(set! constants '())
(set! enum '())
(set! struct '())
(set! m3typemap '())

(define (add-m3-type! nm m3-name)
  (set! m3typemap
        (cons
         (cons nm m3-name)
         m3typemap)))

(define (compile structs)
  (wr-close (filewr-open (string-append deriv-dir "derived.m3m")))
  (map compile-one structs)
  )

(define (put-m3-imports wr)
  (dis "<*NOWARN*>FROM NetTypes IMPORT U8, U16, U32;" dnl
       "<*NOWARN*>IMPORT WrNet, RdNet, NetError;" dnl
       dnl
       wr))

(define (open-m3 nm)
  (let ((i-wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".i3"))))
        (m-wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".m3")))))
    (let ((m3m-wr (FileWr.OpenAppend (string-append deriv-dir "derived.m3m"))))
      (dis "derived_interface(\""nm"\",VISIBLE)" dnl
           "derived_implementation(\""nm"\")" dnl
           m3m-wr)
      (wr-close m3m-wr))
          
                  
    (dis "INTERFACE " nm ";" dnl i-wr)
    (put-m3-imports i-wr)
    
    (dis "MODULE " nm ";" dnl m-wr)
    (put-m3-imports m-wr)
    
    (list i-wr m-wr nm)))

(define (close-m3 wrs)
  (let ((i-wr (car wrs))
        (m-wr (cadr wrs))
        (nm   (caddr wrs)))

    (dis dnl
         "CONST Brand = \"" nm "\";" dnl i-wr)
    (dis dnl
         "END " nm "." dnl i-wr)
    (dis dnl
         "BEGIN END " nm "." dnl m-wr)
    (wr-close i-wr)
    (wr-close m-wr)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-one x)
  (let ((category (car x))
        (nm       (cadr x)))
    (eval `(set! ,category (cons (cdr x) ,category)))
    (eval `(,(symbol-append 'compile- category) nm (cddr x)))
    #t
    ))

(define (compile-m3-const-value v wr)
  (dis "CONST " (scheme->m3 (car v)) " = " (cadr v) ";" dnl wr)
  )

(define (compile-constants-m3 nm x)
  (dis "compiling constants :  " nm dnl)
  (let* ((wire-type    (car x))
         (m3-wire-type (scheme->m3 wire-type))
         (names        (cadr x))
         (values       (caddr x))
         (m3-name      (cadr (assoc 'm3 names)))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         )
    (dis "m3-name " m3-name dnl)
    (add-m3-type! nm m3-name)
    
    (dis "TYPE W = " m3-wire-type ";" dnl
         dnl
         i3-wr)
    (dis "CONST Write = WrNet.Put" m3-wire-type ";" dnl i3-wr)
    (dis "CONST Read  = RdNet.Get" m3-wire-type ";" dnl
         dnl i3-wr)
    (map (lambda(x)(compile-m3-const-value x i3-wr)) values)

    (close-m3 m3-wrs)
    )
  )

(define compile-constants compile-constants-m3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define e '())

(define (put-m3-read-proc i3-wr m3-wr)
  (map 
   (lambda(wr)
     (dis "PROCEDURE Read(rd : Rd.T) : T RAISES { Rd.Failure, NetError.OutOfRange, Rd.EndOfFile, Thread.Alerted }" wr))
   (list i3-wr m3-wr))
  (dis ";" dnl i3-wr)
  (dis " =" dnl m3-wr))

(define (put-m3-write-proc i3-wr m3-wr)
  (map 
   (lambda(wr)
     (dis "PROCEDURE Write(wr : Wr.T; t : T) RAISES { Wr.Failure, Thread.Alerted }" wr))
   (list i3-wr m3-wr))
  (dis ";" dnl i3-wr)
  (dis " ="dnl m3-wr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-enum nm x)
  (dis "compiling enum      :  " nm dnl)
  (let* ((wire-type    (car x))
         (m3-wire-type (scheme->m3 wire-type))
         (names        (cadr x))
         (values       (caddr x))
         (m3-name      (cadr (assoc 'm3 names)))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         )

    (dis "m3-name " m3-name dnl)
    (add-m3-type! nm m3-name)

    ;; final IMPORTs
    
    (map (lambda(wr)
           (dis "IMPORT Rd, Wr, Thread;" dnl dnl wr)) (list i3-wr m3-wr))

    ;; declare wire type 

    (dis "TYPE W = " m3-wire-type ";" dnl
         dnl
         i3-wr)

    (dis "CONST Length = " (get-m3-type-size wire-type) ";" dnl
         dnl i3-wr)

    (put-m3-read-proc i3-wr m3-wr)
    (dis
     "  BEGIN RETURN V2T(RdNet.Get"m3-wire-type"(rd)) END Read;" dnl
     dnl m3-wr)

    (put-m3-write-proc i3-wr m3-wr)
    (dis
    "  BEGIN WrNet.Put"m3-wire-type"(wr, Vals[t]) END Write;" dnl
         dnl m3-wr)
         
    (dis "PROCEDURE V2T(w : W) : T RAISES { NetError.OutOfRange };" dnl
         dnl i3-wr)
    
    (let loop ((i   0)
               (x   values)
               (t   "TYPE T = { ")
               (t2v "CONST Vals = ARRAY T OF W { ")
               (v2t (string-append
                     "PROCEDURE V2T(w : W) : T RAISES { NetError.OutOfRange } =" dnl
                     "  BEGIN" dnl
                     "    CASE w OF" dnl
                     )))

      (if (null? x)

          (begin
            (dis t "};" dnl
                 dnl
                 i3-wr)
            (dis t2v "};" dnl
                 dnl
                 i3-wr)
            (dis v2t
                 "    ELSE RAISE NetError.OutOfRange(w)" dnl
                 "    END" dnl
                 "  END V2T;" dnl
                 dnl
                 m3-wr)
            )

          ;; else
          
          (let* ((sym (scheme->m3 (caar x)))
                 (comma (if (null? (cdr x)) "" ", "))
                 (valspec (cdar x))
                 (val (cond ((null? valspec) i)
                            ((< (car valspec) i)
                             (error (string-append
                                     "bad value for enum "
                                     (stringify (car valspec)))))
                            (else (car valspec)))))
            (loop (+ val 1)
                  (cdr x)
                  (string-append t sym comma)
                  (string-append t2v val comma)
                  (string-append v2t "    | " val " => RETURN T." sym dnl)))
          ) ;; fi
      ) ;; pool
          

    (close-m3 m3-wrs)
    )
  )

(define (array-type? type) (and (list? type) (eq? 'array (car type))))

(define (get-elem-type type)
  (cond ((symbol? type)                                    type)
        ((array-type? type)                          (cadr type))
        (else
         (error (string-append "Illegal type descriptor " (stringify type))))
        ))

(define (get-m3-type type)
  (cond ((member? type '(u8 u16 u32)) (scheme->m3 type))
        ((array-type? type)
         (string-append "ARRAY [0.." (caddr type) "-1] OF "
                        (get-m3-type (cadr type))))
        (else
         (let ((rec (assoc type m3typemap)))
           (if (not rec)
               (error (string-append "Unknown type " (stringify type)))
               (symbol->string (symbol-append (cdr rec) ".T")))))))

(define (get-m3-type-size type)  ;; PACKED size! -- wire protos are packed!
  (cond ((eq? type 'u8) "1")
        ((eq? type 'u16) "2")
        ((eq? type 'u32) "4")
        ((array-type? type) (string-append (caddr type)
                                           "*("
                                           (get-m3-type-size (cadr type))
                                           ")"))
        (else
           (let ((rec (assoc type m3typemap)))
             (if (not rec)
                 (error (string-append "Unknown type " (stringify type)))
                 (symbol->string (symbol-append (cdr rec) ".Length")))))))

(define (emit-struct-field-type f i-wr)
  (dis "    " (car f) " : " (get-m3-type (cadr f)) ";" dnl i-wr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-m3-read-type type lhs lev ind)
  (cond ((member? type '(u8 u16 u32))
         (string-append ind lhs " := RdNet.Get"(scheme->m3 type)"(rd)"))
        ((array-type? type)
         (string-append
          ind "FOR i"lev" := 0 TO " (caddr type) "-1 DO" dnl

          (get-m3-read-type
           (cadr type)
           (string-append "  " lhs "[i"lev"]")
           (+ lev 1)
           (string-append ind "  ")
           )

          ind "END"
          ))
        (else
           (let ((rec (assoc type m3typemap)))
             (if (not rec)
                 (error (string-append "Unknown type " (stringify type)))
                 (string-append ind lhs " := " (cdr rec) ".Read(rd)"))))))
         
(define (emit-struct-field-read f m-wr)
  (dis (get-m3-read-type (cadr f)
                         (string-append "t." (car f))
                         0
                         "    ")
       ";" dnl m-wr))

(define (get-m3-write-type type rhs lev ind)
  (dis "RHS " rhs dnl)
  (cond ((member? type '(u8 u16 u32))
         (string-append ind "WrNet.Put"(scheme->m3 type)"(wr,"rhs")"))
        ((array-type? type)
         (string-append
          ind "FOR i"lev" := 0 TO " (caddr type) "-1 DO" dnl

          (get-m3-write-type
           (cadr type)
           (string-append "  " rhs "[i"lev"]")
           (+ lev 1)
           (string-append ind "  ")
           )
          dnl

          ind "END"
          ))
        (else
           (let ((rec (assoc type m3typemap)))
             (if (not rec)
                 (error (string-append "Unknown type " (stringify type)))
                 (string-append ind (cdr rec) ".Write(wr,"rhs")"))))))

(define (emit-struct-field-write f m-wr)
  (dis 
       (get-m3-write-type (cadr f)
                          (string-append "t."(car f))
                          0
                          "    ")

       ";" dnl m-wr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-struct nm x)
  (dis "compiling struct    :  " nm dnl)
  (let* ((names (car x))
         (m3-name      (cadr (assoc 'm3 names)))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         (fields       (cadr x))
         (types        (uniq eq? (map cadr fields)))

         (import-intfs
          (filter (lambda(x) (not (member? x '(u8 u16 u32))))
                  (map get-elem-type types)))
         )
    (dis "m3-name " m3-name dnl)
    (add-m3-type! nm m3-name)
    (set! e import-intfs)

    (map (lambda(wr)
           (dis "IMPORT Rd, Wr, Thread;" dnl dnl wr)) (list i3-wr m3-wr))

    (map (lambda (wr)
           (map (lambda(intf)
                  (dis "IMPORT " (cdr (assoc intf m3typemap)) ";" dnl wr))
                import-intfs))
         (list i3-wr m3-wr))

    (dis dnl
         "TYPE" dnl
         "  T = RECORD" dnl
         i3-wr)
    (map (lambda(f)(emit-struct-field-type f i3-wr)) fields)
    (dis "  END;" dnl
         dnl i3-wr)

    (dis "CONST Length = 0" i3-wr)
    (map
     (lambda(f)(dis
                (string-append " + " (get-m3-type-size (cadr f)))
                i3-wr))
     fields)
    (dis ";" dnl i3-wr)

    (dis dnl m3-wr)
    (put-m3-read-proc i3-wr m3-wr)
    (dis "  VAR t : T;" dnl m3-wr)
    (dis "  BEGIN" dnl m3-wr)
    (map (lambda(f)(emit-struct-field-read f m3-wr)) fields)
    (dis "    RETURN t" dnl m3-wr)
    (dis "  END Read;" dnl
         dnl m3-wr)

    (put-m3-write-proc i3-wr m3-wr)
    (dis "  BEGIN" dnl m3-wr)
    (map (lambda(f)(emit-struct-field-write f m3-wr)) fields)
    (dis "  END Write;" dnl
         dnl m3-wr)

    (dis dnl m3-wr)
    (close-m3 m3-wrs)
    )
  )

(compile structs)
(exit)

        
    
                                     
    
