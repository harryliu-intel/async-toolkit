(require-modules "basic-defs" "m3" "display" "hashtable" "struct" "set")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SET DESTINATION

(define deriv-dir "../AMD64_LINUX/")
;;(define deriv-dir "./out/")

;; a small helper

(define (fromhex x) (Scan.Int (stringify x) 16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; USER DEFINITIONS
;;;
;;; refer to fm_model_message.h for more information:
;;;
;;;  White Model Packet Queue Interface Message Types
;;;
;;; The set of possible message type values for ''fm_modelMessage''
;;; when sending a message to the white model packet queue interface.
;;;

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

    (constants fm-hlp-api-regs-int
               u32
               ((m3 FmHlpApiRegsInt))
               ((hlp-reg-version 16_1109)   ;; hacky for now
                (hlp-reg-tag     16_12614)
                ))

    (constants fm-model-constants
               u32
               ((m3 FmModelConstants))
               ((socket-port-disable 16_ffff)))

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
    
    (enum fm-model-attr-type
               u8
               ((c fm_modelAttrType FM_MODEL_ATTR)
                (m3 FmModelAttrType))
               ((get-request 1)
                (get-response)
                (set)
                (set-ack))
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

    (enum fm-model-info-type
          u8
          ((m3 FmModelInfoType))
          ((request 1)
           (response)
           ))

    (enum fm-model-data-type
          u8
          ((m3 FmModelDataType))
          ((data-packet ,(fromhex 'a0))
           (data-sb-id)
           (data-sb-tc)
           (data-packet-meta)))

    (enum fm-model-ctrl-type
          u8
          ((m3 FmModelCtrlType))
          ((chip-reset-req 1) ;; request
           (chip-reset-rep)   ;; response
           ))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; general note:
    ;;
    ;; types that end in -hdr are header types.  This means that where
    ;; they are present in the input stream, they are generally
    ;; followed by some variable-length payload.
    ;;
    ;; headers are BIG ENDIAN
    ;;
    
    (header fm-model-sideband-data
                 ((c fm_modelSidebandData)
                  (m3 FmModelSideBandData))
                 ((idTag          u32)
                  (tc              u8)
                  (pktMeta (array  u8 32))  ;; whats up here? is this real?
                  ))

    (header fm-model-message-hdr
                 ((c fm_modelMessageHdr)
                  (m3 FmModelMessageHdr))
                 ((msgLength       u32)
                  (version         u16)
                  (type            fm-model-msg-type)
                  (sw              u16)
                  (port            u16)
                  ;; payload is left out here
                  ))

    ;; types below here added by mika
    
    (header fm-model-msg-error-hdr
            ((m3 FmModelMsgErrorHdr))
            ((type                 u8)))

    (header fm-model-msg-set-egress-info-hdr
            ((m3 FmModelMsgSetEgressInfoHdr))
            ((port                 u16)
             ;; payload is left out here
             ))

    (header fm-model-msg-mgmt-32  
            ((m3 FmModelMsgMgmt32))
            ((mgmtType   fm-model-mgmt-type)
             (address    u32)
             (value      u32)))
              
    (header fm-model-msg-mgmt-64 
            ((m3 FmModelMsgMgmt32))
            ((type       fm-model-mgmt-type)
             (address    u32)
             (value      u64)))

    (header fm-model-msg-attr 
            ((m3 FmModelMsgAttr))
            ((type         fm-model-attr-type)
             (keyLength    u16)
             (key          (array u8 256))
             (value        (array u8 256))))

    (header fm-model-msg-get-info
            ((m3 FmModelMsgGetInfo))
            ((type                 fm-model-info-type)
             (nPortsSupported      u16)
             (padding              (array u8 51))))

    (header fm-model-msg-packet-eot
            ((m3 FmModelMsgPacketEot))
            ((transmissionSize     u16)))

    (header fm-model-msg-version-hdr
            ((m3 FmModelMsgVersionHdr))
            ((versionNum    u16)))

    (header fm-model-set-egress-info-hdr
            ((m3 FmModelSetEgressInfoHdr))
            ((tcpPort       u16)))


    ;; sideband formats (all LITTLE ENDIAN)

    ;; single formats

    (bitstruct iosf-reg-read-req
            ((m3 IosfRegReadReq))
            (
             ;; DW 0
             (dest        8)
             (source      8)
             (opcode      8 (constant 16_0))
             (tag         3)
             (bar         3)
             (al          1 (constant 16_1))
             (eh0         1 (constant 16_1))

             ;; DW 1
             (exphdr      7 (constant 16_0))
             (eh1         1 (constant 16_0))
             (sai        16)
             (rs          3 (constant 16_0))
             (rsvd0       5)

             ;;DW 2
             (fbe         4 (constant 16_f))
             (sbe         4 (constraint (or (= 16_f sbe) (= 16_0 sbe))))
             
             (fid         8 (constant 16_0))
             (addr0      16)

             ;; DW3
             (addr1      12)
             (addr2      20 (constant 16_0))
             )
            );;bitstruct 
    
    (bitstruct iosf-reg-write-req
            ((m3 IosfRegWriteReq))
            (
             ;; DW 0
             (dest        8)
             (source      8)
             (opcode      8 (constant 16_1))
             (tag         3)
             (bar         3)
             (al          1 (constant 16_1))
             (eh0         1 (constant 16_1))

             ;; DW 1
             (exphdr      7 (constant 16_0))
             (eh1         1 (constant 16_0))
             (sai        16)
             (rs          3 (constant 16_0))
             (rsvd0       5)

             ;;DW 2
             (fbe         4 (constant 16_f))
             (sbe         4 (constraint (or (= 16_f sbe) (= 16_0 sbe))))
             
             (fid         8 (constant 16_0))
             (addr0      16)

             ;; DW3
             (addr1      12)
             (addr2      20 (constant 16_0))

             ;; DW 4
             (data0      32)

             ;; DW 5
             (data1      32)
             )
            );;bitstruct 
    

    ;; block formats
    
    (bitstruct iosf-reg-blk-addr
               ((m3 IosfRegBlkAddr))
               ((ndw       4 (constraint (and (= 0 (mod ndw 2)) (<= ndw 14))))
                (addr     28))
               )

    (bitstruct iosf-reg-blk-data
               ((m3 IosfRegBlkData))
               ((data      32))
               )

    ;; discussed with Andrea 5/3-5/4/18 about whether eh0 should be 0 or 1
    ;; depends on whether we simulate CPK or HLP ...
    (bitstruct iosf-reg-blk-read-req-hdr
            ((m3 IosfRegBlkReadReqHdr))
            (
             ;; DW 0
             (dest     8)
             (source   8)
             (opcode   8 (constant 16_10))
             (tag      3)
             (bar      3)
             (al       1 (constant 16_1))
             (eh0      1 ;; (constant 16_1) ;; comment for now
                       )

             ;; DW 1
             (exphdr   7 (constant 16_0))
             (eh1      1 (constant 16_0))
             (sai     16)
             (rs       3 (constant 16_0))
             (rsvd0    5)

             ;;DW 2
             (ndw      8 (constraint (and (= 0 (mod ndw 2)) (<= ndw 126))))
             
             (fid      8 (constant 16_0))
             (addr0   16)

             ;; DW3
             (addr1   12)
             (addr2   20 (constant 16_0))
             )
            );;bitstruct iosf-reg-blk-read-req-hdr

    (bitstruct iosf-reg-blk-write-req-hdr
            ((m3 IosfRegBlkWriteReqHdr))
            (
             ;; DW 0
             (dest     8)
             (source   8)
             (opcode   8 (constant 16_11))
             (tag      3)
             (bar      3)
             (al       1 (constant 16_1))
             (eh0      1 ;; (constant 16_1) ;; comment for now
                       )

             ;; DW 1
             (exphdr   7 (constant 16_0))
             (eh1      1 (constant 16_0))
             (sai     16)
             (rs       3 (constant 16_0))
             (rsvd0    5)

             ;;DW 2
             (ndw      8 (constraint (and (= 0 (mod ndw 2)) (<= ndw 124))))
             
             (fid      8 (constant 16_0))
             (addr0   16)

             ;; DW3
             (addr1   12)
             (addr2   20 (constant 16_0))
             )
            );;bitstruct iosf-reg-blk-write-req-hdr

    ;; completions
    
    (bitstruct iosf-reg-comp-no-data
               ((m3 IosfRegCompNoData))
               (
                ;; DW 0
                (dest     8)
                (source   8)
                (opcode   8 (constant 16_20))
                (tag      3)
                (rsp      2)
                (rsvd0    2)
                (eh0      1 (constant 16_1))

                ;; DW 1
                (exphdr   7 (constant 16_0))
                (eh1      1 (constant 16_0))
                (sai     16)
                (rs       8 (constant 16_0))
                )
               );; bitstruct iosf-reg-comp-no-data

    (bitstruct iosf-reg-comp-data-hdr
               ((m3 IosfRegCompDataHdr))
               (
                ;; DW 0
                (dest     8)
                (source   8)
                (opcode   8 (constant 16_21))
                (tag      3)
                (rsp      2)
                (rsvd0    2)
                (eh0      1 (constant 16_1))

                ;; DW 1
                (exphdr   7 (constant 16_0))
                (eh1      1 (constant 16_0))
                (sai     16)
                (rs       8 (constant 16_0))

                ;; followed by DWs of DATA
                )
               );; bitstruct iosf-reg-comp-data-hdr
               
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;  CODE BELOW HERE SHOULD GO IN A LIBRARY  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HELPER FUNCS

(define (scheme->m3 sym)
  (IdStyles.Convert (symbol->string sym)
                    'Lower 'Camel
                    'Hyphen 'None))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GLOBALS

(define constants '())
(define enum      '())
(define header    '())
(define m3typemap '())
(define bitstruct    '())

(set! constants   '())
(set! enum        '())
(set! header      '())
(set! m3typemap   '())

(define (add-m3-type! nm m3-name)
  (set! m3typemap
        (cons
         (cons nm m3-name)
         m3typemap)))

(define (get-m3-typemapping type)
  (let ((rec (assoc type m3typemap)))
    (if (not rec)
        (error (string-append "No M3 type mapping for " (stringify type)))
        (cdr rec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OUTPUT FILE HANDLING

(define (open-m3 nm)
  (let ((i-wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".i3.tmp"))))
        (m-wr (filewr-open (symbol->string (symbol-append deriv-dir nm ".m3.tmp")))))
    (let ((m3m-wr (FileWr.OpenAppend (string-append deriv-dir "derived.m3m"))))
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

  (define (cmp-files-safely fn1 fn2)
    (let ((res #f))
      (unwind-protect
       (set! res (cmp-files fn1 fn2)) #f #f)
      res))

  (define (rename-file-if-different fn1 fn2)
    (if (not (cmp-files-safely fn1 fn2))
        (fs-rename fn1 fn2) ;; copy the scratch over the target
        (FS.DeleteFile fn1) ;; else delete the scratch
        ))

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
    (rename-file-if-different (string-append deriv-dir nm ".i3.tmp")
                              (string-append deriv-dir nm ".i3"))
    (rename-file-if-different (string-append deriv-dir nm ".m3.tmp")
                              (string-append deriv-dir nm ".m3"))
    ))

(define (put-m3-imports wr)
  (dis "<*NOWARN*>FROM NetTypes IMPORT U8, U16, U32, U64;" dnl
       "<*NOWARN*>IMPORT WrNet, RdNet, NetError, ServerPacket AS Pkt;" dnl
       "<*NOWARN*>IMPORT Wx, NetTypes, Fmt, NetContext;" dnl
       dnl
       wr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define e '()) ;; debugging slush bucket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROTOTYPES FOR Read AND Write AND OTHER FUNCTIONS EXPORTED
;;;
;;; legend for optional versions
;;;   C   : with Context
;;;   S   : against ServerPacket
;;;   E   : against ServerPacket, and at a ServerPacket.End
;;;   ..B : with BOOLEAN result for the match
;;;


(define read-proc-name "Read")
(define read-proto "(rd : Rd.T) : T RAISES { Rd.Failure, NetError.OutOfRange, Rd.EndOfFile, Thread.Alerted }")

(define write-proc-name "Write")
(define write-proto "(wr : Wr.T; READONLY t : T) RAISES { Wr.Failure, Thread.Alerted }")

(define readc-proc-name "ReadC")
(define readc-proto "(rd : Rd.T; VAR cx : NetContext.T) : T RAISES { Rd.Failure, NetError.OutOfRange, Rd.EndOfFile, Thread.Alerted, NetContext.Short }")

(define writec-proc-name "WriteC")
(define writec-proto "(wr : Wr.T; READONLY t : T; VAR cx : NetContext.T) RAISES { Wr.Failure, Thread.Alerted }")

(define writes-proc-name "WriteS")
(define writes-proto "(s : Pkt.T; at : CARDINAL; READONLY t : T)")

(define readsb-proc-name "ReadSB")
(define readsb-proto "(s : Pkt.T; at : CARDINAL; VAR res : T) : BOOLEAN")

(define readeb-proc-name "ReadEB")
(define readeb-proto "(s : Pkt.T; e : Pkt.End; VAR res : T) : BOOLEAN")

(define writee-proc-name "WriteE")
(define writee-proto "(s : Pkt.T; e : Pkt.End; READONLY t : T)")

(define format-proc-name "Format")
(define format-proto "(READONLY t : T) : TEXT")

(define check-proc-name "Check")
(define check-proto "(READONLY t : T) : BOOLEAN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;
;;; CONSTANTS

(define (compile-m3-const-value v wr)
  (dis "CONST " (scheme->m3 (car v)) " = " (cadr v) ";" dnl wr)
  )

(define (get-m3-name nm lst)
  (let ((pair (assoc 'm3 lst)))
    (if (null? pair)
        (error (string-append "No M3 name for " nm))
        (cadr pair))))

(define (compile-constants-m3! nm x)
  (dis "compiling constants :  " nm dnl)
  (let* ((wire-type    (car x))
         (m3-wire-type (scheme->m3 wire-type))
         (names        (cadr x))
         (values       (caddr x))
         (m3-name      (get-m3-name nm names))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         )
    ;;(dis "m3-name " m3-name dnl)
    (add-m3-type! nm m3-name)
    
    (dis "TYPE W = " m3-wire-type ";" dnl
         dnl
         i3-wr)
    (dis "CONST Write = WrNet.Put" m3-wire-type ";" dnl i3-wr)
    (dis "CONST WriteC = WrNet.Put" m3-wire-type "C;" dnl i3-wr)
    (dis "CONST WriteS = WrNet.Put" m3-wire-type "S;" dnl i3-wr)
    (dis "CONST WriteE = WrNet.Put" m3-wire-type "G;" dnl i3-wr)
    (dis "CONST Read  = RdNet.Get" m3-wire-type ";" dnl i3-wr)
    (dis "CONST ReadC  = RdNet.Get" m3-wire-type "C;" dnl
         dnl i3-wr)
    (map (lambda(x)(compile-m3-const-value x i3-wr)) values)

    (close-m3 m3-wrs)
    ) ;; *tel
  )

(define compile-constants! compile-constants-m3!)

(define (compile-enum! nm x)
  (dis "compiling enum      :  " nm dnl)
  (let* ((wire-type    (car x))
         (m3-wire-type (scheme->m3 wire-type))
         (names        (cadr x))
         (values       (caddr x))
         (m3-name      (get-m3-name nm names))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         )

    ;;(dis "m3-name " m3-name dnl)
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

    (put-m3-proc 'read i3-wr m3-wr)
    (dis
     "  BEGIN RETURN V2T(RdNet.Get"m3-wire-type"(rd)) END Read;" dnl
     dnl m3-wr)

    (put-m3-proc 'readc i3-wr m3-wr)
    (dis
     "  BEGIN RETURN V2T(RdNet.Get"m3-wire-type"C(rd,cx)) END ReadC;" dnl
     dnl m3-wr)

    (put-m3-proc 'write i3-wr m3-wr)
    (dis
    "  BEGIN WrNet.Put"m3-wire-type"(wr, Vals[t]) END Write;" dnl
         dnl m3-wr)

    (put-m3-proc 'writec i3-wr m3-wr)
    (dis
    "  BEGIN WrNet.Put"m3-wire-type"C(wr, Vals[t], cx) END WriteC;" dnl
         dnl m3-wr)

    (put-m3-proc 'writes i3-wr m3-wr)
    (dis
    "  BEGIN WrNet.Put"m3-wire-type"S(s, at, Vals[t]) END WriteS;" dnl
         dnl m3-wr)

    (put-m3-proc 'writee i3-wr m3-wr)
    (dis
    "  BEGIN WrNet.Put"m3-wire-type"G(s, e, Vals[t]) END WriteE;" dnl
         dnl m3-wr)

    (dis "PROCEDURE V2T(w : W) : T RAISES { NetError.OutOfRange };" dnl
         dnl i3-wr)

    (put-m3-proc 'format i3-wr m3-wr)
    (dis "BEGIN RETURN Names[t] END Format;" dnl m3-wr)
    
    (let loop ((i   0)
               (x   values)
               (t   "TYPE T = { ")
               (names "CONST Names = ARRAY T OF TEXT { ")
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
            (dis names "};" dnl
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
            ) ;; nigeb

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
                  (string-append names "\"" sym "\"" comma)
                  (string-append t2v val comma)
                  (string-append v2t "    | " val " => RETURN T." sym dnl)))
          ) ;; fi
      ) ;; pool

    (close-m3 m3-wrs)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HEADER FIELD TYPES

(define (array-type? type) (and (list? type) (eq? 'array (car type))))

(define (get-elem-type type)
  (cond ((symbol? type)                                    type)
        ((array-type? type)                          (cadr type))
        (else
         (error (string-append "Illegal type descriptor " (stringify type))))
        ))

(define (get-m3-type type)
  (cond ((member? type '(u8 u16 u32 u64)) (scheme->m3 type))
        ((array-type? type)
         (string-append "ARRAY [0.." (caddr type) "-1] OF "
                        (get-m3-type (cadr type))))
        (else
         (symbol->string (symbol-append (get-m3-typemapping type) ".T")))))

(define (get-m3-type-size type)  ;; PACKED size! -- wire protos are packed!
  (cond ((eq? type 'u8) "1")
        ((eq? type 'u16) "2")
        ((eq? type 'u32) "4")
        ((eq? type 'u64) "4")
        ((array-type? type) (string-append (caddr type)
                                           "*("
                                           (get-m3-type-size (cadr type))
                                           ")"))
        (else
         (symbol->string (symbol-append (get-m3-typemapping type) ".Length")))))

(define (emit-header-field-type f i-wr)
  (dis "    " (car f) " : " (get-m3-type (cadr f)) ";" dnl i-wr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-m3-read-type whch type lhs lev ind)
  (cond ((member? type '(u8 u16 u32 u64))
         (case whch
           ((read)
            (string-append ind lhs " := RdNet.Get"(scheme->m3 type)"(rd)"))
           ((readc)
            (string-append ind lhs " := RdNet.Get"(scheme->m3 type)"C(rd,cx)"))
           (else (error whch))))
           
        ((array-type? type)
         (string-append
          ind "FOR i"lev" := 0 TO " (caddr type) "-1 DO" dnl

          (get-m3-read-type
           whch
           (cadr type)
           (string-append lhs "[i"lev"]")
           (+ lev 1)
           (string-append ind "  ")
           ) dnl
          ind "END"
          ))
        (else
         (case whch
           ((read)
            (string-append ind lhs " := " (get-m3-typemapping type) ".Read(rd)"))
           ((readc)
            (string-append ind lhs " := " (get-m3-typemapping type) ".ReadC(rd,cx)"))
           (else (error whch))))))
         
(define (emit-header-field-readx whch f m-wr updn)
  (dis (get-m3-read-type whch
                         (cadr f)
                         (string-append "t." (car f))
                         0
                         "    ")
       ";" dnl m-wr))

(define (emit-header-field-readc . x)
  (apply emit-header-field-readx (cons 'readc x)))

(define (emit-header-field-read . x)
  (apply emit-header-field-readx (cons 'read x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-m3-format-type type rhs lev ind)
  ;;(dis "RHS " rhs dnl)
  (cond ((member? type '(u8 u16 u32 u64))
         (string-append
          ind
          "Wx.PutText(wx,NetTypes.Format"(scheme->m3 type)"("rhs"))"
          ))
          
        ((array-type? type)
         (string-append
          ind "FOR i"lev" := 0 TO " (caddr type) "-1 DO" dnl
              
          (get-m3-format-type
           (cadr type)
           (string-append "  " rhs "[i"lev"]")
           (+ lev 1)
           (string-append ind "  ")
           )
          dnl

          ind "END"
          ))
        (else
         (string-append
          ind
          "Wx.PutText(wx," (get-m3-typemapping type) ".Format("rhs"))"))))

(define (emit-header-field-format f m-wr updn)
  (let ((field-name (car f))
        (field-type (cadr f)))
    
    (dis
     "    Wx.PutText(wx,\""field-name"=\");" dnl
     (get-m3-format-type field-type
                         (string-append "t."field-name)
                         0
                         "    "
                         ) 
     ";" dnl
     "    Wx.PutText(wx,\" \");" dnl
     m-wr)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-m3-write-type whch type rhs lev ind updn)
  ;;(dis "RHS " rhs dnl)
  ;;(dis whch " " type dnl)

  (string-append

   ;;
   ;; in case of the generic write 'writee , the fields are
   ;; processed twice: once in ascending, then once in descending
   ;; order.
   ;;
   ;; we need to ensure that only one copy is actually emitted!
   ;;

   (if (eq? whch 'writee)
       (string-append ind
                      (case updn
                        ((up) "IF e=Pkt.End.Back THEN ")
                        ((dn) "IF e=Pkt.End.Front THEN ")
                        ) ;; esac
         )
       ""
       )

   ;; and now for your regularly scheduled presentation...
  (cond ((member? type '(u8 u16 u32 u64))
         (case whch
           ((write)
            (string-append ind "WrNet.Put"(scheme->m3 type)"(wr,"rhs")"))
           ((writec)
            (string-append ind "WrNet.Put"(scheme->m3 type)"C(wr,"rhs",cx)"))
           ((writes)
            (string-append ind "WrNet.Put"(scheme->m3 type)"S(s,at,"rhs")"))
           ((writee)
            (string-append ind "WrNet.Put"(scheme->m3 type)"G(s,e,"rhs")"))
           (else (error whch))))
         
        ((array-type? type)
         (string-append
          ind
          (case updn
            ((up)
             (string-append "FOR i"lev" := 0 TO " (caddr type) "-1 DO"))

            ((dn)
             (string-append "FOR i"lev" := " (caddr type) "-1 TO 0 BY -1 DO"))
            )
          dnl

          (get-m3-write-type
           whch
           (cadr type)
           (string-append "  " rhs "[i"lev"]")
           (+ lev 1)
           (string-append ind "  ")
           updn
           )
          dnl

          ind "END"
          ))

        (else
         (case whch
           ((write)
            (string-append ind (get-m3-typemapping type) ".Write(wr,"rhs")"))
           ((writec)
            (string-append ind (get-m3-typemapping type) ".WriteC(wr,"rhs",cx)"))
           ((writes)
            (string-append ind (get-m3-typemapping type) ".WriteS(s,at,"rhs")"))
           ((writee)
            (string-append ind (get-m3-typemapping type) ".WriteE(s,e,"rhs")"))
           (else (error whch)))))
  ;; dnoc
  
  (if (eq? whch 'writee) " END" "")
  
  ) ;; string-append
  )

(define (emit-header-field-writex whch f m-wr updn)
  (dis 
   (get-m3-write-type whch
                      (cadr f)
                      (string-append "t."(car f))
                      0
                      "    "
                      updn)
   
   ";" dnl m-wr))

(define (emit-header-field-writee . x)
  (apply emit-header-field-writex (cons 'writee x)))

(define (emit-header-field-writes . x)
  (apply emit-header-field-writex (cons 'writes x)))

(define (emit-header-field-writec . x)
  (apply emit-header-field-writex (cons 'writec x)))

(define (emit-header-field-write . x)
  (apply emit-header-field-writex (cons 'write x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BITSTRUCTS

(define (get-m3-bitstype n)
  (M3Support.Modula3Type n))

(define (emit-bitstruct-field-type f i-wr)
  (let* ((field-name (car f))
         (field-type (cadr f))
         (tail       (cddr f))
         (defval     (if (or (null? tail)
                             (not (eq? (caar tail) 'constant)))
                         ""
                         (string-append " := " (symbol->m3integer (cadar tail)))))
          
        )
    (dis "    " field-name " : " (get-m3-bitstype field-type) defval ";" dnl i-wr)))

(define (emit-bitstruct-field-format f wr dummy)
  (let ((field-name (car f))
        (field-type (cadr f))
        )
    (dis
     "    Wx.PutText(wx,\""field-name"=\");" dnl
     "    Wx.PutText(wx,StructField.Format(t."field-name",wid:="field-type"));" dnl
     "    Wx.PutChar(wx, ' ');" dnl
     wr
     )
    )
  )

(define (symbol->m3integer x)
  (M3Support.ReformatNumber (symbol->string x))
  )
   
(define (get-bitstruct-constraint-check nm t constraint)
  ;;(dis "CONSTRAINT: " (stringify constraint) dnl)
  (case (car constraint)
    ((constant)
     (string-append "    IF t."nm" # " (symbol->m3integer (cadr constraint))
                    " THEN RETURN FALSE END;" dnl))
    
    ((constraint)
     (string-append "    IF NOT "
                    (format-m3-expr (lambda(nm)(string-append "t." nm)) (cadr constraint))
                    " THEN RETURN FALSE END;" dnl)
      )

    (else (error (string-append "Unknown constraint " (stringify constraint))))

    ) ;; esac
  )

(define (digit? c)
  (and
   (>= (char->integer c) (char->integer #\0))
   (<= (char->integer c) (char->integer #\9))
   ))
       
(define (mynumber? expr)
  (or (number? expr)
      (and (symbol? expr)
           (let ((lst (string->list (symbol->string expr))))
             (and (not (null? lst))
                  (digit? (car lst)))))))

(define (mynumber->m3integer x)
  (cond ((symbol? x) (symbol->m3integer x))
        ((number? x) (number->string x))
        (else (error "unknown number " (stringify x)))))

(define (intersperse lst sep)
  ;; this routine MUST BE tail-recursive, or we shall definitely
  ;; run out of stack space!
  (define (helper lst so-far)
    (cond ((null? lst) so-far)
          ((null? (cdr lst)) (cons (car lst) so-far))

          (else 
           (helper (cdr lst) 
                   (cons sep  (cons (car lst) so-far))))))
  (reverse (helper lst '()))
  )

(define (infixize string-list sep)
  (if (null? string-list) ""
      (apply string-append 
             (intersperse string-list sep))))

(define (unary-op? op)
  (member? op '(- not)))

(define (binary-op? op)
  (member? op '(+ - * / mod > >= < <= = and or)))

(define (op->m3op op)
  (case op
    ((not)   " NOT ")
    ((/)     " DIV ")
    ((mod)   " MOD ")
    ((and)   " AND ")
    ((or)    " OR ")
    (else (symbol->string op))))

(define (format-m3-expr sym-formatter expr)
  (cond ((mynumber? expr) ;; a number
         (mynumber->m3integer expr)) 

        ((symbol? expr) ;; the name of a field (same or other)
         (sym-formatter (symbol->string expr)))

        ((list? expr) ;; an expression
         (let ((me     (lambda(x)(format-m3-expr sym-formatter x)))
               (op     (car expr))
               (args   (cdr expr)))
           (cond

            ((and (= (length args) 1) (unary-op? op))
             (string-append (op->m3op op) (me (car args))))
            
            ((and (> (length args) 1) (binary-op? op))
             (string-append "(" (infixize (map me args) (op->m3op op)) ")"))

            (else
             (string-append (op->m3op op) "(" (infixize (map me args) ",") ")"))
            
            );;dnoc
           );;tel
         )
        
        (else
         (error (string-append "cant format expression " (stringify expr))))
        );;dnoc
  )

(define (emit-bitstruct-field-check f wr dummy)
  (let ((field-name (car f))
        (field-type (cadr f))
        (tail       (cddr f)))
    (if (null? tail)
        (dis "    (*no constraint "field-name" *)" dnl wr)
        (dis "    (*have constraint "field-name" " (stringify (car tail)) " *)" dnl
             (get-bitstruct-constraint-check field-name field-type (car tail))
             dnl wr))))

(define (emit-bitstruct-field-readsb f wr start-bit)
  (let ((field-name (car f))
        (field-type (cadr f)))
    (dis "    (* "field-name" @ "start-bit":+"field-type" *)" dnl
         wr)
    (dis "    IF NOT Pkt.ExtractBits(s, at, "start-bit", "field-type", w) THEN RETURN FALSE END;" dnl wr)
    (dis "    t."field-name" := w;" dnl wr)
    (+ start-bit field-type)))

(define (emit-bitstruct-field-writee f wr start-bit)
  (let ((field-name (car f))
        (field-type (cadr f)))
    (dis "    (* "field-name" @ "start-bit":+"field-type" *)" dnl
         wr)
    (dis "    Pkt.ArrPut(a, "start-bit", t."field-name", "field-type");" dnl wr)
    (+ start-bit field-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAIN COMPILER

(define (compile-bitstruct! nm x)
  (dis "compiling bitstruct :  " nm dnl)

  (let* ((names (car x))
         (m3-name      (cadr (assoc 'm3 names)))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         (fields       (cadr x))
         )

    (set! e fields)

    (define (emit-m3-t)
      (dis dnl
           "TYPE" dnl
           "  T = RECORD" dnl
           i3-wr)
      (map (lambda(f)(emit-bitstruct-field-type f i3-wr)) fields)
      (dis "  END;" dnl
           dnl i3-wr)
      )

    (define (emit-proc whch decls pre-return return q)
      ;; emit a procedure to do "something" (in parameter whch)
      ;; return value provided in parameter return
      (let ((emitter (eval (symbol-append 'emit-bitstruct-field- whch))))
        (put-m3-proc whch i3-wr m3-wr)
        (dis "  VAR " decls dnl m3-wr)
        (dis "  BEGIN" dnl m3-wr)
        (map (lambda(f)(set! q (emitter f m3-wr q))) fields)

        (dis "    " pre-return dnl m3-wr)
        (dis "    RETURN " return dnl m3-wr)
        (dis "  END " (eval (symbol-append whch '-proc-name)) ";" dnl
             dnl m3-wr)))

    (add-m3-type! nm m3-name)
     
    ;; the obvious imports (should these really be here?) 
    (map (lambda(wr)
           (dis "<*NOWARN*>IMPORT Byte, Word, StructField, Rd, Wr, Thread;" dnl dnl wr)) (list i3-wr m3-wr))

    ;; the matching Modula-3 declaration
    (emit-m3-t)
    (emit-proc 'format
               "wx := Wx.New(); <*UNUSED*>VAR ok := Assert(Check(t));" ;; decls
               ""
               (string-append "Fmt.F(\"<"m3-name">{ %s}\", Wx.ToText(wx))")
               #f)

    (emit-proc 'check
               ""        ;; decls
               ""        ;; pre-return
               "TRUE"    ;; return
               #f)

    (dis "PROCEDURE Check2(READONLY t : T; VAR u : T) : BOOLEAN = "dnl"  BEGIN" dnl"    WITH check = Check(t) DO IF check THEN u := t END; RETURN check END" dnl"  END Check2;" dnl dnl m3-wr)

    (dis "PROCEDURE Start(sz : CARDINAL; e : Pkt.End) : CARDINAL ="dnl
         "  BEGIN" dnl
         "    CASE e OF" dnl
         "      Pkt.End.Front => RETURN 0" dnl
         "    | " dnl
         "      Pkt.End.Back =>" dnl
         "      <*ASSERT LengthBits MOD 8 = 0*>" dnl
         "      RETURN sz - LengthBits DIV 8" dnl
         "    END" dnl
         "  END Start;" dnl
         dnl
         m3-wr)

    (emit-proc 'readsb
               "w : Word.T; t : T;"
               ""
               "Check2(t,res)"
               0) ;; read from ServerPacket at i

    (put-m3-proc 'readeb i3-wr m3-wr)
    (dis
     "  BEGIN" dnl
     "    WITH ok = ReadSB(s, Start(s.size(),e), res) DO" dnl
     "      IF ok THEN Pkt.Truncate(s, e, LengthBits DIV 8) END;" dnl
     "      RETURN ok" dnl
     "    END" dnl
     "  END " readeb-proc-name ";" dnl
     dnl
     m3-wr)

    (dis "PROCEDURE Assert(q : BOOLEAN) : BOOLEAN = BEGIN <*ASSERT q*>RETURN q END Assert;" dnl dnl m3-wr)

    (emit-proc 'writee
               "a : ARRAY [0..(LengthBits-1) DIV 8 + 1-1] OF Byte.T;<*UNUSED*>VAR ok := Assert(Check(t)); "
               "Pkt.PutA(s,e,a);"
               ""
               0)
    
    (dis dnl "CONST LengthBits = " (accumulate + 0 (map cadr fields)) ";" dnl
         dnl i3-wr)

    (close-m3 m3-wrs)

    ))

(define (compile-header! nm x)
  (dis "compiling header    :  " nm dnl)
  (let* ((names (car x))
         (m3-name      (cadr (assoc 'm3 names)))
         (m3-wrs       (open-m3 m3-name))
         (i3-wr        (car m3-wrs))
         (m3-wr        (cadr m3-wrs))
         (fields       (cadr x))
         (types        (uniq eq? (map cadr fields)))

         (import-intfs
          (filter (lambda(x) (not (member? x '(u8 u16 u32 u64))))
                  (map get-elem-type types)))
         )

    (define (emit-m3-t)
      (dis dnl
           "TYPE" dnl
           "  T = RECORD" dnl
           i3-wr)
      (map (lambda(f)(emit-header-field-type f i3-wr)) fields)
      (dis "  END;" dnl
           dnl i3-wr)
      )

    (define (emit-length)
      (dis "CONST Length = 0" i3-wr)
      (map
       (lambda(f)(dis
                  (string-append " + " (get-m3-type-size (cadr f)))
                  i3-wr))
       fields)
      (dis ";" dnl i3-wr)
      
      (dis dnl m3-wr)
      )

    (define (emit-proc whch decls return)
      ;; emit a procedure to do "something" (in parameter whch)
      ;; return value provided in parameter return
      (let ((emitter (eval (symbol-append 'emit-header-field- whch))))
        (put-m3-proc whch i3-wr m3-wr)
        (dis "  VAR " decls dnl m3-wr)
        (dis "  BEGIN" dnl m3-wr)
        (map (lambda(f)(emitter f m3-wr 'up)) fields)

        ;; special case for the generic write, can do backwards too
        (if (eq? whch 'writee)
            (map (lambda(f)(emitter f m3-wr 'dn)) (reverse fields)))
        
        (dis "    RETURN " return dnl m3-wr)
        (dis "  END " (eval (symbol-append whch '-proc-name)) ";" dnl
             dnl m3-wr)))
      
    ;;(dis "m3-name " m3-name dnl)
    (add-m3-type! nm m3-name)
    (set! e import-intfs)

    ;; the obvious imports (should these really be here?) 
    (map (lambda(wr)
           (dis "IMPORT Rd, Wr, Thread;" dnl dnl wr)) (list i3-wr m3-wr))

    ;; emit the imports for the interfaces needed
    (map (lambda (wr)
           (map (lambda(intf)
                  (dis "IMPORT " (get-m3-typemapping intf) ";" dnl wr))
                import-intfs))
         (list i3-wr m3-wr))

    ;; the matching Modula-3 declaration
    (emit-m3-t)

    ;; a symbol called Length, denoting the wire size of the record
    (emit-length)

    ;; procedures for reading the record off the wire
    (emit-proc 'read   "t : T;" "t")   ;; raw read
    (emit-proc 'readc   "t : T;" "t")  ;; read with context

    ;; various ways of writing the record to the wire
    (emit-proc 'write  "" "")          ;; raw write
    (emit-proc 'writec  "" "")         ;; write with context
    (emit-proc 'writes  "" "")         ;; write to ServerPacket at i
    (emit-proc 'writee  "" "")         ;; write to ServerPacket at frt/bck

    ;; procedures for human-readable formatting of packet
    (emit-proc 'format
               "wx := Wx.New();"
               (string-append "Fmt.F(\"<"m3-name">{ %s }\", Wx.ToText(wx))"))
    
    (dis dnl m3-wr)
    (close-m3 m3-wrs)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAIN FUNCTIONS : COMPILE THE DATA STRUCTURES

(define (compile-one! x)
  (let ((category (car x))
        (nm       (cadr x)))
    (eval `(set! ,category (cons (cdr x) ,category)))
    (eval `(,(symbol-append 'compile- category `!) nm (cddr x)))
    #t
    ))

(define (compile! structs)
  (wr-close (filewr-open (string-append deriv-dir "derived.m3m")))
  (map compile-one! structs)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RUN OUR CODE ON THE DEFINITIONS AT THE TOP

(compile! structs)
(exit)



        
    
                                     
    
