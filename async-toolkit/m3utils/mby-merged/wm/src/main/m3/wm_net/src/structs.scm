;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data structures for HLP/MBY White Model
;;; mainly relating to ModelServer.m3 here
;;;
;;; Author: Mika Nystrom <mika.nystroem@intel.com>
;;; April, 2018
;;;
;;; legend: a "wire type" is a big-endian type defined for network
;;;         interchange.  Base types of wire types are u8, u16, u32, u64.
;;;         (a.k.a. big-endian integers of size 8, 16, 32, and 64 bits)
;;;
;;;         a "constant" is a wire type consisting of a list of named
;;;         integer constants.  These are CONST in Modula-3.
;;;
;;;         a "enum" is a wire type consisting of an enumeration with
;;;         an optional mapping to integers.  These are enumerations
;;;         in Modula-3.
;;;
;;;         a struct is a wire type consisting of enumerations, integers
;;;         and structs.  These are RECORDs in Modula-3.
;;;
;;;         a bitstruct is a packed type for layout in local memory or
;;;         interchange between machines running the same program.  It
;;;         is LITTLE-ENDIAN and fields are of widths 1..64 bits.
;;;         Fields can be constrained to be constant or to adhere to
;;;         arbitrary functional specs, and bitstructs can be
;;;         pattern-matched.
;;;
;;;  None of this stuff is at present guaranteed to work on a 32-bit
;;;  machine.  (But it can all be made to work, and it is not that hard.)
;;;


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
               ((port-link-up              1)
                (port-link-down            0)
                (n-phys-ports             32)
                (socket-port-disable 16_ffff)))

    (enum fm-socket-type
               u8 ;; wire type
               ((c      fm_socketType FM_SOCKET_TYPE)
                (m3     FmSocketType)
                (scala  FmSocketType))
               ((closed)
                (tcp)
                (udp)
                (pipe)
                (max))
               )

    (enum fm-model-msg-type
               u16
               ((c      fm_modelMsgType FM_MODEL_MSG)
                (m3     FmModelMsgType)
                (scala  FmModelMsgType))
               ((packet                       0)
                (link-state                    )
                (switch-state                  )
                (set-egress-info               )
                (enable-alternative-data-path  )
                (packet-loopback               )
                (packet-eot                    )
                (mgmt                          )
                (attr                          )
                (get-info                      )
                (error                         )
                (iosf                          )
                (ctrl                          )
                (version-info                  )
                (nvm-read                      )
                (command-quit                  ))
               )
    
    (enum fm-model-attr-type
               u8
               ((c      fm_modelAttrType FM_MODEL_ATTR)
                (m3     FmModelAttrType)
                (scala  FmModelAttrType))
               ((get-request 1)
                (get-response)
                (set)
                (set-ack))
               )

    (enum fm-model-mgmt-type
               u8
               ((c      fm_modelMgmtType FM_MODEL_MGMT)
                (m3     FmModelMgmtType)
                (scala  FmModelMgmtType))
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
          ((m3     FmModelInfoType)
           (scala  FmModelInfoType))
          ((request 1)
           (response)
           ))

    (enum fm-model-data-type
          u8
          ((m3     FmModelDataType)
           (scala  FmModelDataType))
          ((packet ,(fromhex 'a0))
           (sb-id)
           (sb-tc)
           (packet-meta)))

    (enum fm-model-ctrl-type
          u8
          ((m3     FmModelCtrlType)
           (scala  FmModelCtrlType))
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
                  (scala FmModelSideBandData)
                  (m3 FmModelSideBandData))
                 ((idTag          u32)
                  (tc              u8)
                  (pktMeta (array  u8 32))  ;; whats up here? is this real?
                  ))

    (header fm-model-message-hdr
                 ((c fm_modelMessageHdr)
                  (scala FmModelMessageHdr)
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
            ((m3 FmModelMsgErrorHdr)
             (scala FmModelMsgErrorHdr))
            ((type                 u8)))

    (header fm-model-msg-set-egress-info-hdr
            ((m3 FmModelMsgSetEgressInfoHdr)
             (scala FmModelMsgSetEgressInfoHdr))
            ((port                 u16)
             ;; payload is left out here
             ))

    (header fm-model-msg-mgmt-32  
            ((m3 FmModelMsgMgmt32)
             (scala FmModelMsgMgmt32))
            ((mgmtType   fm-model-mgmt-type)
             (address    u32)
             (value      u32)))
              
    (header fm-model-msg-mgmt-64 
            ((m3 FmModelMsgMgmt32)
            (scala FmModelMsgMgmt32))
            ((type       fm-model-mgmt-type)
             (address    u32)
             (value      u64)))

    (header fm-model-msg-attr 
            ((m3 FmModelMsgAttr)
            (scala FmModelMsgAttr))
            ((type         fm-model-attr-type)
             (keyLength    u16)
             (key          (array u8 256))
             (value        (array u8 256))))

    (header fm-model-msg-get-info
            ((m3 FmModelMsgGetInfo)
            (scala FmModelMsgGetInfo))
            ((type                 fm-model-info-type)
             (nPortsSupported      u16)
             (padding              (array u8 51))))

    (header fm-model-msg-packet-eot
            ((m3 FmModelMsgPacketEot)
            (scala FmModelMsgPacketEot))
            ((transmissionSize     u16)))

    (header fm-model-msg-version-hdr
            ((m3 FmModelMsgVersionHdr)
            (scala FmModelMsgVersionHdr))
            ((versionNum    u16)))

    (header fm-model-set-egress-info-hdr
            ((m3 FmModelSetEgressInfoHdr)
            (scala FmModelSetEgressInfoHdr))
            ((tcpPort       u16)))

    (header fm-model-msg-port-link-state
            ((m3 FmModelPortLinkState)
            (scala FmModelPortLinkState))
            ((state       u8)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; sideband formats (all LITTLE ENDIAN)
    ;;

    ;; single formats

    (bitstruct iosf-reg-read-req
            ((m3 IosfRegReadReq)
             (scala IosfRegReadReq))
            (
             ;; DW 0
             (dest        8)
             (source      8)
             (opcode      8 (constant 16_0))
             (tag         3)
             (bar         3)
             (al          1 (constant 16_1))
    ;; discussed with Andrea 5/3-5/4/18 about whether eh0 should be 0 or 1
    ;; depends on whether we simulate CPK or HLP ...
             (eh0      1 ;; (constant 16_1) ;; comment for now
                       )

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
            ((m3 IosfRegWriteReq)
             (scala IosfRegWriteReq))
            (
             ;; DW 0
             (dest        8)
             (source      8)
             (opcode      8 (constant 16_1))
             (tag         3)
             (bar         3)
             (al          1 (constant 16_1))
             (eh0      1 ;; (constant 16_1) ;; comment for now
                       )

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
               ((m3 IosfRegBlkAddr)
                (scala IosfRegBlkAddr))
               ((ndw       4 (constraint (and (= 0 (mod ndw 2)) (<= ndw 14))))
                (addr     28))
               )

    (bitstruct iosf-reg-blk-data
               ((m3 IosfRegBlkData)
                (scala IosfRegBlkData))
               ((data      32))
               )

    (bitstruct iosf-reg-blk-read-req-hdr
            ((m3 IosfRegBlkReadReqHdr)
             (scala IosfRegBlkReadReqHdr))
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
            ((m3 IosfRegBlkWriteReqHdr)
             (scala IosfRegBlkWriteReqHdr))
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
               ((m3 IosfRegCompNoData)
                (scala IosfRegCompNoData))
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
               ((m3 IosfRegCompDataHdr)
                (scala IosfRegCompDataHdr))
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
;;;
;;; RUN THE STRUCTURE COMPILER ON THE DEFINITIONS ABOVE

(compile! structs)
;; and exit .. removing this line dumps us into the Scheme REPL
(exit)
