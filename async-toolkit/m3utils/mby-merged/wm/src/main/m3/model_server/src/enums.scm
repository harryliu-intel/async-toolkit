(define enums
  `((enum mby-parser-flags
          u8
          ((m3 MbyPaFlags))
          (
           (nop            0)
           (general-0              1)
           (sup-l4-csum-val                3)
           (otr-l4-udp-v           4)
           (otr-l4-tcp-v           5)
           (otr-l4-sctp-v          6)
           (general-7              7)
           (otr-esp-v              8)
           (window-parse-v         9)
           (otr-head-frag-v                10)
           (otr-payload-frag-v             11)
           (otr-esp-prot           12)
           (otr-nat-t              13)
           (general-1              14)
           (otr-l2-vlan1           18)
           (otr-l2-vlan2           19)
           (otr-l2-v2first         20)
           (otr-mpls-v             21)
           (otr-l3-v               22)
           (otr-l4-v               23)
           (general-24             24)
           (inr-l2-v               25)
           (inr-l2-vlan1           26)
           (inr-l2-vlan2           27)
           (inr-l2-v2first         28)
           (inr-mpls-v             29)
           (inr-l3-v               30)
           (inr-l4-v               31)
           (otr-opt-flags          32)
           (inr-opt-flags          38)
           (general-2              44)

           ))

    (enum mby-parser-keys
          u8
          (options (n 84))
          ((m3 MbyPaKeys))
          (
           (inner-dmac             0)
           (inner-smac             3)
           (outer-dmac             6)
           (outer-smac             9)
           (outer-etype            12)
           (outer-vlan1            14)
           (outer-vlan2            15)
           (outer-l4src            16)
           (outer-l4dst            17)
           (inner-etype            18)
           (inner-vlan1            20)
           (inner-vlan2            21)
           (inner-l4src            22)
           (inner-l4dst            23)
           (mpls           24)
           (general                32)
           (inner-ip-hdr                36)
           (outer-ip-hdr                42)
           (outer-sipdip           48)
           (inner-sipdip           64)
           ))

    (enum mby-parser-ptrs-index
          u8
          ((m3 MbyPaPtrsIndex))
          (
           (nop            0)
           (otr-mpls-ptr           1)
           (otr-l3-ptr             2)
           (otr-l4-ptr             3)
           (inr-l2-ptr             4)
           (inr-mpls-ptr           5)
           (inr-l3-ptr             6)
           (inr-l4-ptr-esp-ptr     7)

           ))

    (enum mby-realign-keys
          u8
          (options (n 80))
          ((m3 MbyRealignKeys))
          (
           (inner-dmac             0)
           (inner-smac             3)
           (outer-dmac             6)
           (outer-smac             9)
           (outer-etype            12)
           (outer-vlan1            14)
           (outer-vlan2            15)
           (outer-l4src            16)
           (outer-l4dst            17)
           (inner-etype            18)
           (inner-vlan1            20)
           (inner-vlan2            21)
           (inner-l4src            22)
           (inner-l4dst            23)
           (mpls           24)
           (general-8b             32)
           (inner-ip-ttl-prot              36)
           (inner-ip-len           37)
           (inner-ip-ds-flow               38)
           (inner-ip-flow          39)
           (ip-isl0-msb            40)
           (ip-isl0-lsb            41)
           (outer-ip-ttl-prot              42)
           (outer-ip-len           43)
           (outer-ip-ds-flow               44)
           (outer-ip-flow          45)
           (sglort         46)
           (dglort         47)
           (outer-sip              48)
           (outer-dip              56)
           (inner-sip              64)
           (inner-dip              72)

           ))

    (enum mby-meta-offsets
          u8
          ((m3 MbyMetaOffsets))
          (
           (type-off               0)
           (lan-dst-port-off               2)
           (dsi-dst-port-off               4)
           (ip-hdr-off             11)
           (esp-hdr-off            15)

           ))

    (enum mby-meta-types
          u8
          ((m3 MbyMetaTypes))
          (
           (lan-rx         16_00)
           (lan-tx         16_01)
           (marker         16_02)
           (dsi-rx         16_14)
           (dsi-tx         16_16)
           (rimmon-rx              16_18)

           ))

    (enum mby-port-defaults
          u8
          ((m3 MbyPortDefaults))
          (
           (keys-l         0)
           (keys-h         79)
           (force-keys-l           80)
           (force-keys-h           95)
           (act24-l-l              96)
           (act24-l-h              111)
           (act24-u-l              112)
           (act24-u-h              127)
           (act4-4-l               128)
           (act4-4-h               159)
           (act4-2-l               160)
           (act4-2-h               191)
           (act4-1-l               192)
           (act4-1-h               223)
           (act1-flags             224)

           ))
    )
  )

(compile! enums)
