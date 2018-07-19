(define constants
  `((constants mapper-sizes
               u64 ;; doesnt matter here
               ((m3 MbyMapperSizes))

               (
                (n-realign-keys         80)
                (n-is-ip-bits           2)
                (n-mac-routable-bits            4)
                (n-vlan-router-id               2)
                (n-map8         8)
                (n-map16                8)
                (n-rewrite-key8-bits            32)
                (n-rewrite-key16-bits           16)
                (otr-l3-len-limit               14)
                (otr-tun-len-limit              18)
                (inr-l3-len-limit               14)
                (l2-min-size            14)
                (mpls-min-size          0)
                (l3-min-size            20)
                (l4-tcp-min-size                18)
                (l4-min-size            8)
                )
               )

    (enum mapper-tc-source
               u64
               ((m3 MbyMapperTcSource))
               (
                (vpri 0)
                (mpls 1)
                (dscp 2)
                (meta 3)
                )
               )

    (enum mapper-source
               u64
               ((m3 MbyMapperSource))
               (
                (noop           0)
                (map-port               1)
                (map-outer-prot         2)
                (map-outer-etype                3)
                (map-outer-dmac-h               4)
                (map-outer-dmac-l               5)
                (map-outer-smac-h               6)
                (map-outer-smac-l               7)
                (map-outer-dip          8)
                (map-outer-sip          9)
                (map-outer-l4-src-l             12)
                (map-outer-l4-src-h             15)
                (map-outer-l4-dst-l             16)
                (map-outer-l4-dst-h             19)
                (pa-flags-l             20)
                (pa-flags-h             31)
                (ffu-scenario-l         32)
                (ffu-scenario-h         33)
                (map-inner-prot         34)
                (map-inner-etype                35)
                (map-inner-dmac-h               36)
                (map-inner-dmac-l               37)
                (map-inner-smac-h               38)
                (map-inner-smac-l               39)
                (map-inner-dip          40)
                (map-inner-sip          41)
                (map-inner-l4-src-l             44)
                (map-inner-l4-src-h             47)
                (map-inner-l4-dst-l             48)
                (map-inner-l4-dst-h             51)
                (ex             52)
                (csum           53)
                (ip-info                54)
                )
               )

    ))

(compile! constants)
