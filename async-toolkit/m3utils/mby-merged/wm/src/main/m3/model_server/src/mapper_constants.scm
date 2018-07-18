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
)))

(compile! constants)
