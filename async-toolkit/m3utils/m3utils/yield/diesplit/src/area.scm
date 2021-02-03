(define tfc-expanded-areas
  '(tfc
    (misc 8.0325 ram-area 2.008125 channel-area 2.008125)
    (tm-core (* #f 2 2 (tm-core-core 43.47 ram-area 14.3451)))
    (gpio 7.65 channel-area 7.65)
    (evenodd
     (serdes 237.60000000000002 serdes-area 237.60000000000002)
     (mac (* #f 4 4
             (* mac-spare 17 16 (onemac 1.0125000000000002))))
     (mac-channel 56 channel-area 56 repair 0.95 repair-cost 0.01)
     (ppu-pipe
      (parde 41.434337279999994 ram-area 13.6733313024)
      (ppu-core 45.6685092864)
      (tcam-array 28.705920122879995 ram-area 16.4445555456)
      (stm
       (stm-ram 94.38884750131201 ram-area 94.38884750131201 repair 0.9 repair-cost 0.01)
       (stm-logic 17.81503650201597 repair 0.87 repair-cost 0.01)))
     (cdm
      (cdm-logic 26.50168399605522 repair 0.95 repair-cost 0.01)
      (cdm-sram (* cdm-spare 193 192
                   (cdm-block 0.387495842496 ram-area 0.387495842496))))))
  )

(define tfc-max-area
  (+ 8.0325
     (* 2 43.47)
     7.65
     237.6
     (* 4 17 1.0125)
     (* 56 1.01)
     41.434337279999994
     45.6685092864
     28.705920122879995
     (* 94.38884750131201 1.01)
     (* 17.81503650201597 1.01)
     (* 26.50168399605522 1.01)
     (* 193 0.387495842496)
     )
  )

(define tfc-min-area
  (+ 8.0325
     (* 2 43.47)
     7.65
     237.6
     (* 4 16 1.0125)
     56
     41.434337279999994
     45.6685092864
     28.705920122879995
     94.38884750131201
     17.81503650201597
     26.50168399605522
     (* 192 0.387495842496)
     )
  )
