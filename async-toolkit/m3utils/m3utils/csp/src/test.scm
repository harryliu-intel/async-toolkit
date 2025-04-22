
(define lif '
(local-if
                    ((id implement-waiting-if16)
                        (sequence
                            (label L51)
                            (recv (array-access (id L) <BigInt.T>0) (id x))
                        )
                    )
                    ((id implement-waiting-if17)
                        (sequence
                            (label L52)
                            (recv (array-access (id L) <BigInt.T>1) (id x))
                        )
                    )
                )
                (local-if
                    ((id not-done-convert-waiting-if20)
                        (waitfor L)
                    )
                    (else
                        (unlock L)
                    )
                )
(goto L53)

)

(define seq

  '(sequence
     
 (local-if
        ((id not-done-convert-waiting-if20)
            (sequence
                (local-if
                    ((id implement-waiting-if16)
                        (sequence
                            (label L51)
                            (recv (array-access (id L) <BigInt.T>0) (id x))
                        )
                    )
                    ((id implement-waiting-if17)
                        (sequence
                            (label L52)
                            (recv (array-access (id L) <BigInt.T>1) (id x))
                        )
                    )
                )
                (local-if
                    ((id not-done-convert-waiting-if20)
                        (waitfor L)
                    )
                    (else
                        (unlock L)
                    )
                )
                (goto L53)
            )
        )
        (else
            skip
        )
    )
    (eval
        (call-intrinsic print (id x))
    )
    (label L54)
    (send (id R) (id x))
)
)

(define i3
  '     (local-if (a (sequence (label A) A1))
                  (b (sequence (label B) B1)))
)

(define s2
  `(sequence
     ,i3
     (local-if (c C)(d D))
     (goto LX)
     )
  )




(define f0
  '    (sequence
                                                (label L322)
                                                (parallel
                                                    (sequence
                                                        (label L319)
                                                        (send (id TO) (id to-uniquify-temp2))
                                                    )
                                                    (sequence
                                                        (label L320)
                                                        (send (id R) (id x))
                                                    )
                                                )
                                                (label L321)
                                            )
                                            (local-if
                                                (else
                                                    skip
                                                )
                                            )
)


(define f1
  '(sequence (assign (id to-uniquify-temp2) (id keep_msb_bit-inline-eval-handle-func61))
                            (label L18)
                            (parallel
                                (sequence
                                    (label L15)
                                    (send (id TO) (id to-uniquify-temp2))
                                )
                                (sequence
                                    (label L16)
                                    (send (id R) (id x))
                                )
                            )
                            (label L17)
                            )
)
