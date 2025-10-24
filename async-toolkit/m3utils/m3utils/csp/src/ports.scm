; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (get-portdef-type pdef)
  (cadddr pdef)
  )

(define (port-type-width ptype)
  (cond ((eq? 'node (car ptype)) (caddr ptype))

        ((and (eq? 'channel             (car  ptype))
              (eq? 'standard.channel.bd (cadr ptype)))
         
         (caaddr ptype))

        (else '*unknown-port-type*))
  )


(define (port-type-short ptype)
  (cond ((eq? 'node (car ptype))
         `(node ,(caddr ptype)))

        ((and (eq? 'channel             (car  ptype))
              (eq? 'standard.channel.bd (cadr ptype)))
         `(bd ,(caaddr ptype)))

        (else '*unknown-port-type*))
  )
