(cnt-sequence-by-name the-map '(mpp 0 shm FWD_TABLE0 1 0 DATA))

(dis (stringify
      (FieldData.ArrayGet the-addresses
                          (get-zip-seq-offset
                           zz
                           (cnt-sequence-by-name
                            the-map
                            '(mpp 0 shm FWD_TABLE_MOD 9 16383 DATA))))
      ) dnl)

;; the following should give the same answer

(compute-offset-from-seq the-chip-offset-tree '(1 0 0 0 0 0 0))

(fielddata->lsb (FieldData.ArrayGet the-addresses
                                    (get-zip-seq-offset zz '(1 0 0 0 0 0 0))))
