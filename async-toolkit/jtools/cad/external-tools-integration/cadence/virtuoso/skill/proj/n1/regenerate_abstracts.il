(defun RegenerateAbstracts ()
  (let (cells currCV leaf bd mid tunable topmet)

    leaf=nil
    bd=t
    mid=nil
    tunable=nil

    (when leaf
    cells = (list
     ; CTREE
     "lib.util.ctree.CTREE2.0"
     "lib.util.ctree.CTREE2.1"
     "lib.util.ctree.CTREE2.2"
     "lib.util.ctree.CTREE2.3"
     "lib.util.ctree.CTREE2.0n"
     "lib.util.ctree.CTREE2.1n"
     "lib.util.ctree.CTREE2.2n"
     "lib.util.ctree.CTREE2.3n"
     "lib.util.ctree.CTREE3.0"
     "lib.util.ctree.CTREE3.1"
     "lib.util.ctree.CTREE3.2"
     "lib.util.ctree.CTREE3.3"
     "lib.util.ctree.CTREE3.0n"
     "lib.util.ctree.CTREE3.1n"
     "lib.util.ctree.CTREE3.2n"
     "lib.util.ctree.CTREE3.3n"
     ; physical cells
     ;"globals.bdc01ld.z04"
     ;"globals.bdc01ld.z08"
     )


     (foreach cell cells
        currCV = (nrOpenCellViewReadableByName cell "abstract")
        ( CDSP4Edit
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                   currCV 
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
                 )
        currCV = (nrOpenCellViewReadableByName cell "layout")
        (MakeLeafAbstract ?CV currCV)
       )
    )

    (when bd
    cells = (list
     ; BD_CONTROLLER
     "lib.bd.metastable.arbiter.ARBITER_CTRL.0"
     "lib.bd.standard.merge.MERGE_CTRL-L2-R.0"
     "lib.bd.standard.split.ALT_SPLIT_CTRL-L2-R.0"
     "lib.bd.standard.split.SPLIT_CTRL-L2-R.0"
     "lib.bd.standard.merge.MERGE_CTRL-L2-R.0"
     "lib.bd.standard.merge.ALT_MERGE_CTRL-L2-R.0"
     "lib.bd.standard.receive.RECV_CTRL.0"
     "lib.bd.standard.send.SEND_CTRL.0"
     "lib.bd.standard.pipeline.ISOLATE_TOK_CTRL.0"
     "lib.bd.standard.pipeline.ISOLATE_CTRL.0"
     "lib.bd.standard.pipeline.TOK_CTRL.0"
     "lib.bd.standard.pipeline.CTRL.0"
;     "lib.bd.6T.bank.BD_SRAM_CTRL.0"
     )


     (foreach cell cells
        currCV = (nrOpenCellViewReadableByName cell "abstract")
        ( CDSP4Edit
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                   currCV 
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
                 )
        currCV = (nrOpenCellViewReadableByName cell "layout")
        topmet = 2
        (MakeAbstract ?CV currCV ?topMetal topmet)
;        (MakeLeafAbstract ?CV currCV)
       )
    )


    (when mid
    cells = (list
     ; 6T
      "lib.bd.6T.bank.SRAM_BANK_CTRL.0"
      "lib.synchronous.6T.sram.WRAP_SRAM-L1024_40-R.0"
      "lib.synchronous.6T.sram.WRAP_SRAM-L1024_64-R.0"
      "lib.synchronous.6T.sram.WRAP_SRAM-L2048_24-R.0"
      "lib.synchronous.6T.sram.WRAP_SRAM-L2048_40-R.0"
      "lib.synchronous.6T.sram.WRAP_SRAM-L2048_72-R.0"
      "lib.synchronous.6T.sram.WRAP_SRAM-L512_64-R.0"
     ; SERIAL
      "lib.qdi.serial.funny_register.CONFIG_SHIFT_FIFO-L16_20598-R.0"
      "lib.qdi.serial.funny_register.CONFIG_SHIFT_FIFO-L16_20730-R.0"
     )

     (foreach cell cells
        currCV = (nrOpenCellViewReadableByName cell "abstract")
        ( CDSP4Edit
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                   currCV 
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
                 )
        currCV = (nrOpenCellViewReadableByName cell "layout")
        topmet = (dbGetPropByName currCV "topMetal")->value
        (MakeAbstract ?CV currCV ?topMetal topmet)
        )
      )

    (when tunable
    cells = (list
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L2-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L4-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L6-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L8-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L10-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L12-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L14-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L16-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L18-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L20-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L22-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L24-R.10000"
     )

     (foreach cell cells
        currCV = (nrOpenCellViewReadableByName cell "abstract")
        ( CDSP4Edit
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                   currCV 
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
                 )
        currCV = (nrOpenCellViewReadableByName cell "layout")
        topmet = 1
        (MakeAbstract ?CV currCV ?topMetal topmet)

        )
      )




    t
    )
  )
