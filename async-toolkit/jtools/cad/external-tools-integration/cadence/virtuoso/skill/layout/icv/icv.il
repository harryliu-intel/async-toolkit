; Copyright 2015 Intel Corporation.  All rights reserved.
; $Id: //mrl/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/layout/assura/assura.il#5 $
; $DateTime: 2015/04/02 00:54:56 $
; $Author: amlines $

; generate assign statements based on user supplied mapping
(defun ICVGetAssign (layerMappings)
  (apply 'strcat
    (mapcan
      (lambda (mapping)
        (append
          (append
            (list (strcat (car mapping) "=assign_openaccess({"))
            (maplist
              (lambda (lpp)
                (strcat "{\"" (caar lpp) "\", \"" (cadar lpp) "\"}"
                        (if (cadr lpp) "," "")))
              (cdr mapping)))
          (list "});\n")))
      layerMappings)))

(defun ICVGetOutputLayers (layerMappings)
  (strcat
    "oa_output_layers = {\n"
    (apply 'strcat
      (maplist
        (lambda (lmap)
          (let ((mapping (car lmap)))
            (sprintf nil "    {%s, {\"%s\", \"%s\"}}%s"
              (car mapping)
              (caadr mapping)
              (cadadr mapping)
              (if (cdr lmap) ",\n" ""))))
        layerMappings))
    "};\n"))

; generate runset
(defun ICVGetRunset (inLayerMap outLayerMap runset libFile
                     srcLib srcCell srcView dstLib dstCell dstView)
  (strcat "\
#include <icv.rh>\n\
library(\n\
    format=OPENACCESS,\n\
    library_definition_file=\"" libFile "\",\n\
    library_name=\"" srcLib "\",\n\
    cell=\"" srcCell "\"\n\
);\n\
openaccess_options(\n\
    view=\"" srcView "\"\n\
);\n\
hierarchy_options(\n\
    flatten={\"*\"}\n\
);\n\
oa_output_library=openaccess_library(\n\
    library_definition_file=\"" libFile "\"\n\
);\n"
(if inLayerMap (ICVGetAssign inLayerMap) "")
"\
oa_output_layers : list of openaccess_write_layer_map_s = {};\n\
#include \"" runset "\"\n"
(if outLayerMap (ICVGetOutputLayers outLayerMap) "")
"\
write_openaccess(\n\
    output_library=oa_output_library,\n\
    library=\"" dstLib "\",\n\
    output_cell=\"" dstCell "\",\n\
    view=\"" dstView "\",\n\
    layers=oa_output_layers\n\
);\n\
"
  ))

; run ICV layer processing
(defun ICVLayerProcessor (inLayerMap outLayerMap runSetName
                          srcLib srcCell srcView
                          dstLib dstCell dstView)
  (letseq ((PDK_ROOT (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT"))
           (TEMP (strcat (ConfigFileGetValue TheCDSConfigTable "TEMP") "/icv/"))
           (WDIR (strcat TEMP srcCell))
           (rsCacheDir (strcat TEMP ".icvrscache"))
           runSetFile)

    (if (FileUtilMakeDir WDIR t) != 0
      (error "Can't create working directory: %s\n" WDIR))

    ; if runSetName contains /, treat it as a path (either absolute or
    ; relatve), otherwise, find icv/runSetName.rs in the PDK.
    ; Use "./runset" to refer to a runset in the current directory.
    (if (cdr (parseString runSetName "/"))
      (let (parts)
        runSetFile = (simplifyFilename runSetName t)
        parts = (parseString runSetFile "/")
        parts = (parseString (car (last parts)) ".")
        runSetName = (car parts))
      runSetFile = (sprintf nil "%s/share/Fulcrum/icv/runsets/%s.rs" PDK_ROOT runSetName))

    (letseq ((runLog (sprintf nil "%s/%s.log" WDIR runSetName))
             (topRunset (sprintf nil "%s/%s.rs" WDIR runSetName))
             (out (outfile topRunset)))
      (if out
        (let ((cmd (sprintf nil "cd '%s' && env ICV_RUNSET_CACHE_DIR='%s' '%s' icv '%s' >'%s' 2>&1"
                                WDIR rsCacheDir
                                (or (getShellEnvVar "ICV_SCRIPT") "")
                                topRunset runLog)))
          (fprintf out "%s" (ICVGetRunset inLayerMap outLayerMap runSetFile
                              (ddGetUpdatedLib)
                              srcLib srcCell srcView
                              dstLib dstCell dstView))
          (close out)
          
          ; run ICV
          (list (system cmd) runLog)
        )
        (error "Can't write ICV run set: %s\n" topRunset)))))

; convenience function for ICVLayerProcessor
; Copies resulting shapes to dstCV
(defun ICVLayerProcessorSimple (CV dstCV runSetName inLayerMap outLayerMap)
  (let (result tempCV)
    result = (ICVLayerProcessor inLayerMap outLayerMap runSetName
                                CV->libName CV->cellName CV->viewName
                                dstCV->libName dstCV->cellName "icv_temp")
    tempCV = (dbOpenCellViewByType dstCV->libName dstCV->cellName "icv_temp")
    (if (car result)==0
      (or tempCV (error "Unable to open ICV output %s %s %s (log file: %s)\n"
                        dstCV->libName dstCV->cellName dstCV->viewName (cadr result)))
      (error "ICV run failed on %s %s %s for runset %s (log file: %s)\n"
             CV->libName CV->cellName CV->viewName runSetName (cadr result))
      )
    (foreach shape tempCV->shapes (dbCopyFig shape dstCV))
    (ddDeleteObj (ddGetObj dstCV->libName dstCV->cellName "icv_temp"))
    dstCV
    )
  )

; Use ICV to flatten a specified list of LPP's
(defun ICVMergeShapes (CV dstCV lpps)
  (let (map tempCV)
    (foreach lpp lpps
             map = (cons (list (sprintf nil "%s_%s" (car lpp) (cadr lpp)) lpp) map)
             )
    (ICVLayerProcessorSimple CV dstCV "mergeShape" map map)
    )
  )
