list( "1.Run VUE" "runVUE" )

; hack to prevent loading errors
if( isFile(strcat(getShellEnvVar("ICV_HOME_DIR") "/etc/VUE/SkillVueWrapper.il")) then
    load(strcat(getShellEnvVar("ICV_HOME_DIR") "/etc/VUE/SkillVueMenu.il"))
else
    load("/nfs/site/eda/tools/synopsys/icv/2011.09-SP2/common/etc/VUE/SkillVueMenu.il")
)

(defvar origVueSetLayTopCell nil)
(defvar origVueLayOpen nil)
(defvar origVueCmd nil)
(defvar runVueLibraryFormat nil)
(defvar runVueBlock nil)
(defvar runVueLibrary nil)

(defun vueReplaceFunctions ()
  (unless origVueSetLayTopCell
    (putd 'origVueSetLayTopCell (getd 'vueSetLayTopCell))
    (putd 'vueSetLayTopCell
      (lambda (t_cellName t_uniqueCellName)
        ;(printf "vueSetLayTopCell 1: %s %s\n" t_cellName t_uniqueCellName)
        (let ((CID (NameStartRename "cell" "gds2" "cadence")))
          t_cellName = (NameDoRename CID t_cellName)
          t_uniqueCellName = (NameDoRename CID t_uniqueCellName)
          (ipcKillProcess CID)
          ;(printf "vueSetLayTopCell 2: %s %s\n" t_cellName t_uniqueCellName)
          (origVueSetLayTopCell t_cellName t_uniqueCellName)))))

  (unless origVueLayOpen
    (putd 'origVueLayOpen (getd 'vueLayOpen))
    (putd 'vueLayOpen
      (lambda (t_cellName @optional (t_uniqueCellName nil))
        ;(printf "vueLayOpen 1: %s %s\n" t_cellName t_uniqueCellName)
        (let ((CID (NameStartRename "cell" "gds2" "cadence")))
          t_cellName = (NameDoRename CID t_cellName)
          (if t_uniqueCellName
            t_uniqueCellName = (NameDoRename CID t_uniqueCellName))
          (ipcKillProcess CID)
          ;(printf "vueLayOpen 2: %s %s\n" t_cellName t_uniqueCellName)
          (origVueLayOpen t_cellName t_uniqueCellName)))))

  ; in recent versions of ICV, user defined settings in .icv_vuerc are
  ; overwritten by values auto-populated by SKILL, intercept those calls to
  ; substitute with correct names
  (unless origVueCmd
    (putd 'origVueCmd (getd 'vueCmd))
    (putd 'vueCmd
      (lambda (t_cmd)
        ;(printf "vueCmd 1: %s\n" t_cmd)
        (cond
          ((and (StringStartsWith t_cmd "vueSetLayInfo -lib ")
                runVueLibrary)
           t_cmd = (sprintf nil "vueSetLayInfo -lib %s" runVueLibrary))
          ((and (StringStartsWith t_cmd "vueSetLayInfo -format ")
                runVueLibraryFormat)
           t_cmd = (sprintf nil "vueSetLayInfo -format %s" runVueLibraryFormat))
          ((and (StringStartsWith t_cmd "vueSetLayInfo -block ")
                runVueBlock)
           t_cmd = (sprintf nil "vueSetLayInfo -block %s" runVueBlock)))
        ;(printf "vueCmd 2: %s\n" t_cmd)
        (origVueCmd t_cmd))))
)

(defun translateName (type from to input)
  (let ((CID (NameStartRename type from to)) output)
    output = (NameDoRename CID input)
    (ipcKillProcess CID)
    output))

(defun createGDS2 (@key (CV (geGetEditCellView))
                        (heap (nrGetMaxHeapSize))
                        (output nil)
                        (outputRoot nil))
  (let (temp cid castname cmd pdkroot result)
    temp = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    pdkroot = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    (unless output
      (sprintf output "%s/%s.gds2" temp CV->cellName))
    (unless outputRoot
      outputRoot = (translateName "cell" "cadence" "gds2" CV->cellName))
    castname = (translateName "cell" "cadence" "cast" CV->cellName)
    (sprintf cmd "gdsIIWrite --noproperties \
                             --65mode=0 \
                             --use-tag \
                             --tag-orientation=MXR90 \
                             --flatten-pcells \
                             --max-heap-size='%dM' \
                             --fulcrum-pdk-root='%s' \
                             --working-dir='%s' \
                             --cast-path='%s' \
                             --dfII-dir='%s' \
                             --cell='%s' \
                             --view='%s' \
                             --output='%s' \
                             --output-root-cell-name='%s'"
             heap
             pdkroot
             temp
             (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
             (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
             castname
             CV->viewName
             output
             outputRoot)
    result = (shell cmd)
    (unless result
      (printf "Command returned non-zero status: %s\n" cmd))
    result))

(defun prepareICV (@key (CV (geGetEditCellView)))
  (let ((custom CV->viewName == "custom_tag"))
    (if !custom && (rexMatchp ".*_tag" CV->viewName)
        (error "Cannot run on non-custom tag view!")
      (let ((wid (geGetCellViewWindow CV))
            (gds2 (sprintf nil "%s/%s.gds2"
                           (ConfigFileGetValue TheCDSConfigTable "TEMP")
                           CV->cellName))
            (gdsname (translateName "cell" "cadence" "gds2" CV->cellName))
            port)
        (unless (createGDS2 ?CV CV ?output gds2 ?outputRoot gdsname)
          (error "Can't create a GDS2 file"))
        (if !custom
          (geChangeCellView wid
                            CV->libName
                            CV->cellName
                            (sprintf nil "%s_tag" CV->viewName)
                            "a"))
        runVueLibraryFormat = "GDSII"
        runVueBlock = gdsname
        runVueLibrary = gds2
        port = (outfile ".icv_vuerc")
        (fprintf port "vue_set runDirectoryField %s\n" 
                      (ConfigFileGetValue TheCDSConfigTable "TEMP"))
        (fprintf port "vue_set inputBlockField %s\n" gdsname)
        (fprintf port "vue_set inputLibraryField %s\n" gds2)
        (fprintf port "vue_set runsetFileField %s/share/Fulcrum/icv/drc.rul\n"
                      (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT"))
        (close port)
        (vueRunVUE "-lay virtuoso")))))

(vueReplaceFunctions)

( UICreateMenuItemCallBack
  `runVUECB
  (lambda () (prepareICV ?CV (UIGetCellView))))
