; Sync-to-Netlist with default options.
;
; Copyright 2010 Fulcrum Microsystems.  All rights reserved.

; Sync-to-netlist with appropriate options
(defun SyncToNetlist (@key (CV (geGetEditCellView)) (old nil))
  (let (routed TEMP NetFile DirectivesFile ExecCmd Command status)

    ; check usage
    (unless CV->viewName=="layout_pg" || CV->viewName=="layout" || 
            CV->viewName=="flatten" ||
            CV->viewName=="floorplan" || CV->viewName=="prelayout"
            (printf "Can't identify view type for %s. SyncToNetlist will run in flat mode.\n" CV->cellName))
    
    ; select routed mode
    routed = !(CV->viewName=="floorplan" || CV->viewName=="prelayout")

    ; load wires.il to set maxHeapSize
    (LoadWires ?CV CV)

    ; paths, files, commands
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    NetFile = (sprintf nil "%s/ilnets/%s.netlist.il" TEMP CV->cellName)
    DirectivesFile = (sprintf nil "%s/ildirectives/%s.directives.il"
                              TEMP CV->cellName)
    ExecCmd = (if (and (boundp `ExecCmd) (stringp ExecCmd)) ExecCmd "")

    ; run cast2skill
    Command = (sprintf nil
                       "%s %s/cast2skill --cast-path=%s --cell=%s %s --output-dir=%s --cadence-name --root-only --suppress-pins --max-heap-size=%dM %s --64"
                       ExecCmd
                       (PackageGetBinRoot)
                       (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
                       CV->cellName
                       (if routed "--routed" "")
                       TEMP
                       (nrGetMaxHeapSize)
                       (nrGetJavaArguments)
                       )
    (printf "%s\n" Command)
    status = (shell Command)
    (unless status (error "Cast/Spec didn't parse.\n"))
    
    ; update DFII information
    if( old then
    (UpdateNetlistGenFromSkillNetlistUsingPDKInfo
      CV (sprintf nil "%s/ilnets" TEMP) (sprintf nil "%s/ildirectives" TEMP))
    else
    (SyncNetlistGenFromSkillNetlistUsingPDKInfo
      CV (sprintf nil "%s/ilnets" TEMP) (sprintf nil "%s/ildirectives" TEMP))
    )

    ; set protected property on subcells
    (foreach inst CV->instances
             (unless inst->libName==TechLibName (ProtectInstance inst)))

    ; set routed property
    (dbReplaceProp CV "routed" "string"
                   (if (setof key DirectiveTable key=="routed")
                       (if (arrayref DirectiveTable "routed") "yes" "no")
                     "undefined"))
    t
    )
  )

; obsolete form for backward compatibility
(defun nrSyncToNetlist (CV @key (routed nil))
  (SyncToNetlist ?CV CV ?old t)
  )
