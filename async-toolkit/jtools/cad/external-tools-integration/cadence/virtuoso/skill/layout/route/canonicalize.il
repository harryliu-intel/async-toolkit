; Copyright 2010 Fulcrum Microsy\stems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/route/canonicalize.il#1
; $DateTime$
; $Author$

; note, file names are not conducive to concurrent runs of the same cell
; in the same directory.. a hopefully very unlikely situation.

; Canonical Query, run cast_query generates table of cadence names to cadence canonical names
;                  from a list of vaid cast names NOT cadence names!
(defun CanonicalQuery
    (namefilename skillfile cellName @key
        (maxheap nil)  ; max memory, nil to use MaxHeapSize form
        (verbose nil)  ; verbose reporting
        )
    let( (machine mnfile cmd rv )
        rv=t
        ; get an automatic max-heap-size
        if(isFile(namefilename) then
            if(maxheap==nil maxheap=nrGetMaxHeapSize())
            ; find the cast path if not specified
            castpath=ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
            if(castpath then
                if(verbose printf("Finding canonical names...\n"))
                cmd=sprintf( nil "cast_query --cadence-name --no-header --translate=cadence --cast-path='%s' --task=canonical_name='%s:skill' --output='%s' --cell='%s' --routed --max-heap-size=%dM"
                    castpath namefilename skillfile cellName maxheap )
                if(verbose printf("%s\n" cmd))
                if(shell(cmd) then
                    if( isFile(skillfile) then
                        if(verbose printf("Done\n"))
                    else
                        rv=nil
                        if(verbose printf("%s\nFailed\n" cmd))
                    )
                else
                    rv=nil
                    if(verbose printf("%s\nFailed\n" cmd))
                )
            else
                rv=nil
                printf("Error: CAST_PATH not defined, cannot proceed\n")
            )
        else
            rv=nil
        )
    rv
    )
)

; Convert net names to canonical CAST (actually cadence) names (see BUG 17471)
(defun CanonicalizeNetNames
  (names @key
    (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
    (maxheap nil)  ; max memory, nil to use MaxHeapSize form
    (query t)      ; force rerunning cast_query
    (verbose nil)  ; verbose reporting
    (dofix t)      ; rename nets, or nil for report only
    )
    let( (namefile cmd skillfile NodeCanonicalMap )
        NodeCanonicalMap=nil
        if(names && length(names) then
            skillfile=sprintf( nil "%s/%s.ncanon.il" ConfigFileGetValue( TheCDSConfigTable "TEMP") CV~>cellName)
            ; create name file for cast_query
            namefile=outfile(sprintf(nil "%s.names" CV~>cellName ))
            foreach(name names fprintf(namefile "%s\n" name))
            close( namefile )
            ; uniqify the list, just in case
            shell(sprintf(nil "sort -u %s.names -o %s.names" CV~>cellName CV~>cellName))
            ; convert names to cast names for cast_query, output of cast_query is still cadence names
            shell(sprintf(nil "cat %s.names | rename --type=node --from=cadence --to=cast > %s.names1" CV~>cellName CV~>cellName))
            if(CanonicalQuery( sprintf(nil "%s.names1" CV~>cellName) skillfile CV~>cellName ?verbose verbose) then
                load( skillfile )
                NodeCanonicalMap=NodeCanonicalTable()
            )
        )
    NodeCanonicalMap
    )
)

; Convert net names on shapes and vias to canonical CAST (actually cadence) names (see BUG 17471)
(defun CanonicalizeNets
(@key
    (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
    (maxheap nil)  ; max memory, nil to use MaxHeapSize form
    (query t)      ; force rerunning cast_query
    (verbose nil)  ; verbose reporting
    (dofix t)      ; rename nets, or nil for report only
    )
    let( (namefile namefilename cmd rv skillfile)
        rv=t
        skillfile=sprintf( nil "%s/%s.ncanon.il" ConfigFileGetValue( TheCDSConfigTable "TEMP") CV~>cellName)
        if(query || ! isFile(skillfile) then
            ; create a list of names... lots of duplicates
            namefilename=sprintf(nil "%s.names" CV~>cellName )
            namefile=outfile(namefilename)
            foreach( path setof(shape CV~>shapes shape~>net && shape~>net~>name)
                fprintf(namefile "%s\n" path~>net~>name))
            foreach( inst CV~>instances
                foreach( term setof(te inst~>instTerms te~>net && te~>net~>name )
                    fprintf(namefile "%s\n" term~>net~>name)))
            close( namefile )
            ; uniqify the list
            shell(sprintf(nil "sort -u %s -o %s" namefilename namefilename))
            ; convert names to cast names for cast_query
            shell(sprintf(nil "cat %s | rename --type=node --from=cadence --to=cast > %s1" namefilename namefilename))
            ; get an automatic max-heap-size
            rv=CanonicalQuery( sprintf(nil "%s1" namefilename) skillfile CV~>cellName ?verbose verbose ?maxheap maxheap)
        )
        if(rv && isFile(skillfile) then
            rv=DoFixCanonical(CV
                ?tfile skillfile
                ?verbose verbose
                ?dofix dofix )
        else
            rv=nil
        )
    DeleteUnusedNets( ?CV CV)
    rv
    )
)

; actually fix up the net names identified above
(defun DoFixCanonical
  (CV @key
      (tfile "cqc.out.il")
      (dofix t)
      (verbose nil)
      )
    let((nnil tnnil ccnt pcnt ncnt tccnt tpcnt tncnt NodeCanonicalMap errfile errname rv termname)
        load( tfile )
        rv=t
        NodeCanonicalMap=NodeCanonicalTable()
        ccnt=0
        pcnt=0
        ncnt=0
        errname=sprintf(nil "%s.canon.err" CV~>cellName )
        errfile=outfile(errname)
        nnil=length(setof(shape CV~>shapes shape~>objType=="path" && shape~>net==nil &&
                                shape->purpose!="bus" && shape->purpose!="boundary"))
        if(nnil then
            if(verbose printf( "Error: paths exist without connectivity\n"))
            if(errfile fprintf( errfile "Error: paths exist without connectivity\n"))
            rv=nil)
        foreach( path setof(shape CV~>shapes shape~>net && shape~>net~>name)
            let( (newname net )
                pcnt=pcnt+1
                newname=arrayref( NodeCanonicalMap path~>net~>name )
                (cond (newname == nil
                        ncnt=ncnt+1
                        rv=nil
                        if(verbose printf( "Error: Unrecognized net name %s\n" path~>net~>name ))
                        if(errfile fprintf(errfile "Error: Unrecognized net name %s\n" path~>net~>name ))
                        )
                      ( newname != path~>net~>name
                            if(verbose printf("Rename path %s to %s\n" path~>net~>name newname ))
                            if(errfile fprintf(errfile "Rename path %s to %s\n" path~>net~>name newname ))
                            if( dofix then
                                net = dbFindNetByName( CV newname )
                                (cond (net == nil
                                    if(verbose printf("Canonical net %s did not exist, Sync To Netlist?\n" newname ))
                                    if(errfile fprintf(errfile "Canonical net %s did not exist, Sync To Netlist?\n" newname ))
                                    rv=nil
                                    net = dbCreateNet( CV newname )))
                                dbSubFigFromNet( path )
                                if( !dbAddFigToNet( path net)
                                    printf("Error: cound not attach new net %s to shape\n" newname))
                            )
                            ccnt = ccnt+1))
            )
        )
        tccnt=0
        tpcnt=0
        tncnt=0
        tnnil=0
        foreach( inst CV~>instances
            tnnil=tnnil+length(setof(te inst~>instTerms te~>net==nil) )
            foreach( term setof(te inst~>instTerms te~>net && te~>net~>name )
                tpcnt=tpcnt+1
                let( ( newname net )
                    newname=arrayref( NodeCanonicalMap term~>net~>name )
                    (cond (newname == nil
                            tncnt=tncnt+1
                            rv=nil
                            if(verbose printf( "Error: Unrecognized %s instance->pin %s->%s\n" term~>net~>name inst~>name term~>name))
                            if(errfile fprintf(errfile "Error: Unrecognized %s instance->pin %s->%s\n" term~>net~>name inst~>name term~>name))
                            )
                          ( newname != term~>net~>name
                                if(verbose printf("Rename inst %s term %s %s to %s\n" inst~>name term~>name term~>net~>name newname ) )
                                if(errfile fprintf(errfile "Rename inst %s term %s %s to %s\n" inst~>name term~>name term~>net~>name newname ) )
                                if( dofix then
                                    net = dbFindNetByName( CV newname )
                                    (cond (net == nil
                                        if(verbose printf("Canonical net %s did not exist, Sync To Netlist?\n" newname ))
                                        if(errfile fprintf(errfile "Canonical net %s did not exist, Sync To Netlist?\n" newname ))
                                        rv=nil
                                        net = dbCreateNet( CV newname )
                                        ))
                                    termname=term~>name
                                    dbDeleteObject( term )
                                    if( !dbCreateConnByName( net inst termname )
                                        printf("Error: cound not attach new net %s to terminal %s\n" newname termname))
                                )
                                tccnt = tccnt+1))
                )
            )
        )
        if(tnnil > 0 then
            if(verbose printf( "Error: inst terms exist without connectivity\n"))
            if(errfile fprintf( errfile "Error: inst terms exist without connectivity\n"))
            rv=nil
        )
        if( verbose then
            printf("%d nets total\n" pcnt )
            printf("%d nets renamed\n" ccnt )
            printf("%d nets ok\n" pcnt-ccnt-ncnt )
            printf("%d nets not recognized\n" ncnt ))
        if(errfile then
            fprintf(errfile "%d nets total\n" pcnt )
            fprintf(errfile "%d nets renamed\n" ccnt )
            fprintf(errfile "%d nets ok\n" pcnt-ccnt-ncnt )
            fprintf(errfile "%d nets not recognized\n" ncnt ))
        if(verbose then
            printf("%d inst terms total\n" tpcnt )
            printf("%d inst terms renamed\n" tccnt )
            printf("%d inst terms ok\n" tpcnt-tccnt-tncnt )
            printf("%d inst terms not recognized\n" tncnt ))
        if(errfile then
            fprintf(errfile "%d inst terms total\n" tpcnt )
            fprintf(errfile "%d inst terms renamed\n" tccnt )
            fprintf(errfile "%d inst terms ok\n" tpcnt-tccnt-tncnt )
            fprintf(errfile "%d inst terms not recognized\n" tncnt ))
        if(errfile then
            if( rv then
                printf( "Info see %s\n" errname)
            else
                printf( "%d errors, see %s\n" ncnt+tncnt errname)))
        close(errfile)
        if(dofix DeleteUnusedNets())
        rv
    )
)
