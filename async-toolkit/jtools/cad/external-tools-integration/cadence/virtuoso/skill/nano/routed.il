; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


; AVOID: nrCastQuery does almost the same thing and is normally used instead.
defun( nrLoadRoutedDirective ( CellView 
                        @key
                        ( Verbose nil )
                        ( RegenerateFromCast nil )
                        ( MaxHeapSize (nrGetMaxHeapSize) )
                        )
  let( ( TargetDFIIPath NetFile DirectiveDir DirectivesFile ExecCmd Command routed DirectiveTable cellName)
    TargetDFIIPath = ConfigFileGetValue( TheCDSConfigTable "TEMP" )
    if( !isDir( TargetDFIIPath ) then createDir( TargetDFIIPath ))
    NetFile = sprintf( nil "%s/ilnets/%s.netlist.il"
                          TargetDFIIPath
                          CellView~>cellName )  
    DirectiveDir = sprintf( nil "%s/ildirectives" TargetDFIIPath )
    if( !isDir( DirectiveDir )
      if( !createDir( DirectiveDir ) error( "Error: Failed to create dir %s.\n" DirectiveDir ) )   
    )
  
    DirectivesFile = sprintf( nil "%s/ildirectives/%s.directives.il"
                          TargetDFIIPath
                          CellView~>cellName )
    ExecCmd = cond( ( ( boundp( `ExecCmd ) && stringp( ExecCmd ) ) 
                               ExecCmd )
                    ( "" ) )
    when( (!isFile( DirectivesFile ) || !isFile( NetFile ) || 
          (RegenerateFromCast && compareTime( getCurrentTime() timeToString( fileTimeModified(DirectivesFile)))>100) )
; regenerate only if the skill file is more than 120 seconds old.

      Command = sprintf( nil
                "%s %s/cast2skill --cast-path=%s --cell=%s --output-dir=%s --cadence-name --root-only --suppress-pins --max-heap-size=%dM --64"
                ExecCmd
                PackageGetBinRoot( )
                ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
                CellView~>cellName 
                ConfigFileGetValue( TheCDSConfigTable "TEMP" )
                MaxHeapSize 
                )
       printf( "%s\n" Command )
       shell( Command )
     )
     if( isFile(DirectivesFile) then
       load( DirectivesFile )
       routed=if( setof( key DirectiveTable key=="routed" )
              if( arrayref( DirectiveTable "routed" ) "yes" "no")
            "undefined")
     else
       printf("----------ERROR----------\n")
       printf("Cast/Spec didn't parse. Check your terminal.\n")
       routed="fail"
     )
     routed
   )
)

; Tests if the specified cell is routed=true.  Uses a bunch of silly
; shortcuts before actually running cast_query.
defun( nrIsRoutedCell ( libname cellname viewname                     
                        @key
                        ( routedDirectiveList nil )
                        ( Verbose nil )
                        ( RegenerateFromCast nil )
                        ( MaxHeapSize (nrGetMaxHeapSize) )
                      )
  let( ( isRoutedCell CellView )
    isRoutedCell= nil

    CellView=dbOpenCellViewByType( libname cellname viewname )

    ; if its not a CastCell assume its routed
    (unless (IsCastCell CellView) isRoutedCell=t)

    ; if there is any cell from "gate" "stack" "tsmc13lg" "globals", don't inline them
    if( (strncmp( cellname "gate" 4)==0 || strncmp( cellname "stack" 5)==0 ) then  
       isRoutedCell= t)

    ; assume all leaf cell to be routed.
    if( nrIsLeafCell( libname cellname viewname ) then isRoutedCell= t)

    ; check routedDirectiveList
    if( isRoutedCell==nil then
      unless( routedDirectiveList
              routedDirectiveList = nrCastQuery( CellView ?RegenerateFromCast RegenerateFromCast)
              )
      isRoutedCell = member( cellname routedDirectiveList )!=nil
      )
    isRoutedCell 
  )
)



defun( nrIsLeafCell ( libname cellname viewname 
                       @key (tempdir ConfigFileGetValue(TheCDSConfigTable "TEMP"))
                            (castpath ConfigFileGetValue(TheCDSConfigTable "CAST_PATH")) )
  let((isLeafCell Command filename file line)

    isLeafCell = nil

    filename = strcat( tempdir "/" cellname ".leaf" )
    ExecCmd = cond( ( ( boundp( `ExecCmd ) && stringp( ExecCmd ) ) 
                               ExecCmd )
                    ( "" ) )
    when( !isFile(filename)
      Command = sprintf( nil
                "%s %s/cast_query --cast-path=%s --cell=%s --output=%s --cadence-name  --max-heap-size=%dM --task=subcells --filter=\"leaf&!ancestor=standard.attributes.proteus\""
                ExecCmd
                PackageGetBinRoot( )
                castpath
                cellname 
                filename
                nrGetMaxHeapSize( )
                )
       printf( "%s\n" Command )
       shell( Command )
    )

    file=infile( filename )
    if( file then
      while( gets( line file )
        when( car(parseString(line " "))==cellname
          isLeafCell= t
        )
      )
    else
      printf("Can't open file %s.\n" filename)
    )
    close( file )
    isLeafCell
  )
)
   

; Horribly named function.  Returns a list of cellName strings for all
; routed subcells of specified CellView, including CellView itself.
defun( nrCastQuery ( CellView
                           @key
                           ( RegenerateFromCast t )                 
                           ( MaxHeapSize (nrGetMaxHeapSize) )                 
                   )

  let((routedTrueFile routedFalseFile p_in Command routedTrueList routedFalseList line cellName)

    TargetDFIIPath = ConfigFileGetValue( TheCDSConfigTable "TEMP" )
    DirectiveDir = sprintf( nil "%s/ildirectives" TargetDFIIPath )
    if( !isDir( DirectiveDir )
;      if( !ictMkDir( DirectiveDir ) error( "Error: Failed to create dir %s.\n" DirectiveDir ) )   
      if( system(strcat("mkdir " DirectiveDir))==1 error( "Error: Failed to create dir %s.\n" DirectiveDir ) )   
    )

    routedTrueFile = sprintf( nil "%s/ildirectives/%s.routedTrue.txt"
                          TargetDFIIPath
                          CellView~>cellName )
    when( !isFile( routedTrueFile ) || RegenerateFromCast
       Command = sprintf( nil
                "%s/cast_query --cast-path=%s --cell=%s --task=subcells --filter=routed --translate=cadence --cadence-name --output=%s --max-heap-size=%dM --64"
                PackageGetBinRoot( )
                ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
                CellView~>cellName
                routedTrueFile 
                MaxHeapSize 
                )
       printf( "%s\n" Command )
       shell( Command )
    )
    p_in=infile(routedTrueFile)
    routedTrueList=nil
    while( gets( line p_in)
      sscanf( line "%s" cellName )
      routedTrueList= cons( cellName routedTrueList ) 
    )
    close(p_in)
    routedTrueList
  )
)

