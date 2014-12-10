/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */

(defun RouteBuffers (path bufferList layerPatternList @key
                     (bufDist 460.0) (startDist nil) (termDist nil) 
                     (firstPattern nil) (lastPattern nil)
                     (colPathGuide nil)
                     (flipSegPattern nil)
                     (reserveGap nil)
                     (turnGap nil)
                     (inlinePrefix nil)
                     (guideX nil) (guideY nil)
                     (locException nil)
                     (sramCfg nil) 
                     (bufSegOffset 0)
                     (balance nil)
                     (nestedArray nil)
                     (outfp nil) (checkPath t)
                     (debug 0) (drawGuide nil) (h nil) )
  (prog (bufSegNum sramSegNum segPropList lastCellLoc segPoints leftList len
         rightList result segIdx entry col colPathList x y turnGapX turnGapY
         bufAlignList sramSegIndex bufPrefixName numCols colBufDist seg
         cnt point nextPoint pathOffset shiftDist busMaxOff el hasErr )
    (if !startDist startDist = bufDist)
    (if !termDist termDist = bufDist)
    (if h then
        printf("\n\n")
        printf("Route buffers along a given path\n")
        printf("Usage: RouteBuffers(path bufferList layerPatternList [OPTION])\n")
        printf("       ?h t                        - this help\n")
        printf("       ?bufDist <value>            - Specify distance between segments. Default %.1f\n" bufDist)
        printf("       ?startDist <value>          - Specify distance of first segment. Default %.1f\n" startDist)
        printf("       ?termDist <value>           - Specify distance of last segment. Default %.1f\n" termDist)
        printf("       ?locException <list>        - Overide segment location\n")

        printf("       ?colPathGuide <list>        - Route along the specify node path or bus path with specified offsets\n")
        printf("                                     Either a list of node entries or list of offsets for each column\n")
        printf("                                     list(\".RX_KEY_CPU_CODE[2].e\" \".RX_STATS_EACL_COUNT[7].e\")")
        printf("                                     list(list(<pathOffset> [firstSegOffset] [lastSegOffset]))\n")
        printf("       ?firstPattern <pat>         - Pattern of first segment from input pin\n")
        printf("       ?lastPattern <pat>          - Pattern of last segment to output pin\n")
        printf("       ?flipSegPattern <list>      - List of segments to flip bus pattern\n")
        printf("       ?reserveGap <list>          - List of gaps to prevent buffers being placed within this reserved space\n")
        printf("       ?turnGap <value>            - Gap to reserve when turning corner. A single value for both vertical
                                                     and horizontal reserve distance or a list for each value\n")
        printf("       ?inlinePrefix <string>      - Name preappended to buffer due to inline\n")
        printf("       ?balance <t or nil>         - Balance left and right side of vertical buffer stack\n")
        printf("       ?checkPath <t or nil>       - Perform some sanity of path alignment\n")
        printf("       ?drawGuide <t or nil>       - Draw debug guide paths\n")
        return(t)
    )

    (if debug >= 3 printf("RouteBuffers along path %L with bufDist %L\n" path bufDist))

    (if stringp(car(car(bufferList))) bufferList = list(bufferList))
    (if debug >= 1 then
        cnt = 0
        (foreach col bufferList
            printf("##################### Col#%d:######################\n" cnt)
            bufSegNum = 0
            (foreach entry col
                bufSegNum++
                printf("  Seg#%d: %L\n" bufSegNum entry)
            )
            cnt++
        )
    )
    busMaxOff = -9999.99
    (foreach el layerPatternList
        (if busMaxOff < nth(2 el) busMaxOff = nth(2 el))
    )

    (if drawGuide then
        dbCreatePath((geGetEditCellView)  list("M2" "bus") path 0.8)
        ru_drawPathGuide(path guideX guideY)
        (if reserveGap then
            (foreach el reserveGap
                segIdx = nth(0 el)
                (if ru_getSegDir(nth(segIdx path) nth(segIdx+1 path))=="VERT" then
                    x = xCoord(nth(segIdx path)) + floor(busMaxOff)/2
                    seg = list(x:nth(1 el) x:nth(2 el))
                else
                    y = yCoord(nth(segIdx path)) + floor(busMaxOff)/2
                    seg = list(nth(1 el):y nth(2 el):y)
                )
                dbCreatePath((geGetEditCellView)  list("M5" "block") seg floor(busMaxOff))
            )
        )
    )

    (if !layerPatternList error("RouteBuffers: layerPatternList is nil\n"))
    (if !path error("RouteBuffers: path is nil\n"))
    (if checkPath then
        cnt = 0
        hasErr = nil
        (foreach point path
            (if mod(floor((yCoord(point)+TrackPitch/64)/TrackPitch) 2) == 1 then
                len = length(path)
                (cond
                  (cnt == 0 || cnt == len-1 
                    (if debug >= 2 printf("RouteBuffers: Y coord of point#%d path points on even track but got %L or (%.2f,%.2f) in tracks\n" 
                      cnt point xCoord(point)/TrackPitch yCoord(point)/TrackPitch))
                  )
                  ((cnt <= 1 && Ru_GetDistance(car(path) cadr(path)) < 400.0) ||
                    (cnt >= len-2 && Ru_GetDistance(nth(len-2 path) nth(len-1 path)) < 400.0)
                    (if debug >= 1 printf("WARN: RouteBuffers: Y coord of point#%d path points should be on even track but got %L or (%.2f,%.2f) in tracks\n" 
                      cnt point xCoord(point)/TrackPitch yCoord(point)/TrackPitch))
                  )
                  (t
                    printf("ERROR: RouteBuffers: Y coord of point#%d path points must on even track but got %L or (%.2f,%.2f) in tracks\n" 
                      cnt point xCoord(point)/TrackPitch yCoord(point)/TrackPitch)
                    hasErr = t
                  )
                )
            )
            (if mod(floor((xCoord(point)+TrackPitch/64)/TrackPitch) 2) == 1 then
                printf("WARN: RouteBuffers: X coord of point#%d path points should be on even track but got %L or (%.2f,%.2f) in tracks\n" 
                      cnt point xCoord(point)/TrackPitch yCoord(point)/TrackPitch)
            )
            (if cnt > 1 && cnt < length(path)-2 then
                (if !ru_AlignTo((xCoord(point)+TrackPitch/2) TrackPitch) then
                    printf("ERROR: RouteBuffers: X coord of point#%d path points should be aligned to middle of track but got %L or (%f,%f) in tracks\n" 
                          cnt point xCoord(point)/TrackPitch yCoord(point)/TrackPitch)
                    hasErr = t
                )
                (if !ru_AlignTo((yCoord(point)+TrackPitch/2) TrackPitch) then
                    printf("ERROR: RouteBuffers: Y coord of point#%d path points should be aligned to middle of track but got %L or (%f,%f) in tracks\n" 
                          cnt point xCoord(point)/TrackPitch yCoord(point)/TrackPitch)
                    hasErr = t
                )
            )
            nextPoint = nth(cnt+1 path)
            (if nextPoint && ((almostEqual(xCoord(point) xCoord(nextPoint)) && almostEqual(yCoord(point) yCoord(nextPoint))) ||
                             (!almostEqual(xCoord(point) xCoord(nextPoint)) && !almostEqual(yCoord(point) yCoord(nextPoint))) ) then
                (if cnt == 1 || cnt == length(path)-3 then
                    printf("WARN: RouteBuffers: segment #%d is neither vertical or horizontal, could be okay for zero-length turn segment\n" cnt )
                else
                    printf("ERROR: RouteBuffers: segment #%d is neither vertical or horizontal\n" cnt )
                    hasErr = t
                )
            )
            cnt++
        )
        (if hasErr then
            printf("Path: %L\n" ToTrack(path))
            error("There are errors above. Please check your path. Use ?checkPath option to disable if deemed okay\n")
        )
        
    )

    numCols = length(bufferList)
    (for colCnt 0 numCols-1
        (if colPathGuide then
            (if stringp(nth(colCnt colPathGuide)) then
                colPath = Ru_GetPathNode(path layerPatternList nth(colCnt colPathGuide) 
                                     ?firstPattern firstPattern ?lastPattern lastPattern)
            else
                pathOffset =  nth(0 nth(colCnt colPathGuide))
                colPath = OffsetPath(path pathOffset pathOffset 
                                     ?firstOffset nth(1 nth(colCnt colPathGuide))
                                     ?lastOffset nth(2 nth(colCnt colPathGuide)))
               (if flipSegPattern then
                   (foreach el flipSegPattern
                        /*
                        (if el == 0 && firstPattern then
                            error("Does not support flipping first segment when firstPattern is specified. Use ModifyChannel\n"))
                        (if el == length(colPath)-1 && lastPattern then
                            error("Does not support flipping last segment when lastPattern is specified. Use ModifyChannel\n"))
                        */
                        (if ru_getSegDir(nth(el colPath) nth(el+1 colPath))=="VERT" then
                            x = -2*pathOffset + RoundUpTrack(busMaxOff)
                            y = 0
                        else 
                            x = 0
                            y = -2*pathOffset + RoundUpTrack(busMaxOff)
                        )
                        (if debug >= 1 printf("Flip column #%d segment #%d pattern by x=%f y=%f tracks\n" colCnt el x/TrackPitch y/TrackPitch))
                        colPath = OffsetSomePoints(colPath el (el+1) x y) 
                   )
               )
            )
        else colPath = path)
        colPathList = append(colPathList list(colPath))
    )
    ;must rearrange path segment to avoid paths crossing each other
;    (if numCols > 1 colPathList = Ru_FixPath(colPathList))

;FIXME not very accurate, need to fix logic to know when to turn
    turnGapX = ru_getColParm(turnGap 0)
    turnGapY = ru_getColParm(turnGap 1)
    (if !turnGapX turnGapX = RoundUpTrack(4*TrackPitch + busMaxOff/2/numCols))
    (if !turnGapY turnGapY = RoundUpTrack(4*TrackPitch + busMaxOff/2/numCols))
    (if debug >= 1 printf("turnGapXY %.1f %.1f busMaxOffset %.1f\n" turnGapX/TrackPitch turnGapY/TrackPitch busMaxOff/TrackPitch))

    (for colCnt 0 numCols-1
        colPath = nth(colCnt colPathList)
        (if drawGuide && colPathGuide then dbCreatePath((geGetEditCellView)  list("M4" "bus") colPath 0.8))
        bufSegNum = 0
        sramSegNum = 0
        segPropList = nil
        bufPrefixName = "MBUF_GROUP"
        (if numCols > 1 bufPrefixName = sprintf(nil "COL#%d MBUF_GROUP" colCnt))
        lastCellLoc = nth(0 colPath)
        segPoints = ru_getNextSegment(lastCellLoc colPath ru_getColParm(startDist colCnt) guideX guideY sramCfg ru_getColParm(termDist colCnt))
        sramSegIndex = ru_getSramSegmentIndex(lastCellLoc segPoints sramCfg)

        (while segPoints && bufSegNum < 200 && sramSegNum < 200
            colBufDist = ru_getColParm(bufDist colCnt)
            (if sramSegIndex >= 0 then nextCellLoc = nth(2 nth(sramSegIndex sramCfg))
            else nextCellLoc = nth((length(segPoints)-1) segPoints))

            (if debug >= 2 printf("###### %s bufSegNum#%d points %L bufLoc %L sramSegNum %d sramSegIndex %L\n" 
                            bufPrefixName bufSegNum segPoints nextCellLoc sramSegNum sramSegIndex))
            ; any exception
            (if locException then
                nextBufX = xCoord(nextCellLoc)
                nextBufY = yCoord(nextCellLoc)
                exception = nil
                (if sramSegIndex < 0 exception = ru_getLocException(locException bufPrefixName bufSegNum colCnt))
                (if exception then
                    type = nth(2 exception)
                    (case type
                      ( type == "D" t)
                      ( type == "S" t )
                      ( type == "F"
                        nextBufX = nth(3 exception) ;
                        nextBufY = nth(4 exception);
                        (if debug >= 1 printf("%s[%d] is FIXED to %f,%f\n" 
                                    bufPrefixName bufSegNum nextBufX nextBufY ) )
                      )
                      ( type == "OX"
                        offset = nth(3 exception)
                        nextBufX = nextBufX + offset ;
                        (if debug >= 1 printf("%s[%d]: OFFSET to %f,%f by %f in XCoord\n"
                                   bufPrefixName bufSegNum nextBufX nextBufY offset) )
                      )
                      ( type == "OY"
                        offset = nth(3 exception)
                        nextBufY = nextBufY + offset ;
                        (if debug >= 1 printf("%s[%d]: OFFSET to %f,%f by %f in YCoord\n" 
                                    bufPrefixName bufSegNum nextBufX nextBufY offset) )
                      )
                      ( t printf("ERROR: Unknown exception type [%s]\n" type)
                      )
                    )
                    nextCellLoc = nextBufX:nextBufY
                    ;replace last segPoint
                    tempSegPoints = nil
                    (for cnt 0 (length(segPoints)-2)
                        tempSegPoints = append(tempSegPoints list(nth(cnt segPoints)))
                    )
                    segPoints = append(tempSegPoints list(nextCellLoc))
                )
            )
            (if sramSegIndex >= 0 then
                sramName =  nth(0 nth(sramSegIndex sramCfg))
                ;replace last segPoint
                tempSegPoints = nil
                (for cnt 0 (length(segPoints)-2)
                    tempSegPoints = append(tempSegPoints list(nth(cnt segPoints)))
                )
                segPoints = append(tempSegPoints list(nextCellLoc))

                segProp = list(list(list("SRAM" sramName nextCellLoc sramSegIndex) segPoints))
                (if bufSegNum == 0 then segPropList = segProp
                else segPropList = append(segPropList segProp)
                )
                (if debug >= 1 printf("################# %s SRAM#%d Loc %L bufSeg %L\n" 
                            sramName sramSegNum nextCellLoc segPoints))
                sramSegNum++
            else
                bufName = sprintf(nil "%s[%d]" bufPrefixName (bufSegNum+bufSegOffset))
                segProp = list(list(list("BUF" bufName nextCellLoc (bufSegNum+bufSegOffset)) segPoints))
                (if bufSegNum == 0 then segPropList = segProp
                else segPropList = append(segPropList segProp)
                )
                (if debug >= 1 printf("################# BUF#%d %s Loc %L=(%.1f,%.1f) bufSeg %L\n" 
                            bufSegNum bufName nextCellLoc xCoord(nextCellLoc)/TrackPitch yCoord(nextCellLoc)/TrackPitch segPoints))
                bufSegNum++
            )
            lastCellLoc = nextCellLoc
            exception = nil
            (if sramSegIndex < 0 exception = ru_getLocException(locException bufPrefixName bufSegNum colCnt))
            type = ""
            (if exception then
                type = nth(2 exception)
                (case type
                  ( type == "D"
                    colBufDist = nth(3 exception) ;
                    (if debug >= 1 printf("%s[%d] distance from previous buffer is set to %f\n" 
                                bufPrefixName bufSegNum colBufDist ) )
                  )
                  ( type == "S"
                    (if shiftDist error("%s[%d] Cannot have shift option consecutively. Last shiftDist %L New %L\n" 
                        bufPrefixName bufSegNum shiftDist nth(3 exception))
                    )
                    shiftDist = nth(3 exception);
                    colBufDist = colBufDist + shiftDist
                    (if debug >= 0 printf("%s[%d] shift buffer back by %f, distance from previous buffer is now %f\n" 
                                bufPrefixName bufSegNum shiftDist colBufDist ) )
                  )
                )
            )
            ;previous buffer is shifted by shiftDist, adjust current buffer so it is at the same place
            (if type != "S" && shiftDist then
                colBufDist = colBufDist - shiftDist 
                shiftDist = nil
            )
            segPoints = ru_getNextSegment(lastCellLoc colPath 
                                       colBufDist guideX guideY sramCfg ru_getColParm(termDist colCnt))
            sramSegIndex = ru_getSramSegmentIndex(lastCellLoc segPoints sramCfg)
        )

        result = ru_getPathSegment(lastCellLoc colPath)
        segIdx = nth(0 result)
        segPoints = nil
        (for cnt segIdx+1 length(colPath)-1
            segPoints = append(segPoints list(nth(cnt colPath)))
        )
        segProp = list(list(list("END" "" car(last(colPath)) (bufSegNum+bufSegOffset)) segPoints))
        segPropList = append(segPropList segProp)
        (if debug >= 1 printf("################# ENDPATH Seg %L\n" 
                    segPoints))

        (if debug >= 1 printf("%s: segList: %L\n" bufPrefixName segPropList))

    (if debug  >= 1 printf("############################## Stacking Buffers ###############################\n"))
    bufSegNum = 0
    (if colPathGuide then
        (if stringp(nth(colCnt colPathGuide)) then
            pathOffset = Ru_GetChannelNodeOffset(layerPatternList nth(colCnt colPathGuide) ?nestedArray nestedArray)
        else
            pathOffset =  nth(0 nth(colCnt colPathGuide))
        )
    else pathOffset = 4*TrackPitch)
    (foreach segProp segPropList
            cellLocProp = nth(0 segProp)
            cellType = nth(0 cellLocProp)
            cellName = nth(1 cellLocProp)
            cellLoc = Ru_AlignBufXY(nth(2 cellLocProp))
            index = nth(3 cellLocProp)
            segPath = append(last(nth(1 nth(bufSegNum segPropList))) nth(1 nth(bufSegNum+1 segPropList)))
            mbufList = nth(bufSegNum nth(colCnt bufferList))
            (if mbufList then
                colList =  mbufList
                (if length(segPath) <= 1 error("Too many buffer segments for the given path. Either decrease number of buffer segment\n %s %L"
                                           "or decrease bufDist option so all the buffers can fit along the given path.\n" segPath))
                (if debug >= 2 printf("%s[%d]: Loc %L for column#%d buffers: %L\n" 
                                      bufPrefixName bufSegNum cellLoc colCnt colList) )
;FIXME
singleCol = balance
                (if singleCol then
                    leftList = colList
                    rightList = nil

                else 
                    leftList = nil
                    rightList = nil
                    (foreach bufName colList
                        nodeBaseName = Ru_GetNodeBaseNameFromBufName(bufName)
                        nodeName = sprintf(nil ".%s.e" nodeBaseName)

                        (if rexMatchp("bufReset" bufName) || rexMatchp("rampReset" bufName) then
                            nodeOffset = TrackPitch/2
                        else 
                            ;The test below check between track, but node offset is in the middle of track
                            result = Ru_GetChannelNodeOffset(layerPatternList nodeName ?nestedArray nestedArray)
                            (if !result result = Ru_GetChannelNodeOffset(layerPatternList sprintf(nil ".%s.D.e" nodeBaseName) ?nestedArray nestedArray)) ;DFT
                            (if !result error("%s: Unable to find %L in %L\n" bufName nodeName layerPatternList))
                            nodeOffset = result + TrackPitch/2
                        )
                        ;printf("%s: %s %s: pathOffset %L(%L) nodeOffset %L(%L) leftRightLimit %L(%L) %L(%L)\n" 
                        ;       bufName nodeBaseName nodeName 
                        ;       pathOffset pathOffset/TrackPitch nodeOffset nodeOffset/TrackPitch
                        ;       (floor(pathOffset/TrackPitch)+2)*TrackPitch (floor(pathOffset/TrackPitch)+2)
                        ;       (floor(Ru_AlignY(pathOffset)/TrackPitch)+2)*TrackPitch (floor(Ru_AlignY(pathOffset)/TrackPitch)+2))
                        (if !nodeOffset error("%s: Node: %s pathOffset %L nodeOffset %L. Unable to find nodeOffset\n"
                                              bufName nodeName pathOffset nodeOffset))

                        (if nodeOffset < (floor(Ru_AlignY(pathOffset)/TrackPitch)+2)*TrackPitch then
                            leftList = append(leftList list(bufName))
                        else rightList = append(rightList list(bufName)))
                    )
                )
                (if debug >= 2 printf("LEFT  COL#%d Seg#%d:  %L\n" colCnt bufSegNum leftList))
                (if debug >= 2 printf("RIGHT COL#%d Seg#%d: %L\n" colCnt bufSegNum rightList))

                bufAlignList = Ru_StackBuffer(segPath leftList rightList turnGapX turnGapY
                                             ?flipSegPattern flipSegPattern ?bufferPath colPath
                                             ?reserveGap reserveGap ?bufPrefix inlinePrefix
                                             ?balance balance ?bufSegNum bufSegNum)
            else
                (if length(segPropList) < bufSegNum 
                    printf("%s[%d]: WARN No buffer specified at %L. %L\n" bufPrefixName bufSegNum cellLoc segPropList)) 
            )
            bufSegNum++
    ) ;for segPropList

        (if outfp then
            printf("%s: uses %d MBUFs\n" bufPrefixName bufSegNum)
            (if bufSegOffset == 0 then fprintf(outfp "%s: uses %d MBUFs\n" bufPrefixName bufSegNum)
            else fprintf(outfp "%s: uses %d MBUFs starting with %s\n" bufPrefixName bufSegNum
                                sprintf(nil "%s[%d]" bufPrefixName bufSegOffset)))
        )
    ) ;for colCnt
    return(segPropList)
  )
) ; 


(defun Ru_RemoveLastEl (name delim)
  (let (el ret cnt)
    el = parseString(name delim)
    ret = car(el)
    (for cnt 1 length(el)-2
        ret = strcat(ret sprintf(nil "[%s]" nth(cnt el)))
    )
    ret
  )
)

;return the node base name given the buf instance name
(defun Ru_GetNodeBaseNameFromBufName (bufName)
  (prog (nodeBaseName cnt el prefixEnd)
;FIXME
    ;remove segment index
    el = parseString(bufName "[]")
    nodeBaseName = car(el)
    (for cnt 1 length(el)-2
        nodeBaseName = strcat(nodeBaseName sprintf(nil "[%s]" nth(cnt el)))
    )
    ;remove the mbuf prefix
    prefixEnd = 1
    (while prefixEnd < strlen(nodeBaseName) && substring(nodeBaseName prefixEnd 1) != "_" prefixEnd++)
    prefixEnd++
    ;printf("bufName %L nodeBaseName %L prefixEnd %L\n" bufName nodeBaseName prefixEnd)
    (if prefixEnd > strlen(nodeBaseName) then return(nodeBaseName))
    nodeBaseName = substring(nodeBaseName prefixEnd strlen(nodeBaseName))
    ;printf("Ru_GetNodeBaseNameFromBufName %s %s\n" bufName nodeBaseName)
    rexCompile( "_V$" )
    nodeBaseName = rexReplace( nodeBaseName ".V" 1)
    rexCompile( "_D$" )
    nodeBaseName = rexReplace( nodeBaseName ".D" 1)
    rexCompile( "_D\\[" )
    nodeBaseName = rexReplace( nodeBaseName ".D[" 1)
    return(nodeBaseName)
  )
)

(defun Ru_GetNodeBaseNameFromNodeName (nodeName)
  (let (nodeBaseName)
;FIXME
    nodeBaseName = substring(nodeName 2 strlen(nodeName)-3)
    Ru_ReplaceDotWithDash(nodeBaseName)
  )
)

(defun Ru_SplitNodeName (nodeName @key (delim "_"))
  (let (nodeBaseName elName el)
;FIXME
    el = parseString(nodeName ".")
    ;printf("NodeName %L el %L %d\n" nodeName el length(el))
    (if length(el) == 3 && rexMatchp("^DFT" car(el)) && (cadr(el)=="D"||cadr(el)=="C") then ;FIXME hardcoded for DFT
        elName = sprintf(nil "%s.%s" nth(1 el) nth(2 el))
        nodeBaseName = car(el)
    else
        elName = nth(length(el)-1 el)
        nodeBaseName = car(el)
        (for cnt 1 length(el)-2
            nodeBaseName = strcat(nodeBaseName sprintf(nil "%s%s" delim nth(cnt el)))
        )
    )
    list(nodeBaseName elName)
  )
)

;matched to the nodeList entry
(defun Ru_BaseNameMatched (subName name)
  (prog (subResult result cnt idx range max min value entry)
    subResult = parseString(subName ".[]_")
    result = parseString(name ".[]_")
    ;printf("Ru_BaseNameMatched: subName: %L name: %L\n" subResult result)
    ;should match all the sub fields
    (for cnt 0 length(subResult)-1
        (if nth(cnt subResult) == nth(0 result) then
            matched = t
            (for idx cnt+1 length(subResult)-1
                ;printf("subResult#%d:%L  Result#%d: %L matched %L\n" (cnt+idx) nth((cnt+idx) subResult) idx nth(idx result) nth((cnt+idx) subResult) != nth(idx result))
; load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il")
                entry = nth((cnt+idx) subResult)
                (if !entry return(nil)) ;does not match completely
                (if matched &&  entry!=nth(idx result) then
                    range = parseString(entry "-")
                    (if length(range) == 2 then
                        min = Ru_StringToInt(car(range))
                        max = Ru_StringToInt(cadr(range))
                        value = Ru_StringToInt( nth(idx result))
                        ;printf("Range %L min %L max %L value %L\n" range min max value)
                        (if value < min && value > max then
                            ;printf("Field #%d %L not matched %d %L\n" (cnt+idx) nth((cnt+idx) subResult) idx nth(idx result))
                            matched = nil
                        else
                            value = 0 ;stub
                            ;printf("Field #%d %L is within range %d %L\n" (cnt+idx) nth((cnt+idx) subResult) idx nth(idx result))
                        )
                        
                    else
                        ;printf("Field #%d %L not matched %d %L\n" (cnt+idx) nth((cnt+idx) subResult) idx nth(idx result))
                        matched = nil
                    )
                )
            )
           (if matched return(t)) 
        else
            idx = 0 ;stub
            ;printf("First Field #%d %L not matched %d %L\n" cnt nth(cnt subResult) 0 nth(cnt result))
            ;find next match if there is one
        )
    )
    return(nil)
  )
)

(defun Ru_GetChannelNodeOffset (channel node @key (nestedArray nil))
  (let (ch ret exactMatch nd nodeLen el
        subNode subNodeE fieldList indexList fieldListE indexListE)
exactMatch = nil
    nodeLen = strlen(node)
    subNode = parseString(node (if nestedArray "._[]" "._"))
    ;printf("GetChannelNodeOffset: node %L nestedArray %L channel %L \n" node nestedArray channel)
    (foreach ch channel
      (if !ret then
        (cond
          (nestedArray
            nd = nth(0 ch)
            subNodeE = parseString(nd "._[]")
            indexList = nil
            fieldList = nil
            indexListE = nil
            fieldListE = nil
            (foreach el subNode
                (if Ru_StringIsInt(el) then indexList = append(indexList list(el))
                else fieldList = append(fieldList list(el)))
            )
            (foreach el subNodeE
                (if Ru_StringIsInt(el) then indexListE = append(indexListE list(el))
                else fieldListE = append(fieldListE list(el)))
            )
            ;printf("ENTRY %L %L %L indexList %L %L\n" nd subNodeE strlen(nd) indexListE fieldListE)
            ;printf("NODE  %L %L %L indexList %L %L\n" node subNode nodeLen indexList fieldList)
            (if indexListE == indexList && fieldListE == fieldList then
                ret = caddr(ch)
                ;printf("FOUND1 subNode %L subNodeE: %L ret: %L %L\n" subNodeE subNode ret ch)
            )
          )
          (!exactMatch
            nd = nth(0 ch)
            ;printf("entry %L node %L\n" nd node)
            (if nodeLen == strlen(nd) then
                ;since we replace . with _ for wire node name
                subNodeE = parseString(nd "._")
                (if subNodeE == subNode ret = caddr(ch))
                ;(if ret printf("FOUND1 subNode %L subNodeE: %L ret: %L %L\n" subNodeE subNode ret ch))
            )
          )
         (t
            (if node == nth(0 ch) then
                ret = caddr(ch)
                ;(if ret printf("FOUND2 subNode %L subNodeE: %L ret: %L %L\n" subNodeE subNode ret ch))
            )
         )
        )
      )
    ) ;foreach
    ret
  )
)

(defun Ru_GetChannelEntry (channel node)
  (let (ch ret)
    (foreach ch channel
	(if !ret && node == nth(0 ch) then
            ret = ch
        )
    )
    ret
  )
)

(defun Ru_GetBusPatternEntry (busPattern name node)
  (let (bp layerPattern ret)
    (foreach bp busPattern
	(if !ret && name == nth(0 bp) then
            layerPattern = nth(1 bp)
            ret = Ru_GetChannelEntry(layerPattern node)
        )
    )
    ret
  )
)

(defun Ru_CanUseDoubleVias (pattern nodeEntry)
  (prog (minOff el nodeName)
    ;printf("nodeEntry %L withinGrid %L off %L\n" nodeEntry ru_WithinGrid((nth(2 nodeEntry)-TrackPitch/2) TrackPitch 4*TrackPitch/24) Ru_TrackOffset(nth(2 nodeEntry)))
    (if ru_WithinGrid((nth(2 nodeEntry)-TrackPitch/2) TrackPitch 4*TrackPitch/24) return(nil))
    minOff = 99999.0
    nodeName = nth(0 nodeEntry)
    (foreach el pattern
        (if abs(nth(2 nodeEntry)-nth(2 el)) < minOff && 
                nodeName != nth(0 el) && nth(3 nodeEntry) == nth(3 el) then
            minOff = abs(nth(2 nodeEntry)-nth(2 el))
            (if minOff <= 2.5*wirePitch then
                ;printf("%L Only use single vias minOff: %L\n" nodeName minOff)
                return(nil)
            )
        )
    )
    return(t)
  )
)

(defun RouteNode (path nodeList layerPatternList
                  @key (prefix "") (bufPrefix "mbuf") (wirePrefix "w") (postFix "")
                 (firstPattern nil) (lastPattern nil) (inlinePrefix nil)
                 (flipSegPattern nil)  (turnToStartPinLayer t) (turnToEndPinLayer t)
                 (segLException nil) (segRException nil) (enableProp nil)
                 (startViasLoc nil) (endViasLoc nil) (forceSingleVias nil)
                 (reserveStartPinGap t) (reserveEndPinGap t)
                 (skipTermToStartPin  nil) (skipTermToEndPin nil)
                 (startSwitchPattern nil) (endSwitchPattern nil)
                 (replaceDotWithDash t) (ignoreGnd nil)
                 (filterList nil) (filterExcludeList nil)
                 (maxManDist 480.0) (maxManDistFirstSeg nil) (maxManDistLastSeg nil)
                 (debug 0) (debugStackVias nil) (h nil) )
  (if debug >= 1 printf("RouteNode path %L\n" path ))
  (prog (wirePitch bp nodeName width layer termWire termWire filterNode bufInst lastBufInst numExtraBufs
        wireName lastBufSegNum nodeBaseName nodeElName result el cnt idx minOff temp wireDir parseNode
         x y flipSegX flipSegY maxEntry minEntry indexList fieldList entry routeNode
         busMaxOff maxFirstOff maxLastOff termToStartPin termToEndPin skipFirstSeg skipLastSeg
         DEFAULT_WIRE_PREFIX)
    (if h then
        printf("\n\n")
        printf("Route wires to the buffers along a given path and channel pattern\n")
        printf("Usage: RouteNode(path [maxBuffers,nodeList] layerPatternList [OPTION])\n")
        printf("       ?h t                        - this help\n")
        printf("       ?firstPattern <pat>         - Pattern of first segment from input pin\n")
        printf("       ?lastPattern <pat>          - Pattern of last segment to output pin\n")
        printf("       ?filterList <filter>        - Only route nodes in this list. Uses regexp matching\n")
        printf("       ?filterExcludeList <filter> - Only route nodes not in this list. Uses regexp matching\n")
        printf("       ?maxManDist <value>         - Warn if wire segment is above this value\n")
        printf("       ?maxManDistFirstSeg <value> - Warn if first wire segment is above this value. Default is 80 percent of maxManDist\n")
        printf("       ?maxManDistLastSeg <value>  - Warn if last wire segment is above this value. Default is 80 percent of maxManDist\n")
        printf("       ?flipSegPattern <list>      - Specify a list of segments to flip bus pattern\n")
        printf("       ?turnToStartPinLayer <bool> - Specify whether first segment to turn to pin layer\n")
        printf("       ?turnToEndPinLayer <bool>   - Specify whether last segment to turn to pin layer\n")
        printf("       ?doubleVias <t or nil>      - Specify if channel turn vias are single or double\n")
        printf("       ?startViasLoc <value>       - Specify vias X or Y location for channel to escape to start pin\n")
        printf("       ?endViasLoc <value>         - Specify vias X or Y location for channel to escape to end pin\n") 
        printf("       ?skipTermToStartPin <bool>  - Specify whether to terminate to start pins or let routing connecting them\n")
        printf("       ?skipTermToEndPin <bool>    - Specify whether to terminate to end pins or let routing connecting them\n") 
        printf("       ?ignoreGnd <bool>           - Specify whether to route GND wires. Default is nil\n") 
        printf("       ?replaceDotWithDash <bool>  - Specify whether to replace dot separator with dash. Default is true\n") 
        printf("       ?debugStackVias <t or nil>  - Used to check if a wire crossing stacked vias\n")

        return(t)
    )
    ;can't have "mbuf_" due to wireDir
    DEFAULT_WIRE_PREFIX = "w"
    (if bufPrefix == "mbuf" && prefix != "" bufPrefix = sprintf(nil "mbuf%s" prefix))
     (if wirePrefix == DEFAULT_WIRE_PREFIX && prefix != "" wirePrefix = sprintf(nil "w%s" prefix))
;printf("PREFIX: %L %L %L\n" bufPrefix wirePrefix prefix)
    wirePitch = TrackPitch/24
    numExtraBufs = 0
;    (if wirePrefix != "w_" && nodeList error("Not support wirePrefix of %L\n" wirePrefix)) ;due to wireDir
    wirePitch = TrackPitch/24
    (if flipSegPattern then
        busMaxOff = -9999.99
        (foreach el layerPatternList
            (if busMaxOff < nth(2 el) busMaxOff = nth(2 el))
        )
        (foreach el flipSegPattern
            (if el == 0 && firstPattern then
                error("Does not support flipping first segment when firstPattern is specified. Use ModifyChannel instead\n"))
            (if el == length(path)-1 && lastPattern then
                error("Does not support flipping last segment when lastPattern is specified. Use ModifyChannel instead\n"))
            (if Ru_GetDistance(nth(el path) nth(el+1 path)) == 0.0 then ;zero length then use next next segment
                (if ru_getSegDir(nth(el+1 path) nth(el+2 path))!="VERT" then
                    flipSegX = append(flipSegX list(el))
                    ;shift the segment
                    path = OffsetSomePoints(path el (el+1) RoundDownTrack(busMaxOff) 0) 
                else 
                    flipSegY = append(flipSegY list(el))
                    path = OffsetSomePoints(path el (el+1) 0 RoundDownTrack(busMaxOff)) 
                )
            else
                (if ru_getSegDir(nth(el path) nth(el+1 path))=="VERT" then
                    flipSegX = append(flipSegX list(el))
                    ;shift the segment
                    path = OffsetSomePoints(path el (el+1) RoundDownTrack(busMaxOff) 0) 
                else 
                    flipSegY = append(flipSegY list(el))
                    path = OffsetSomePoints(path el (el+1) 0 RoundDownTrack(busMaxOff)) 
                )
            )
            (if debug >= 0 printf("Flip segment #%d pattern. busMaxOff %f flipSegX %L flipSegY %L\n" el busMaxOff/TrackPitch flipSegX flipSegY))
       )
    )

    ;check to see if we can use double vias
    firstSegViasTable = makeTable("firstSegViasTable" 0)
    lastSegViasTable = makeTable("lastSegViasTable" 0)
    busViasTable = makeTable("busViasTable" 0)
    maxFirstOff = -9999.99
    (foreach bp firstPattern
       (if maxFirstOff < nth(2 bp) maxFirstOff = nth(2 bp))
        nodeName = nth(0 bp)
        firstSegViasTable[nodeName] = (if forceSingleVias nil Ru_CanUseDoubleVias(firstPattern bp))
    )
    maxLastOff = -9999.99
    (foreach bp lastPattern
       (if maxLastOff < nth(2 bp) maxLastOff = nth(2 bp))
        nodeName = nth(0 bp)
        lastSegViasTable[nodeName] = (if forceSingleVias nil Ru_CanUseDoubleVias(lastPattern bp))
    )
    busMaxOff = -9999.99
    (foreach bp layerPatternList
       (if busMaxOff < nth(2 bp) busMaxOff = nth(2 bp))
        nodeName = nth(0 bp)
        busViasTable[nodeName] = (if forceSingleVias nil Ru_CanUseDoubleVias(layerPatternList bp))
    )


(if firstPattern && startSwitchPattern==t then
segOff = 0.0
segNegDir = nil; going from max to min instead from min to max
    (if ru_getSegDir(nth(0 path) nth(1 path))=="VERT" then
        segOff = xCoord(nth(0 path)) - xCoord(nth(2 path))
        (if yCoord(nth(2 path)) > yCoord(nth(0 path)) segNegDir = t )
    else 
        segOff = yCoord(nth(0 path)) - yCoord(nth(2 path))
        (if xCoord(nth(2 path)) > xCoord(nth(0 path)) segNegDir = t )
    )
    startSwitchPattern = GenerateSwitchPattern(layerPatternList firstPattern segOff segNegDir ?debug debug)
)
(if lastPattern && endSwitchPattern==t then
segOff = 0.0
segNegDir = nil; going from max to min instead from min to max
pathLen = length(path)
    (if ru_getSegDir(nth(pathLen-1 path) nth(pathLen-2 path))=="VERT" then
        segOff = xCoord(nth(pathLen-3 path)) - xCoord(nth(pathLen-1 path))
        (if yCoord(nth(pathLen-3 path)) > yCoord(nth(pathLen-1 path)) segNegDir = t )
    else 
        segOff = yCoord(nth(pathLen-1 path)) - yCoord(nth(pathLen-3 path))
        (if xCoord(nth(pathLen-3 path)) > xCoord(nth(pathLen-1 path)) segNegDir = t )
    )
    endSwitchPattern = GenerateSwitchPattern(layerPatternList lastPattern segOff segNegDir ?debug debug)
)

    (foreach bp layerPatternList
      nodeName = nth(0 bp)
      width = nth(1 bp)
      nodeOff = nth(2 bp)
      layer = nth(3 bp)
      termWire = t
      (if filterList then
           filterNode = ru_GetFilterList(filterList nodeName)
           (if debug >= 2 then
               (if filterNode then printf("%s Found in filter list: %L\n" nodeName filterNode) 
               else printf("%s Not Found in filter list %L\n" nodeName filterList))
           )
           termWire = (filterNode!=nil)
      )
      (if filterExcludeList then
          filterNode = ru_GetFilterList(filterExcludeList nodeName)
          (if debug >= 4 && filterNode printf("%L Found in filter exclude list: %L\n" nodeName filterNode))
          (if debug >= 4 && !filterNode printf("%L Not Found in filter exclude list\n" nodeName))
          termWire = !(filterNode!=nil)
      )

; hasBuffer gndPath gndLayer start end gridLoc viasLoc cuts segDir
       hasBuffer = t
       (if termWire && nodeName == "GND" then
         (if !ignoreGnd then
          nodePath = OffsetPath(path nodeOff nodeOff)
          (foreach cnt flipSegPattern
              segDir = ru_getSegDir(nth(cnt nodePath) nth(cnt+1 nodePath))
              ;path has been offset once, so need to do 2 times here
              nodePath = OffsetSomePoints(nodePath cnt cnt+1 
                                         (if segDir == "HOR" 0 -2*nodeOff)
                                         (if segDir == "HOR" -2*nodeOff 0))
          )
          gndPath = nil
          start = (if firstPattern 1 0)
          end = length(nodePath)-1
          (if lastPattern end = end - 1)
          (for cnt start end
            (cond ;offset 1 track to prevent overlap to pins
              (cnt == 0
                  segDir = ru_getSegDir(nth(cnt nodePath) nth(cnt+1 nodePath) ?detail t)
                  (cond
                    (segDir == "RIGHT" offX = TrackPitch offY = 0)
                    (segDir == "LEFT" offX = -TrackPitch offY = 0)
                    (segDir == "UP" offX = 0 offY = TrackPitch)
                    (segDir == "DOWN" offX = 0 offY = -TrackPitch)
                  )
                  gndPath = append(gndPath list(OffsetPoint(nth(cnt nodePath) offX offY)))
              )
              (cnt == length(nodePath)-1
                  segDir = ru_getSegDir(nth(cnt-1 nodePath) nth(cnt nodePath) ?detail t)
                  (cond
                    (segDir == "RIGHT" offX = -TrackPitch offY = 0)
                    (segDir == "LEFT" offX = TrackPitch offY = 0)
                    (segDir == "UP" offX = 0 offY = -TrackPitch)
                    (segDir == "DOWN" offX = 0 offY = TrackPitch)
                  )
                  gndPath = append(gndPath list(OffsetPoint(nth(cnt nodePath) offX offY)))
              )
              (t gndPath = append(gndPath list(nth(cnt nodePath))))
            )
          )
          (DrawWire layer nodeName width gndPath ?doubleVias nil)
          ;draw the vias in the middle of each segment
          (for cnt 0 length(gndPath)-2
            pA = nth(cnt gndPath)
            pB = nth(cnt+1 gndPath)
            (if Ru_GetDistance(pA pB) > 8*TrackPitch then
                segDir = ru_getSegDir(pA pB)
                gndLayer = nth(2 layer)
                (if segDir == "HOR" then
                  viasLoc = RoundDownTrack((xCoord(pA)+xCoord(pB))/2 ?numTrack 2):yCoord(pA)
                  (if gndLayer == "M3_M2" then gndLayer = "M4_M3")
                  cuts = "_2CUT_H"
                else
                  viasLoc = xCoord(pA):RoundDownTrack((yCoord(pA)+yCoord(pB))/2 ?numTrack 2)
                  cuts = "_2CUT_V"
                )
                DrawVia("GND" gndLayer viasLoc width cuts ?doubleVias t)
            )
          )
        ) ;ignoreGnd
        termWire = nil
      )

      (if termWire then
        nodePath = Ru_GetPathNode(path layerPatternList nodeName 
                                  ?firstPattern firstPattern ?lastPattern lastPattern
                                  ?firstSwitchPattern startSwitchPattern
                                  ?lastSwitchPattern endSwitchPattern
                                  ?flipSegX flipSegX ?flipSegY flipSegY)
        ;(DrawWire BusMetal67 nodeName wirePitch nodePath)
        result = Ru_SplitNodeName(nodeName ?delim (if replaceDotWithDash "_" "."))
        nodeBaseName = car(result)
        nodeElName = cadr(result)
    (cond
      (nodeList == nil || nodeName == "GND"
        hasBuffer = nil
        minBufSeg = 0
        maxBufSeg = 0
      )
      (integerp(nodeList) then
        ;simple range from 0 to max value
        minBufSeg = 0
        maxBufSeg = nodeList
      )
      (t 
        minBufSeg = nil
        (foreach el nodeList
          (if !minBufSeg then ;don't know how to break out
            ;printf("ROUTE: nodeName %L nodeBaseName  %L nodeElName %L \n" car(el) nodeBaseName el)
            temp = car(el) ;FIXME find a better way
            (if substring(temp 1 1) == "-"  ||
                substring(temp 1 1) == "+" then ;remove direction flag
                temp = substring(temp 2 strlen(temp)-1)
            ) 
            ;printf("##### NodeListEntry: %L nodeName: %L matched: %L\n" temp nodeBaseName Ru_BaseNameMatched(temp nodeBaseName))
            (if Ru_BaseNameMatched(temp nodeBaseName) then
                minEntry = nil
                parseNode = parseString(car(el) "[]")
                (if length(parseNode) == 3 then
                    result = parseString(nth(1 parseNode) "-")
                    minEntry = Ru_StringToInt(car(result))
                    maxEntry = Ru_StringToInt(cadr(result))
                    idx = Ru_StringToInt(cadr(parseString(nodeBaseName "[]")))
                else
                    (if nth(1 el) then
                        result = parseString(nth(1 el) ".")
                        minEntry = Ru_StringToInt(car(result))
                        maxEntry = Ru_StringToInt(cadr(result))
                        idx = Ru_StringToInt(car(last(parseString(nodeBaseName "[]"))))
                    )
                )
                ;printf("nodeBaseName: %L parse: %L parseNode %L idx %L min %L max %L NodeList entry %L %L\n" nodeBaseName parseString(nodeBaseName "[]") parseNode idx minEntry maxEntry el result)
                (if !minEntry || !idx || (idx >= minEntry && idx <= maxEntry) then
                  (if !nth(3 el) then ;no buffer for this entry
                    hasBuffer = nil
                    minBufSeg = 0
                    maxBufSeg = 0
                  else
                    result = parseString(nth(3 el) ".")
                    minBufSeg = Ru_StringToInt(car(result))
                    maxBufSeg = Ru_StringToInt(cadr(result))+1
                   (if debug >= 1 printf("%s minBufSeg %d maxBufSeg %d matched to nodeList entry %L\n" nodeBaseName minBufSeg maxBufSeg el))
                   (if length(parseNode) == 3 then
                       result = parseString(nodeBaseName "[]")
                       (if debug >= 1 printf("%s has embedded array %L parseNode %L\n" nodeBaseName result parseNode ))
                       indexList = nil
                       fieldList = nil
                       (foreach temp result
                           (if Ru_StringIsInt(temp) then indexList = append(indexList list(temp))
                           else fieldList = append(fieldList list(temp)))
                       )
                       result = ""
                       (foreach entry fieldList
                           result = strcat(result entry)
                       )
                       (foreach entry indexList
                           result = strcat(result sprintf(nil "[%s]" entry))
                       )
                       (if debug >= 1 printf("%s is changed to %s. fieldList %L indexList %L" nodeBaseName result fieldList indexList))
                       nodeBaseName = result
                   )
                 ) ;if !nth(3 el)
                )
            )
          )
        )
      )
    ) ;cond

(if !minBufSeg error("Unable to match node name %L in nodeList %L\n" nodeName nodeList))

        bufSegNum = minBufSeg-1
        lastBufSeg = nil
        lastBufInst = nil
        wireDir = ""
        (while bufSegNum < maxBufSeg
          lastBufInst = bufInst
          ;find next buffer
          bufInst = nil
          lastBufSegNum = bufSegNum
          (while hasBuffer && !bufInst && bufSegNum < maxBufSeg
            bufSegNum++
            bufName = sprintf(nil "%s%s_%s[%d]" 
                              (if inlinePrefix inlinePrefix "")
                              bufPrefix nodeBaseName bufSegNum)
            bufInst = (dbFindAnyInstByName (geGetEditCellView) bufName)
            (if !bufInst then ;FIXME find a better way?
                temp = sprintf(nil "%s%sr_%s[%d]" 
                              (if inlinePrefix inlinePrefix "")
                               bufPrefix nodeBaseName bufSegNum)
;printf("%s: Trying to find if the buffer is reverse direction\n" temp)
                bufInst = (dbFindAnyInstByName (geGetEditCellView) temp)
                (if bufInst then wireDir = "r" bufName = temp)
            )
          )
          (if bufInst && bufSegNum == maxBufSeg then
              ;printf("WARN: RouteNode: max buffer index is %d but found a buffer %s. %s\n" maxBufSeg-1 bufName nodeElName)
              bufInst = nil
              (if rexMatchp("e$" nodeElName) numExtraBufs++)
          )
          (cond
            (!hasBuffer
              (if nodeBaseName == "GND" then
                wireName = "GND"
              else
                wireName = sprintf(nil "%s%s%s%s%s.%s" (if inlinePrefix inlinePrefix "") wirePrefix 
                           (if substring(nodeName 1 1)=="." "." "") nodeBaseName postFix nodeElName)
;  load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il")
;printf("%L: inlinePrefix %L wirePrefix %L nodeBaseName %L nodeName %L postFix %L %L\n" wireName inlinePrefix wirePrefix nodeBaseName nodeName postFix nodeElName)
              )
              bufSegNum = maxBufSeg
              bufName = "NO_BUF"
            )
            (bufInst 
              wireName = sprintf(nil "%s%s%s_%s[%d].%s" (if inlinePrefix inlinePrefix "") wirePrefix wireDir nodeBaseName bufSegNum nodeElName)
            )
            (t
              wireName = sprintf(nil "%s%s%s_%s[%d].%s" (if inlinePrefix inlinePrefix "") wirePrefix wireDir nodeBaseName lastBufSegNum+1 nodeElName)
            )
          )
;printf("#### %L: %L maxBufSeg %L nodeList %L\n" bufName bufSegNum maxBufSeg nodeList)
          (if debug >= 1 printf("RouteNode: %s: wireName %s: nodeName %s nodeBaseName %s bufSegNum %d layer %L nodePath %L\n" 
                                 bufName  wireName nodeName nodeBaseName bufSegNum layer nodePath))

          (if !lastBufInst && !bufInst && hasBuffer
              error("Ru_TerminateNode: %s There is no buffer %s found along the path. nodeName %L nodeBaseName %L nodeElName %L\n"
                    wireName bufName nodeName nodeBaseName nodeElName)
          )
           
          (if bufInst temp = Ru_LocatePathSeg(nodePath bufInst->xy 0))
          (if bufInst && !nth(temp nodePath) then
                temp = car(last(nodePath))
                x = xCoord(bufInst->xy)-xCoord(temp)
                y = yCoord(bufInst->xy)-yCoord(temp)
                printf("ERROR: %s position %L (DIFF: %L:%L) is not located along the path %L\n" bufInst->name  bufInst->xy x y nodePath)
                bufSegNum = maxBufSeg ; done
                bufInst = nil
          else
            routeNode = t
            termToStartPin = t
            termToEndPin = t
            skipFirstSeg = nil
            skipLastSeg = nil
            (if skipTermToStartPin then
                (if listp(skipTermToStartPin) then
                    filterNode = ru_GetFilterList(skipTermToStartPin nodeName)
                    (if filterNode then
                        (if cadr(filterNode) then skipFirstSeg = t
                        else termToStartPin = nil)
                    )
                else termToStartPin = nil)
                (if !termToStartPin && debug >=1 printf("%L: Skip terminate to start pin\n" wireName))
            )
            (if skipTermToEndPin then
                (if listp(skipTermToEndPin) then
                    filterNode = ru_GetFilterList(skipTermToEndPin nodeName)
                    (if filterNode then
                        (if cadr(filterNode) then skipLastSeg = t
                        else termToEndPin = nil)
                    )
                else termToEndPin = nil)
                (if !termToEndPin && debug >=1 printf("%L: Skip terminate to end pin\n" wireName))
            )
            (if termToStartPin==nil && lastBufInst==nil routeNode = nil) ;first segment

            (if termToEndPin==nil && lastBufInst!=nil && bufInst== nil routeNode = nil) ;last segment

            (if routeNode then
                temp = substring(nodeElName 1 2)
                Ru_TerminateNode(nodeName wireName lastBufInst bufInst nodePath layer path
                             ?firstPattern firstPattern ?lastPattern lastPattern
                             ?skipFirstSeg skipFirstSeg ?skipLastSeg skipLastSeg 
                             ?firstDoubleVias firstSegViasTable[nodeName]
                             ?lastDoubleVias lastSegViasTable[nodeName]
                             ?doubleVias busViasTable[nodeName]
                             ?maxManDist maxManDist
                             ?startViasLoc startViasLoc ?endViasLoc endViasLoc
                             ?reserveStartPinGap reserveStartPinGap ?reserveEndPinGap reserveEndPinGap
                             ?segLException segLException ?segRException segRException
                             ?backwardDir (wireDir=="r")
                             ?flipSegPattern flipSegPattern
                             ?termPin (temp!="C.") ?enableProp enableProp 
                             ?turnToStartPinLayer turnToStartPinLayer ?turnToEndPinLayer turnToEndPinLayer
                             ?maxManDistFirstSeg maxManDistFirstSeg ?maxManDistLastSeg maxManDistLastSeg
                             ?debugStackVias debugStackVias ?debug debug)
            else
               (if debug >= 1 printf("%L: %L: %L: Skipping bufSegNum %d\n" bufName  wireName nodeName bufSegNum))
            )
          )
        )
      ) ;termWire
    ) ;foreach
    ;count is per node, so estimate number of buffers
    (if numExtraBufs > 0 then printf("There are at least %d unused mbufs\n" numExtraBufs))
  )
  t
)

(defun Ru_TerminateNodeR (nodeName wireName bufInst layer bufSegDir bufSegX bufSegY 
                          @key (debug 0) (doubleVias t) (termPin t) (backwardDir nil)
                          (bufSegNum nil) (flipSegPattern nil) (segRException nil) (enableProp nil)
                          (debugStackVias nil))
  (let (bufLoc pinROff pinRLoc reSegOff layerList X Y reSeg dblVias flipX flipY pinExt pinExtLayer cuts 
        fromLayer toLayer viasList reLayer rsLayer turnLayer startPoint nodeOff turnSeg segDist
        prop propValue)

    (if !nodeName || !stringp(nodeName)  error("Ru_TerminateNodeR: Invalid nodeName %L\n" nodeName))
    (if !wireName || !stringp(wireName)  error("Ru_TerminateNodeR: Invalid wireName %L\n" wireName))
    (if !listp(layer) || length(layer) <= 1 error("Ru_TerminateNodeR: Invalid layer %L\n" layer))

    (if debug >= 3 printf("Ru_TerminateNodeR: wireName %L layer %L dir %L bufSegX %L bufSegY %L\n"
                          wireName layer bufSegDir bufSegX bufSegY))
    nodeOff = 0.0 ; path is already the node path
    bufLoc = bufInst->xy
    flipX = (bufInst->orient == "MX" || bufInst->orient == "R180")
    flipY = (bufInst->orient == "MY" || bufInst->orient == "R180")
    (if backwardDir then
        pinROff = Ru_getPinLLoc(bufInst->master->cellName nodeName bufInst->orient)
    else
        pinROff = Ru_getPinRLoc(bufInst->master->cellName nodeName bufInst->orient)
    )
    pinRLoc = Ru_OffsetBox(pinROff bufLoc)
    (if debug >= 1 printf("Ru_TerminateNodeR: %s bufSegDir %s backwardDir %L PINR off %L pinLLoc %L\n" 
            wireName bufSegDir backwardDir pinROff pinRLoc))
    reSegOff = 0.0
    layerList = nil
    dblVias = t
    pinExt = nil
    pinExtLayer = 3
    segDist = 0.0
turnSeg = t
    (if bufSegDir == "HOR" then
        X = bufSegX + ru_getTurnOffset(bufLoc bufInst->master->cellName nodeName (backwardDir==t)  flipY bufInst)
        Y = bufSegY
        fromLayer = ru_layerToValue(nth(0 nth(0 layer)))
        toLayer = (if fromLayer == 3 4 (fromLayer-1))
        (if toLayer == 4 then
            reSeg = list( Ru_OffsetPoint(X:Y 0.0:nodeOff) X:Ru_MidY(pinRLoc))
        else
            startPoint = Ru_OffsetPoint(X:Y nodeOff:0.0)
pinSwitchOff = wirePitch*4
(if member(bufSegNum flipSegPattern) pinSwitchOff = -pinSwitchOff)
(if rexMatchp("1of1" bufInst->master->cellName) then
  (if (GetProp bufInst "Position" "")=="TOP" pinSwitchOff = (2*TrackPitch-pinSwitchOff))
else
  (if yCoord(startPoint) > (yCoord(bufLoc)+2*TrackPitch) then
     (if member(bufSegNum flipSegPattern) then
          pinSwitchOff = pinSwitchOff;FIXME
     else pinSwitchOff = (4*TrackPitch-pinSwitchOff))

  )
)
            reSeg = list( startPoint X:(yCoord(bufLoc)+pinSwitchOff))
;                         (if flipX -1 1)*(if yCoord(startPoint) > (yCoord(bufLoc)+2*TrackPitch) && 
;                             !rexMatchp("1of1" bufInst->master->cellName) (4*TrackPitch-pinSwitchOff) pinSwitchOff)) )
            pinExt = list(car(last(reSeg))  X:Ru_MidY(pinRLoc))
            pinExtLayer = 4
            (if Ru_PathDistance(pinExt) < TrackPitch/24/4 pinExt = nil) ; already on top of pin
        )

        startPoint = car(reSeg)
        dblVias = (if ru_WithinGrid(xCoord(car(reSeg)) TrackPitch 4*TrackPitch/24) nil t)
        dist = Ru_GetDistance(nth(0 reSeg) nth(1 reSeg))
        (if dist < TrackPitch/24/4 then
            reSeg = nil ; no need to turn, on top on pin
            turnSeg = nil
        )
        viasList = GenViasList(fromLayer toLayer)
        layerList = ru_GenLayerList(toLayer pinExtLayer) ; going to multiple layers
        viasList = GenViasList(toLayer 3)
        reLayer = sprintf(nil "M%d" toLayer)
    else
        X = bufSegX
        fromLayer = ru_layerToValue(nth(0 nth(1 layer)))
        toLayer = (if fromLayer == 2 3 5)
        viasList = GenViasList(fromLayer toLayer)

        Y = Ru_MidY(pinRLoc)
        reSeg = nil
        segExcep = ru_GetSegException(segRException wireName)
        (if enableProp then
            (if !segExcep then
                ;special case for SchedToIngress to prevent bus overlap
                 prop = (GetProp bufInst "OffsetNodeMatch" "")
    ;printf("%s: OffsetNodeMatch %L by %L\n" wireName prop propValue->value)
                (if prop && prop!="" && rexMatchp(prop wireName) then
                    propValue = (dbFindProp bufInst "OffsetNodeValue")
                    segExcep = list(nil (if propValue->value propValue->value 0)*wirePitch)
    ;printf("%s: OffsetNode by %L\n" wireName propValue->value)
                    termPin = nil
                )
            )
            prop = (GetProp bufInst "SingleViasMatch" "")
            (if prop && prop!="" && rexMatchp(prop wireName) then
                propValue = (dbFindProp bufInst "SingleViasValue")
                dblVias = (if propValue->value=="TRUE" nil t)
;printf("%s: R SingleVias  %L\n" wireName propValue->value)
            )
        )
;        (if !segExcep && !(GetProp bufInst "LeftRightAlign" t) then
;            segExcep = list(nil TrackPitch)
;            termPin = nil
;        )
        (if segExcep  then
            reSegOff = nth(1 segExcep)
            (if debug >= 1 printf("%s R horizontal segment X %f Y %f is offset by %L\n" wireName X Y reSegOff))
            Y = Y + reSegOff
            termPin = nil
        )
        startPoint = Ru_OffsetPoint(X:Y nodeOff:0.0)
        (if nil && Ru_IsBetween(xCoord(lowerLeft(pinRLoc)) (X+nodeOff) xCoord(upperRight(pinRLoc))) then
            reSeg = nil ; no need to turn
            turnSeg = nil
        else
            (if toLayer == 3 then
                reSeg = list( Ru_OffsetPoint(X:Y nodeOff:0.0) Ru_MidX(pinRLoc):Y)
            else 
pinSwitchOff = wirePitch*4
                reSeg = list( Ru_OffsetPoint(X:Y nodeOff:0.0) (RoundDownTrack(xCoord(bufLoc))+(if flipY -pinSwitchOff pinSwitchOff)):Y)
                pinExt = list(car(last(reSeg)) Ru_MidX(pinRLoc):Y)
                (if Ru_PathDistance(pinExt) < TrackPitch/2 pinExt = nil) ; already on top of pin
            )
        )
        dblVias = (if ru_WithinGrid(yCoord(car(reSeg)) TrackPitch 4*TrackPitch/24) nil dblVias)
        layerList = ru_GenLayerList(toLayer pinExtLayer) ; going to multiple layers
        viasList = GenViasList(toLayer 3)
        reLayer = sprintf(nil "M%d" toLayer) 
    )
    (if debug >= 1 printf("R_END %s startPoint %L layer %L dblVias %L bufSegDir %s\n"
                          wireName startPoint layer dblVias bufSegDir))
    (if debug >= 1 printf("R_END: %s segment %L to buffer going from layer %d to %d. reLayer %L\n"
                          wireName reSeg fromLayer toLayer reLayer))
    ;can use double vias
    turnLayer = GenViasList(fromLayer toLayer)
    (if dblVias then DrawVia(wireName turnLayer startPoint wirePitch 
                             (if bufSegDir == "HOR" "_2CUT_H" "_2CUT_V"))
    else DrawVia(wireName turnLayer startPoint wirePitch "" ?doubleVias nil))
    (if reSeg && Ru_PathDistance(reSeg) > TrackPitch/64 then
        Ru_ValidatePath(reSeg sprintf(nil "%s R_END" wireName))
        segDist = segDist + Ru_PathDistance(reSeg)
        (DrawSegment wireName reLayer wirePitch nth(0 reSeg) nth(1 reSeg) t t nil)
    )
    (if termPin && pinExt then
        Ru_ValidatePath(pinExt sprintf(nil "%s R_PINEXT" wireName))
        (DrawSegment wireName sprintf(nil "M%d" pinExtLayer) wirePitch nth(0 pinExt) nth(1 pinExt) t t nil)
        segDist = segDist + Ru_PathDistance(pinExt)
        (if pinExtLayer != 3 then ;HOR connection
            cuts = (if dblVias "_2CUT_H" "") 
           (if !dblVias then dblVias = t cuts = "_2CUT_V")
            DrawVia(wireName GenViasList(pinExtLayer 3) car(last(pinExt))
                    wirePitch cuts ?doubleVias dblVias)
        )
    )

    (if termPin && toLayer!=3 
        (if pinExtLayer != 3 then ;HOR connection
            (if toLayer != 6 error("R_END: %s Unexpected toLayer %L with pinExtLayer %L\n" wireName toLayer pinExtLayer))
            cuts = "_2CUT_V"
            DrawVia(wireName GenViasList(toLayer pinExtLayer) car(pinExt)
                    wirePitch cuts ?doubleVias t)
        else
            cuts = (if dblVias "_2CUT_H" "") 
            (if !dblVias && bufSegDir == "HOR" then  cuts = "_2CUT_V")
            dblVias = t
            DrawVia(wireName viasList (if reSeg car(last(reSeg)) startPoint)
                 wirePitch cuts ?doubleVias dblVias)
        )
    )
    ;debug stacked vias
    (if debugStackVias && layerList then
        (if cuts == "_2CUT_H" then X = 2*wirePitch Y = 0.0 else X = 0.0 Y = 2*wirePitch)
        (if turnSeg then
            reSeg = list(Ru_OffsetPoint(car(last(reSeg)) -X:-Y) Ru_OffsetPoint(car(last(reSeg)) X:Y))
        else reSeg = list(Ru_OffsetPoint(startPoint -X:-Y) Ru_OffsetPoint(startPoint X:Y)))
        (if debug >= 2 printf("VIAS R SEG %s seg %L layer %L\n" wireName reSeg layerList))
        (foreach layer layerList
            (DrawWire layer wireName wirePitch reSeg)
        )
    )
    list(startPoint toLayer segDist)
  )
)

(defun Ru_TerminateNode (nodeName wireName startBufInst endBufInst nodePath layer busPath
                         @key (firstPattern nil) (lastPattern nil)
                              (firstDoubleVias nil) (lastDoubleVias nil)
                              (skipFirstSeg nil) (skipLastSeg nil)
                              (segLException nil) (segRException nil)
                              (doubleVias t) (maxManDist 500.0)
                              (maxManDistFirstSeg nil) (maxManDistLastSeg nil)
                              (startViasLoc nil) (endViasLoc nil)
                              (reserveStartPinGap t) (reserveEndPinGap t)
                              (turnToStartPinLayer t) 
                              (turnToEndPinLayer t)
                              (flipSegPattern nil)
                              (backwardDir nil) (termPin t) (enableProp nil)
                              (debugStackVias nil) (debug 0))
 (prog (bufLoc flipX flipY nodeSeg bufSegX bufSegY bufSegNum startSegNum cuts maxWarnDist viasLoc
      result bufSegDir segNum leSegOff layerList X Y lsSeg leSeg dblVias turnSeg segDist 
      fromLayer toLayer viasList lsLayer leLayer turnLayer segExcep leSegOff beginExt pinExt pinExtLayer
      startPoint endPoint rStartLayer pathLen segDir startNode startLayer endNode endLayer segLayer noBuffer
      ll ur )
     
    (if !wireName || !stringp(wireName)  error("Ru_TerminateNode: Invalid wireName %L\n" wireName))
    (if !nodeName || !stringp(nodeName)  error("Ru_TerminateNode: Invalid nodeName %L\n" nodeName))
    (if !listp(nodePath) || length(nodePath) <= 1 error("Ru_TerminateNode: Invalid nodePath %L\n" nodePath))
    (if !listp(layer) || length(layer) <= 1 error("Ru_TerminateNode: Invalid layer %L\n" layer))

rexMagic(t)
    startPoint = nil
    endPoint = nil
    turnSeg = t ; need to turn to the buffer
    noBuffer = nil
    segDist = 0.0
    (if startBufInst && !dbIsId(startBufInst) 
        error("Ru_TerminateNode: %s Invalid startBufInst %L parameter.\n" wireName startBufInst))
    (if endBufInst && !dbIsId(endBufInst) 
        error("Ru_TerminateNode: %s Invalid endBufInst %L parameter.\n" wireName endBufInst))
    (if !endBufInst && !startBufInst then 
        ;error("Ru_TerminateNode: %s Both startBufInst %L endBufInst %L are nil.\n" wireName startBufInst endBufInst))
        noBuffer = t
    )
;debug = 3
    (if debug >= 3 printf("Ru_TerminateNode: node %L wire %L startInst %L endInst %L path %L layer %L\n"
       nodeName wireName startBufInst endBufInst nodePath layer))
    ;find location of the buffer along the nodePath
    (if !startBufInst then ;start path
        (if firstPattern then
            leSeg = list(car(nodePath) cadr(nodePath))
            bufSegDir = ru_getSegDir(car(nodePath) cadr(nodePath))
            startNode = Ru_GetChannelEntry(firstPattern nodeName)
            (if !startNode error("Unable to find %L in firstPattern %L\n" nodeName firstPattern))
            startLayer = nth(3 startNode)
            (if !startLayer error("%L in firstPattern must have BusMetal specified\n" startNode))
            (if bufSegDir == "VERT" then
                fromLayer = ru_layerToValue(nth(0 nth(0 layer)))
                segLayer = ru_layerToValue(nth(0 nth(1 layer)))
                toLayer = ru_layerToValue(nth(0 nth(1 startLayer)))
            else
                fromLayer = ru_layerToValue(nth(0 nth(1 layer)))
                segLayer = ru_layerToValue(nth(0 nth(0 layer)))
                toLayer = ru_layerToValue(nth(0 nth(0 startLayer)))
            )
            (if turnToStartPinLayer segLayer = (if fromLayer < toLayer fromLayer+1 fromLayer-1))
            viasList = GenViasList(fromLayer segLayer)
            (if debug >= 1 printf("L_START %s: Start segment layer %L leSeg %L startLayer %L fromLayer %L toLayer %L startSegLayer %L\n" 
                                   wireName layer leSeg startLayer fromLayer toLayer segLayer))

(if skipFirstSeg then
            (if debug >= 1 printf("L_START %s: Skipping first segment to pin\n"))

else
            (if firstDoubleVias then DrawVia(wireName viasList cadr(leSeg) wirePitch 
                                     (if bufSegDir == "HOR" "_2CUT_V" "_2CUT_H"))
            else DrawVia(wireName viasList cadr(leSeg) wirePitch "" ?doubleVias nil))
            leLayer = sprintf(nil "M%d" segLayer)
            segDist = segDist + Ru_PathDistance(leSeg)

            ;need to use buspath when there are pos and neg offsets
            segDir = ru_getSegDir(car(busPath) cadr(busPath) ?detail t)
            dist = Ru_PathDistance(leSeg) 
            dblVias = (if dist > wirePitch*5 firstDoubleVias nil)
            viasLoc = Ru_GetViasToPinLoc(wireName nth(0 nodePath) nth(1 nodePath) startViasLoc segDir)
pinViasOff = 5 
toPinLoc = nil
            (if reserveStartPinGap then
                (if (segLayer==toLayer) then
                    viasLoc = Ru_OffsetPoint(car(leSeg) Ru_GetDirLocOffset(segDir -TrackPitch))
                else
                    viasLoc = Ru_OffsetPoint(viasLoc Ru_GetDirLocOffset(segDir -wirePitch*pinViasOff))
                    toPinLoc =  Ru_OffsetPoint(car(leSeg) Ru_GetDirLocOffset(segDir -TrackPitch))
                )                
            else
                (if (segLayer==toLayer) then
                    ;if we don't need vias and don't have pins, then start from begining
                    viasLoc = car(leSeg)
                else
                    viasLoc = Ru_OffsetPoint(viasLoc Ru_GetDirLocOffset(segDir -wirePitch*pinViasOff))
                    toPinLoc =  car(leSeg)
                )   
            )
;printf("START: leSeg %L segDir %L viasLoc %L toPinLoc %L\n" leSeg segDir viasLoc toPinLoc)
            ;turn to vias
            (if Ru_GetDistance(viasLoc nth(1 leSeg)) > wirePitch/4 then
                Ru_ValidatePath(list(viasLoc nth(1 leSeg))  sprintf(nil "%s L_START_SEGVIAS" wireName))
                DrawSegment(wireName leLayer wirePitch viasLoc
                   nth(1 leSeg) (segLayer!=toLayer) t nil)
            )
            ;vias to pin if needed, Not needed when on the same track as the pin
            (if toPinLoc && (!reserveStartPinGap || Ru_GetDistance(viasLoc toPinLoc) > TrackPitch) then
                ;extend to the start of the pin when vias is not next to pin end
                Ru_ValidatePath(list(toPinLoc viasLoc)  sprintf(nil "%s L_START_VIAS_TO_PIN" wireName))
                DrawSegment(wireName sprintf(nil "M%d" toLayer) wirePitch
                  toPinLoc viasLoc nil (segLayer!=toLayer) nil)
            )
            (if segLayer != toLayer then ; need to switch layer in order to get up to pin
                viasList = GenViasList(segLayer toLayer)
;                (if dist <= wirePitch*5 then ;see above check
;                    printf("WARN: %s start turn too short %.1f to create vias to pin\n"  wireName dist)
;                    printf("       Move to a different layer to avoid vias to pin, or move\n")
;                    printf("       to a different track, or replace with e1of2 shifted by 4 wirePitch.\n")
;                )
                ;(if dblVias then DrawVia(wireName viasList viasLoc wirePitch 
                ;                         (if bufSegDir == "HOR" "_2CUT_H" "_2CUT_V") ?stacked t)
                ;else 
                Ru_DrawStackedVias(wireName viasList viasLoc wirePitch bufSegDir)
                (if debugStackVias then
                    (if segDir == "LEFT" || segDir == "RIGHT" then
                        leSeg = list(Ru_OffsetPoint(viasLoc -2*wirePitch:0) 
                                     Ru_OffsetPoint(viasLoc 2*wirePitch:0))
                     else
                        leSeg = list(Ru_OffsetPoint(viasLoc 0:-2*wirePitch) 
                                     Ru_OffsetPoint(viasLoc 0:2*wirePitch))
                     )
                    layerList = ru_GenLayerList(segLayer toLayer)
                    (if debug >= 2 printf("VIAS L_START SEG %s seg %L layer %L\n" wireName leSeg layerList))
                    (foreach layer layerList
                        DrawWire(layer wireName wirePitch leSeg)
                    )
                )

            )
) ;skipFirstSeg
            startPoint = cadr(nodePath)
            bufSegDir = ru_getSegDir(nth(1 nodePath) nth(2 nodePath))
            startSegNum = 1 
            beginExt = t

        else
            ;no firstpattern
            (if reserveStartPinGap then 
                segDir = ru_getSegDir(car(busPath) cadr(busPath) ?detail t)
                startPoint = Ru_OffsetPoint(car(nodePath) Ru_GetDirLocOffset(segDir -TrackPitch))
            else
                startPoint = car(nodePath)
            )
            result = ru_getPathSegment(startPoint nodePath)
            bufSegDir = ru_getSegDir(car(nodePath) cadr(nodePath))
            startSegNum = 0
            beginExt = nil
        )
    else
startSegNum = 0
        ;start location is a buffer
        bufSegNum = Ru_LocatePathSeg(nodePath startBufInst->xy startSegNum)
        bufLoc = startBufInst->xy 
        startPoint = nth(bufSegNum nodePath)
        (if !startPoint then
            (DrawWire BusMetal2 startBufInst->name wirePitch nodePath)
            error("Nil startPoint %d: %s is not located along the path %L\n"
                bufSegNum startBufInst->name nodePath)) 
        (if !nth(bufSegNum+1 nodePath) error("Nil point#%d: %s is not located along the path %L\n"
            bufSegNum+1 startBufInst->name nodePath))
        bufSegDir = ru_getSegDir(startPoint nth(bufSegNum+1 nodePath))
        (if bufSegDir == "HOR" then
           bufSegX = xCoord(bufLoc)
           bufSegY = yCoord(startPoint)
        else
           bufSegX = xCoord(startPoint)
           bufSegY = yCoord(bufLoc)
        )

        result = Ru_TerminateNodeR(nodeName wireName startBufInst layer bufSegDir bufSegX bufSegY
                          ?doubleVias doubleVias ?termPin termPin ?segRException segRException
                          ?bufSegNum bufSegNum ?flipSegPattern flipSegPattern
                          ?backwardDir backwardDir ?enableProp enableProp ?debug debug)
        startPoint = car(result)
        rStartLayer = cadr(result)
        segDist = segDist + nth(2 result)
        startSegNum = bufSegNum
        beginExt = t
    )

    ;EndPoint
    nodeSeg = list(startPoint)
    bufSegX = 0.0
    bufSegY = 0.0
    nodeOff = 0.0 ; path is already node path
    ;printf("########### Ru_TerminateNode: startSegNum %d startPoint %L nodePath %L \n" startSegNum startPoint nodePath)
    (if endBufInst then
        (if debug >= 4 printf("CellView: %L\n"  endBufInst->master->cellName))
        bufLoc = endBufInst->xy
        flipX = (endBufInst->orient == "MX" || endBufInst->orient == "R180")
        flipY = (endBufInst->orient == "MY" || endBufInst->orient == "R180")
        bufSegNum = Ru_LocatePathSeg(nodePath bufLoc (if startSegNum > 0 (startSegNum-1) 0))
        (for segNum startSegNum+1 bufSegNum
           nodeSeg = append(nodeSeg list(nth(segNum nodePath)))
        )
        startPoint = nth(bufSegNum nodePath) 
        (if !startPoint then
            Ru_DrawMarker(bufLoc)
            DrawWire(BusMetal2 endBufInst->name wirePitch nodePath)
            error("Nil endPoint #%d startSegNum %d: %s is not located along the path %L\n"
                bufSegNum startSegNum endBufInst->name nodePath)) 
        bufSegDir = ru_getSegDir(startPoint nth(bufSegNum+1 nodePath))
        (if bufSegDir == "HOR" then
           bufSegX = xCoord(bufLoc)
           bufSegY = yCoord(startPoint)
        else
           bufSegX = xCoord(startPoint)
           bufSegY = yCoord(bufLoc)
        )

        (if debug >=2 printf("%s startSegNum %L bufSegNum %d nodeSeg %L X %f Y %f \n"
                             wireName startSegNum bufSegNum nodeSeg bufSegX bufSegY))

        (if bufSegNum < 0 printf("ERROR: Ru_TerminateNode: Unable to match buffer(%s) location %L to nodePath %L\n" 
                                  endBufInst->cellName bufLoc nodePath))
;pinLOff
         (if backwardDir then
             pinLOff = Ru_getPinRLoc(endBufInst->master->cellName nodeName endBufInst->orient)
         else
             pinLOff = Ru_getPinLLoc(endBufInst->master->cellName nodeName endBufInst->orient)
         )
         pinLLoc = Ru_OffsetBox(pinLOff bufLoc)
         (if debug >= 1 printf("Ru_TerminateNode: %s bufSegDir %s backwardDir %L PINL off %L pinLLoc %L\n" 
                wireName bufSegDir backwardDir pinLOff pinLLoc))
         ;dbCreatePath((geGetEditCellView)  list("M4" "bus") nodeSeg 0.4)

         leSegOff = 0.0
         layerList = nil
         dblVias = t
         pinExt = nil
         pinExtLayer = 3
         (if bufSegDir == "HOR" then
            X = bufSegX + ru_getTurnOffset(bufLoc endBufInst->master->cellName nodeName (backwardDir==nil) flipY endBufInst)
            Y = bufSegY
            fromLayer = ru_layerToValue(nth(0 nth(0 layer)))
            toLayer = (if fromLayer == 3 4 (fromLayer-1))
            lsSeg = Ru_OffsetPoints(append(nodeSeg list(X:Y)) 0.0:nodeOff)
            dblVias = (if ru_WithinGrid(xCoord(car(last(lsSeg))) TrackPitch 4*TrackPitch/24) nil t)
            (if toLayer == 4 then
                leSeg = list( Ru_OffsetPoint(X:Y 0.0:nodeOff) X:Ru_MidY(pinLLoc))
            else
                startPoint = Ru_OffsetPoint(X:Y nodeOff:0.0)
pinSwitchOff = wirePitch*4
(if member(bufSegNum flipSegPattern) pinSwitchOff = -pinSwitchOff)
(if rexMatchp("1of1" endBufInst->master->cellName) then
  (if (GetProp endBufInst "Position" "")=="TOP" pinSwitchOff = (2*TrackPitch-pinSwitchOff))
else
  (if yCoord(startPoint) > (yCoord(bufLoc)+2*TrackPitch) then
     (if member(bufSegNum flipSegPattern) then
          pinSwitchOff = pinSwitchOff ;FIXME redundant with above member check
     else pinSwitchOff = (4*TrackPitch-pinSwitchOff))
  )
)
                leSeg = list( startPoint X:(yCoord(bufLoc)+pinSwitchOff))
;                             (if flipX -1 1)*(if yCoord(startPoint) > (yCoord(bufLoc)+2*TrackPitch) && 
;                             !rexMatchp("1of1" endBufInst->master->cellName) (4*TrackPitch-pinSwitchOff) pinSwitchOff)) )
                pinExt = list(car(last(leSeg))  X:Ru_MidY(pinLLoc))
                pinExtLayer = 4
                (if Ru_PathDistance(pinExt) < TrackPitch/24/4 pinExt = nil) ; already on top of pin
            )
            dist = Ru_GetDistance(nth(0 leSeg) nth(1 leSeg))
            (if dist < TrackPitch/24/4 then
                leSeg = nil ; no need to turn, on top on pin
                turnSeg = nil
            )
            ll = lowerLeft(endBufInst->bBox)
            ur = upperRight(endBufInst->bBox)
;(if fromLayer == 5 printf("wireName %s fromLayer %d toLayer %d %L %L %L\n" wireName fromLayer toLayer yCoord(ll) (Y+nodeOff) yCoord(ur)))
            (if fromLayer <=3 && Ru_IsBetween(yCoord(ll) (Y+nodeOff) yCoord(ur)) then
printf("@@@@@@@@@ ERROR: %s is using the buffer tracks with layer %d. Move it to another track or layer\n" wireName fromLayer)
            )
pinSwitchOff = wirePitch*4
            (if fromLayer == 5 && 
                (Ru_IsBetween(yCoord(ll) (Y+nodeOff) (yCoord(ll)+pinSwitchOff+3*wirePitch)) ||
                 (Ru_IsBetween(yCoord(ll)-(pinSwitchOff+3*wirePitch) (Y+nodeOff-4*TrackPitch) yCoord(ll))&&
                  !rexMatchp("1of1" endBufInst->master->cellName)) ) then
printf("@@@@@@@@@ ERROR: %s is using the reserved area for vias to turn down. Move it off a bit or use different track or layer, or use inlv_e1of2_X3 or X4.\n" wireName)
            )
;leSeg = ru_AdjustSegLength(leSeg TrackPitch/2)
            viasList = GenViasList(fromLayer toLayer)
            lsLayer = layer
            layerList = ru_GenLayerList(toLayer pinExtLayer) ; going to multiple layers
            viasList = GenViasList(toLayer 3)
            leLayer = sprintf(nil "M%d" toLayer)
            (if debug >= 3 printf("%s HOR SegStart %L %L SegEnd %L %L layerList %L\n"
                                  wireName lsSeg lsLayer leSeg leLayer layerList))
         else
            X = bufSegX
            fromLayer = ru_layerToValue(nth(0 nth(1 layer)))
            toLayer = (if fromLayer == 2 3 5)
            viasList = GenViasList(fromLayer toLayer)
            lsLayer = layer
            ;vertical wires on layer2 must be on the side of the buffer
            Y = Ru_MidY(pinLLoc)
            leSeg = nil
            segExcep = ru_GetSegException(segLException wireName)
            (if enableProp then
                (if !segExcep then
                    ;special case for SchedToIngress to prevent bus overlap
                     prop = (GetProp endBufInst "OffsetNodeMatch" "")
        ;printf("%s: OffsetNodeMatch %L by %L\n" wireName prop propValue->value)
                    (if prop && prop!="" && rexMatchp(prop wireName) then
                        propValue = (dbFindProp endBufInst "OffsetNodeValue")
                        segExcep = list(nil (if propValue->value propValue->value 0)*wirePitch)
        ;printf("%s: OffsetNode by %L\n" wireName propValue->value)
                        termPin = nil
                    )
                )
                prop = (GetProp endBufInst "SingleViasMatch" "")
                (if prop && prop!="" && rexMatchp(prop wireName) then
                    propValue = (dbFindProp endBufInst "SingleViasValue")
                    dblVias = (if propValue->value=="TRUE" nil t)
;printf("%s: L dblVias %L SingleVias  %L \n" wireName dblVias propValue->value)
                )
            )
;            (if !segExcep && !(GetProp endBufInst "LeftRightAlign" t) then
;                ;Offset by 1 TrackPitch to prevent overlapping when buffer is not aligned on correct side
;                segExcep = list(nil -TrackPitch)
;               termPin = nil
;            )
            (if segExcep  then
                leSegOff = nth(1 segExcep)
                (if debug >= 1 printf("%s L horizontal segment X %f Y %f is offset by %L\n" wireName X Y leSegOff))
                lsSeg = Ru_OffsetPoints(append(nodeSeg list(X:(Y+leSegOff))) nodeOff:0.0)
                leSeg = append(last(lsSeg) list(Ru_MidX(pinLLoc):(Y+leSegOff))) ;just close by
            else
                lsSeg = Ru_OffsetPoints(append(nodeSeg list(X:Y)) nodeOff:0.0)
                (if nil && Ru_IsBetween(xCoord(lowerLeft(pinLLoc)) (X+nodeOff) xCoord(upperRight(pinLLoc))) then
                    leSeg = nil ; no need to turn
                    turnSeg = nil
                else 
                    (if toLayer == 3 then
                        leSeg = list( Ru_OffsetPoint(X:Y nodeOff:0.0) Ru_MidX(pinLLoc):Y)
                    else 
pinSwitchOff = wirePitch*4
                        leSeg = list( Ru_OffsetPoint(X:Y nodeOff:0.0) (RoundDownTrack(xCoord(bufLoc))+(if flipY -pinSwitchOff pinSwitchOff)):Y)
                        pinExt = list(car(last(leSeg)) Ru_MidX(pinLLoc):Y)
                        (if Ru_PathDistance(pinExt) < TrackPitch/2 pinExt = nil) ; already on top of pin
                    )
                )
            )
            dblVias = (if ru_WithinGrid(yCoord(car(last(lsSeg))) TrackPitch 4*TrackPitch/24) nil dblVias)
            layerList = ru_GenLayerList(toLayer pinExtLayer) ; going to multiple layers
            viasList = GenViasList(toLayer 3)
            leLayer = sprintf(nil "M%d" toLayer)
            (if debug >= 3 printf("%s VERT SegStart %L %L SegEnd %L %L layerList %L\n"
                                  wireName lsSeg lsLayer leSeg leLayer layerList))
         )
        (if debug >= 2 printf("[%s pinLLoc %L bufLoc %L nodeSeg %L pattern %L layer: %L]\n" 
                              wireName pinLLoc bufLoc nodeSeg width layer))
        (if debug >= 1 printf("L_START %s: segment %L from pin using %L leSeg %L fromLayer %L toLayer %L pinExt %L doubleVias %L dblVias %L\n"
                              wireName lsSeg lsLayer leSeg fromLayer toLayer pinExt doubleVias dblVias))

        Ru_ValidatePath(lsSeg sprintf(nil "%s L_START" wireName))
        Ru_DrawWire(lsLayer wireName wirePitch lsSeg ?beginExt beginExt ?endExt t ?doubleVias doubleVias)
        segDist = segDist + Ru_PathDistance(lsSeg)

        turnLayer = GenViasList(fromLayer toLayer)
        (if dblVias then DrawVia(wireName turnLayer car(last(lsSeg)) wirePitch 
                                 (if bufSegDir == "HOR" "_2CUT_H" "_2CUT_V"))
        else DrawVia(wireName turnLayer car(last(lsSeg)) wirePitch "" ?doubleVias nil))

        (if debug  >= 1 printf("L_END segment %L to buffer going between layer %d to %d. leLayer %L turnLayer %L\n"
                               leSeg toLayer 3 leLayer turnLayer))
        (if leSeg && Ru_PathDistance(leSeg) > TrackPitch/64 then
            Ru_ValidatePath(leSeg sprintf(nil "%s L_END" wireName))
            segDist = segDist + Ru_PathDistance(leSeg)
            DrawSegment(wireName leLayer wirePitch nth(0 leSeg) nth(1 leSeg) t t nil)
        )

        (if termPin && pinExt then
            Ru_ValidatePath(pinExt sprintf(nil "%s L_PINEXT" wireName))
            segDist = segDist + Ru_PathDistance(pinExt)
            DrawSegment(wireName sprintf(nil "M%d" pinExtLayer) wirePitch nth(0 pinExt) nth(1 pinExt) t t nil)            
            (if pinExtLayer != 3 then ;HOR connection
               cuts = (if dblVias "_2CUT_H" "") 
               (if !dblVias then dblVias = t cuts = "_2CUT_V")
                DrawVia(wireName GenViasList(pinExtLayer 3) car(last(pinExt))
                        wirePitch cuts ?doubleVias dblVias)
            )
        )

        (if termPin && toLayer!=3 
            (if pinExtLayer != 3 then ;HOR connection
                (if toLayer != 6 error("L_END: %s Unexpected toLayer %L with pinExtLayer %L\n" wireName toLayer pinExtLayer))
                cuts = "_2CUT_V"
                DrawVia(wireName GenViasList(toLayer pinExtLayer) car(pinExt)
                        wirePitch "_2CUT_V" ?doubleVias t)
            else
               cuts = (if dblVias "_2CUT_H" "") 
               (if !dblVias && bufSegDir == "HOR" then  cuts = "_2CUT_V")
               dblVias = t
               DrawVia(wireName viasList (if leSeg car(last(leSeg)) car(last(lsSeg))) 
                    wirePitch cuts ?doubleVias dblVias)
            )
        )
        (if debugStackVias && layerList then
            (if cuts == "_2CUT_H" then X = 2*wirePitch Y = 0.0 else X = 0.0 Y = 2*wirePitch)
            (if turnSeg then
                leSeg = list(Ru_OffsetPoint(car(last(leSeg)) -X:-Y) 
                             Ru_OffsetPoint(car(last(leSeg)) X:Y))
            else leSeg = list(Ru_OffsetPoint(car(last(lsSeg)) -X:-Y)
                              Ru_OffsetPoint(car(last(lsSeg)) X:Y)))
            (if debug >= 2 printf("VIAS L SEG %s seg %L layer %L\n" wireName leSeg layerList))
            (foreach layer layerList
                DrawWire(layer wireName wirePitch leSeg)
            )
        )
    else
        ;to end path
        lsLayer = layer
        lsSeg = list(startPoint)
        pathLen = length(nodePath)
        (if lastPattern then
            (for segNum startSegNum+1 pathLen-2
                lsSeg = append(lsSeg list(nth(segNum nodePath)))
            )
            (if length(lsSeg) <= 1 then
              printf("@@@@@@@@@ ERROR: %s Must have a turn at the end when specifying lastPattern option\n" wireName)
            else
            leSeg = list(nth(pathLen-2 nodePath) nth(pathLen-1 nodePath))
            bufSegDir = ru_getSegDir(nth(pathLen-2 nodePath) nth(pathLen-1 nodePath))
            endNode = Ru_GetChannelEntry(lastPattern nodeName)
            (if !endNode error("Unable to find %L in lastPattern %L\n" nodeName lastPattern))
            endLayer = nth(3 endNode)
            (if !endLayer error("%L in lastPattern must have BusMetal specified\n" endLayer))
            (if bufSegDir == "VERT" then
                fromLayer = ru_layerToValue(nth(0 nth(0 layer)))
                segLayer = ru_layerToValue(nth(0 nth(1 layer)))
                toLayer = ru_layerToValue(nth(0 nth(1 endLayer)))
            else
                fromLayer = ru_layerToValue(nth(0 nth(1 layer)))
                segLayer = ru_layerToValue(nth(0 nth(0 layer)))
                toLayer = ru_layerToValue(nth(0 nth(0 endLayer)))
            )
            (if turnToEndPinLayer segLayer = (if fromLayer < toLayer fromLayer+1 fromLayer-1))
            viasList = GenViasList(fromLayer segLayer)
            (if debug >= 1 printf("R_END %s: End segment %d layer %L leSeg %L endLayer %L fromLayer %L toLayer %L endSegLayer %L\n" 
                                   wireName startSegNum layer leSeg endLayer fromLayer toLayer segLayer))

(if skipLastSeg then
            (if debug >= 1 printf("L_START %s: Skipping first segment to pin\n"))

else
            (if lastDoubleVias then DrawVia(wireName viasList car(leSeg) wirePitch 
                                     (if bufSegDir == "HOR" "_2CUT_V" "_2CUT_H"))
            else DrawVia(wireName viasList car(leSeg) wirePitch "" ?doubleVias nil))
            leLayer = sprintf(nil "M%d" segLayer)
            segDist = segDist + Ru_PathDistance(leSeg)

            ;need to use busPath when there are neg and pos offsets
            segDir = ru_getSegDir(nth(pathLen-2 busPath) nth(pathLen-1 busPath) ?detail t)
            dist = Ru_PathDistance(leSeg) 
            ;turn distance is too short to use double vias to pin
            dblVias = (if dist > wirePitch*5 lastDoubleVias nil)
            viasLoc = Ru_GetViasToPinLoc(wireName nth(pathLen-1 nodePath) nth(pathLen-2 nodePath) endViasLoc segDir)
pinViasOff = 5
toPinLoc = nil
            (if reserveEndPinGap then
                (if (segLayer==toLayer) then
                    ;only to the edge of pin location to prevent routing violation
                    viasLoc = Ru_OffsetPoint(cadr(leSeg) Ru_GetDirLocOffset(segDir TrackPitch))
                else
                    viasLoc = Ru_OffsetPoint(viasLoc Ru_GetDirLocOffset(segDir wirePitch*pinViasOff))
                    toPinLoc =  Ru_OffsetPoint(cadr(leSeg) Ru_GetDirLocOffset(segDir TrackPitch))
                )                
            else
                (if (segLayer==toLayer) then
                    ;if we don't need vias and don't have pins, then start from begining
                    viasLoc = cadr(leSeg)
                else
                    viasLoc = Ru_OffsetPoint(viasLoc Ru_GetDirLocOffset(segDir wirePitch*pinViasOff))
                    toPinLoc =  cadr(leSeg)
                )   
            )
;printf("END: leSeg %L viasLoc %L toPinLoc %L\n" leSeg viasLoc toPinLoc)
            ;only to the vias location to prevent routing violation
            (if Ru_GetDistance(nth(0 leSeg) viasLoc) > wirePitch/4 then
                Ru_ValidatePath(list(nth(0 leSeg) viasLoc)  sprintf(nil "%s L_END_SEGVIAS" wireName))
                DrawSegment(wireName leLayer wirePitch nth(0 leSeg)
                    viasLoc t (segLayer!=toLayer) nil)
            )

            ;Not needed when on the same track as the pin
            (if toPinLoc && (!reserveEndPinGap || Ru_GetDistance(viasLoc toPinLoc) > TrackPitch) then
                Ru_ValidatePath(list(viasLoc toPinLoc)  sprintf(nil "%s L_END_VIA_TO_PIN" wireName))
                ;extend to the end of the pin when vias is not next to pin
                DrawSegment(wireName sprintf(nil "M%d" toLayer) wirePitch viasLoc 
                    toPinLoc (segLayer!=toLayer) nil nil)
            )
            (if segLayer != toLayer then ;need to switch layer in order to get up to pin
                viasList = GenViasList(segLayer toLayer)
                ;(if dist <= wirePitch*5 then ;see above test
                ;    printf("WARN: %s end turn too short %.1f to create vias to pin.\n"  wireName dist)
                ;    printf("       Move to a different layer to avoid vias to pin, or move\n")
                ;    printf("       to a different track, or replace with e1of2 shifted by 4 wirePitch.\n")
                ;)

                ;(if dblVias then DrawVia(wireName viasList viasLoc wirePitch 
                ;                         (if bufSegDir == "HOR" "_2CUT_H" "_2CUT_V") ?stacked t)
                ;else
                ;will cause drc error if using double vias 
                Ru_DrawStackedVias(wireName viasList viasLoc wirePitch bufSegDir)

                (if debugStackVias then
                    (if bufSegDir == "HOR" then
                        leSeg = list(Ru_OffsetPoint(viasLoc -2*wirePitch:0) 
                                     Ru_OffsetPoint(viasLoc 2*wirePitch:0))
                     else
                        leSeg = list(Ru_OffsetPoint(viasLoc 0:-2*wirePitch) 
                                     Ru_OffsetPoint(viasLoc 0:2*wirePitch))
                     )
                    layerList = ru_GenLayerList(segLayer toLayer)
                    (if debug >= 2 printf("VIAS R_END SEG %s seg %L layer %L\n" wireName leSeg layerList))
                    (foreach layer layerList
                        DrawWire(layer wireName wirePitch leSeg)
                    )
                )
            )
            ) ;if length(lsSeg) <= 1 
) ;skipLastSeg

        else
            ;no lastPattern
            (for segNum startSegNum+1 pathLen-1
                (if segNum == pathLen-1 && reserveEndPinGap then
                    segDir = ru_getSegDir(nth(pathLen-2 busPath) nth(pathLen-1 busPath) ?detail t)
                    lsSeg = append(lsSeg list(Ru_OffsetPoint(nth(segNum nodePath) Ru_GetDirLocOffset(segDir TrackPitch))))
                else
                    lsSeg = append(lsSeg list(nth(segNum nodePath)))
                )
            )
        )
        (if length(lsSeg) > 1 /*&& Ru_PathDistance(lsSeg) > TrackPitch/24*/ then ;valid segment
            (if debug >= 1 printf("R_END %s: End segment %d rStartLayer %L layer %L lsSeg %L\n" wireName startSegNum rStartLayer layer lsSeg))
            Ru_ValidatePath(lsSeg sprintf(nil "%s R_END" wireName))
            segDist = segDist + Ru_PathDistance(lsSeg)
            Ru_DrawWire(lsLayer wireName wirePitch lsSeg ?beginExt t ?endExt nil ?doubleVias doubleVias)
        )
    )

    (cond 
        (startBufInst == nil result = "first " maxWarnDist = (if maxManDistFirstSeg maxManDistFirstSeg maxManDist*0.80))
        (endBufInst == nil result = "last " maxWarnDist = (if maxManDistLastSeg maxManDistLastSeg maxManDist*0.80))
        (t  result = "" maxWarnDist = maxManDist)
    )
    (if segDist > maxWarnDist printf("WARN: %s Total %ssegment distance %.2f is over limit %.2f\n"
        wireName result segDist maxWarnDist))
    return(segDist)  
  )

)

(defun Ru_GetDirLocOffset (segDir offset)
  (let (locOff)
    (cond
      (segDir == "UP"
          locOff = 0:-offset)
      (segDir == "DOWN" 
          locOff = 0:offset)
      (segDir == "LEFT" 
          locOff = offset:0)
      (segDir == "RIGHT"
          locOff = -offset:0)
      (t error("Ru_GetDirOffset: Unknown %L\n" segDir))
    )
  locOff
  )
)

;Manually tie path to ground, in case BundledRoute not doing it and don't want to use NanoRoute
;vertDir direction of the pattern
(defun TieGNDToGrid (point pattern vertDir @key (doubleVias t))
  (let (el nodeName wirePitch nodeOff layer viasLoc gridLoc cuts)
    (if vertDir then
      gridLoc = RoundDownTrack(xCoord(point) ?numTrack 2):yCoord(point)
    else
      gridLoc = xCoord(point):RoundDownTrack(yCoord(point) ?numTrack 2)
    )
    cuts = strcat((if doubleVias "_2CUT" "") (if vertDir "_H" "_V")) 
    (foreach el pattern
      nodeName = nth(0 el)
      (if nodeName == "GND" then
          wirePitch = nth(1 el)
          nodeOff = nth(2 el)
          layer = nth(2 nth(3 el))
          (if vertDir && layer == "M3_M2" then layer = "M4_M3")
          viasLoc = OffsetPoint(gridLoc (if vertDir 0 nodeOff) (if vertDir nodeOff 0))
          ;use stacked vias to avoid DRC
          DrawVia("GND" layer viasLoc wirePitch cuts ?doubleVias doubleVias)
      )
    )
  )
  t
)

(defun CreateShield (path pattern)
  (let (el nodeName wirePitch nodeOff layer nodePath pA pB segDir gndLayer cuts viasLoc)
    (foreach el pattern
      nodeName = nth(0 el)
      (if nodeName == "GND" then
          wirePitch = nth(1 el)
          nodeOff = nth(2 el)
          layer = nth(3 el)
          ;FIXME
          (if length(path) == 2 then
             (if ru_getSegDir(car(path) cadr(path)) == "HOR" then
                  nodePath = OffsetPoints(path 0 nodeOff)
             else
                  nodePath = OffsetPoints(path nodeOff 0)
             )
          else
              nodePath = OffsetPoints(path nodeOff nodeOff)
          )
          (DrawWire layer nodeName wirePitch nodePath ?doubleVias nil)
          ;draw the vias in the middle of each segment
          (for cnt 0 length(nodePath)-2
            pA = nth(cnt nodePath)
            pB = nth(cnt+1 nodePath)
            (if Ru_GetDistance(pA pB) > 8*TrackPitch then
                segDir = ru_getSegDir(pA pB)
                gndLayer = nth(2 layer)
                (if segDir == "HOR" then
                  viasLoc = RoundDownTrack((xCoord(pA)+xCoord(pB))/2 ?numTrack 2):yCoord(pA)
                  (if gndLayer == "M3_M2" then gndLayer = "M4_M3")
                  cuts = "_2CUT_H"
                else
                  viasLoc = xCoord(pA):RoundDownTrack((yCoord(pA)+yCoord(pB))/2 ?numTrack 2)
                  cuts = "_2CUT_V"
                )
                DrawVia("GND" gndLayer viasLoc wirePitch cuts ?doubleVias t)
            )
          )
        )
     )
  )
)


;single stacked vias
(defun Ru_DrawStackedVias (wireName viasList viasLoc wirePitch segDir)
  (let (vias)
    (foreach vias viasList
        DrawVia(wireName vias viasLoc wirePitch (if bufSegDir == "HOR" "_2CUT_H" "_2CUT_V") ?doubleVias nil ?stacked t)
    )
  )
)

(defun ToTrack (points)
  (prog (newPath point)
    (if !listp(points) return(points/TrackPitch))
    (if !listp(car(points)) return(xCoord(points)/TrackPitch:yCoord(points)/TrackPitch))
    (foreach point points
        newPath = append(newPath list(xCoord(point)/TrackPitch:yCoord(point)/TrackPitch))
    )
    return(newPath)
  )
)

(defun Ru_GetViasToPinLoc (wireName pinLoc turnLoc viasLocCfg segDir)
  (prog (viasLoc loc locSel vert pinVal turnVal defaultLoc)
    (if !viasLocCfg return(pinLoc))
    (if !listp(viasLocCfg) viasLocCfg = list(viasLocCfg))
    locSel = 99999.0
    vert = (segDir=="UP" || segDir=="DOWN")
    ;printf("pinLoc %L turnLoc %L viasLocCfg %L\n" pinLoc turnLoc viasLocCfg)
    (foreach loc viasLocCfg
        (if listp(loc) then
           (if ru_IsNameMatched(car(loc) wireName ?exactMatch t) then
               (if vert return(xCoord(pinLoc):cadr(loc))
               return(cadr(loc):yCoord(pinLoc)))
           )
        else
            (if !defaultLoc defaultLoc = loc)
            (if vert then
                pinVal = yCoord(pinLoc)
                turnVal = yCoord(turnLoc)
            else
                pinVal = xCoord(pinLoc)
                turnVal = xCoord(turnLoc)
            )
            ;printf("pinVal %L loc %L turnVal %L between %L\n" pinVal loc turnVal  Ru_IsBetween(pinVal loc turnVal))
            (if Ru_IsBetween(pinVal loc turnVal) &&
                abs(loc-turnVal) < abs(locSel-turnVal) then
                ;printf("New loc %f old %f\n" loc locSel)
                locSel = loc
            )
        )
    )
    (if locSel==99999.0 locSel = defaultLoc)
    (if vert return(xCoord(pinLoc):locSel)
    return(locSel:yCoord(pinLoc)))
  )
)

(defun RoundDownTrack (value @key (numTrack 1))
    (floor((value+TrackPitch/64)/(numTrack*TrackPitch))*(numTrack*TrackPitch))
)

(defun RoundUpTrack (value)
    (ceiling((value-TrackPitch/64)/TrackPitch)*TrackPitch)
)

(defun busMetalToStr (busMetal)
  (let (busMetalStr)
    layerStr = nth(0 nth(0 busMetal))
    (cond
      (layerStr=="M7" busMetalStr = "BusMetal67")
      (layerStr=="M5" busMetalStr = "BusMetal45")
      (t busMetalStr = "BusMetal23")
    )
  busMetalStr
  )
)

;busPattern is multiple layer pattern
;toPattern is a single layer pattern
;offset is the offset from busPattern to toPattern
(defun GenerateSwitchPattern (busPattern toPattern offset dirNeg @key (debug 0))
  (let (outsideList insideList bp el fromOffset toOffset nodeOff 
        maxOff minOff layer pat len sortOffset trackOffi layerStr)
    maxOff = -9999.0
    minOff = 9999.0
    (foreach bp busPattern
      (if minOff > nth(2 bp) minOff = nth(2 bp))
      (if maxOff < nth(2 bp) maxOff = nth(2 bp))
    )
    minOff = floor(ToTrack(minOff+TrackPitch/2))
    maxOff = floor(ToTrack(maxOff+TrackPitch/2))

    declare( m3InUsed[maxOff+1])
    declare( m5InUsed[maxOff+1])
    declare( m7InUsed[maxOff+1])

    (for cnt minOff maxOff
      m3InUsed[cnt] = 0
      m5InUsed[cnt] = 0
      m7InUsed[cnt] = 0
    )
    (foreach bp busPattern
      nodeName = nth(0 bp)
      len = strlen(nodeName)
      (if substring(nodeName len len) == "e" then ;baseNode
        result = Ru_SplitNodeName(nodeName)
        nodeBaseName = car(result)
        nodeElName = cadr(result)
        pat = nth(1 bp)
        nodeOff = nth(2 bp)
        fromOffset = floor(ToTrack(nodeOff+TrackPitch/2))
        layer = nth(3 bp)
        layerStr = nth(0 nth(0 layer))
        ;printf("fromOFffset %L %L\n" fromOffset bp)
        (cond
          (layerStr == "M6" || layerStr == "M7" m7InUsed[fromOffset] = m7InUsed[fromOffset] + 1 )
          (layerStr == "M4" || layerStr == "M5" m5InUsed[fromOffset] = m5InUsed[fromOffset] + 1  )
          (layerStr == "M2" || layerStr == "M3" m3InUsed[fromOffset] = m3InUsed[fromOffset] + 1  )
          (t error("Unknown %L\n" layerStr))
        )

        (foreach el toPattern
          (if nodeName == nth(0 el) then
            toOffset = floor(ToTrack(nth(2 el) + TrackPitch/2 + offset))
            sortOffset = - fromOffset
            (cond 
                (toOffset < minOff || toOffset > maxOff outsideList = append(outsideList list(list(sortOffset nodeBaseName layer fromOffset toOffset))))
                (t insideList = append(insideList list(list(sortOffset nodeBaseName layer fromOffset toOffset))))
            )
          )
        )
      )
    )

    (foreach el outsideList
        layerStr = nth(0 nth(0 nth(2 el)))
        fromOffset = nth(3 el)
        ;printf("%L %L\n" el fromOffset)
        (cond
          (layerStr == "M6" || layerStr == "M7" m7InUsed[fromOffset] = m7InUsed[fromOffset] - 1)
          (layerStr == "M4" || layerStr == "M5" m5InUsed[fromOffset] = m5InUsed[fromOffset] - 1)
          (layerStr == "M2" || layerStr == "M3" m3InUsed[fromOffset] = m3InUsed[fromOffset] - 1)
          (t error("Unknown %L\n" el))
        )
    )

    declare( trackOff[maxOff+1])
    (if dirNeg then
        startOff = maxOff
        endOff = minOff
    else
        startOff = minOff
        endOff = maxOff
    )
    declare( switchTrackInUse[maxOff+1])
    (for cnt minOff maxOff
        switchTrackInUse[cnt] = nil
    )
    (for cnt minOff maxOff
      trackOff[cnt] = startOff
    )
    showPat = t
    c = nil
    (foreach el outsideList
        ;printf("EL: %L LAYER: %L \n" el  nth(1 nth(2 el)))
        layer = nth(0 nth(1 nth(2 el)))
        result = Ru_GetFreeTrack(nth(1 el) switchTrackInUse layer startOff endOff)
        trackIdx = car(result)
        inlv = cadr(result)
        busMetal = nth(2 result)
        fromOffset = nth(3 el)
        (if !dirNeg trackOff[fromOffset] < trackIdx trackOff[fromOffset] = trackIdx)
        (if dirNeg trackOff[fromOffset] > trackIdx trackOff[fromOffset] = trackIdx)
        (if debug >= 1 printf("TRACK: %L %L result %L EL: %L\n" trackIdx inlv result el ))
        (if showPat printf("       (list \"%s\"                      inlv_e1of4%s  %d*TrackPitch %s)\n" nth(1 el) inlv trackIdx busMetalToStr(busMetal)))
        c = (cons (list nth(1 el) (if inlv=="_A" inlv_e1of4_A inlv_e1of4_B) trackIdx*TrackPitch busMetal) c)
    )
    (if debug >= 2 then
        (for cnt minOff maxOff
          printf("TRACK: %L offset %L\n" cnt trackOff[cnt])
        )
        (for cnt minOff maxOff
          printf("INUSE: %L layer %L\n" cnt switchTrackInUse[cnt])
        )
    )

    ;figuring trackOffset for inside tracks that the opposide end is done
    trackFree = t ; at least run once
    runCnt = 0
    newTracksAdded = 1
  (while newTracksAdded!=0
    trackFree = nil
    newTracksAdded = 0
    runCnt++
    remainingList = reverse(insideList);make a copy
    (for cnt minOff maxOff
      ;printf("TRACK#%d: %L %L %L\n" cnt m3InUsed[cnt] m5InUsed[cnt] m7InUsed[cnt])
      (if m7InUsed[cnt]==0 && m5InUsed[cnt]==0 && m3InUsed[cnt]==0 then
          trackFree = append(trackFree list(cnt))

          (foreach el insideList
            ;printf("cnt %d el %L\n" cnt el)
            (if cnt == nth(4 el) then
            newTracksAdded++
            toOffset = nth(4 el) 
            (if debug >= 1 printf("Adding toOffset %L trackOff %L  %L\n" toOffset trackOff[toOffset] el))
            layerStr = nth(0 nth(1 nth(2 el)))
            result = Ru_GetFreeTrack(nth(1 el) switchTrackInUse layerStr trackOff[toOffset] endOff)
            trackIdx = car(result)
            inlv = cadr(result)
            busMetal = nth(2 result)
            fromOffset = nth(3 el)
            (if !dirNeg trackOff[fromOffset] < trackIdx trackOff[fromOffset] = trackIdx)
            (if dirNeg trackOff[fromOffset] > trackIdx trackOff[fromOffset] = trackIdx)
            (if debug >= 1 printf("TRACKIN: %L %L result %L EL: %L\n" trackIdx inlv result el ))
            remainingList = Ru_RemoveFromList(el remainingList)
            (cond
              (layerStr == "M6" || layerStr == "M7" m7InUsed[fromOffset] = m7InUsed[fromOffset] - 1)
              (layerStr == "M4" || layerStr == "M5" m5InUsed[fromOffset] = m5InUsed[fromOffset] - 1)
              (layerStr == "M2" || layerStr == "M3" m3InUsed[fromOffset] = m3InUsed[fromOffset] - 1)
              (t error("Unknown %L\n" el))
            )

            (if showPat printf("       (list \"%s\"                      inlv_e1of4%s  %d*TrackPitch %s)\n" nth(1 el) inlv trackIdx busMetalToStr(busMetal)))
            c = (cons (list nth(1 el) (if inlv=="_A" inlv_e1of4_A inlv_e1of4_B) trackIdx*TrackPitch busMetal) c)
            )
          )
      )
    )

    (if debug >= 2 then
        printf("##################### RUN %L Added %L ####################\n" runCnt newTracksAdded)
        printf("TRACKFREE: %L len %L %L\n" trackFree length(remainingList) length(insideList) )
        (for cnt minOff maxOff
          printf("TRACK: %L offset %L\n" cnt trackOff[cnt])
        )
        (for cnt minOff maxOff
          printf("INUSE: %L layer %L\n" cnt switchTrackInUse[cnt])
        )
    )
    insideList = remainingList
  ) ;while

    (if debug >= 1 printf("##################### SECOND RUN\n"))
    trackFree = nil
    insideList = remainingList
    remainingList = reverse(insideList);make a copy
    (for cnt minOff maxOff
        ;printf("TRACK#%d: %L %L %L\n" cnt m3InUsed[cnt] m5InUsed[cnt] m7InUsed[cnt])
        trackFree = append(trackFree list(cnt))
        (foreach el insideList
            ;printf("cnt %d el %L\n" cnt el)
            (if cnt == nth(4 el) then
            toOffset = nth(4 el) 
            (if debug >= 1 printf("Adding toOffset %L trackOff %L endOff %L EL: %L\n" toOffset trackOff[toOffset] endOff el))
              layerStr = nth(0 nth(1 nth(2 el)))
              result = Ru_GetFreeTrack(nth(1 el) switchTrackInUse layerStr trackOff[toOffset] endOff)
              trackIdx = car(result)
              inlv = cadr(result)
              busMetal = nth(2 result)
            fromOffset = nth(3 el)
            (if !dirNeg trackOff[fromOffset] < trackIdx trackOff[fromOffset] = trackIdx)
            (if dirNeg trackOff[fromOffset] > trackIdx trackOff[fromOffset] = trackIdx)
            (if debug >= 1 printf("TRACKIN: %L %L result %L EL: %L\n" trackIdx inlv result el ))
                      ;outsideList = append(list(el) outsideList)
            remainingList = Ru_RemoveFromList(el remainingList)
            (cond
              (layerStr == "M6" || layerStr == "M7" m7InUsed[fromOffset] = m7InUsed[fromOffset] - 1)
              (layerStr == "M4" || layerStr == "M5" m5InUsed[fromOffset] = m5InUsed[fromOffset] - 1)
              (layerStr == "M2" || layerStr == "M3" m3InUsed[fromOffset] = m3InUsed[fromOffset] - 1)
              (t error("Unknown %L\n" el))
            )

            (if showPat printf("       (list \"%s\"                      inlv_e1of4%s  %d*TrackPitch %s)\n" nth(1 el) inlv trackIdx busMetalToStr(busMetal)))
            c = (cons (list nth(1 el) (if inlv=="_A" inlv_e1of4_A inlv_e1of4_B) trackIdx*TrackPitch busMetal) c)
            )
        )
    )
    (if debug >= 2 then
        printf("TRACKFREE: %L len %L %L\n" trackFree length(remainingList) length(insideList) )
        (for cnt minOff maxOff
          printf("TRACK: %L offset %L\n" cnt trackOff[cnt])
        )
        (for cnt minOff maxOff
          printf("INUSE: %L layer %L\n" cnt switchTrackInUse[cnt])
        )
    )
  )
  (DefChan c)
)

(defun Ru_RemoveFromList (entry elList)
  (let (el newList)
    (foreach el elList
        (if entry != el newList = append(newList list(el)))
    )
  newList
  )
)

(defun Ru_GetFreeTrack (nodeName switchTrackInUse layer startOff endOff)
  (prog (cnt freeTrack freePat)
    (if startOff < endOff then endCnt = (endOff+1) incrCnt = 1 cnt = startOff
    else endCnt = (endOff-1) incrCnt = -1 cnt = startOff)
;printf("############### layer %L %L startOff %L %L %L %L %L\n" layer (layer == "M6") startOff endOff endCnt incrCnt cnt)
    (while cnt != endCnt
(cond
  (layer == "M6" 
;printf("####INUSE: %L %L member %L %L\n" cnt switchTrackInUse[cnt] !member("M6_A" switchTrackInUse[cnt]) !member("M6_B" switchTrackInUse[cnt]))
    (if !member("M6_A" switchTrackInUse[cnt]) then
        switchTrackInUse[cnt] = append(switchTrackInUse[cnt] list("M6_A"))
;printf("FOUND: %L %L\n" cnt switchTrackInUse[cnt])
        return(list(cnt "_A" BusMetal67))
    )
    (if !member("M6_B" switchTrackInUse[cnt]) then
        switchTrackInUse[cnt] = append(switchTrackInUse[cnt] list("M6_B"))
;printf("FOUND: %L %L\n" cnt switchTrackInUse[cnt])
        return(list(cnt "_B" BusMetal67))
    )
  )
  (layer == "M4" 
    (if !member("M4_A" switchTrackInUse[cnt]) then
        switchTrackInUse[cnt] = append(switchTrackInUse[cnt] list("M4_A"))
        return(list(cnt "_A" BusMetal45))
    )
    (if !member("M4_B" switchTrackInUse[cnt]) then
        switchTrackInUse[cnt] = append(switchTrackInUse[cnt] list("M4_B"))
        return(list(cnt "_B" BusMetal45))
    )
  )
  (layer == "M2" 
    (if !member("M2_A" switchTrackInUse[cnt]) then
        switchTrackInUse[cnt] = append(switchTrackInUse[cnt] list("M2_A"))
        return(list(cnt "_A" BusMetal23))
    )
    (if !member("M2_B" switchTrackInUse[cnt]) then
        switchTrackInUse[cnt] = append(switchTrackInUse[cnt] list("M2_B"))
        return(list(cnt "_B" BusMetal23))
    )
  ) 
  (t error("Ru_GetFreeTrack: Unsupported layer %L\n" layer))
)
        
        cnt = cnt + incrCnt
    )
cnt = endOff
switchTrackInUse[cnt] = append(switchTrackInUse[cnt] list("M6_A_X"))
printf("#### %L: NO MORE FREE TRACKS\n" nodeName)
    return(list(cnt "_A" BusMetal67))
  )
)





(defun DrawPathPrBound (path layerPatternList @key
                        (firstPattern nil) (lastPattern nil) 
                        (firstResetPattern nil) (lastResetPattern nil)
                        (deletePrBound t)
                        (debug 0) )
  (let (tempPath lowerBound upperBound point busMinOff busMaxOff poly
        el firstMaxOff firstMinOff lastMaxOff lastMinOff limit)

    (if deletePrBound then
        (foreach shape (geGetEditCellView)->shapes
            (if shape~>layerName=="prBoundary" (dbDeleteObject shape))
        )
    )

    busMaxOff = -9999.99
    busMinOff = 9999.99
    (foreach el layerPatternList
        (if busMaxOff < nth(2 el) busMaxOff = nth(2 el))
        (if busMinOff > nth(2 el) busMinOff = nth(2 el))
    )

    (if firstPattern then
        firstMaxOff = -9999.99
        firstMinOff = 9999.99
        (foreach el firstPattern
            (if firstMaxOff < nth(2 el) firstMaxOff = nth(2 el))
            (if firstMinOff > nth(2 el) firstMinOff = nth(2 el))
        )
    )
    (if firstResetPattern then
        (foreach el firstResetPattern
            (if firstMaxOff < nth(2 el) firstMaxOff = nth(2 el))
            (if firstMinOff > nth(2 el) firstMinOff = nth(2 el))
        )
    )
    (if lastPattern then
        lastMaxOff = -9999.99
        lastMinOff = 9999.99
        (foreach el lastPattern
            (if lastMaxOff < nth(2 el) lastMaxOff = nth(2 el))
            (if lastMinOff > nth(2 el) lastMinOff = nth(2 el))
        )
    )
    (if lastResetPattern then
        (foreach el lastResetPattern
            (if lastMaxOff < nth(2 el) lastMaxOff = nth(2 el))
            (if lastMinOff > nth(2 el) lastMinOff = nth(2 el))
        )
    )
    tempPath = OffsetPath(path busMinOff busMinOff ?firstOffset firstMinOff ?lastOffset lastMinOff)
;dbCreatePath((geGetEditCellView)  list("M2" "bus") tempPath 0.8)
    limit = TrackPitch/24/4
    (foreach point tempPath
        lowerBound = append(lowerBound  list(RoundDownTrack(xCoord(point)):RoundDownTrack(yCoord(point))))
    )
    tempPath = OffsetPath(path busMaxOff busMaxOff ?firstOffset firstMaxOff ?lastOffset lastMaxOff)
;dbCreatePath((geGetEditCellView)  list("M2" "bus") tempPath 0.8)
    (foreach point tempPath
        upperBound = append(upperBound list(RoundUpTrack(xCoord(point)):RoundUpTrack(yCoord(point))))
    )

    result = Ru_FixPath(list(lowerBound upperBound) ?debug debug)
    poly = append(nth(0 result) reverse(nth(1 result)))
    dbCreatePolygon((geGetEditCellView) list("prBoundary" "drawing") poly)
    poly
  )
)

(defun Ru_FixPath (pathList @key (debug 0))
  (let (lowerBound upperBound 
        el segDir lastSegDir nextSegDir pathLen segLow segHi x1 x2 y1 y2 
        p1 p2 segLowDir segHiDir swapX swapY temp)

    lastSegDir == ""
    temp = 0.0
    lowerBound = nth(0 pathList)
    upperBound = nth(1 pathList)
    pathLen = length(lowerBound)
    p1 = list(car(lowerBound))
    p2 = list(car(upperBound))
   
    (for cnt 0 pathLen-2
        segLow = list(nth(cnt lowerBound) nth(cnt+1 lowerBound))
        segHi = list(nth(cnt upperBound) nth(cnt+1 upperBound))
        (if Ru_PathDistance(segLow) > TrackPitch/24 then
            segDir = ru_getSegDir(car(segLow) cadr(segLow) ?detail t)
        else 
            segDir = ru_getSegDir(car(segHi) cadr(segHi) ?detail t)
        )
        (if cnt < pathLen-2 then
            (if Ru_PathDistance(list(nth(cnt+1 lowerBound) nth(cnt+2 lowerBound))) > TrackPitch/24 then
                nextSegDir = ru_getSegDir(nth(cnt+1 lowerBound) nth(cnt+2 lowerBound) ?detail t)
            else 
                nextSegDir = ru_getSegDir(nth(cnt+1 upperBound) nth(cnt+2 upperBound) ?detail t)
            )
        else nextSegDir = "END"
        )
        (if debug >= 1 printf("lastSegDir %L segDir %L nextSegDir %L\n" lastSegDir segDir nextSegDir))

        swapX = nil
        swapY = nil
        (if (segDir == "UP" || segDir == "DOWN") then
            y1 = yCoord(car(last(p1)))
            y2 = yCoord(car(last(p2)))
            x1 =  xCoord(car(segLow))
            x2 = xCoord(car(segHi))
            (if debug >= 1 then
                printf("SwapX p2 seg %d %L %f %f %f %L %f %f %f\n" cnt 
                        Ru_IsBetween(yCoord(car(last(p1))) yCoord(car(last(p2))) yCoord(cadr(segLow)))
                        yCoord(car(last(p1))) yCoord(car(last(p2))) yCoord(cadr(segLow))
                        Ru_IsBetween(xCoord(cadr(segHi)) x1 xCoord(car(last(p2)))) 
                        xCoord(cadr(segHi)) x1 xCoord(car(last(p2))) )
                printf("SwapX p1 seg %d %L %f %f %f %L %f %f %f\n" cnt 
                        Ru_IsBetween(yCoord(car(last(p2))) yCoord(car(last(p1))) yCoord(cadr(segHi)))
                        yCoord(car(last(p2))) yCoord(car(last(p1))) yCoord(cadr(segHi))
                        Ru_IsBetween(xCoord(cadr(segLow)) x2 xCoord(car(last(p1)))) 
                        xCoord(cadr(segLow)) x2 xCoord(car(last(p1))) )
            )
            (if (Ru_IsBetween(yCoord(car(last(p1))) yCoord(car(last(p2))) yCoord(cadr(segLow))) &&
                 Ru_IsBetween(xCoord(cadr(segHi)) x1 xCoord(car(last(p2)))) ) ||
                (Ru_IsBetween(yCoord(car(last(p2))) yCoord(car(last(p1))) yCoord(cadr(segHi))) &&
                 Ru_IsBetween(xCoord(cadr(segLow)) x2 xCoord(car(last(p1)))) ) then
                swapX = t
                (if debug >= 1 printf("#### SwapX %d\n" cnt))
            )
        )
        (if (segDir == "LEFT" || segDir == "RIGHT") then
            x1 = xCoord(car(last(p1)))
            x2 = xCoord(car(last(p2)))
            y1 = yCoord(car(segLow))
            y2 = yCoord(car(segHi))
            (if debug >= 1 then
                printf("SwapY p2 seg %d %L %f %f %f %L %f %f %f\n" cnt 
                        Ru_IsBetween(xCoord(car(last(p1))) xCoord(car(last(p2))) xCoord(cadr(segLow)))
                        xCoord(car(last(p1))) xCoord(car(last(p2))) xCoord(cadr(segLow))
                        Ru_IsBetween(yCoord(cadr(segHi)) y1 yCoord(car(last(p2)))) 
                        yCoord(cadr(segHi)) y1 yCoord(car(last(p2))) )
                printf("SwapY p1 seg %d %L %f %f %f %L %f %f %f\n" cnt 
                        Ru_IsBetween(xCoord(car(last(p2))) xCoord(car(last(p1))) xCoord(cadr(segHi)))
                        xCoord(car(last(p2))) xCoord(car(last(p1))) xCoord(cadr(segHi))
                        Ru_IsBetween(yCoord(cadr(segLow)) y2 yCoord(car(last(p1)))) 
                        yCoord(cadr(segLow)) y2 yCoord(car(last(p1))) )
            )
            (if (Ru_IsBetween(xCoord(car(last(p1))) xCoord(car(last(p2))) xCoord(cadr(segLow))) &&
                 Ru_IsBetween(yCoord(cadr(segHi)) y1 yCoord(car(last(p2)))) )  ||
                (Ru_IsBetween(xCoord(car(last(p2))) xCoord(car(last(p1))) xCoord(cadr(segHi))) &&
                 Ru_IsBetween(yCoord(cadr(segLow)) y2 yCoord(car(last(p1)))) ) then
                swapY = t
                (if debug >= 1 printf("#### SwapY seg %d\n" cnt))
            )
        )
        (cond
           (swapX
                p1 = append(p1 list(x2:y1))
                p2 = append(p2 list(x1:y2))
                ;dbCreatePath((geGetEditCellView)  list("M7" "bus") p1 0.8)
                ;dbCreatePath((geGetEditCellView)  list("M4" "bus") p2 0.8)
           )
           (swapY
                p1 = append(p1 list(x1:y2))
                p2 = append(p2 list(x2:y1))
                ;dbCreatePath((geGetEditCellView)  list("M7" "bus") p1 0.8)
                ;dbCreatePath((geGetEditCellView)  list("M4" "bus") p2 0.8)
           )
           (t
                p1 = append(p1 list(x1:y1))
                p2 = append(p2 list(x2:y2))
           )
        )
        lastSegDir = segDir
    )
    x1 = xCoord(car(last(lowerBound)))
    x2 = xCoord(car(last(upperBound)))
    y1 = yCoord(car(last(lowerBound)))
    y2 = yCoord(car(last(upperBound)))
    (cond
       (swapX
            p1 = append(p1 list(x2:y1))
            p2 = append(p2 list(x1:y2))
       )
       (swapY
            p1 = append(p1 list(x1:y2))
            p2 = append(p2 list(x2:y1))
       )
       (t
            p1 = append(p1 list(x1:y1))
            p2 = append(p2 list(x2:y2))
       )
    )
    list(p1 p2)
  )
)

(defun Ru_GetChannelPattern (nodeType nodeList freeTrackList startIdx)
  (let (node nodeBaseName size type result es ee nodeCnt nodeStr arraySize 
        freeTrack trackCnt inlvStr channelList numTracks)
     nodeCnt = 0
     numTracks = 0
     (foreach node nodeList
          result = Ru_NodeNameToLable(nth(0 node))
          nodeBaseName = nth(2 result)
          size = nth(1 node)
          type = nth(2 node)
          arraySize = (if size parseString(size ",") size)
          (cond 
            (arraySize==nil
                 freeTrack = nth((numTracks+startIdx) freeTrackList)
                 name = sprintf(nil "\".%s\"" nodeBaseName)
                 (if !freeTrack freeTrack = list(99 "BusMetalXX"))
                 (cond
                   (rexMatchp("1of2" nodeType) || rexMatchp("1of3" nodeType) || rexMatchp("1of4" nodeType)
                         inlvStr = "A"
                         (if mod(nodeCnt 2)==1 then inlvStr = "B" numTracks++)
                         nodeStr = sprintf(nil "inlv_%s_%s" nodeType inlvStr)
                         (if rexMatchp("1of2" nodeType) nodeStr = strcat(nodeStr "3"))
                   )
                   (t nodeStr = nodeType numTracks++)
                 )
                 (cond
                   (cadr(freeTrack) == "BusMetal67" offset = -10000)
                   (cadr(freeTrack) == "BusMetal45" offset = -1000)
                   (cadr(freeTrack) == "BusMetal23" offset = -100)
                   (t offset = 0)
                 )
                 offset = offset + mod(nodeCnt 2)*0.1 ;for sorting _A, _B
                 nodeCnt++
    ;printf("       (list %-30s %s %2d*TrackPitch %s)\n" name nodeStr car(freeTrack) cadr(freeTrack))
                 channelList = append(channelList 
                                      list(list(offset+car(freeTrack) sprintf(nil "       (list %-30s %s %2d*TrackPitch %s)\n" name nodeStr car(freeTrack) cadr(freeTrack)))))
            )
            (length(arraySize)==1
                result = Ru_RangeToInt(size)
                (for i car(result) cadr(result) 
                   freeTrack = nth((numTracks+startIdx) freeTrackList)
                   name = sprintf(nil "\".%s[%d]\"" nodeBaseName i)
                   (if !freeTrack freeTrack = list(99 "BusMetalXX"))
                   (cond
                       (rexMatchp("1of2" nodeType) || rexMatchp("1of3" nodeType) || rexMatchp("1of4" nodeType)
                             inlvStr = "A"
                             (if mod(nodeCnt 2)==1 then inlvStr = "B" numTracks++)
                             nodeStr = sprintf(nil "inlv_%s_%s" nodeType inlvStr)
                             (if rexMatchp("1of2" nodeType) nodeStr = strcat(nodeStr "3"))
                       )
                       (t nodeStr = nodeType numTracks++)
                   )
                   (cond
                       (cadr(freeTrack) == "BusMetal67" offset = -10000)
                       (cadr(freeTrack) == "BusMetal45" offset = -1000)
                       (cadr(freeTrack) == "BusMetal23" offset = -100)
                       (t offset = 0)
                   )
                   offset = offset + mod(nodeCnt 2)*0.1
                   nodeCnt++
    ;printf("       (list %-30s %s %2d*TrackPitch %s)\n" name nodeStr car(freeTrack) cadr(freeTrack))
                   channelList = append(channelList 
                                        list(list(offset+car(freeTrack) sprintf(nil "       (list %-30s %s %2d*TrackPitch %s)\n" name nodeStr car(freeTrack) cadr(freeTrack)))))
                )
            )
            (length(arraySize)==2
               result = Ru_RangeToInt(car(arraySize))
               result2 = Ru_RangeToInt(cadr(arraySize))
               (for i car(result) cadr(result) 
                 (for j car(result2) cadr(result2) 
                   freeTrack = nth((numTracks+startIdx) freeTrackList)
                   name = sprintf(nil "\".%s[%d][%d]\"" nodeBaseName i j)
                   (if !freeTrack freeTrack = list(99 "BusMetalXX"))
                   (cond
                       (rexMatchp("1of2" nodeType) || rexMatchp("1of3" nodeType) || rexMatchp("1of4" nodeType)
                             inlvStr = "A"
                             (if mod(nodeCnt 2)==1 then inlvStr = "B" numTracks++)
                             nodeStr = sprintf(nil "inlv_%s_%s" nodeType inlvStr)
                             (if rexMatchp("1of2" nodeType) nodeStr = strcat(nodeStr "3"))
                       )
                       (t nodeStr = nodeType numTracks++)
                   )
                   (cond
                       (cadr(freeTrack) == "BusMetal67" offset = -10000)
                       (cadr(freeTrack) == "BusMetal45" offset = -1000)
                       (cadr(freeTrack) == "BusMetal23" offset = -100)
                       (t offset = 0)
                   )
                   offset = offset + mod(nodeCnt 2)*0.1
                   nodeCnt++
    ;printf("       (list %-30s %s %2d*TrackPitch %s)\n" name nodeStr car(freeTrack) cadr(freeTrack))
                   channelList = append(channelList 
                                        list(list(offset+car(freeTrack) sprintf(nil "       (list %-30s %s %2d*TrackPitch %s)\n" name nodeStr car(freeTrack) cadr(freeTrack)))))
                 )
               )
            )
            (t error("Unsupported arraySize %L\n" arraySize))
          )
      )
    (if inlvStr == "A" numTracks++)
    list(numTracks channelList nodeCnt)
  )
)

;distribute the pattern evenly
(defun Ru_GenerateFreeTrackList (startTrack endTrack)
  (let (freeList right start end)
    (if startTrack > endTrack error("start %d must be less than end %d\n" startTrack endTrack))
    start = startTrack
    end = endTrack
    (while start<=end
        (if right then
            freeList = append(freeList list(list(end "BusMetal67")))
            end--
        else
            freeList = append(freeList list(list(start "BusMetal67")))
            start++
        )
        right = !right
    )
    start = startTrack
    end = endTrack
    (while start<=(end-4)
        (if right then
            freeList = append(freeList list(list(end "BusMetal45")))
            end--
        else
            freeList = append(freeList list(list(start "BusMetal45")))
            start++
        )
        right = !right
    )
    start = startTrack
    end = endTrack
    (while start<=(end-4)
        (if right then
            freeList = append(freeList list(list(end "BusMetal23")))
            end--
        else
            freeList = append(freeList list(list(start "BusMetal23")))
            start++
        )
        right = !right
    )
    freeList
  )
)

(defun GenerateChannelPattern (nodeColList @key (channelName "XXX") (colList nil) (colMaxTrack nil))
  (let (nodeName node cnt size type label nodeType name addNext result el
       colCnt col bufName bufNodeName nodeEntries nodeBaseName nodeList nodeCnt totalNode
       e1of1List e1of2List e1of3List e1of4List chanDftList channelList
       trackCnt totalTracks freeTrackList startTrack endTrack)
    printf(";Pattern auto generated by GenerateChannelPattern. Edit X to assign track\n")
    printf("%sBusPattern = (DefChan\n" channelName) 
    printf("   (list\n")

    totalTracks = 0
    totalNode = 0
    startTrack = 0
    (if !colList then
      colCnt = 0
      (if stringp(car(car(nodeColList))) nodeColList = list(nodeColList))
      (foreach nodeList nodeColList
          (if colMaxTrack then
              endTrack = nth(colCnt colMaxTrack)
          else
              trackCnt = 0
              (foreach node nodeList
                  size = nth(1 node)
                  result = Ru_RangeToInt(size)
                  (if size then
                      trackCnt = trackCnt + cadr(result) - car(result) + 1
                  else trackCnt++)
              )
              endTrack = ceiling((trackCnt/2+6)/3.0/2)*2 - 1;assume one column
              printf("No colMaxTrack specified: Estimated TotalTracks %L, TotalNodeCnt\n"  endTrack+1 trackCnt)
          )
          freeTrackList = Ru_GenerateFreeTrackList(startTrack endTrack)
          startTrack = endTrack+1

          colCnt++
          trackCnt = 0
          nodeCnt = 0
          e1of1List = nil
          e1of2List = nil
          e1of3List = nil
          e1of4List = nil
          chanDftList = nil
          channelList = nil
          (if length(nodeColList)!=1 printf(";Column #%d: \n%L\n" colCnt nodeColList))
          (foreach node nodeList
              type = nth(2 node)
              (cond
                 (rexMatchp("1of1" type) e1of1List = append(e1of1List list(node)))
                 (rexMatchp("1of2" type) e1of2List = append(e1of2List list(node)))
                 (rexMatchp("1of3" type) e1of3List = append(e1of3List list(node)))
                 (rexMatchp("1of4" type) e1of4List = append(e1of4List list(node)))
                 (rexMatchp("ChanDft" type) chanDftList = append(chanDftList list(node)))
                 (t error("Unsupported type: %L\n" type))
              )
          )
          result = Ru_GetChannelPattern("ChanDft2" chanDftList freeTrackList trackCnt)
          (if cadr(result) then channelList = append(channelList cadr(result)) trackCnt = trackCnt + car(result) nodeCnt = nodeCnt + nth(2 result))
          result = Ru_GetChannelPattern("e1of4" e1of4List freeTrackList trackCnt)
          (if cadr(result) then channelList = append(channelList cadr(result)) trackCnt = trackCnt + car(result) nodeCnt = nodeCnt + nth(2 result))
          result = Ru_GetChannelPattern("e1of3" e1of3List freeTrackList trackCnt)
          (if cadr(result) then channelList = append(channelList cadr(result)) trackCnt = trackCnt + car(result) nodeCnt = nodeCnt + nth(2 result))
          result = Ru_GetChannelPattern("e1of2" e1of2List freeTrackList trackCnt)
          (if cadr(result) then channelList = append(channelList cadr(result)) trackCnt = trackCnt + car(result) nodeCnt = nodeCnt + nth(2 result))
          result = Ru_GetChannelPattern("e1of1" e1of1List freeTrackList trackCnt)
          (if cadr(result) then channelList = append(channelList cadr(result)) trackCnt = trackCnt + car(result) nodeCnt = nodeCnt + nth(2 result))
          totalTracks = totalTracks + trackCnt
          totalNode = totalNode + nodeCnt
          channelList = sort(channelList 'Ru_SortCarChannelElByTrack)

          (foreach el channelList
              printf("%s" cadr(el))
          )
          printf(";Column #%d: %d channels using %d tracks\n" colCnt nodeCnt trackCnt)
       )
    else
error("Need proper support for ?colList option\n") 
        colCnt = 0
        rexMagic(nil)
        (foreach col colList
            printf(";Column #%d: \n%L\n" colCnt col)
            colCnt++
            (foreach bufName nth(length(col)/2 col) ; get the middle one, assume it has all the entries
                matchNode = nil
                (foreach node nodeColList
                  nodeBaseName = nth(0 node)
                  size = nth(1 node)
                  result = Ru_RangeToInt(size)
                  es = car(result)
                  ee = cadr(result)
                  nodeEntries = nil
                  (if !size then 
                     nodeEntries = append(nodeEntries list(nodeBaseName))
                  else  
                    (for i es ee 
                       nodeEntries = append(nodeEntries list(sprintf(nil "%s[%d]" nodeBaseName i)))
                    )
                  )

                  ;printf("bufName: %L nodeBaseName: %L nodeEntries: %L\n" bufName nodeBaseName nodeEntries)
    (foreach nodeName nodeEntries
    ;FIXME might need to make this more accurate
                  (if !matchNode && rexMatchp(nodeName bufName) then
                    ;printf("bufName: %L node: %L\n" bufName nodeName)
                    label = Ru_ReplaceDotWithDash(nodeName)
                    size = nth(1 node)
                    type = nth(2 node)
                    nodeType = "e1of4"
                    addNext = nil
                    (cond
                       (rexMatchp("FAST_MBUF_2_1of2" type) nodeType = "inlv_e1of2_X" addNext = t)
                       (rexMatchp("FAST_MBUF_4" type) nodeType = "inlv_e1of4_X" addNext = t)
                       (rexMatchp("1of1" type) nodeType = "e1of1")
                       (rexMatchp("1of2" type) nodeType = "inlv_e1of2_X")
                       (rexMatchp("1of3" type) nodeType = "e1of3")
                       (rexMatchp("1of4" type) nodeType = "inlv_e1of4_X")
                       (t error("Unsupported type: %L\n" type))
                    )
                    name = sprintf(nil "\".%s\"" nodeName)
                    printf("       (list %-30s %s x*TrackPitch BusMetal67)\n" name nodeType)
                    (if addNext then
                       result = parseString(nodeName "[]")
                       (if length(result)!=2 error("Unexpected nodeName %L\n" nodeName))
                       name = sprintf(nil "\".%s[%d]\"" car(result) Ru_StringToInt(cadr(result))+1)
                       printf("       (list %-30s %s x*TrackPitch BusMetal67)\n" name nodeType)
                    )
                 )

    )
               ) 
            )
        )
        rexMagic(t)
    )
    printf("  )\n")
    printf(")\n\n\n")
    printf(";This %d channels can fit into %.1f tracks wide using %d columns\n" totalNode (totalTracks+6*colCnt)/3.0 colCnt)
  )
)

;Dump the node order based on pin pattern
(defun DumpNodeOrder (pattern)
  (let (lastName el offset nameList result name)
    lastName = ""
    (foreach el pattern
        result= parseString(nth(0 el) ".")
        name = car(result)
        (for cnt 1 length(result)-2
            name = strcat(name ".")
            name = strcat(name nth(cnt result))
        )
        (if lastName != name then
            offset = -nth(2 el)
            nameList = append(nameList list(list(offset name)))
        )
        lastName = name
    )
    nameList = sort(nameList 'Ru_SortCarChannelElByTrack)
    (foreach el nameList
        (if cadr(el) != "GND" then
            printf("Offset: %8.1f(%4.0fT)  %L\n" -car(el) -car(el)/TrackPitch cadr(el))
        )
    )
    t
  )
)

(defun GetColIndex (colCfg offset)
  (prog (el colIdx)
    colIdx = 0
    offset = offset/TrackPitch
    (foreach el colCfg
      (if (el+1) > offset return(colIdx))
      colIdx++
    )
    return(colIdx)
  )
)
;colMaxTrack, busPattern - given a bus pattern generate columns specified by colMaxTrack
;segRange - only generate from given range
;inlinePrefix - for inline routing
;prefix - for multiple paths in a single cell
(defun GenerateMbuf (nodeList @key (nMbufs nil) (numColumns 1) (colMaxTrack nil) (busPattern nil)
                    (reverseList nil) (sortPattern nil) (segRange nil) (lastSeg nil) (startSeg 9999)
                    (resetGroup nil) (resetBaseName "Reset") (noReset nil) 
                    (reverseResetDir nil) (collapseSegList nil) (inlinePrefix "") (prefix "")
                    (debug 0) )
  (prog (nodeName mbufList n i size type label nodeDir name col1 col2 range nodeMin nodeMax arrayStr
        segMbufs bufPrefix leftRightList el offset segMbufsOff es ee ss se temp addSeg nestedArray
        resetBase endSeg maxSeg result result2 collapseSeg entry resetN busOffset segList)

    (if resetBaseName == "Reset" && prefix != "" resetBaseName = strcat(resetBaseName prefix))
    segment = t
    (if !resetGroup resetGroup = resetBaseName)
    (foreach node nodeList
        (if length(node) > 3 segment  = nil)
    )
    (if busPattern && !colMaxTrack error("colMaxTrack must be specified when busPattern is specified\n"))
    (if !busPattern && colMaxTrack error("Pattern must be specified when colMaxTrack is specified\n"))

    (if segment then
       (foreach node nodeList
           result = Ru_NodeNameToLable(nth(0 node))
           label = car(result)
           nodeDir = cadr(result)
           bufPrefix = sprintf(nil "%smbuf%s%s" inlinePrefix prefix nodeDir)
           size = nth(1 node)
           type = nth(2 node)
           arraySize = (if size parseString(size ",") size)
           (cond
             (arraySize==nil mbufList = append(mbufList list(sprintf(nil "%smbuf%s_%s" inlinePrefix prefix label))))
             (length(arraySize)==1
               result = Ru_RangeToInt(car(arraySize))
               (for i car(result) cadr(result)
                   mbufList = append(mbufList list(sprintf(nil "%s_%s[%d]" bufPrefix label i)))
               )
             )
             (length(arraySize)==2 ;2-D array both at end
               result = Ru_RangeToInt(car(arraySize))
               result2 = Ru_RangeToInt(cadr(arraySize))
               (for i car(result) cadr(result)
                 (for j car(result2) cadr(result2)
                   mbufList = append(mbufList list(sprintf(nil "%s_%s[%d][%d]" bufPrefix label i j)))
                 )
               )
             )
             (t error("Unsupported arraySize %L\n" arraySize))
           )
        )
        return(mbufList)
    )

    (if !nMbufs then

        endSeg = 0
        maxSeg = 0
        (foreach node nodeList
           seg = nth(3 node)
           result = Ru_RangeToInt(seg)
           se = cadr(result)
           (if maxSeg < se maxSeg = se)
         )
        declare(segBufList[maxSeg+1])
        declare(segMbufsOff[maxSeg+1])
        declare(mbufCol[maxSeg+1])
        (for n 0 maxSeg segBufList[n] = nil)
        (for n 0 maxSeg segMbufsOff[n] = nil)
        (for n 0 maxSeg mbufCol[n] = nil)
        numCols = 1
        (if colMaxTrack numCols = length(colMaxTrack))
        declare(colList[numCols+1])
        (for n 0 numCols colList[n] = nil)


        (foreach node nodeList
           collapseSeg = nth(5 node)
           (if collapseSeg then
               result = parseString(collapseSeg ",")
               (foreach entry result
                   (if !member(entry collapseSegList) collapseSegList = append(collapseSegList list(entry)))
               )
           )
        )
(if collapseSegList printf("collapseSegList: %L\n" collapseSegList))

        (foreach node nodeList
           result = Ru_NodeNameToLable(nth(0 node))
           label = car(result)
           nodeDir = cadr(result)
           nodeName = nth(2 result)
           bufPrefix = sprintf(nil "%smbuf%s%s" inlinePrefix prefix nodeDir)
           size = nth(1 node)
           type = nth(2 node)
           seg = nth(3 node)
           result = Ru_RangeToInt(seg)
           ss = car(result)
           se = cadr(result)
           resetBase = nth(4 node)
           (if !resetBase resetBase = resetBaseName)
          
           (if resetGroup == resetBase then
             (if startSeg > ss startSeg = ss)
             (if endSeg < se endSeg = se)
             collapseSeg = nil
             (if nth(5 node) then
                 collapseSeg = parseString(nth(5 node) ",")
             )
             ;check for nested array
             nestedArray = nil
             temp = parseString(nodeName "[]")
             (if length(temp) == 3 then
               range = parseString(nth(1 temp) "-")
               (if length(range) == 2 then
                 nodeMin = Ru_StringToInt(car(range))
                 nodeMax = Ru_StringToInt(cadr(range))
               else
                 printf("ERROR: Unhandled node format %L\n" node)
               )
               nodeName = strcat(nth(0 temp) nth(2 temp))
               temp = parseString(label "[]")
               (if length(temp) != 3 error("Unexpected lable %L\n" label))
               label = strcat(nth(0 temp) nth(2 temp))
               nestedArray = t
             else
               nodeMin = -1
               nodeMax = -1
             )

           (for aidx nodeMin nodeMax
             (if aidx >=0 then arrayStr = sprintf(nil "[%d]" aidx)
             else arrayStr = "")

(if lastSeg ss = se)
             (for n ss se
               (if !member(sprintf(nil "%d" n) collapseSeg) then
                   arraySize = (if size parseString(size ",") size)
                   (cond
                     (arraySize==nil
                        busOffset = 0
                        (if busPattern then
                            name = sprintf(nil ".%s.e" nodeName)
                            busOffset = Ru_GetChannelNodeOffset(busPattern name ?nestedArray nestedArray)
                            (if !busOffset error("Unable to find %s in busPattern %L\n" name busPattern))
                        )
                        (if sortPattern then
                          name = sprintf(nil ".%s.e" nodeName)
                          offset = Ru_GetChannelNodeOffset(sortPattern name ?nestedArray nestedArray)
                          (if !offset error("Unable to find %s in sortPattern %L\n" name sortPattern))
                          segMbufsOff[n] = append(segMbufsOff[n] list(list(offset sprintf(nil "%s_%s%s[%d]" bufPrefix label arrayStr n) busOffset)))
                       else
                          bufName = sprintf(nil "%s_%s%s[%d]" bufPrefix label arrayStr n)
                          segBufList[n] = append(segBufList[n] list(list(bufName busOffset)))
                       )
                     )
                     (length(arraySize)==1
                       (if type == "FAST_MBUF_4" || type == "FAST_MBUF_2_1of2" then
                           error("No support for %L\n" type)
                       else
                           result = Ru_RangeToInt(car(arraySize))
                           (for i  car(result) cadr(result)
                               busOffset = 0
                               (if busPattern then
                                   name = sprintf(nil ".%s[%d].e" nodeName i)
                                   busOffset = Ru_GetChannelNodeOffset(busPattern name ?nestedArray nestedArray)
                                   (if !busOffset error("Unable to find %s in busPattern %L\n" name busPattern))
                               )
                               (if sortPattern then
                                   name = sprintf(nil ".%s[%d].e" nodeName i)
                                   offset = Ru_GetChannelNodeOffset(sortPattern name ?nestedArray nestedArray)
                                   (if !offset error("Unable to find %s in sortPattern %L\n" name sortPattern))
                                   segMbufsOff[n] = append(segMbufsOff[n] list(list(offset sprintf(nil "%s_%s%s[%d][%d]" bufPrefix label arrayStr i n) busOffset)))
                               else
                                  bufName = sprintf(nil "%s_%s%s[%d][%d]" bufPrefix label arrayStr i n)
                                  segBufList[n] = append(segBufList[n] list(list(bufName busOffset)))
                               )
                           )
                       )
                     )
                     (length(arraySize)==2
                       result = Ru_RangeToInt(car(arraySize))
                       result2 = Ru_RangeToInt(cadr(arraySize))
                       (for i car(result) cadr(result)
                         (for j car(result2) cadr(result2)
                           busOffset = 0
                           (if busPattern then
                               name = sprintf(nil ".%s[%d][%d].e" nodeName i j)
                               busOffset = Ru_GetChannelNodeOffset(busPattern name ?nestedArray nestedArray)
                               (if !busOffset error("Unable to find %s in busPattern %L\n" name busPattern))
                           )
                           (if sortPattern then
                               name = sprintf(nil ".%s[%d][%d].e" nodeName i j)
                               offset = Ru_GetChannelNodeOffset(sortPattern name ?nestedArray nestedArray)
                               (if !offset error("Unable to find %s in sortPattern %L\n" name sortPattern))
                               segMbufsOff[n] = append(segMbufsOff[n] list(list(offset sprintf(nil "%s_%s%s[%d][%d][%d]" bufPrefix label arrayStr i j n) busOffset)))
                           else
                              bufName = sprintf(nil "%s_%s%s[%d][%d][%d]" bufPrefix label arrayStr i j n)
                              segBufList[n] = append(segBufList[n] list(list(bufName busOffset)))
                           )
                         )
                       )
                     )
                     (t error("Unsupported arraySize %L\n" arraySize))
                   ) ;cond
               ) ;collapseList
             ) ;for
           ) ;for arrayIdx
           )
        )
        (for n startSeg endSeg
          (if segRange==nil || (n>=car(segRange) && n<=cadr(segRange)) then
              addSeg = nil
              (if segMbufsOff[n] then
                  segMbufsOff[n] = sort(segMbufsOff[n] 'Ru_SortCarChannelElByTrack)
                  (foreach el segMbufsOff[n]
                      busOffset = nth(2 el)
                      colIdx = GetColIndex(colMaxTrack busOffset)
;printf("colIdx1 %L el %L bufOffset %L\n" colIdx el busOffset)
                      colList[colIdx] = append(colList[colIdx] list(cadr(el)))
                  )
              else
                  segList = nil
                  (if busPattern then ;FIXME 
                      (foreach el segBufList[n]
                          busOffset = nth(1 el)
                          colIdx = GetColIndex(colMaxTrack busOffset)
;printf("colIdx3 %L el %L bufOffset %L\n" colIdx el busOffset)
;                          segList = append(segList list(car(el)))
                          colList[colIdx] = append(colList[colIdx] list(car(el)))
                      )
                      (if reverseList error("No support with busPattern\n"))
;                      colList[colIdx] = append(colList[colIdx] segList)
                  else
                      (foreach el segBufList[n]
                          busOffset = nth(1 el)
                          colIdx = GetColIndex(colMaxTrack busOffset) ;FIXME might have a problem if this is different
;printf("colIdx2 %L el %L bufOffset %L\n" colIdx el busOffset)
                          segList = append(segList list(car(el)))
                          ;colList[colIdx] = append(colList[colIdx] list(car(el)))
                      )
                      (if reverseList segList = reverse(segList)) ;only reverse per segment
                      colList[colIdx] = append(colList[colIdx] segList)
                  )
;printf("reverseList %L colIdx %L %L\n" reverseList colIdx colList[colIdx])
              )
              segMbufs = append(segMbufs addSeg)

              (if !resetN resetN = n)
              (if member(sprintf(nil "%d" n+1) collapseSegList) then
                  (if nil printf("Seg#%d is collapse to previous segment\n" n+1))
              else
                  (for colIdx 0 numCols-1
                    (if !noReset && colIdx==0 then
                      (if reverseResetDir then
                          ;reset at end of column
                          colList[colIdx] = append(colList[colIdx] list(sprintf(nil "ramp%s[%d]" resetGroup resetN) sprintf(nil "buf%s[%d]" resetGroup resetN) ))
                      else 
                          colList[colIdx] = append(list(sprintf(nil "buf%s[%d]" resetGroup resetN) sprintf(nil "ramp%s[%d]" resetGroup resetN) )  colList[colIdx])
                      )
                    )
                    mbufCol[colIdx] = append(mbufCol[colIdx] list(colList[colIdx]))
;printf("Seg#%d COL#%L: %L\n" n colIdx colList[colIdx])
                    colList[colIdx] = nil
                  )
                  resetN = nil
              )
          )
        )
      mbufList = nil
      (if numCols == 1 then
          mbufList = mbufCol[0]
;printf("COL%d: %L\n" colIdx mbufCol[0])
      else
          (for colIdx 0 numCols-1
;printf("COL#%d: %L\n" colIdx mbufCol[colIdx])
            mbufList = append(mbufList list(mbufCol[colIdx]))
          )
      )
    else ; all segments having the same number of buffers
        ; deprecated
        (for n 0 nMbufs-1
            segMbufs = nil
            segMbufsOff = nil
            segMbufs = append(segMbufs list(sprintf(nil "bufReset[%d]" n)))
            segMbufs = append(segMbufs list(sprintf(nil "rampReset[%d]" n)))

          (foreach node nodeList
            nodeName = nth(0 node)
            label = Ru_ReplaceDotWithDash(nodeName)
            size = nth(1 node)
            type = nth(2 node)
            (if !size || size < 0 then
                (if sortPattern then
                    name = sprintf(nil ".%s.e" nodeName)
                    offset = Ru_GetChannelNodeOffset(sortPattern name)
                    (if !offset error("Unable to find %s in sortPattern %L\n" name sortPattern))
                    segMbufsOff = append(segMbufsOff list(list(offset sprintf(nil "mbuf_%s[%d]" label n))))
                else
                    segMbufs = append(segMbufs list(sprintf(nil "mbuf_%s[%d]" label n)))
                )
            else  
                (for i 0 size-1 
                    (if sortPattern then
                        name = sprintf(nil ".%s[%d].e" nodeName i)
                        offset = Ru_GetChannelNodeOffset(sortPattern name)
                        (if !offset error("Unable to find %s in sortPattern %L\n" name sortPattern))
                        segMbufsOff = append(segMbufsOff list(list(offset sprintf(nil "mbuf_%s[%d][%d]" label i n))))
                    else
                        segMbufs = append(segMbufs list(sprintf(nil "mbuf_%s[%d][%d]" label i n)))
                    )
                )
            )
          )
          (if segMbufsOff then
              segMbufsOff = sort(segMbufsOff 'Ru_SortCarChannelElByTrack)

              segMbufs = nil          
              (foreach el segMbufsOff
                  segMbufs = append(segMbufs list(cadr(el)))
              )
          )
          (if reverseList segMbufs = reverse(segMbufs))
          (cond
            (numColumns==1 mbufList = append(mbufList list(list(segMbufs))))
            (numColumns==2 
                leftRightList = Ru_SplitList(segMbufs (length(segMbufs)+1)/2)
                (if reverseList leftRightList = reverse(leftRightList))
                mbufList = append(mbufList list(leftRightList))
            )
            (t error("Ru_GenerateMbuf: No support for numColumns = %d\n"))
          )
        )
    )
    (if !mbufList then
        error("No buffer found: nodeList %L \n in resetGroup %L resetBase %L resetBaseName %L\n" nodeList resetGroup resetBase resetBaseName)
    )
    return(mbufList)
  )
)

(defun Ru_RangeToInt (range)
  (let (es ee result)
   (if range && integerp(range) then
       es = 0
       ee = range
   )
   (if range && !integerp(range) then
       result = parseString(range ".")
       es = Ru_StringToInt(car(result))
       ee = Ru_StringToInt(cadr(result))
   )
    list(es ee)
  )
)

(defun Ru_NodeNameToLable (nodeStr)
  (let (nodeDir label nodeName)
    nodeDir = ""
    nodeName = nodeStr
    (cond
       (substring(nodeName 1 1) == "-" 
           nodeDir = "r"
           nodeName = substring(nodeStr 2 strlen(nodeStr)-1)
       )
       (substring(nodeName 1 1) == "+" 
           nodeName = substring(nodeStr 2 strlen(nodeStr)-1)
       )
    )
    label = Ru_ReplaceDotWithDash(nodeName)
    list(label nodeDir nodeName)
  )
)
;nMbufs - assume all channels have the same number of buffers and generate slack directive
(defun GenerateCast (nodeList @key (inPrefix "L") (outPrefix "R") (channelName "XXX") 
                    (implicitReset nil) (collapse "") (inline nil) (prefix "") (nMbufs nil)
                    (resetBaseName "Reset") (reverseResetDir nil))
  (prog (nodeName nodeStr node size arraySize type label nodeDir nodeType seg es ee ss se result result2
        str passthru collapseSeg collapseSegList collapseSegStrA collapseSegStrB entry hasCollapseSeg
        startSeg endSeg resetBase el resetList resetStartSeg resetEndSeg bufPrefix wirePrefix
        parseNode range nodeMin nodeMax parseLabel nestedArray directive)

    segment = t
    (if inPrefix == "L" && prefix != "" inPrefix = strcat(inPrefix prefix))
    (if outPrefix == "R" && prefix != "" outPrefix = strcat(outPrefix prefix))
    (if resetBaseName == "Reset" && prefix != "" resetBaseName = strcat(resetBaseName prefix))
    (foreach node nodeList
        (if length(node) > 3 segment = nil)
    )
    (if segment then
        printf("\n\n")
        printf("define ROUTE_%s_XX() (%s -L, +R; node -_L_RESET, +_R_RESET)\n" channelName channelName)
        printf("                       <+ AltaConstants <+ routed <+ floorplan_hierarchy <: NO_RESET_CELL {\n")
        printf("  /*Auto generated by GenerateCast()*/\n")
        printf("  subcells {\n")
        printf("    node _RESET;\n")
        printf("    RAMP_RESET ramp%s(_L_RESET,_RESET);\n" resetBaseName)
        printf("    BUF_RESET buf%s(_RESET,_R_RESET);\n" resetBaseName)
       (foreach node nodeList
           nodeName = nth(0 node)
           result = Ru_NodeNameToLable(nodeName)
           label = car(result)
           nodeDir = cadr(result)
           size = nth(1 node)
           result = Ru_RangeToInt(size)
           es = car(result)
           ee = cadr(result)
           type = nth(2 node)
           (if type then
               (if !size then 
                   printf("                %s mbuf_%s (%s.%s,%s.%s);\n"
                          type label inPrefix label outPrefix label)
               else  
                   result = sprintf(nil "%d..%d:" es ee)
                   printf("    <i:%-7s  %s mbuf_%s[i] (%s.%s[i],%s.%s[i]);>\n"
                      result type label inPrefix label outPrefix label)
               )
           else
               passthru = t
               printf("                %s.%s = %s.%s;\n" outPrefix nodeName inPrefix nodeName)
           )
        )
        printf("\n")
        printf("  }\n")
       (if passthru printf("  directives { wiring = true; }\n"))
        printf("}\n")
        printf("\n\n")
        return(t)
    )
    printf("\n\n")
    (if inline then
        printf("define PATH_%s() (\n" channelName)
        (foreach node nodeList
           result = Ru_NodeNameToLable(nth(0 node))
           label = car(result)
           size = nth(1 node)
           result = Ru_RangeToInt(size)
           ee = cadr(result)
           type = nth(2 node)
           (if type then
               cond(
                   (rexMatchp("1of4" type) nodeType = "e1of4")
                   (rexMatchp("1of3" type) nodeType = "e1of3")
                   (rexMatchp("1of2" type) nodeType = "e1of2")
                   (rexMatchp("1of1" type) nodeType = "e1of1")
                   (rexMatchp("ChanDft" type)
                       (if !rexMatchp("^DFT" label) error("ChanDft channel must start with DFT but got %L\n" label))
                       nodeType = "ChanDft"
                   )
                   (t error("Unsupported type %L\n" type))
               )
               (if !size then printf("            %s -%s_%s, +%s_%s;\n" nodeType inPrefix label outPrefix label)
               else  printf("            %s[%d] -%s_%s, +%s_%s;\n" nodeType ee+1  inPrefix label outPrefix label ))
           )
        )
    else printf("define PATH_%s() (%s -%s, +%s;\n" channelName channelName inPrefix outPrefix))
    (if implicitReset then printf("                       )\n")
    else printf("                       node -_L_RESET, +_R_RESET)\n"))
    printf("                       <+AltaAssemblyConstants\n") 
    printf("                       <+AltaConstants\n") 
    printf("                       <+routed\n") 
    printf("                       <+floorplan_hierarchy {\n")
    printf("  /*Auto generated by GenerateCast()*/\n")
    (if nMbufs then
        printf("  int NBUFS = %d; //Adjust to change number of buffers segments\n\n" nMbufs)
directive = t
     )
    (if directive then
        (foreach node nodeList
           nodeStr = nth(0 node)
           result = Ru_NodeNameToLable(nodeStr)
           label = car(result)
           printf("  float T_%s = 0.0;\n" label)
        )
        printf("\n")
    )
    printf("  subcells {\n")
    (if t then
        startSeg = 9999
        endSeg = 0
        ;Node declaration
        (foreach node nodeList
           nodeStr = nth(0 node)
           result = Ru_NodeNameToLable(nodeStr)
           label = car(result)
           nodeDir = cadr(result)
           nodeName = nth(2 result)
           wirePrefix = sprintf(nil "w%s%s" prefix nodeDir)
           size = nth(1 node)
           type = nth(2 node)

           (if type then
               (if nMbufs then
                   startSeg = 0
                   endSeg = nil ;should not be used
                   ss = 0
                   endSegStr = "(NBUFS-1)"
                   endSegStr1 = "NBUFS"
               else
                   seg = nth(3 node)
                   (if !seg error("Must specify number of segments\n"))
                   result = Ru_RangeToInt(seg)
                   ss = car(result)
                   se = cadr(result)
                   (if startSeg > ss startSeg = ss)
                   (if endSeg < se endSeg = se)
                   endSegStr = sprintf(nil "%d" se)
                   endSegStr1 = sprintf(nil "%d" se+1)
               )
               nodeType = "UNKNOWN"
               cond(
                   (rexMatchp("1of4" type) nodeType = "e1of4")
                   (rexMatchp("1of3" type) nodeType = "e1of3")
                   (rexMatchp("1of2" type) nodeType = "e1of2")
                   (rexMatchp("1of1" type) nodeType = "e1of1")
                   (rexMatchp("ChanDft" type) 
                       (if !rexMatchp("^DFT" label) error("ChanDft channel must start with DFT but got %L\n" label))
                       nodeType = "ChanDft"
                   )
               )
               arraySize = (if size parseString(size ",") nil)
               ;check for nested array
               parseNode = parseString(nodeName "[]")
               (if length(parseNode) == 3 then
                 range = parseString(nth(1 parseNode) "-")
                 (if length(range) == 2 then
                   nodeMin = Ru_StringToInt(car(range))
                   nodeMax = Ru_StringToInt(cadr(range))
                   arraySize = append(list(sprintf(nil "%d..%d" nodeMin nodeMax)) arraySize )
                   parseLabel = parseString(label "[]")
                   (if length(parseLabel) != 3 error("Unexpected lable %L\n" label))
                   label = strcat(nth(0 parseLabel) nth(2 parseLabel))
                 )
               )

               (cond
                 (arraySize==nil printf("    %s %s_%s[%d..%s];\n" nodeType wirePrefix label ss endSegStr1))
                 (length(arraySize)==1
                   result = Ru_RangeToInt(car(arraySize))
                   printf("    %s %s_%s[%d..%d,%d..%s];\n" nodeType wirePrefix label car(result) cadr(result) ss endSegStr1)
                 )
                 (length(arraySize)==2
                   result = Ru_RangeToInt(car(arraySize))
                   result2 = Ru_RangeToInt(cadr(arraySize))
                   printf("    %s %s_%s[%d..%d,%d..%d,%d..%s];\n" nodeType wirePrefix label car(result) cadr(result) car(result2) cadr(result2) ss endSegStr1)
                 )
                 (t error("Unsupported arraySize %L\n" arraySize))
               )
           )
        )
        printf("\n")
        printf("    node w_buf%s[%d..%s];\n" resetBaseName startSeg (if nMbufs endSegStr1 sprintf(nil "%d" endSeg+1)))
        printf("    node w_ramp%s[%d..%s];\n" resetBaseName startSeg (if nMbufs endSegStr sprintf(nil "%d" endSeg)))
        (if implicitReset then
            printf("    w_buf%s[%d] = _RESET;\n" resetBaseName startSeg)
        else
            (if reverseResetDir printf("    //Reset is in reverse direction\n"))
            printf("    w_buf%s[%d] = %s;\n" resetBaseName startSeg (if reverseResetDir "_R_RESET" "_L_RESET"))
        )
        ;L wires
        (foreach node nodeList
           nodeStr = nth(0 node)
           result = Ru_NodeNameToLable(nodeStr)
           label = car(result)
           nodeDir = cadr(result)
           nodeName = nth(2 result)
           wirePrefix = sprintf(nil "w%s%s" prefix nodeDir)
           size = nth(1 node)
           type = nth(2 node)
           collapseSeg = nth(5 node)
           (if collapseSeg then
               hasCollapseSeg = t
               result = parseString(collapseSeg ",")
               (foreach entry result
                   (if !member(entry collapseSegList) collapseSegList = append(collapseSegList list(entry)))
               )
           )
           (if type then
               (if nMbufs then
                   ss = 0
               else
                   seg = nth(3 node)
                   result = Ru_RangeToInt(seg)
                   ss = car(result)
                   se = cadr(result)
               )
               arraySize = (if size parseString(size ",") nil)
               ;check for nested array
               nestedArray = nil
               parseNode = parseString(nodeName "[]")
               (if length(parseNode) == 3 then
                 range = parseString(nth(1 parseNode) "-")
                 (if length(range) == 2 then
                   nodeMin = Ru_StringToInt(car(range))
                   nodeMax = Ru_StringToInt(cadr(range))
                   parseLabel = parseString(label "[]")
                   (if length(parseLabel) != 3 error("Unexpected lable %L\n" label))
                   label = strcat(nth(0 parseLabel) nth(2 parseLabel))
                   nestedArray = t
                 )
               )
               (cond
                 (arraySize==nil 
                     (if nestedArray then
                         str = sprintf(nil "%d..%d:" nodeMin nodeMax)
                         printf("    <i:%-7s  %s_%s[i,%d] = %s%s%s[i]%s;>\n" 
                                str wirePrefix label ss
                                inPrefix (if inline "_" ".") nth(0 parseNode) nth(2 parseNode))
                     else
                       printf("                %s_%s[%d] = %s%s%s;\n" 
                                     wirePrefix label ss inPrefix (if inline "_" ".") nodeName)
                     )
                 )
                 (length(arraySize)==1
                   (if nestedArray then
                     result = Ru_RangeToInt(car(arraySize))
                     printf("    <i:%d..%d:\n" nodeMin nodeMax)
                     str = sprintf(nil "%d..%d:" car(result) cadr(result))
                     printf("      <j:%-7s  %s_%s[i,j,%d] = %s%s%s[i]%s[j];>\n" 
                            str wirePrefix label ss
                            inPrefix (if inline "_" ".") nth(0 parseNode) nth(2 parseNode))
                     printf("    >\n")
                   else
                     result = Ru_RangeToInt(car(arraySize))
                     str = sprintf(nil "%d..%d:" car(result) cadr(result))
                     printf("    <i:%-7s  %s_%s[i,%d] = %s%s%s[i];>\n" 
                            str wirePrefix label ss
                            inPrefix (if inline "_" ".") nodeName)
                   )
                 )
                 (length(arraySize)==2
                   (if nestedArray error("No support yet\n"))
                   result = Ru_RangeToInt(car(arraySize))
                   result2 = Ru_RangeToInt(cadr(arraySize))
                   printf("    <i:%d..%d:\n" car(result) cadr(result))
                   str = sprintf(nil "%d..%d:" car(result2) cadr(result2))
                   printf("      <j:%-7s  %s_%s[i,j,%d] = %s%s%s[i,j];>\n" 
                          str wirePrefix label ss
                          inPrefix (if inline "_" ".") nodeName)
                   printf("    >\n")
                 )
                 (t error("Unsupported arraySize %L\n" arraySize))
               )
           )
        )
        printf("\n")

        printf("    <n:%d..%s:\n" startSeg (if nMbufs endSegStr sprintf(nil "%d" endSeg)))
        collapseSegStrA = ""
        collapseSegStrB = ""
        (if collapseSegList then
            (foreach collapseSeg collapseSegList
                collapseSegStrA = strcat(collapseSegStrA sprintf(nil " & n!=%s" collapseSeg))
                collapseSegStrB = strcat(collapseSegStrB sprintf(nil " | n==%s" collapseSeg))
            )
        )
        (if collapse printf("      [~%scollapse_channel_buffering%s ->\n" collapse collapseSegStrA))
        (if reverseResetDir then
            printf("        BUF_RESET buf%s[n](w_buf%s[n+1],w_buf%s[n]);\n" resetBaseName resetBaseName resetBaseName)
            printf("        RAMP_RESET ramp%s[n](w_buf%s[n],w_ramp%s[n]);\n" resetBaseName resetBaseName resetBaseName)
        else
            printf("        BUF_RESET buf%s[n](w_buf%s[n],w_buf%s[n+1]);\n" resetBaseName resetBaseName resetBaseName)
            printf("        RAMP_RESET ramp%s[n](w_buf%s[n+1],w_ramp%s[n]);\n" resetBaseName resetBaseName resetBaseName)
        )
        (if collapse then 
        printf("      ]\n")
        printf("      [%scollapse_channel_buffering%s ->\n" collapse collapseSegStrB)
        printf("        w_buf%s[n] = w_buf%s[n+1];\n" resetBaseName resetBaseName)
        printf("        w_buf%s[n+1] = w_ramp%s[n];\n" resetBaseName resetBaseName)
        printf("      ]\n")
        ) ;collapse
        printf("    >\n")
        resetList = nil
        (foreach node nodeList
           resetBase = nth(4 node)
           (if resetBase && !member(resetBase resetList) resetList = append(resetList list(resetBase)))
        )
        (foreach el resetList
           resetStartSeg = 9999
           resetEndSeg = 0
           (foreach node nodeList
              resetBase = nth(4 node)
              (if resetBase && resetBase==el then
                 seg = nth(3 node)
                 result = Ru_RangeToInt(seg)
                 ss = car(result)
                 se = cadr(result)
                 (if resetStartSeg > ss resetStartSeg = ss)
                 (if resetEndSeg < se resetEndSeg = se)
              )
           )
           printf("    //Alternate Reset, edit to hook up to main reset properly\n")
           printf("    node w_buf%s[%d..%d];\n" el resetStartSeg resetEndSeg+1)
           printf("    node w_ramp%s[%d..%d];\n" el resetStartSeg resetEndSeg)
           printf("    <n:%d..%d:\n" resetStartSeg resetEndSeg)
           (if collapse printf("      [~%scollapse_channel_buffering%s ->\n" collapse collapseSegStrA))
           printf("        BUF_RESET buf%s[n](w_buf%s[n+1],w_buf%s[n]);\n" el el el)
           printf("        RAMP_RESET ramp%s[n](w_buf%s[n],w_ramp%s[n]);\n" el el el)
           (if collapse then
           printf("      ]\n")
           printf("      [%scollapse_channel_buffering%s ->\n" collapse collapseSegStrB)
           printf("        w_buf%s[n+1] = w_buf%s[n];\n" el el)
           printf("        w_buf%s[n] = w_ramp%s[n];\n" el el)
           printf("      ]\n")
           ) ;collapse
           printf("    >\n")
        )
        printf("\n")

        ;Buffers
        (foreach node nodeList
           nodeStr = nth(0 node)
           result = Ru_NodeNameToLable(nodeStr)
           label = car(result)
           nodeDir = cadr(result)
           nodeName = nth(2 result)
           bufPrefix = sprintf(nil "mbuf%s%s" prefix nodeDir)
           wirePrefix = sprintf(nil "w%s%s" prefix nodeDir)
           size = nth(1 node)
           type = nth(2 node)
           (if type then
               (if nMbufs then
                   ss = 0
                   endSegStr = "(NBUFS-1)"
                   endSegStr1 = "NBUFS"
               else
                   seg = nth(3 node)
                   result = Ru_RangeToInt(seg)
                   ss = car(result)
                   se = cadr(result)
                   endSegStr = sprintf(nil "%d" se)
                   endSegStr1 = sprintf(nil "%d" se+1)
               )
               resetBase = nth(4 node)
               (if !resetBase resetBase = resetBaseName)
               
               printf("    <n:%d..%s:\n" ss endSegStr)

               collapseSegStrA = ""
               collapseSegStrB = ""
               (if nth(5 node) then
                   collapseSegList = parseString(nth(5 node) ",")
                   (foreach collapseSeg collapseSegList
                       collapseSegStrA = strcat(collapseSegStrA sprintf(nil " & n!=%s" collapseSeg))
                       collapseSegStrB = strcat(collapseSegStrB sprintf(nil " | n==%s" collapseSeg))
                   )
               )
               arraySize = (if size parseString(size ",") nil)
               ;check for nested array
               parseNode = parseString(nodeName "[]")
               (if length(parseNode) == 3 then
                 range = parseString(nth(1 parseNode) "-")
                 (if length(range) == 2 then
                   nodeMin = Ru_StringToInt(car(range))
                   nodeMax = Ru_StringToInt(cadr(range))
                   arraySize = append(list(sprintf(nil "%d..%d" nodeMin nodeMax)) arraySize )
                   parseLabel = parseString(label "[]")
                   (if length(parseLabel) != 3 error("Unexpected lable %L\n" label))
                   label = strcat(nth(0 parseLabel) nth(2 parseLabel))
                 )
               )
               (cond
                 (arraySize==nil 
                   (if collapse printf("      [~%scollapse_channel_buffering%s ->\n" collapse collapseSegStrA))
                   printf("                %s %s_%s[n] (%s_%s[n%s],%s_%s[n%s])(Vdd,GND,w_ramp%s[n]);\n"
                          type bufPrefix label wirePrefix label (if nodeDir == "" "" "+1")
                          wirePrefix label (if nodeDir == "" "+1" "") resetBase)
                   (if collapse then
                   printf("      ]\n")
                   printf("      [%scollapse_channel_buffering%s ->\n" collapse collapseSegStrB)
                   printf("               %s_%s[n] = %s_%s[n+1];\n"
                          wirePrefix label wirePrefix label)
                   printf("      ]\n")
                   ) ;collapse
                 )
                 (length(arraySize)==1  
                   result = Ru_RangeToInt(car(arraySize))
                   str = sprintf(nil "%d..%d:" car(result) cadr(result))
                   (if collapse printf("      [~%scollapse_channel_buffering%s ->\n" collapse collapseSegStrA))
                   printf("        <i:%-7s  %s %s_%s[i,n] (%s_%s[i,n%s],%s_%s[i,n%s])(Vdd,GND,w_ramp%s[n]);>\n"
                          str type bufPrefix label wirePrefix label (if nodeDir == "" "" "+1")
                          wirePrefix label (if nodeDir == "" "+1" "") resetBase)
                   (if collapse then printf("      ]\n")
                   printf("      [%scollapse_channel_buffering%s ->\n" collapse collapseSegStrB)
                   printf("        <i:%-7s  %s_%s[i,n] = %s_%s[i,n+1];>\n"
                      str wirePrefix label wirePrefix label)
                   printf("      ]\n")
                   ) ;collapse
                 )
                 (length(arraySize)==2
                   result = Ru_RangeToInt(car(arraySize))
                   result2 = Ru_RangeToInt(cadr(arraySize))
                   printf("     <i:%d..%d:\n" car(result) cadr(result))
                   str = sprintf(nil "%d..%d:" car(result2) cadr(result2))
                   (if collapse printf("      [~%scollapse_channel_buffering%s ->\n" collapse collapseSegStrA))
                   printf("        <j:%-7s  %s %s_%s[i,j,n] (%s_%s[i,j,n%s],%s_%s[i,j,n%s])(Vdd,GND,w_ramp%s[n]);>\n"
                          str type bufPrefix label wirePrefix label (if nodeDir == "" "" "+1")
                          wirePrefix label (if nodeDir == "" "+1" "") resetBase)
                   (if collapse then printf("      ]\n")
                   printf("      [%scollapse_channel_buffering%s ->\n" collapse collapseSegStrB)
                   printf("        <j:%-7s  %s_%s[i,j,n] = %s_%s[i,j,n+1];>\n"
                      str wirePrefix label wirePrefix label)
                   printf("      ]\n")
                   printf("     >\n")
                   ) ;collapse
                 )
                 (t error("Unsupported arraySize %L\n" arraySize))
               )
               printf("    >\n")
           ) ;if type
        )
        printf("\n")
        ;R wires
        (foreach node nodeList
           nodeStr = nth(0 node)
           result = Ru_NodeNameToLable(nodeStr)
           label = car(result)
           nodeDir = cadr(result)
           nodeName = nth(2 result)
           wirePrefix = sprintf(nil "w%s%s" prefix nodeDir)
           size = nth(1 node)
           type = nth(2 node)
           (if type then
               (if nMbufs then
                   ss = 0
                   endSegStr = "(NBUFS-1)"
                   endSegStr1 = "NBUFS"
               else
                   seg = nth(3 node)
                   result = Ru_RangeToInt(seg)
                   ss = car(result)
                   se = cadr(result)
                   endSegStr = sprintf(nil "%d" se)
                   endSegStr1 = sprintf(nil "%d" se+1)
               )
               arraySize = (if size parseString(size ",") nil)
               ;check for nested array
               nestedArray = nil
               parseNode = parseString(nodeName "[]")
               (if length(parseNode) == 3 then
                 range = parseString(nth(1 parseNode) "-")
                 (if length(range) == 2 then
                   nodeMin = Ru_StringToInt(car(range))
                   nodeMax = Ru_StringToInt(cadr(range))
                   parseLabel = parseString(label "[]")
                   (if length(parseLabel) != 3 error("Unexpected lable %L\n" label))
                   label = strcat(nth(0 parseLabel) nth(2 parseLabel))
                   nestedArray = t
                 )
               )
               (cond
                 (arraySize==nil
                     (if nestedArray then
                         str = sprintf(nil "%d..%d:" nodeMin nodeMax)
                         printf("    <i:%-7s  %s_%s[i,%s] = %s%s%s[i]%s;>\n" 
                                str wirePrefix label endSegStr1
                                outPrefix (if inline "_" ".") nth(0 parseNode) nth(2 parseNode))
                     else
                        printf("                %s_%s[%s] = %s%s%s;\n" 
                                     wirePrefix label endSegStr1 outPrefix (if inline "_" ".") nodeName)
                     )
                 )
                 (length(arraySize)==1
                   (if nestedArray then
                     result = Ru_RangeToInt(car(arraySize))
                     printf("    <i:%d..%d:\n" nodeMin nodeMax)
                     str = sprintf(nil "%d..%d:" car(result) cadr(result))
                     printf("      <j:%-7s  %s_%s[i,j,%s] = %s%s%s[i]%s[j];>\n" 
                            str wirePrefix label endSegStr1
                            outPrefix (if inline "_" ".") nth(0 parseNode) nth(2 parseNode))
                     printf("    >\n")
                   else
                     result = Ru_RangeToInt(car(arraySize))
                     str = sprintf(nil "%d..%d:" car(result) cadr(result))
                     printf("    <i:%-7s  %s_%s[i,%s] = %s%s%s[i];>\n" 
                            str wirePrefix label endSegStr1
                            outPrefix (if inline "_" ".") nodeName)
                   )
                 )
                 (length(arraySize)==2
                   result = Ru_RangeToInt(car(arraySize))
                   result2 = Ru_RangeToInt(cadr(arraySize))
                   printf("    <i:%d..%d:\n" car(result) cadr(result))
                   str = sprintf(nil "%d..%d:" car(result2) cadr(result2))
                   printf("      <j:%-7s  %s_%s[i,j,%s] = %s%s%s[i,j];>\n" 
                          str wirePrefix label endSegStr1
                          outPrefix (if inline "_" ".") nodeName)
                   printf("    >\n")
                 )
                 (t error("Unsupported arraySize %L\n" arraySize))
               )
           else
               passthru = t
               printf("                %s.%s = %s%s%s;\n" outPrefix nodeName 
                      inPrefix (if inline "_" ".") nodeName)
           )
        )
        (if !implicitReset then 
            printf("                w_buf%s[%s] = %s;\n" resetBase 
                    (if nMbufs endSegStr1 sprintf(nil "%d" endSeg+1))
                    (if reverseResetDir "_L_RESET" "_R_RESET"))
        )


    else ;same number of buffer for all segments
        printf("    int NBUFS = xx;\n\n")
        printf("    node w_bufReset[0..NBUFS];\n")
        printf("    node w_rampReset[0..NBUFS];\n")
        (foreach node nodeList
           nodeName = nth(0 node)
           label = Ru_ReplaceDotWithDash(nodeName)
           size = nth(1 node)
           type = nth(2 node)
           nodeType = "UNKNOWN"
           cond(
               (rexMatchp("1of4" type) nodeType = "e1of4")
               (rexMatchp("1of3" type) nodeType = "e1of3")
               (rexMatchp("1of2" type) nodeType = "e1of2")
               (rexMatchp("1of1" type) nodeType = "e1of1")
               (rexMatchp("ChanDft" type) nodeType = "ChanDft")
               (t error("Unsupported type %L\n" type))
           )
           (if !size || size < 0 then printf("    %s w_%s[0..NBUFS];\n" nodeType label)
           else  printf("    %s w_%s[0..%d,0..NBUFS];\n" nodeType label size))
        )
        printf("\n\n")
        printf("    w_bufReset[0] = _RESET;\n")
        (foreach node nodeList
           nodeName = nth(0 node)
           label = Ru_ReplaceDotWithDash(nodeName)
           size = nth(1 node)
           type = nth(2 node)
           (if !size || size < 0 then printf("            w_%s[0] = %s.%s;\n" label inPrefix nodeName)
           else  printf("    <i:%2d:  w_%s[i,0] = %s.%s[i];>\n" size label inPrefix nodeName))
        )
        printf("\n\n")

        printf("    <n:NBUFS:\n")
        printf("        BUF_RESET bufReset[n](w_bufReset[n],w_bufReset[n+1]);\n\n")
        printf("        RAMP_RESET rampReset[n](w_bufReset[n+1],w_rampReset[n]);\n")
        (foreach node nodeList
           nodeName = nth(0 node)
           label = Ru_ReplaceDotWithDash(nodeName)
           size = nth(1 node)
           type = nth(2 node)
           (if !size || size < 0 then 
               printf("                %s mbuf_%s[n] (w_%s[n],w_%s[n+1])(Vdd,GND,w_rampReset[n]);\n"
                      type label label label)
           else  
               printf("        <i:%2d:  %s mbuf_%s[i,n] (w_%s[i,n],w_%s[i,n+1])(Vdd,GND,w_rampReset[n]);>\n"
                      size type label label label)
           )
        )
        printf("    >\n")
        printf("\n")
        (foreach node nodeList
           nodeName = nth(0 node)
           label = Ru_ReplaceDotWithDash(nodeName)
           size = nth(1 node)
           type = nth(2 node)
           (if !size || size < 0 then printf("            w_%s[NBUFS] = %s.%s;\n" label outPrefix nodeName)
           else  printf("    <i:%2d:  w_%s[i,NBUFS] = %s.%s[i];>\n" size label outPrefix nodeName))
        )
    )
    printf("\n")
    printf("  }\n")
    ;(if passthru printf("  directives { wiring = true; }\n"))
    (if directive then
        printf("  directives {\n")
        printf("      slacker_leaf = false;\n")
        (foreach node nodeList
           nodeStr = nth(0 node)
           result = Ru_NodeNameToLable(nodeStr)
           label = car(result)
           nodeDir = cadr(result)
           nodeName = nth(2 result)
           printf("      slacker_time(%s.%s) = T_%s;\n" inPrefix label label)
           printf("      slacker_time(%s.%s) = T_%s + 2.0*NBUFS;\n" outPrefix label label)
        )
        printf("  }\n")
    )

    (if !inline then
    printf("  env {\n")
    printf("    digital {\n")
    printf("      subcells {\n")
    (if !implicitReset printf("       _RESET = _L_RESET;\n"))
    printf("        rsource_%s    _(%s);\n" channelName inPrefix)
    printf("        bitbucket_%s  _(%s);\n" channelName outPrefix)
    printf("      }\n")
    printf("      directives {\n")
    result = Ru_NodeNameToLable(car(car(nodeList)))
    nodeName = nth(2 result)
    printf("        ntpc_spec(%s.%s%s.e) = 18;\n" outPrefix nodeName (if cadr(car(nodeList)) "[0]" ""))
    printf("      }\n")
    printf("    }\n")
    printf("  }\n")
    ) ;inline
    printf("}\n")
    printf("\n\n")

    (if hasCollapseSeg then
        printf("################ Channel Segments: ##################\n")
        (foreach node nodeList
           seg = nth(3 node)
           result = Ru_RangeToInt(seg)
           ss = car(result)
           se = cadr(result)
           collapseSeg = nth(5 node)
           result = 0
           (if collapseSeg then
               result = length(parseString(collapseSeg ","))
           )
           printf("%-40s: %d\n" nth(0 node) (se-ss+1-result)) 
        )
        printf("\n\n")
    )

    return(t)
  )
)

(defun Ru_DumpBundledList (entries name)
  (let (cnt)
     (if entries then
         printf("    (list\n")
         printf("        (list %s)\n" name)
         cnt = 0
         (foreach el entries
             (if mod(cnt 3) == 0 then printf("\n            "))
             printf(" %L" el)
             cnt++
         )
         printf("\n        )\n")
         printf("    )\n")
     )
  )
)

(defun GenerateBundledList (nodeList @key (inPrefix "L") (outPrefix "R"))
  (let (nodeName node size type label nodeType seg es ee ss se result
        resetBase el resetList resetStartSeg resetEndSeg
        e1of1List e1of2List e1of4List e1of2InlvList e1of4InlvList)
    printf("; auto-bundled-routing channels\n")
    printf("bundled_channels = list(    \n")
    (if t then
        (foreach node nodeList
           nodeName = nth(0 node)
           label = Ru_ReplaceDotWithDash(nodeName)
           size = nth(1 node)
           result = Ru_RangeToInt(size)
           es = car(result)
           ee = cadr(result)
           type = nth(2 node)
           seg = nth(3 node)
           result = Ru_RangeToInt(seg)
           ss = car(result)
           se = cadr(result)
           resetBase = nth(4 node)
           (if !resetBase resetBase = "Reset")
           
;           (for n ss se+1
               (if !size then
;                   wireName =  sprintf(nil "w_%s[%d]" label n)
                   wireName =  sprintf(nil "w_%s[%d..%d]" label ss se+1)
                   (cond
                       (rexMatchp("1of1" type) e1of1List = append(e1of1List list(wireName)))
                       (rexMatchp("1of2" type) e1of2List = append(e1of2List list(wireName)))
                       (rexMatchp("1of4" type) e1of4List = append(e1of4List list(wireName)))
                       (t error("Unsupported type: %L\n" type))
                   )
               else  
;                   (for i es ee
;                       wireName =  sprintf(nil "w_%s[%d,%d]" label i n)
                       wireName =  sprintf(nil "w_%s[%d..%d,%d..%d]" label es ee ss se+1)
                       (cond
                           (rexMatchp("FAST_MBUF_2_1of2" type) e1of2InlvList = append(e1of2InlvList list(wireName)))
                           (rexMatchp("FAST_MBUF_4" type) e1of4InlvList = append(e1of4InlvList list(wireName)))
                           (rexMatchp("1of1" type) e1of1List = append(e1of1List list(wireName)))
                           (rexMatchp("1of2" type) e1of2List = append(e1of2List list(wireName)))
                           (rexMatchp("1of4" type) e1of4List = append(e1of4List list(wireName)))
                           (t error("Unsupported type: %L\n" type))
                       )
;                   )
               )
;           )
        )
     )
     Ru_DumpBundledList(e1of1List "\"e1of1\"")
     Ru_DumpBundledList(e1of2List "\"e1of2\"")
     Ru_DumpBundledList(e1of4List "\"e1of4\"")
     Ru_DumpBundledList(e1of2InlvList "\"inlv_e1of2_A\" \"inlv_e1of2_B\"")
     Ru_DumpBundledList(e1of4InlvList "\"inlv_e1of4_A\" \"inlv_e1of4_B\"")
  )
)

;validate path for zero length segments and return total distance
(defun Ru_ValidatePath (points msg)
  (let (dist totalDist)
    totalDist = 0.0
    (if length(points)<=1 printf("ERROR: %s: Invalid path %L\n" msg dist points))
    (for cnt 0 length(points)-2
        dist = Ru_GetDistance(nth(cnt points) nth(cnt+1 points))
        (if dist < TrackPitch/24/4 then
            printf("ERROR: %s: Distance %f between to points is zero in path %L\n" msg dist points)
        )
      totalDist = totalDist + dist
    )

    totalDist
  )
)

(defun Ru_TrimLeadingDots (name)
  (let (entries ret)
    entries = parseString(name ".")
    ret = nth(0 entries)
    (for cnt 1 length(entries)-1
        ret = strcat(ret ".")
        ret = strcat(ret nth(cnt entries))
    )
    ret
  )
)
(defun Ru_ReplaceDotWithDash (name)
  (let (entries ret)
    entries = parseString(name ".")
    ret = nth(0 entries)
    (for cnt 1 length(entries)-1
        ret = strcat(ret "_")
        ret = strcat(ret nth(cnt entries))
    )
    ret
  )
)

(defun DeletePaths (@key (CV (geGetEditCellView)))
  (let (cnt shape)
    cnt = 0
    (foreach shape CV->shapes
        (if shape->objType=="path" then
            (dbDeleteObject shape)
            cnt=cnt+1
        )
    )
    cnt
    )
)

(defun DeleteObstructions (@key (CV (geGetEditCellView)) (all t))
  (let (cnt shape)
    cnt = 0
    (foreach shape CV->shapes
        (if shape->objType=="rect" && shape->layerName=="OBS" then
            (if all || (GetProp shape "AutoGenerated" nil) then
                (dbDeleteObject shape)
                cnt=cnt+1
            )
        )
    )
    cnt
    )
)

(defun ViasSummary (@key (CV (geGetEditCellView)))
  (let (viasCnt dblViasCnt contact)
    viasCnt = 0
    dblViasCnt = 0
    (foreach contact CV->instances
        (if contact->terminals then
            ;printf("%L\n" contact->cellName)
            viasCnt++
            (if rexMatchp("2CUT" contact->cellName) dblViasCnt++)
        )
    )
    printf("Total Vias: %d\n" viasCnt)
    printf("Total Double Vias: %d, %.2f\n" dblViasCnt (dblViasCnt*100.0)/viasCnt)

  )
)

(defun FindInstOverlaps (@key (CV (geGetEditCellView)))
  (let (cnt inst1 inst2 len i j urx1 ury1 llx1 lly1 urx2 ury2 llx2 lly2)
    cnt = 0
    len = length(CV->instances)
    printf("Total: %d\n" len)
    (if len > 200 printf("Please be patience, this might take some time\n"))
    (for i 0 len-2
        ;(if mod(i 100) == 0 printf("%d\n" i))
        (for j i+1 len-1
           ;(if mod(j 100) == 0 printf("%d-%d" i j))
            inst1 = nth(i CV->instances)
            inst2 = nth(j CV->instances)
            urx1 = xCoord( upperRight( inst1->bBox ))
            ury1 = yCoord( upperRight( inst1->bBox ))
            llx1 = xCoord( lowerLeft( inst1->bBox ))
            lly1 = yCoord( lowerLeft( inst1->bBox ))
            urx2 = xCoord( upperRight( inst2->bBox ))
            ury2 = yCoord( upperRight( inst2->bBox ))
            llx2 = xCoord( lowerLeft( inst2->bBox ))
            lly2 = yCoord( lowerLeft( inst2->bBox ))
            (if !((urx1 <= urx2 && llx1 <= urx2 && urx1 <= llx2 && llx1 <= llx2) ||
                  (urx1 >= urx2 && llx1 >= urx2 && urx1 >= llx2 && llx1 >= llx2) || 
                  (ury1 <= ury2 && lly1 <= ury2 && ury1 <= lly2 && lly1 <= lly2) ||
                  (ury1 >= ury2 && lly1 >= ury2 && ury1 >= lly2 && lly1 >= lly2) ) then
                printf("\nOverlap: %L %L %L %L" inst1->name inst1->bBox inst2->name inst2->bBox)
                cnt=cnt+1
            )
        )
    )
    cnt
    )
)

(defun Ru_DrawMarker (point)
  (let (seg)
    seg = list(Ru_OffsetPoint(point 0.0:-1*TrackPitch/24) Ru_OffsetPoint(point 0.0:1*TrackPitch/24))
    (DrawWire BusMetal67 "MARKER" TrackPitch seg)
  ) 
)

(defun ru_getColParm (parm idx)
  (let (ret)
    (if listp(parm) then 
        ret = nth(idx parm)
        (if !ret ret = nth(0 parm))
    else ret = parm)
    ret
  )
)

(defun Ru_IsPoint (point)
    (listp(point) && length(point) == 2)
)

(defun Ru_LocatePathSeg (path bufLoc startSegNum)
  (let (minOffset offset segNum lastPoint nextPoint segDir bufSegNum)
    minOffset = 9999.0
    ;find which path segment the buffer is located
    lastPoint = nth(startSegNum path)
    bufSegNum = -1
    (for segNum startSegNum (length(path)-2)
        nextPoint = nth((segNum+1) path)
        segDir = ru_getSegDir(lastPoint nextPoint)
        ;printf("SegNum %d bufSegNum %d segDir %L minOffset %f lastPoint %L nextPoint %L offX %f offY %f\n"
        ;    segNum bufSegNum segDir minOffset lastPoint nextPoint 
        ;    abs(xCoord(bufLoc)-xCoord(lastPoint)) abs(yCoord(bufLoc)-yCoord(lastPoint)))
        (cond
          (segDir == "HOR" && 
           Ru_IsBetween(xCoord(lastPoint) xCoord(bufLoc) xCoord(nextPoint))
            offset = abs(yCoord(bufLoc)-yCoord(lastPoint))
            (if offset < minOffset then
                minOffset = offset
                bufSegNum = segNum
            )
          ) 
          (segDir == "VERT" && 
           Ru_IsBetween(yCoord(lastPoint) yCoord(bufLoc) yCoord(nextPoint))
            offset = abs(xCoord(bufLoc)-xCoord(lastPoint))
            (if offset < minOffset then
                minOffset = offset
                bufSegNum = segNum
            )
          )
        )
        lastPoint = nextPoint
     )
    bufSegNum
  )
)

;handle zero length point, assume some direction base on previous direction
(defun Ru_ClassifyPoints (points)
  (let (a b types lastDirV)
    a = (car points)
    b = (cadr points)
    (cond ((almostEqual (car  a) (car  b)) types=(append types (list "EV")) lastDirV = t)
          ((almostEqual (cadr a) (cadr b)) types=(append types (list "EH")) lastDirV = nil))
    (for i 1 (length points)-2
         a = (nth i-1 points)
         b = (nth i   points)
         (cond ((almostEqual (car  a) (car  b)) && (almostEqual (cadr a) (cadr b)) types=(append types (list (if lastDirV "VH" "HV"))))
               ((almostEqual (car  a) (car  b)) types=(append types (list "VH")) lastDirV = nil)
               ((almostEqual (cadr a) (cadr b)) types=(append types (list "HV")) lastDirV = t))
         )
    a = (nth (length points)-2 points)
    b = (nth (length points)-1 points)
    (cond ((almostEqual (car  a) (car  b)) types=(append types (list "VE")))
          ((almostEqual (cadr a) (cadr b)) types=(append types (list "HE")))
          )
    types
    )
  )

(defun OffsetPath (points offsetX offsetY @key (firstOffset nil) (lastOffset nil)) 
  (let (newpoints index x y hv offX offY nextOffsetX curOffsetX nextOffsetY curOffsetY
       (types (Ru_ClassifyPoints points)))
     newpoints = nil
     index = 0
     numPoints = length(points)
    (foreach point points
        x = (car point)
        y = (cadr point)
        hv = (nth index types)
        nextOffsetX = offsetX
        curOffsetX = offsetX
        nextOffsetY = offsetY
        curOffsetY = offsetY
        (if index <= 1 && firstOffset then
            curOffsetX = firstOffset
            curOffsetY = firstOffset
        )
        (if index >= numPoints-2 && lastOffset then
            nextOffsetX = lastOffset
            nextOffsetY = lastOffset
           (if index >= numPoints-1 then
               curOffsetX = nextOffsetX
               curOffsetY = nextOffsetY
           )
        )
        (cond
          ( hv == "EH"  || hv == "HV"
            offX = nextOffsetX
            offY = curOffsetY)
          ( hv == "VE" || hv == "HE"
                offX = curOffsetX
                offY = curOffsetY)
          (t
                offX = curOffsetX
                offY = nextOffsetY
          )
        )
        x = x+(if (and hv!="HE" hv!="EH") offX 0)
        y = y+(if (and hv!="VE" hv!="EV") offY 0)
        newpoints = (append newpoints (list (list x y)))
        index = index+1
    )
    newpoints
  )
)

(defun Ru_GetPathNode (points layerPatternList nodeName 
                      @key (flipSegX nil) (flipSegY nil)
                      (firstPattern nil) (lastPattern nil)
                      (firstSwitchPattern nil) (lastSwitchPattern nil) )
  (let (node offset newpoints index offsetX offsetY x y hv numPoints flipX flipY el temp
       (types (Ru_ClassifyPoints points)))
    numPoints = length(points)
    (foreach node_offset layerPatternList
         node = (car node_offset)
        (if (node == nodeName) then
             offset = (caddr node_offset)
             newpoints = nil
             index = 0
            (foreach point points
                x = (car point)
                y = (cadr point)
                hv = (nth index types)
                flipX = nil
                flipY = nil
                ; no alternate pattern
                nextOffset = offset
                curOffset = offset
                (if index <= 1 && firstPattern then
                    (foreach node_offset firstPattern
                       (if ((car node_offset) == nodeName) then
                           curOffset = (caddr node_offset)
                       )
                    )
                )
                (if index >= 1 && index <= 2 && firstSwitchPattern then
                    (foreach node_offset firstSwitchPattern
                       (if ((car node_offset) == nodeName) then
                           temp = (caddr node_offset)
                           (if index == 1 nextOffset = temp)
                           (if index == 2 curOffset = temp)
                       )
                    )
                )
                (if index >= numPoints-2 && lastPattern then
                    (foreach node_offset lastPattern
                       (if ((car node_offset) == nodeName) then
                           nextOffset = (caddr node_offset)
                           (if index >= numPoints-1 curOffset = nextOffset)
                       )
                    )
                )
                (if index >= numPoints-3 && index <= numPoints-2 && lastSwitchPattern then
                    (foreach node_offset lastSwitchPattern
                       (if ((car node_offset) == nodeName) then
                           temp = (caddr node_offset)
                           (if index == numPoints-3 nextOffset = temp)
                           (if index == numPoints-2 curOffset = temp)
                       )
                    )
                )
                (cond
                  ( hv == "EH"  || hv == "HV"
                    offsetX = nextOffset
                    offsetY = curOffset)
                  ( hv == "VE" || hv == "HE"
                        offsetX = curOffset
                        offsetY = curOffset)
                  (t
                        offsetX = curOffset
                        offsetY = nextOffset
                  )
                )
                (if flipSegX then
                    (foreach el flipSegX
                        (if el == index || (el+1) == index flipX = t)
                    )
                )
                (if flipSegY then
                    (foreach el flipSegY
                        (if el == index || (el+1) == index flipY = t)
                    )
                )
                x = x+(if flipX -1 1)*(if (and hv!="HE" hv!="EH") offsetX 0)
                y = y+(if flipY -1 1)*(if (and hv!="VE" hv!="EV") offsetY 0)
                newpoints = (append newpoints (list (list x y)))
                index = index+1
            )
        )
    )
    newpoints
  )
)

(defun ru_GetBufSize (inst)
  (let (sizeX sizeY)
    (if inst then
      sizeY = yCoord(upperRight(inst->bBox))-yCoord(lowerLeft(inst->bBox))
      sizeX = xCoord(upperRight(inst->bBox))-xCoord(lowerLeft(inst->bBox))
      sizeX = ceiling(sizeX/TrackPitch-TrackPitch/48)*TrackPitch
    else
      sizeX = 2*TrackPitch
      sizeY = 4*TrackPitch
    )
    list(sizeX sizeY)
  )
)

;Stack multiple buffer columns along a path
;colLeftList and colRightList is mainly for vertical path
;horizontal path just round robin the two list
(defun Ru_StackBuffer (path colLeftList colRightList reserveX reserveY 
                       @key (offsetX 0.0) (offsetY 0.0) (bufPrefix nil)
                      (flipSegPattern nil) (reserveGap nil) (bufferPath nil)
                      (balance nil) (bufSegNum 0) )
  (prog (bufLoc bufOffset curPoint lastPoint nextPoint pathSegIdx dir colPath result advance advanceOff sizeOff
        orient inst bufSizeX bufSizeY lastBufSizeX lastBufSizeY maxBufY reserveDist bufOffX x y gapLow gapHi
        leftCnt rightCnt leftListLen rightListLen segNum segNumOff valLow valHi cnt entry bufLen leftRightOrder position)
    (if debug >= 3 printf("#############Ru_StackBuffer path %L offset %f %f reserve %f %f \n" 
                          path offsetX offsetY reserveX reserveY))
    (if length(path) <= 1 error("Path %L must have at least 2 points\n" path))
    (foreach point path
        colPath = append(colPath list(Ru_AlignX(xCoord(point)):Ru_AlignY(yCoord(point))))
    )
    ;dbCreatePath((geGetEditCellView)  list("M7" "bus") colPath 0.8)
    segNumOff = 0

    (if flipSegPattern || reserveGap then
        (if !bufferPath error("Must have valid bufferPath option when passing flipSegPattern or reserveGap option\n"))
        segNumOff = car(ru_getPathSegment(car(path) bufferPath))
        (if debug >= 1 printf("Buffer segment %L start from segment %d of bufferPath %L\n" path segNumOff bufferPath))
    )
    pathSegIdx = 0
    curPoint = car(colPath)
    lastPoint = curPoint
    nextPoint = nth((pathSegIdx+1) colPath)
    dir = ru_getSegDir(lastPoint nextPoint ?detail t)
    orient = "R0"
    lastBufSizeX = 4*TrackPitch
    lastBufSizeY = 4*TrackPitch
    bufOffX = 0.0
    leftListLen = length(colLeftList)
    rightListLen = length(colRightList)
    leftCnt = 0
    rightCnt = 0
    colPos = "LEFT"
    (cond
      (dir == "DOWN" || dir == "UP"
        reserveDist = reserveY + lastBufSizeY
      )
      (dir == "LEFT" || dir == "RIGHT"
        reserveDist = reserveX + lastBufSizeX
      )
    )
    maxBufY = lastBufSizeX
    advance = t
    inst = nil
    (while leftCnt < leftListLen || rightCnt < rightListLen
         result = ru_GetBufSize(inst)
         lastBufSizeX = car(result) 
         lastBufSizeY = cadr(result)

;FIXME Need to redo this logic a little better, have curPos for 2 columns and check each individually
CHECK_RESERVE_GAP_LABEL = t
(while  CHECK_RESERVE_GAP_LABEL
;(if bufSegNum == 1 printf("#### %L colPos %L dir %L\n" bufName colPos dir))
advanceNextSeg = nil
        (if reserveGap && !(colPos == "RIGHT" && (dir == "DOWN" || dir == "UP")) then
            (foreach entry reserveGap
                segNum = segNumOff + pathSegIdx
;(if bufSegNum == 1 printf("######## %L segNum %L pathSegIdx: %L %L\n" bufName segNum pathSegIdx entry))
                (if car(entry) == segNum then
                    (cond
                      (dir=="LEFT" advanceOff = -2*TrackPitch:0 sizeOff = advanceOff sizeOffHi = advanceOff )
                      (dir=="RIGHT" advanceOff = 2*TrackPitch:0 sizeOff = advanceOff sizeOffHi = advanceOff)
                      (dir=="DOWN" advanceOff = 0:-2*TrackPitch sizeOff = 0:-4*TrackPitch sizeOffHi = 0:(if balance -6 -4)*TrackPitch)
                      (dir=="UP" advanceOff = 0:2*TrackPitch sizeOff = 0:4*TrackPitch sizeOffHi = 0:(if balance 6 4)*TrackPitch)
                    )
                    result = Ru_OffsetPoint(curPoint sizeOff)
                    valLow = (if dir=="LEFT"||dir=="RIGHT" xCoord(result) yCoord(result))
                    result = Ru_OffsetPoint(result sizeOffHi)
                    valHi = (if dir=="LEFT"||dir=="RIGHT" xCoord(result) yCoord(result))

                    cnt = 0
                    gapLow = (nth(1 entry)+TrackPitch/24) 
                    gapHi = (nth(2 entry)-TrackPitch/24)
;(if bufSegNum == 1  printf("#### %L GAP %L: %L [bufRange: %L %L] %L\n" bufName entry gapLow valLow valHi gapHi))
                    (while Ru_IsBetween(gapLow valLow gapHi) || Ru_IsBetween(gapLow valHi gapHi) || Ru_IsBetween(gapLow (valLow+valHi)/2 gapHi)
                        cnt++
                        curPoint = Ru_OffsetPoint(curPoint advanceOff)
                        (if debug >=1 printf("%L: Next buffer is in the reserve gap %L, advance #%d to %L\n" bufName entry cnt curPoint))
                        result = Ru_OffsetPoint(curPoint sizeOff)
                        valLow = (if dir=="LEFT"||dir=="RIGHT" xCoord(result) yCoord(result))
                        result = Ru_OffsetPoint(result sizeOffHi)
                        valHi = (if dir=="LEFT"||dir=="RIGHT" xCoord(result) yCoord(result))
;printf("#### %L GAP %L: %L [%L %L] %L\n" bufName entry gapLow valLow valHi gapHi)
advanceNextSeg = t
                    )
                )
            )
            (if nextPoint remDist = Ru_GetDistance(curPoint nextPoint))
            (if debug >= 2 printf("Ru_StackBuffer: ADVANCE %L pathSegIdx %d reserveDist %f lastPoint %L curPoint %L nextPoint %L dir %s remDist %f bufOffX %f\n" 
                   bufName pathSegIdx reserveDist lastPoint curPoint nextPoint dir remDist bufOffX))
        )


        ;next buffer location is on next path
        (if nextPoint remDist = Ru_GetDistance(curPoint nextPoint))
        (if debug >= 2 printf("Ru_StackBuffer: %L pathSegIdx %d reserveDist %f lastPoint %L curPoint %L nextPoint %L dir %s remDist %f bufOffX %f\n" 
               bufName pathSegIdx reserveDist lastPoint curPoint nextPoint dir remDist bufOffX))
;(if bufName == "mbuf_LAG_MASK[7][10]" || bufName == "mbuf_LAG_MASK[2][10]" printf("Ru_StackBuffer: %L pathSegIdx %d reserveDist %f lastPoint %L curPoint %L nextPoint %L dir %s remDist %f bufOffX %f\n" 
;               bufName pathSegIdx reserveDist lastPoint curPoint nextPoint dir remDist bufOffX))
        bufLen = (if dir=="LEFT"||dir=="RIGHT" lastBufSizeX lastBufSizeY)
        (while remDist < (reserveDist+bufLen) && nextPoint 
            lastPoint = nextPoint
            nextPoint = nth((pathSegIdx+2) colPath)
            (if debug >= 2 printf("Ru_StackBuffer: Advance to pathSegIdx %d lastPoint %L curPoint %L nextPoint %L dir %s remDist %f\n" 
                       pathSegIdx lastPoint curPoint nextPoint dir remDist))
            (if nextPoint then
advanceNextSeg = t
                pathSegIdx++
                dir = ru_getSegDir(lastPoint nextPoint ?detail t)
                result = AdvanceCurPoint(curPoint lastPoint dir reserveX reserveY lastBufSizeY lastBufSizeX)
                curPoint = car(result)
                reserveDist = cadr(result)
                remDist = Ru_GetDistance(curPoint nextPoint)
                (if debug >=2 printf("Ru_StackBuffer: Advance to pathSegIdx %d lastPoint %L curPoint %L nextPoint %L dir %s remDist %f\n" 
                       pathSegIdx lastPoint curPoint nextPoint dir remDist))
            )
            bufLen = (if dir=="LEFT"||dir=="RIGHT" lastBufSizeX lastBufSizeY)
        )
CHECK_RESERVE_GAP_LABEL = nil
(if advanceNextSeg && reserveGap then
    (if debug >= 1 printf("CurPos: %L(%L-%L) Advance to next segment so go back and check for reserve gap\n" curPoint dir colPos))
    CHECK_RESERVE_GAP_LABEL = t
)
) ;while CHECK_RESERVE_GAP_LABEL

        ;printf("%s Loc %L sizeXY %f %f tracks\n" bufName curPoint lastBufSizeX/TrackPitch lastBufSizeY/TrackPitch)
(if debug >=2 printf("leftCnt %d leftListLen %d rightCnt %d rightListLen %d\n" 
                    leftCnt leftListLen rightCnt rightListLen))

        ;find next location
        orient = "R0"
segNum = segNumOff + pathSegIdx
        leftRightOrder = t ; alignment of left or right column
        position = ""; buffer to the right or top
        (cond
          (dir == "DOWN" || dir == "UP"
            reserveDist = reserveY + lastBufSizeY
            (if colPos == "RIGHT" then
                position = "RIGHT"
                ;don't advance just offset right
                advance = nil
                bufOffset = (offsetX+4*TrackPitch):(if balance 2*TrackPitch 0.0)
                bufOffX = bufOffX+lastBufSizeX
                bufName = nth(rightCnt colRightList)
                rightCnt++
                (if !bufName && balance then
                    ;place the left to the right to balance the column
                    bufName = nth(leftCnt colLeftList)
                    leftCnt++
                    leftRightOrder = nil
                )
                colPos = "LEFT"
;printf("%L: RIGHT segNum %d flipSeg %L\n" bufName segNum member(segNum flipSegPattern))
                (if member(segNum flipSegPattern) then
                    orient = (if dir=="DOWN" "MX" "R0")
                    bufOffset = -(offsetX+4*TrackPitch):(if balance 2*TrackPitch 0.0) ;subtract 1 more due to round down
                    (if bufName && substring(bufName 5 1) == "r" then ;FIXME hardcoded
                        ;printf("%s channel direction is reversed\n" bufName)
                        bufOffset = Ru_OffsetPoint(bufOffset 0:(if dir=="DOWN" -4 4)*TrackPitch)
                        orient = (if dir=="DOWN" "R0" "MX")
                    )
                else
                    (if bufName && substring(bufName 5 1) == "r" then ;FIXME hardcoded
                        ;printf("%s channel direction is reversed\n" bufName)
                        bufOffset = Ru_OffsetPoint(bufOffset 0:(if dir=="DOWN" -4 4)*TrackPitch)
                        orient = (if dir=="DOWN" "MY" "R180")
                    else
                        orient = (if dir=="DOWN" "R180" "MY"))
                )
            else
                position = "LEFT"
                advance = t
                advanceOff = 0.0:(if dir=="DOWN" -maxBufY maxBufY)
                ;curPoint = Ru_OffsetPoint(curPoint 0.0:(if dir=="DOWN" -maxBufY maxBufY))
                bufOffset = offsetX:0.0
                bufOffX = lastBufSizeX
                bufName = nth(leftCnt colLeftList)
                leftCnt++
                (if !bufName && balance then
                    ;place the left to the right to balance the column
                    bufName = nth(rightCnt colRightList)
                    rightCnt++
                    leftRightOrder = nil
                )
                colPos = "RIGHT"
;printf("%L: segNum %d flipSeg %L\n" bufName segNum member(segNum flipSegPattern))
                (if member(segNum flipSegPattern) then
                   orient = (if dir=="DOWN" "R180" "MY")
                   bufOffset = -(offsetX+0*TrackPitch):0.0
;printf("%L: LEFT bufOffset %L offsetX %L\n" bufName bufOffset offsetX)
                    (if bufName && substring(bufName 5 1) == "r" then ;FIXME hardcoded
                        ;printf("%s channel direction is reversed\n" bufName)
                        bufOffset = Ru_OffsetPoint(bufOffset 0:(if dir=="DOWN" -4 4)*TrackPitch)
                        orient = (if dir=="DOWN" "MY" "R180")
                    )
                else
                    (if bufName && substring(bufName 5 1) == "r" then ;FIXME hardcoded
                        ;printf("%s channel direction is reversed\n" bufName)
                        bufOffset = Ru_OffsetPoint(bufOffset 0:(if dir=="DOWN" -4 4)*TrackPitch)
                        orient = (if dir=="DOWN" "R0" "MX")
                    else orient = (if dir=="DOWN" "MX" "R0"))
                )
            )
            (if bufName then
                inst = (dbFindAnyInstByName (geGetEditCellView) (if bufPrefix sprintf(nil "%s%s" bufPrefix bufName) bufName) )
                result = ru_GetBufSize(inst)
                bufSizeX = car(result) 
                bufSizeY = cadr(result)
                (if advance then maxBufY = bufSizeY
                else (if maxBufY < bufSizeY maxBufY = bufSizeY))
                ;printf("%s: %L %L %L advance %L\n" bufName bufSizeY lastBufSizeY maxBufY advance)
            )
          )
          (dir == "LEFT" || dir == "RIGHT"
            (if colPos == "LEFT" then
                bufName = nth(leftCnt colLeftList)
                leftCnt++
                colPos = "RIGHT"
                (if !bufName then
                    bufName = nth(rightCnt colRightList)
                    rightCnt++
                )
            else
                bufName = nth(rightCnt colRightList)
                rightCnt++
                colPos = "LEFT"
                (if !bufName then
                    bufName = nth(leftCnt colLeftList)
                    leftCnt++
                )
            )
;printf("%s: dir %L segNum %L %L %L flip %L orient %L \n" bufName dir segNum segNumOff pathSegIdx flipSegPattern orient)
            (if bufName then
                inst = (dbFindAnyInstByName (geGetEditCellView) (if bufPrefix sprintf(nil "%s%s" bufPrefix bufName) bufName) )
                result = ru_GetBufSize(inst)
                bufSizeX = car(result) 
                bufSizeY = cadr(result)
            )

            ;printf("%s: %f %f advance %L\n" bufName bufSizeY lastBufSizeY advance)
            (if advance && bufSizeY <= 2.01*TrackPitch && lastBufSizeY <= 2.01*TrackPitch then
                ;for e1of1
                position = "TOP"
                reserveDist = reserveX + lastBufSizeX
                ;don't advance just offset 
                advance = nil
                bufOffset = Ru_OffsetPoint(0.0:offsetY (if dir=="LEFT" -TrackPitch TrackPitch):2*TrackPitch)
            else
                position = "BOTTOM"
                advance = t
                advanceOff = (if dir=="LEFT" -lastBufSizeX lastBufSizeX):0.0
                ;curPoint = Ru_OffsetPoint(curPoint (if dir=="LEFT" -lastBufSizeX lastBufSizeX):0.0)
                reserveDist = reserveX + lastBufSizeX
                bufOffset = 0.0:offsetY
            )
            (if member(segNum flipSegPattern) then
               orient = (if dir == "LEFT" "R180" "MX")
               bufOffset = -(offsetX-1*TrackPitch):0.0
                (if bufName && substring(bufName 5 1) == "r" then ;FIXME hardcoded
                    ;printf("%s channel direction is reversed\n" bufName)
                    bufOffset = Ru_OffsetPoint(bufOffset (if dir=="LEFT" -2 2)*TrackPitch:0)
                    orient = (if dir=="LEFT" "R180" "MX")
                )
            else
                (if bufName && substring(bufName 5 1) == "r" then ;FIXME hardcoded
                    ;printf("%s channel direction is reversed\n" bufName)
                    bufOffset = Ru_OffsetPoint(bufOffset (if dir=="LEFT" -2 2)*TrackPitch:0)
                    orient = (if dir=="LEFT" "R0" "MY")
                else orient = (if dir == "LEFT" "MY" "R0"))
            )
          )
        )

        (if advance then
            (if !advanceOff error("advanceOff is nil whereas advance is set\n"))
            curPoint = Ru_OffsetPoint(curPoint advanceOff)
/*
            (foreach entry reserveGap
                segNum = segNumOff + pathSegIdx
                ;printf("#### %L segNum %L pathSegIdx: %L %L\n" bufName segNum pathSegIdx entry)
                (if car(entry) == segNum then
                    val = (if dir=="LEFT"||dir=="RIGHT" xCoord(curPoint) yCoord(curPoint))
                    cnt = 0
                    ;printf("#### %L GAP: %L %L %L\n" bufName nth(1 entry) val nth(2 entry))
                    (while Ru_IsBetween(nth(1 entry) val nth(2 entry))
                        cnt++
                        (if debug >=1 printf("%L is in the reserve gap, advance #%d\n" bufName cnt))
                        curPoint = Ru_OffsetPoint(curPoint advanceOff)
                        val = (if dir=="LEFT"||dir=="RIGHT" xCoord(curPoint) yCoord(curPoint))
                    )
                )
            )
            advanceOff = nil ;reset
            (if nextPoint remDist = Ru_GetDistance(curPoint nextPoint))
            (if debug >= 2 printf("Ru_StackBuffer: ADVANCE %L pathSegIdx %d reserveDist %f lastPoint %L curPoint %L nextPoint %L dir %s remDist %f bufOffX %f\n" 
                   bufName pathSegIdx reserveDist lastPoint curPoint nextPoint dir remDist bufOffX))
*/

            bufLen = (if dir=="LEFT"||dir=="RIGHT" bufSizeX bufSizeY)
            (if bufName && nextPoint && remDist < (reserveDist+bufLen-4*TrackPitch) then
                printf("WARN: %L Reserve gap %L is not met %L in tracks. bufLen %L\n" bufName ToTrack(remDist) ToTrack(reserveDist) ToTrack(bufLen))
            )
        )

        (if debug >= 2 printf("Ru_StackBuffer: %L pathSegIdx %d curPoint %L bufOffset %L nextColPos %s dir %s remDist %f lastBufSize %f %f\n" 
               bufName pathSegIdx curPoint bufOffset colPos dir remDist lastBufSizeX lastBufSizeY))
        (if bufName then
            bufLoc = Ru_OffsetPoint(curPoint bufOffset)
            x = Ru_AlignX(xCoord(bufLoc))
            y = Ru_AlignY(yCoord(bufLoc))
            (if !almostEqual(xCoord(bufLoc) x)  then
                printf("ERROR: Ru_StackBuffer: %s is not aligned in xCoord got %f expect %f\n"
                    bufName xCoord(bufLoc) y )
            )
            (if !almostEqual(yCoord(bufLoc) y) then
                printf("ERROR: Ru_StackBuffer: %s is not aligned in yCoord got %f expect %f\n"
                    bufName yCoord(bufLoc) y )
            )
            (if inst then
              inst->xy = bufLoc
              inst->orient = orient
              (dbReplaceProp inst "LeftRightAlign" "boolean" leftRightOrder)
              (dbReplaceProp inst "Position" "string" position)
            )
;(if leftCnt >= 2 error("Loc: %L" bufOffset))
            ;stack the ramp reset next to buf reset
            (if leftCnt == 1 && rexMatchp("bufReset" bufName) && 
                rexMatchp("rampReset" nth(1 colLeftList)) then
                ;printf("%L %L\n" bufName nth(1 colLeftList))
                inst = (dbFindAnyInstByName (geGetEditCellView) (if bufPrefix sprintf(nil "%s%s" bufPrefix nth(1 colLeftList)) nth(1 colLeftList)))
                leftCnt++
                (if inst then
                  inst->xy = Ru_OffsetPoint(bufLoc (if member(segNum flipSegPattern) -1 1)*TrackPitch:0)
                  (if orient inst->orient = orient)
                )
                (if (dir=="LEFT" || dir=="RIGHT" ) then
                    curPoint = Ru_OffsetPoint(curPoint (if dir=="LEFT" -TrackPitch TrackPitch):0.0)
                )
            )
        )
    )
    (if !nextPoint printf("WARN:  Visual inspect for any buffer overlap. Path segment might be too short for all the buffers: %L.\n"
                          list(leftList rightList)))
  return(t)
  )
)

(defun AdvanceCurPoint (curPoint lastPoint dir reserveX reserveY lastBufSizeY lastBufSizeX)
    (cond
      (dir == "DOWN"
        curPoint = Ru_OffsetPoint(lastPoint 0.0:-reserveY)
        curPoint = xCoord(curPoint):Ru_AlignY(yCoord(curPoint))
        reserveDist = reserveY + lastBufSizeY
      )
      (dir == "UP"
        curPoint = Ru_OffsetPoint(lastPoint 0.0:reserveY)
        curPoint = xCoord(curPoint):Ru_AlignY(yCoord(curPoint))
        reserveDist = reserveY + lastBufSizeY
      )
      (dir == "LEFT"
        curPoint = Ru_OffsetPoint(lastPoint -reserveX:0.0)
        curPoint = Ru_AlignX(xCoord(curPoint)):yCoord(curPoint)
        reserveDist = reserveX + lastBufSizeX
      )
      (dir == "RIGHT"
        curPoint = Ru_OffsetPoint(lastPoint reserveX:0.0)
        curPoint = Ru_AlignX(xCoord(curPoint)):yCoord(curPoint)
        reserveDist = reserveX + lastBufSizeX
      )
    )
    list(curPoint reserveDist)
)

;replace - return only entries matching match and replace them with replace
;retAll - return all entries with match and replace option
(defun ModifyChannel (channel @key (flipOffset nil) (filterGnd nil) (filterOffset nil)
                                   (offset nil) (normalizeTrackOffset nil) 
                                   (match nil) (replace nil) (retAll nil) (exclude nil) 
                                   (trimLeadingDots nil) (trimLeadingExtraDots nil)
                                   (forceLayer nil) (clearLayer nil)
                                   (combine nil) (combineOffset nil) (rexMagicMode nil))
  (let (ret newOff node add layer nodeName el layerIdx baseOff trackIdx)
    rexMagic(rexMagicMode)
    (if combine&&!combineOffset error("Must specify combineOffset option when using combine option\n"))
    (if !combine&&combineOffset error("Must specify combine option when using combineOffset option\n"))

    (if replace rexCompile(match))
    (foreach el channel
        nodeName = nth(0 el)
        newOff =  nth(2 el)
        layer = nth(3 el)
        (if offset newOff = newOff+offset)
        (if flipOffset newOff = -newOff)
        (if normalizeTrackOffset then 
            newOff = newOff + TrackPitch/2
            baseOff = RoundDownTrack(newOff ?numTrack normalizeTrackOffset)
            newOff = newOff - baseOff
            layerIdx = mod(floor(ToTrack(newOff)) 3)
            trackIdx = floor(ToTrack(newOff))
            ;every 3 tracks into one track
            newOff = (trackIdx/3)*TrackPitch + (newOff -  RoundDownTrack(newOff))
            (if trackIdx == normalizeTrackOffset-1 then
                (cond
                  (layerIdx == 0 layer = BusMetal23)
                  (layerIdx == 1 layer = BusMetal67)
                  (t layer = BusMetal45)
               )
            else
                (cond
                  (layerIdx == 0 layer = BusMetal67)
                  (layerIdx == 1 layer = BusMetal45)
                  (t layer = BusMetal23)
                )
             ) 

;            (if almostEqual(RoundDownTrack(newOff+TrackPitch/2) 0.0) && replaceLayer then
;                layer = replaceLayer
;            else newOff = newOff - TrackPitch)
;(if nth(2 el) < 9*TrackPitch printf("%L Offset %L newOff %L TrackIdx %L baseOff %L layerIdx %L trackIdx %L baseOff %L normalize %L\n" nodeName nth(2 el) newOff ToTrack(newOff) baseOff layerIdx trackIdx  mod(floor(ToTrack(baseOff)) 2) normalizeTrackOffset))
            newOff = newOff - TrackPitch/2
        )
        (if forceLayer layer = forceLayer)
        (if clearLayer layer = nil)
        (if trimLeadingDots||trimLeadingExtraDots nodeName = Ru_TrimLeadingDots(nodeName))
        (if trimLeadingExtraDots && nodeName != "GND" nodeName = strcat("." nodeName)) ;only one leading dot
        add = retAll
        (if match then
          (if rexMatchp(match nodeName) then
              (if replace then
                  nodeName = rexReplace( nodeName replace 0)
              )
              add = t
          )
        else
          add = t 
        )
        (if filterOffset then
;printf("filterOff: Add %L %L %L %L\n" Ru_IsBetween(car(filterOffset) newOff cadr(filterOffset)) car(filterOffset) newOff cadr(filterOffset))
            add = Ru_IsBetween(car(filterOffset) newOff cadr(filterOffset))
        )
        (if exclude then
          (if rexMatchp(exclude nodeName) then
              add = nil
          )
        )
        (if filterGnd && nodeName == "GND" add = nil)
        (if add ret = append(ret list(list(nodeName nth(1 el) newOff layer))))
    )

    (foreach el combine
        nodeName = nth(0 el)
        newOff =  nth(2 el)
        newOff = newOff+combineOffset
        layer = nth(3 el)
        (if forceLayer layer = forceLayer)
        (if flipOffset newOff = -newOff)
        (if trimLeadingDots||trimLeadingExtraDots nodeName = Ru_TrimLeadingDots(nodeName))
        (if trimLeadingExtraDots && nodeName != "GND" nodeName = strcat("." nodeName))
        add = nil
        (if match then
          (if rexMatchp(match nodeName) then
              (if replace then
                  nodeName = rexReplace( nodeName replace 0)
              )
              add = t
          )
        else
          add = t 
        )
        (if exclude then
          (if rexMatchp(exclude nodeName) then
              add = nil
          )
        )
        (if filterGnd && nodeName == "GND" add = nil)
        (if add ret = append(ret list(list(nodeName nth(1 el) newOff layer))))
    )
    rexMagic(t)
    ret
  )
)

(defun GetPinStartPath (path)
  (let (a pin)
      a = car(path)
      dir = ru_getSegDir(a cadr(path) ?detail t)
      (cond 
        (dir == "UP" pin = list(a Ru_OffsetPoint(a 0:TrackPitch)))
        (dir == "DOWN" pin = list(a Ru_OffsetPoint(a 0:-TrackPitch)))
        (dir == "LEFT" pin = list(a Ru_OffsetPoint(a -TrackPitch:0)))
        (dir == "RIGHT" pin = list(a Ru_OffsetPoint(a TrackPitch:0)))
      )
    pin
  )
)

(defun GetPinEndPath (path)
  (let (a pin len)
      len = length(path)
      a = nth(len-1 path)
      dir = ru_getSegDir(a nth(len-2 path) ?detail t)
      (cond 
        (dir == "UP" pin = list(a Ru_OffsetPoint(a 0:TrackPitch)))
        (dir == "DOWN" pin = list(a Ru_OffsetPoint(a 0:-TrackPitch)))
        (dir == "LEFT" pin = list(a Ru_OffsetPoint(a -TrackPitch:0)))
        (dir == "RIGHT" pin = list(a Ru_OffsetPoint(a TrackPitch:0)))
      )
    pin
  )
)

(defun Ru_CreatePinPath (points busPattern busPrefix)
  (let (pathL)
    pathL = dbCreatePath((geGetEditCellView)  list("M6" "bus") points 0.12)
    (dbReplaceProp pathL "BusArrayPitch" "float" 2.88)
    (dbReplaceProp pathL "BusPattern" "string" busPattern)
    (dbReplaceProp pathL "BusPrefix" "string" busPrefix)
    (dbReplaceProp pathL "BusPin" "boolean" t)
    (dbReplaceProp pathL "BusFlipX" "boolean" nil)
    (dbReplaceProp pathL "BusFlipY" "boolean" nil)
    (dbReplaceProp pathL "BusStartVias" "boolean" nil)
    (dbReplaceProp pathL "BusEndVias" "boolean" nil)
    (dbReplaceProp pathL "BusDoubleVias" "boolean" t)
    pathL
  )
)

(defun LoadBusPattern (filenameList)
  (let (DFII_dir buspatternfile )
    DFII_dir = ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
    (printf "loading channel pattern %L ... ", filenameList)
    (if !listp(filenameList) filenameList = list(filenameList))
    (foreach filename filenameList
        buspatternfile = (sprintf nil "%s/chip/alta/route/buspattern/%s" DFII_dir filename)
        (if (load buspatternfile) then
            (printf "%s loaded.\n" filename)
        else (printf "ERROR: Failed loading %s.\n" filename))
    )
 )
)

(defun LoadPinPattern (filenameList)
  (let (DFII_dir pinpatternfile busLen cnt)
    busLen = length(busPatterns)
    DFII_dir = ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
    (printf "loading pin pattern %L ... ", filenameList)
    (if !listp(filenameList) filenameList = list(filenameList))
    (foreach filename filenameList
        pinpatternfile = (sprintf nil "%s/%s" DFII_dir filename)
        (if (load pinpatternfile) then
            (printf "%s loaded.\n" filename)
        else (printf "ERROR: Failed loading %s.\n" filename))
    )
    printf("New pattern loaded: \n")
    (for cnt busLen length(busPatterns)-1
        printf("%L \n" car(nth(cnt busPatterns)))
    )
    printf("\n")
 )
)




(defun ru_getPathSegment (loc path @key (detail nil))
    (if boundp('debug) && debug >= 3 printf("getPathSegment loc %L path %L\n" loc path))
    (let (locSeg dir pathSegIdx startPoint endPoint)
        locSeg = -1
        dir = "UNKNOWN"
        pathSegIdx = 0
        (while locSeg < 0 && pathSegIdx < (length(path)-1)
            startPoint = nth(pathSegIdx path)
            endPoint = nth((pathSegIdx+1) path)
            (if almostEqual(xCoord(startPoint) xCoord(endPoint)) then
                (if detail then dir = (if yCoord(startPoint) < yCoord(endPoint) "UP" "DOWN")
                 else dir = "VERT")
                (if boundp('debug) && debug >= 4 printf("a. Path#%d segment is %s\n" pathSegIdx dir))
                (if almostEqual(xCoord(startPoint) xCoord(loc)) &&
                    Ru_IsBetween(yCoord(startPoint) yCoord(loc) yCoord(endPoint)) locSeg = pathSegIdx)
            else
                (if detail then dir = (if xCoord(startPoint) < xCoord(endPoint) "RIGHT" "LEFT")
                 else dir = "HOR")
                (if boundp('debug) && debug >= 4 printf("a. Path#%d segment is %s\n" pathSegIdx dir))
                (if almostEqual(yCoord(startPoint) yCoord(loc)) &&
                    Ru_IsBetween(xCoord(startPoint) xCoord(loc) xCoord(endPoint)) locSeg = pathSegIdx)
            )
            pathSegIdx++
        )
        (if boundp('debug) && debug >= 3 printf("getPathSegment return pathSeg %d dir %s\n" locSeg dir))
        list(locSeg dir)
    )
)

(defun ru_getNextSegment (lastBufLoc path bufDist guideX guideY sramCfg termDist) 
    (if boundp('debug) && debug >= 3 printf("getNextSegment lastBufLoc %L bufDist %L path %L\n" lastBufLoc bufDist path))
    (if !path || !bufDist error("ru_getNextSegment Nil parameter path %L bufDist\n" path bufDist))
    (let (lastBufLocSeg dir pathSegIdx startPoint endPoint pointList
          nextPoint endPoint segDist totalDist nextGuideLoc adjDist result)
        ;find which path segment is the last location
        result = ru_getPathSegment(lastBufLoc path)
        lastBufLocSeg = nth(0 result)
        dir = nth(1 result)
        (if lastBufLocSeg < 0 then
            printf("GetNextSegment: ERROR: Unable to locate buffer location %L to path %L\n" lastBufLoc path)
        )

        ; get next path location
        pointList = nil
        pathSegIdx = lastBufLocSeg
        lastPoint = lastBufLoc
        nextPoint = nth((pathSegIdx+1) path)
        segDist = Ru_GetDistance(lastPoint nextPoint)
        totalDist = segDist
        nextGuideLoc = ru_findNextGuideLoc(guideX guideY dir lastPoint nextPoint bufDist)
        (if boundp('debug) && debug >= 2 printf("getNextSegment: lastBufLocSeg %d %s totalDist %f lastPoint %L nextPoint %L nextGuideLoc %L\n"
                lastBufLocSeg dir segDist lastPoint nextPoint nextGuideLoc))

        (while totalDist < bufDist && pathSegIdx < (length(path)-2) && !nextGuideLoc
             pathSegIdx++
             pointList = append(pointList list(nextPoint))
             lastPoint = nextPoint
             nextPoint = nth((pathSegIdx+1) path)
             (if almostEqual(xCoord(lastPoint) xCoord(nextPoint)) then
                 dir = "VERT"
                 (if boundp('debug) && debug >= 4 printf("b. Path#%d segment is %s\n" pathSegIdx dir))
             else
                 dir = "HOR"
                 (if boundp('debug) && debug >= 4 printf("b. Path#%d segment is %s\n" pathSegIdx dir))
             )
             nextGuideLoc = ru_findNextGuideLoc(guideX guideY dir lastPoint nextPoint bufDist)
             segDist = Ru_GetDistance(lastPoint nextPoint)
             totalDist = totalDist + segDist
             (if boundp('debug) && debug >= 2 printf("getNextSegment: pathSegIdx %d %s totalDist %f segDist %f lastPoint %L nextPoint %L nextGuideLoc %L\n"
                    pathSegIdx dir totalDist segDist lastPoint nextPoint nextGuideLoc))
        )

        (cond 
            (nextGuideLoc 
                nextPoint = nextGuideLoc
            )
            (totalDist > bufDist
                ;adjust next point if distance longer than desired
                dir = ru_getSegDir(lastPoint nextPoint)
                adjDist = ru_roundUp(totalDist-bufDist)
                (if dir == "VERT" then
                    (if yCoord(nextPoint) > yCoord(lastPoint) 
                    then nextPoint = xCoord(nextPoint):round((yCoord(nextPoint)-adjDist)/2/TrackPitch)*2*TrackPitch
                    else nextPoint = xCoord(nextPoint):round((yCoord(nextPoint)+adjDist)/2/TrackPitch)*2*TrackPitch )
                else
                    (if xCoord(nextPoint) > xCoord(lastPoint) 
                    then nextPoint = round((xCoord(nextPoint)-adjDist)/TrackPitch)*TrackPitch:yCoord(nextPoint)
                    else nextPoint = round((xCoord(nextPoint)+adjDist)/TrackPitch)*TrackPitch:yCoord(nextPoint)) 
                )
            )
        )

        (if totalDist < 0.01 
        then pointList = nil
        else pointList = append(pointList list(nextPoint))
        )
        ;end
        (if pointList then
            endPoint = car(last(path))
            bufDist = Ru_GetDistance(lastBufLoc endPoint)
            (if bufDist < termDist then
                printf("Last buffer is within %f from endpoint\n" bufDist)
                pointList = nil
            )
        )
        pointList
    )
)

(defun ru_getSramSegmentIndex (lastBufLoc segPoints sramCfgList)
    (if boundp('debug) && debug >= 3 printf("getSramSegmentIndex lastBufLoc %L segPoints %L sramCfg %L\n" lastBufLoc segPoints sramCfgList))

    sramSegIndex = -1
    segPath = append(list(lastBufLoc) segPoints)
    sramIdx = 0
    (foreach sramCfg sramCfgList
        sramLoc = nth(2 sramCfg)
        pathSegIdx = 0
        (while sramSegIndex < 0 && pathSegIdx < (length(segPath)-1)
            startPoint = nth(pathSegIdx segPath)
            endPoint = nth((pathSegIdx+1) segPath)

                (if boundp('debug) && debug >= 4 printf("sramIdx %d pathSegIdx %d sramSegIndex %d SRAM %L %L %L\n"
                                        sramIdx pathSegIdx sramSegIndex startPoint sramLoc endPoint))
                (if almostEqual(xCoord(startPoint) xCoord(endPoint)) then
                    (if almostEqual(xCoord(startPoint) xCoord(sramLoc)) &&
                        !almostEqual(yCoord(startPoint) yCoord(sramLoc)) &&
                        Ru_IsBetween(yCoord(startPoint) yCoord(sramLoc) yCoord(endPoint)) then
                        (if boundp('debug) && debug >= 4 printf("VERT: Found matching SRAM %L %L %L\n" startPoint sramLoc endPoint))
                        sramSegIndex = sramIdx
                    )
                else
                    (if almostEqual(yCoord(startPoint) yCoord(sramLoc)) &&
                        !almostEqual(xCoord(startPoint) xCoord(sramLoc)) &&
                        Ru_IsBetween(xCoord(startPoint) xCoord(sramLoc) xCoord(endPoint)) then
                        ( if debug >= 4 printf("HOR: Found matching SRAM %L %L %L\n" startPoint sramLoc endPoint))
                        sramSegIndex = sramIdx
                    )
                )
            pathSegIdx++
        )
        sramIdx++
    )

    (if sramSegIndex >= 0 && boundp('debug) && debug >= 1 printf("getSramSegmentIndex: Found a SRAM location at index %d Cfg: %L\n"
                                                sramSegIndex nth(sramSegIndex sramCfgList)))
    sramSegIndex
)

(defun ru_openCellView (viewProp)
    view = dbOpenCellViewByType(nth(0 viewProp) nth(1 viewProp) nth(2 viewProp))
    (if !view then
        printf("ERROR: Unable to find view %L\n" viewProp)
    )
    view
)

(defun ru_getViewParms (viewProp)
    (if boundp('debug) && debug >= 3 printf("getViewParms viewProp %L\n" viewProp))
    (let (cellView cellOffset gapHor gapVert subView baseName len idx value)
        cellView = ru_openCellView(viewProp)
        cellOffset = 0.0:0.0
        gapHor = ru_roundUp(abs(xCoord(lowerLeft(cellView->bBox))-xCoord(upperRight(cellView->bBox))))
        gapVert = ru_roundUp(abs(yCoord(lowerLeft(cellView->bBox))-yCoord(upperRight(cellView->bBox))))
        subView = nil
        baseName = nil

        len = length(viewProp)
        (for idx 3 (len-1)
            value = nth(idx viewProp)
            (cond
                ((listp(value) && length(value)==2 && numberp(nth(0 value)) && numberp(nth(1 value))) cellOffset = value) /*location format*/
                (numberp(value) gapVert = value gapHor = value)
                (listp(value) subView = value)
                (stringp(value) baseName = value)
                (t printf("getViewParms: ERROR: Unhandled entry type %L\n" value))
            )
        )
        list(cellView cellOffset gapHor gapVert subView baseName)
    )
)

(defun ru_roundUp (value)
    ;printf("floor %f %d %f\n" value floor(value/TrackPitch) (floor(value/TrackPitch)+1)*TrackPitch)
    (floor(value/TrackPitch - TrackPitch/10)+1)*TrackPitch
)

(defun ru_roundDown (value)
    (floor(value/TrackPitch)+1)*TrackPitch
)

(defun ru_drawCellSegments (bufPrefixName bufirePrefixName path segPropList layerPattern 
                           bufView viewExceptionList sramCfgList
                           firstPathLayerPat lastPathLayerPat altLayerPattern doubleVias drawPat )
    (if boundp('debug) && debug >= 3 printf("drawCellSegments segPropList %L\n" segPropList) )

   ; get the various views and offsets 
    bufOffsetVert = 0.0:0.0
    bufOffsetHor = 0.0:0.0
    bufGapVert = 1*TrackPitch
    bufGapHor = 1*TrackPitch
    rightViewProp = nth(0 bufView)
    rightViewParms = ru_getViewParms(rightViewProp)
    rightView = nth(0 rightViewParms)
    bufOffsetHor = nth(1 rightViewParms)
    bufGapHor = nth(2 rightViewParms)
    bufGapVert = nth(3 rightViewParms)
    bufRightSubViewProp = nth(4 rightViewParms)

    bufViewLen = length(bufView)
    (if boundp('debug) && debug >= 5 printf("BufView %L len %d\n" bufView bufViewLen) )
    (if bufViewLen >= 2 then
        upViewProp = nth(1 bufView)
        upViewParms = ru_getViewParms(upViewProp)
        upView = nth(0 upViewParms)
        bufOffsetVert = nth(1 upViewParms)
        bufGapVert = nth(3 upViewParms)
        bufUpSubViewProp = nth(4 upViewParms)
    else 
        upView = rightView
        bufUpSubViewProp = bufRightSubViewProp
    )

    (if bufViewLen >= 3 then
        leftViewProp = nth(2 bufView)
        leftViewParms = ru_getViewParms(leftViewProp)
        leftView = nth(0 leftViewParms)
        bufLeftSubViewProp = nth(4 leftViewParms)
    else 
        leftView = rightView
        bufLeftSubViewProp = bufRightSubViewProp
    )
    (if bufViewLen >= 4 then
        downViewProp = nth(3 bufView)
        downViewParms = ru_getViewParms(downViewProp)
        downView = nth(0 downViewParms)
        bufDownSubViewProp = nth(4 downViewParms)
    else
        (if bufViewLen == 2 then
            downView = upView
            bufDownSubViewProp = bufUpSubViewProp
        else 
            downView = rightView
            bufDownSubViewProp = bufRightSubViewProp
        )
    )

    segNum = 0
    lastPoint = nth(0 path)
    lastCellLoc = nth(0 path)
    (foreach segProp segPropList
        (if boundp('debug) && debug >= 2 printf("drawCellSegments#%d: %L\n" segNum segProp))
        cellLocProp = nth(0 segProp)
        cellType = nth(0 cellLocProp)
        cellName = nth(1 cellLocProp)
        cellLoc = Ru_AlignBufXY(nth(2 cellLocProp))
        index = nth(3 cellLocProp)

        viewExptCfg = nil
        lastSegment = (segNum == (length(segPropList)-1))
        (if !lastSegment viewExptCfg = ru_getViewException(viewExceptionList cellName))
        (cond 
            (viewExptCfg
                exptViewProp = nth(1 viewExptCfg)
                (if boundp('debug) && debug >= 1 printf("%s: Cellview is overrided with %L\n" cellName exptViewProp))
                exptViewParms = ru_getViewParms(exptViewProp)
                exptView = nth(0 exptViewParms)
                exptCellOffset = nth(1 exptViewParms)
                exptGapHor = nth(2 exptViewParms)
                exptGapVert= nth(3 exptViewParms)
                exptSubViewProp = nth(4 exptViewParms)
                ;assuming view exception is only for BUF cell
                wireName = sprintf(nil "%s[%d]" bufWirePrefixName index)
                cellGapVert = exptGapVert
                cellGapHor = exptGapHor
            )
            (cellType == "BUF"
                wireName = sprintf(nil "%s[%d]" bufWirePrefixName index)
                cellGapVert = bufGapVert
                cellGapHor = bufGapHor
            )
            (cellType == "SRAM"
                sramCfg = nth(index sramCfgList)
                wireName = nth(1 sramCfg)
                sramViewProp = nth(3 sramCfg)
                sramViewParms = ru_getViewParms(sramViewProp)
                sramView = nth(0 sramViewParms)
                sramCellOffset = nth(1 sramViewParms)
                sramGapHor = nth(2 sramViewParms)
                sramGapVert= nth(3 sramViewParms)
                sramSubViewProp = nth(4 sramViewParms)

                cellGapVert = sramGapVert
                cellGapHor = sramGapHor
            )
            (cellType == "END"
                printf("Last segment\n")
                wireName = sprintf(nil "%s[%d]" bufWirePrefixName index)
                cellGapVert = bufGapVert
                cellGapHor = bufGapHor
            )
            (t printf("drawCellSegments: ERROR Unhandle cellType %s\n" cellType))
        )

        dist = abs(xCoord(cellLoc)-xCoord(lastCellLoc)) + abs(yCoord(cellLoc)-yCoord(lastCellLoc))
        maxDist = 470.0 
        (if dist > maxDist then
          printf("drawCellSegments: ERROR: %s (%L) distance to last cell (%L) is %f, maxManDist is %f\n"
                  cellName cellLoc lastCellLoc dist maxDist)
        )
        lastCellLoc= cellLoc
        seg = nth(1 segProp)

        offsetOrientX = 0.0
        offsetOrientY = 0.0
        offsetEndX = 0.0
        offsetEndY = 0.0
        cellOrient = "R0"
        nextPoint = nth(0 seg)
        endPoint = nth((length(seg)-1) seg)
        (if length(seg) > 1 then
          prevPoint = nth((length(seg)-2) seg)
        else
          prevPoint = lastPoint
        )

        ;printf("NEXTPOINT %L %L %f %f  %f %f\n" lastPoint nextPoint xCoord(nextPoint) xCoord(lastPoint) yCoord(nextPoint) yCoord(lastPoint) )
        (if xCoord(nextPoint) == xCoord(lastPoint) then
          (if yCoord(nextPoint) < yCoord(lastPoint) then
            offsetStartY = 0.0
            ;printf("Start Vertical going down\n")
          else
            offsetStartY = 0.0
            ;printf("Start Vertical going up\n")
          )
        )
        (if xCoord(endPoint) == xCoord(prevPoint) then
          (if yCoord(endPoint) < yCoord(prevPoint) then
            (if !lastSegment offsetEndY = cellGapVert)
            cellOrient = "MX"
            offsetOrientX = 0.0
            offsetOrientY = cellGapVert
            direction = "DOWN"
            ;printf("End Vertical going down\n")
          else
            (if !lastSegment offsetEndY = offsetEndY = -cellGapVert)
            direction = "UP"
            offsetOrientX = 0.0
            offsetOrientY = -cellGapVert
            ;printf("End Vertical going up\n")
          )
        )

        (if yCoord(nextPoint) == yCoord(lastPoint) then
          (if xCoord(nextPoint) < xCoord(lastPoint) then
            ;printf("Start Horizontal going left\n")
          else
            ;printf("Start Horizontal going right\n")
          )
        )
        (if yCoord(endPoint) == yCoord(prevPoint) then
          (if xCoord(endPoint) < xCoord(prevPoint) then
            (if !lastSegment offsetEndX = cellGapHor)
            offsetOrientX = cellGapHor
            offsetOrientY = 0.0
            cellOrient = "MY"
            direction = "LEFT"
            ;printf("End Horizontal going left\n")
          else
            (if !lastSegment offsetEndX = -cellGapHor)
            offsetOrientX = -cellGapHor
            offsetOrientY = 0.0
            direction = "RIGHT"
            ;printf("End Horizontal going right\n")
          )
        )

        (if yCoord(nextPoint) == yCoord(lastPoint) then
          ;printf("Start Horizontal\n")
          offsetX = cellGapHor
          (if xCoord(nextPoint) < xCoord(lastPoint) offsetX = -offsetX)
        )

        (if boundp('debug) && debug >= 2 printf("%s segNum %d start %L list %L end %L endOff %L cellGapHor %f cellGapVert %f\n" 
                cellName segNum lastPoint seg endPoint (offsetEndX:offsetEndY) cellGapHor cellGapVert))

        ;not used
        offsetStartX = 0.0
        offsetStartY = 0.0
        pointList = list((xCoord(lastPoint)+offsetStartX):(yCoord(lastPoint)+offsetStartY))
        (for cnt 0 length(seg)-2
            pointList = append(pointList list(nth(cnt seg)))
        )
        pointList = append(pointList list((xCoord(endPoint)+offsetEndX):(yCoord(endPoint)+offsetEndY)))

        (if boundp('debug) && debug >= 0 printf("drawCellSegments: length %d segNum %d dir %s PointList %L cellLoc %L offset %L %L\n" 
                              length(segPropList) segNum direction pointList cellLoc (offsetStartX:offsetStartY) (offsetEndX:offsetEndY)))
        
        (if drawPath then
            pathSeg = dbCreatePath(geGetEditCellView() list("M8" "bus") pointList 0.12 )
            busPattern = ""
            (foreach lp layerPattern
                (if strlen(busPattern) > 1 busPattern = strcat(busPattern " "))
                busPattern = strcat(busPattern nth(0 lp))
            )
	    (dbReplaceProp pathSeg "BusArrayPitch" "float" 2.88)
	    (dbReplaceProp pathSeg "BusPattern" "string" busPattern)
	    (dbReplaceProp pathSeg "BusPrefix" "string" wireName)
	    (dbReplaceProp pathSeg "BusPin" "boolean" nil)
	    (dbReplaceProp pathSeg "BusFlipX" "boolean" nil)
	    (dbReplaceProp pathSeg "BusFlipY" "boolean" nil)
	    (dbReplaceProp pathSeg "BusStartVias" "boolean" nil)
	    (dbReplaceProp pathSeg "BusEndVias" "boolean" nil)
	    (dbReplaceProp pathSeg "BusDoubleVias" "boolean" t)
        else
            ru_createWireSegment(wireName segNum pointList lastSegment path layerPattern
                                firstPathLayerPat lastPathLayerPat altLayerPattern doubleVias)
        )
forceView = nil
        (if !lastSegment then
            inst = (dbFindAnyInstByName (geGetEditCellView) cellName)
            (if !inst then
                cna = parseString(cellName "[]")
                (foreach extra list("_V" "_H")
                    instName = sprintf(nil "%s%s[%s]" nth(0 cna) extra nth(1 cna))
                    (if !inst then
                         inst = (dbFindAnyInstByName (geGetEditCellView) instName)
                    )
                )
            )
            (if inst then
                subViewPropList = nil
                (cond 
                    (viewExptCfg
                        cellOffset = exptCellOffset 
                        (if forceView inst->master = exptView)
                        subViewPropList = exptSubViewProp
                    )
                    (cellType == "BUF"
                        (if direction == "RIGHT" then (if forceView inst->master = rightView) cellOffset = bufOffsetHor subViewPropList = bufRightSubViewProp)
                        (if direction == "UP" then (if forceView inst->master = upView) cellOffset = bufOffsetVert subViewPropList = bufUpSubViewProp)
                        (if direction == "LEFT" then (if forceView inst->master = leftView) cellOffset = bufOffsetHor subViewPropList = bufLeftSubViewProp)
                        (if direction == "DOWN" then (if forceView inst->master = downView) cellOffset = bufOffsetVert subViewPropList = bufDownSubViewProp)
                    )
                    (cellType == "SRAM"
                        cellOffset = sramCellOffset 
                        (if forceView inst->master = sramView)
                        subViewPropList = sramSubViewProp
                    )
                    (t printf("drawCellSegments: ERROR Unhandle cellType %s\n" cellType))
                )
                instOff = (xCoord(cellOffset)+offsetOrientX):(yCoord(cellOffset)+offsetOrientY)
                (if boundp('debug) && debug >= 2 printf("%s: instOff %L cellOffset %L %f %f\n" cellName instOff cellOffset offsetOrientX offsetOrientY))
                inst->xy = Ru_OffsetPoint(cellLoc instOff)
                inst->orient = cellOrient 
                (if subViewPropList then
                    (if boundp('debug) && debug >= 3 printf("%s: Adding subviewList %L\n" cellName subViewPropList))
                    (foreach subViewProp subViewPropList
                        (if boundp('debug) && debug >= 5 printf("%s: Adding subview %L\n" cellName subViewProp))

                        svParms = ru_getViewParms(subViewProp)
                        ;printf("%s: subViewParms %L\n" cellName svParms)
                        subView = nth(0 svParms)
                        svCellOffset = nth(1 svParms)
                        svGapHor = nth(2 svParms)
                        svGapVert= nth(3 svParms)
                        svBaseName = nth(5 svParms)
                        (if !svBaseName || !stringp(svBaseName) then
                            printf("ERROR: %s: Need a proper base name (%L) for the subView %L\n" 
                                    cellName svBaseName subViewProp)
                        else 
                            svCellName = sprintf(nil "%s[%d]" svBaseName segNum)
                            printf("%s: Move to %s with offset %L\n" svCellName cellName svCellOffset)
                            MoveInst(svCellName Ru_OffsetPoint(inst->xy svCellOffset))
                        )                      
                    )
                )
            else
                printf("drawCellSegments: ERROR: Unable to find %s\n" cellName)
            )
        )

        lastPoint = nth((length(seg)-1) seg)
        segNum++
    )
)

(defun ru_createWireSegment (wireName segNum points lastSegment path layerPattern
                               firstPathLayerPat lastPathLayerPat altLayerPattern doubleVias )
    (if boundp('debug) && debug >= 3 printf("createWireSegment: %s points %L lastSeg %L\n" wireName points lastSegment ))
    (let (segLp altLp altPatternList altPatCfg cnt altLayerPatternList lp layer pattern 
          pathSegIdx dir result)
        segLp = nil
        altPatternList = nil
        (if altLayerPattern then
            altPatCfg = nth(segNum altLayerPattern)
            segLp = nth(0 altPatCfg)
            altLp = nth(1 altPatCfg)
            (if boundp('debug) && debug >= 5 then
                (if segLp printf("###createWireSegment: %s Using alternate pattern configuration %L\n" 
                        wireName segLp))
                (if altLp printf("altPatternList %L\n" altLp))
            )
        )    
        (if !segLp segLp = layerPattern)

        cnt = 0 
        altLayerPatternList = nil
        forceLayerPattern = nil
        forceAltLayerPattern = nil
        dir = "UNKNOWN"
        (if firstPathLayerPat || lastPathLayerPat then
            numPoints = length(points)
            result = ru_getPathSegment(nth(0 points) path)
            pathSegIdx = nth(0 result)
            dir = nth(1 result)
            ;printf("result %L %d %s numPoints %d\n" result pathSegIdx dir numPoints)
            (if pathSegIdx == 0 then
                (if numPoints == 2 then
                    (if boundp('debug) && debug >= 1 printf("%s First path segment using firstPathLayerPat\n" wireName))
                    forceLayerPattern = firstPathLayerPat
                else
                    (if boundp('debug) && debug >= 1 printf("%s First path segment switching firstPathLayerPat to layerPattern\n" wireName))
                    forceLayerPattern = firstPathLayerPat
                    forceAltLayerPattern = layerPattern
                )
            )
            (if pathSegIdx == (length(path)-2) && numPoints == 2  then
                (if boundp('debug) && debug >= 1 printf("%s Last path segment using lastPathLayerPat\n" wireName))
                forceLayerPattern = lastPathLayerPat
            )
            (if pathSegIdx == (length(path)-3) && numPoints > 2  then
                (if boundp('debug) && debug >= 1 printf("%s Last path segment switching layerPattern to lastPathLayerPat\n" wireName))
                forceLayerPattern = layerPattern
                forceAltLayerPattern = lastPathLayerPat
            )
        )

        (foreach lp segLp 
            (if forceLayerPattern then
                layer = nth(0 nth(cnt forceLayerPattern))
                pattern = nth(1 nth(cnt forceLayerPattern))
                (if boundp('debug) && debug >= 2 printf("%s: Layer#%d: Using layer %L Pattern %L\n" wireName cnt layer pattern))
            else
                layer = nth(0 lp)
                pattern =  nth(1 lp)
            )
            (if altLp altLayerPatternList = nth(cnt altLp))
            (if boundp('debug) && debug >= 3 printf("###### Seg %d layer#%d %L pattern %L \naltPattern %L \n" 
                                    segNum cnt layer pattern altLayerPatternList))
            (if forceAltLayerPattern then
                layer2 = nth(0 nth(cnt forceAltLayerPattern))
                (if dir == "HOR" then
                    hLayer = nth(0 layer)
                    vLayer = nth(1 layer2)
                else
                    hLayer = nth(1 layer)
                    vLayer = nth(0 layer2)
                )

                hl = ru_layerToValue(nth(0 hLayer))
                vl = ru_layerToValue(nth(0 vLayer))
                viasList = nil
                (if hl < vl then
                    (for l hl (vl-1)
                        viasList = append(viasList list(sprintf(nil  "M%d_M%d" l+1 l)))
                    )
                else
                    (for l vl (hl-1)
                        viasList = append(viasList list(sprintf(nil  "M%d_M%d" l+1 l)))
                    )
                )
                (if boundp('debug) && debug >= 2 printf("%s: hLayer %L vLayer %L hl %d vl %d viasList %L\n"
                        wireName hLayer vLayer hl vl viasList))

                layer = list(hLayer vLayer viasList "V")
                altLayerPatternList = list(list(layer nth(1 nth(cnt forceAltLayerPattern))))
                (if boundp('debug) && debug >= 2 printf("%s: Forcing layer %L Pattern %L\n" wireName layer pattern))
                (if boundp('debug) && debug >= 2 printf("%s: Forcing altLayerPattern %L\n" wireName altLayerPatternList))
            )
           (DrawChannel layer pattern wireName points 
                        ?altLayerPatternList  altLayerPatternList
                        ?doubleVias doubleVias)
            cnt++
        )
        (if boundp('debug) && debug >= 3 printf("createWireSegment: DONE\n"))
    )
)

(defun ru_getNextGuide (guideMap lastLoc dirRight)
    (if boundp('debug) && debug >= 4 printf( "getNextGuide %L dirRight %L\n" guideMap lastLoc dirRight) )
    (let (lastPosIdx lastNegIdx idx val delta negDelta posDelta ret)
        lastPosIdx = length(guideMap)-1
        lastNegIdx = 0
        idx = 0
        (foreach val guideMap
            delta = val - lastLoc
            negDelta = nth(lastNegIdx guideMap) - lastLoc
            posDelta = nth(lastPosIdx guideMap) - lastLoc
            (if boundp('debug) && debug >= 5 printf("guideMap#%d: %f delta %f lastLoc %f lastNegIdx %d negDelta %f lastPosIdx %d posDelta %f\n"
                                  idx nth(idx guideMap) delta lastLoc lastNegIdx negDelta lastPosIdx posDelta) )

            (if (delta > 2.8*8 && delta < posDelta ) lastPosIdx = idx )
            (if (delta < -2.8*8 && delta > negDelta ) lastNegIdx = idx )
            idx++
        )
        (if boundp('debug) && debug >= 3 printf( "getNextGuideX return %d guide loc %f\n",lastNegIdx nth(lastNegIdx guideMap)) )
        (if dirRight then ret = nth(lastPosIdx guideMap)
        else ret = nth(lastNegIdx guideMap)
        )
    )
)

(defun ru_findNextGuideLoc (guideX guideY dir lastPoint nextPoint bufDist)
    (if boundp('debug) && debug >= 4 printf( "findNextGuideLoc dir %s lastPoint %L nextPoint %L\n" dir lastPoint nextPoint) )
    (let (nextGuideLoc nextGuideX dist)
        nextGuideLoc = nil
        (if dir == "HOR" && guideX then
            nextGuideX = ru_getNextGuide(guideX xCoord(lastPoint) (xCoord(nextPoint)>xCoord(lastPoint)))
            nextGuideLoc = nextGuideX:yCoord(lastPoint)
            dist = Ru_GetDistance(lastPoint nextGuideLoc)
            ;printf("last %L guideX %f next %L dist %f\n" lastPoint nextGuideX nextPoint dist)
            (if Ru_IsBetween(xCoord(lastPoint) nextGuideX xCoord(nextPoint)) &&
                dist > TrackPitch && dist < bufDist then
                (if boundp('debug) && debug >= 2 printf("Found a guide xCoord at %f and %f from last buffer\n" nextGuideX dist))
            else 
                nextGuideLoc = nil
            )
        )
        (if dir == "VERT" && guideY then
            nextGuideY = ru_getNextGuide(guideY yCoord(lastPoint) (yCoord(nextPoint)>yCoord(lastPoint)))
            nextGuideLoc = xCoord(lastPoint):nextGuideY
            dist = Ru_GetDistance(lastPoint nextGuideLoc)
            (if Ru_IsBetween(yCoord(lastPoint) nextGuideY yCoord(nextPoint)) &&
                dist > TrackPitch && dist < bufDist then
                (if boundp('debug) && debug >= 2 printf("Found a guide yCoord at %f and %f from last buffer\n" nextGuideY dist))
            else 
                nextGuideLoc = nil
            )
        )
        nextGuideLoc
    )
)

(defun ru_drawPathGuide (path guideX guideY)
    (let (maxX maxY minX minY point)
        maxX = -9999.0
        maxY = -9999.0
        minX = 9999.0
        minY = 9999.0

        (foreach point path
            (if xCoord(point) > maxX maxX = xCoord(point))
            (if xCoord(point) < minX minX = xCoord(point))
            (if yCoord(point) > maxY maxY = yCoord(point))
            (if yCoord(point) < minY minY = yCoord(point))
        )

        (if guideX then
            (foreach X guideX
                dbCreatePath((geGetEditCellView)  list("M2" "drawing") list(X:minY X:maxY) 1 "extendExtend")
            )
        )    
        (if guideY then
            (foreach Y guideY
                dbCreatePath((geGetEditCellView)  list("M3" "drawing") list(minX:Y maxX:Y) 1 "extendExtend")
            )
        )
    )
)

(defun ru_getLocException (locExceptionList bufName bufIdx colIdx)
    (let (listLen foundIdx idx exception nameList name colMatched bufSeg 
         nameMatched numMatched result val1 val2)
        rexMagic(t)
        listLen = length(locExceptionList)
        foundIdx = -1
        (if boundp('debug) && debug >= 4 printf( "getLocException %s.%d listLen %d\n" bufName bufIdx listLen) )
        (for idx 0 listLen-1
          exception = nth(idx locExceptionList)
          nameList = nth(0 exception)
          (if !nameList then nameMatched = t
          else 
              (if !listp(nameList) nameList = list(nameList))
              (foreach name nameList
                  (if rexMatchp(name bufName) nameMatched = t)
              )
          )
          result = parseString(nth(1 exception) ":")
          colMatched = t
          (cond
            (length(result)==1
                bufSeg = nth(1 exception)
            )
            (length(result)==2
                colMatched = (Ru_StringToInt(nth(0 result))==colIdx)
                bufSeg = nth(1 result)
            )
            (t error("Not support for %L\n" result))
          )
          ;printf("nameMatched %L colMatched %L colIdx %L bufSeg %L result %L\n" nameMatched colMatched colIdx bufSeg result)
          (if nameMatched && colMatched then
              numMatched = nil
              (if integerp(bufSeg) then
                  (if bufIdx == bufSeg  numMatched = 1)
              else
                  result = parseString(bufSeg "-")
                  (if length(result) == 2 then
                     val1 = Ru_StringToInt(car(result))
                     val2 = Ru_StringToInt(cadr(result))
                     (if bufIdx >= val1 && bufIdx <= val2 numMatched = t)
                  )
                  result = parseString(bufSeg ",")
                  (if member(sprintf(nil "%d" bufIdx) result) numMatched = 1)
              )
              (if numMatched then
                  (if boundp('debug) && debug >= 3
                    printf("getLocException: %s.%d: Found in exception list index %d : %L\n"
                           bufName bufIdx idx exception)
                  )
                  foundIdx = idx
              )
           )
         )
         (if foundIdx >=0 then ret = nth(foundIdx locExceptionList)
         else ret = nil
         )
    )
)

(defun ru_getViewException (viewExceptionList cellName)
    (if boundp('debug) && debug >= 4 printf( "getViewException %s ExptList %L\n" cellName viewExceptionList) )
    (let (listLen foundIdx idx exception nameList name)
        listLen = length(viewExceptionList)
        foundIdx = -1
        (if boundp('debug) && debug >= 4 printf( "getViewException %s listLen %d\n" cellName listLen) )
        (for idx 0 listLen-1
          exception = nth(idx viewExceptionList)
          nameList = nth(0 exception)
          (foreach name nameList
            (if name == cellName then
              (if boundp('debug) && debug >= 3
                printf("getviewException: %s: Found in exception list index %d : %L\n"
                       cellName idx exception)
              )
              foundIdx = idx
            )
          )
        )
        (if foundIdx >=0 then ret = nth(foundIdx viewExceptionList)
        else ret = nil
        )
    )
)

(defun ru_layerToValue (layer)
    (let (ret)
        (case layer
            ("M2" ret = 2)
            ("M3" ret = 3)
            ("M4" ret = 4)
            ("M5" ret = 5)
            ("M6" ret = 6)
            ("M7" ret = 7)
            (t printf("layerToValue: Unhandled layer %L\n" layer))
        )
        ret
    )
)

(defun ru_getSegDir (startPoint endPoint @key (detail nil))
    (let (dir)
        (if !startPoint || !endPoint 
            error("ru_getSegDir Invalid startPoint %L or endPoint %L\n" startPoint endPoint))
        (if almostEqual(xCoord(startPoint) xCoord(endPoint)) then
           (if detail then dir = (if yCoord(startPoint) < yCoord(endPoint) "UP" "DOWN")
           else dir = "VERT")
        else 
           (if detail then dir = (if xCoord(startPoint) < xCoord(endPoint) "RIGHT" "LEFT")
           else dir = "HOR")
        )
        (if boundp('debug) && debug >= 5 printf("getSegDir %L %L -> %s\n" startPoint endPoint dir))
        dir
    )
)

(defun Ru_PathDistance (path)
  (let (totalDist cnt)
    totalDist = 0.0
    (if path then
        (for cnt 1 length(path)-1
            totalDist = totalDist + Ru_GetDistance(nth(cnt-1 path) nth(cnt path))
        )
    )
    totalDist
  )
)

(defun Ru_GetDistance (a b)
    (if boundp('debug) && debug >= 5 printf("getDistance %L %L\n" a b))
    (let (ret)
        ret = abs(xCoord(a)-xCoord(b)) + abs(yCoord(a)-yCoord(b))
    )
)

;if b is between a c
(defun Ru_IsBetween (a b c)
    (if boundp('debug) && debug >= 5 printf("isBetween %f %f %f\n" a b c))
    (let (ret)
        ;not a very good way to figure out
        (if c > a && b <= c && b >= a ret = t)
        (if c < a && b >= c && b <= a ret = t)
        ret
    )
)

(defun OffsetPoint (point dx dy)
    (let (retPoint)
        retPoint = (xCoord( point)+dx):(yCoord(point)+dy)
    )
)

(defun Ru_OffsetPoint (point offset)
    (if boundp('debug) && debug >= 4 printf( "offsetPoint %L %L\n" point offset) )
    (let (retPoint)
        retPoint = ( xCoord( point)+xCoord(offset)):(yCoord(point)+yCoord(offset))
    )
)

(defun Ru_OffsetPoints (points offset)
    (if boundp('debug) && debug >= 4 printf( "offsetPoints %L %L\n" points offset) )
    (let (retPoints = list(nil))
        (foreach point points
            retPoints = append(retPoints list((xCoord(point)+xCoord(offset)):(yCoord(point)+yCoord(offset))))
        )
        retPoints
    )
)

(defun MoveInst (name loc @key (orient nil))
    (if boundp('debug) && debug >= 3 printf("moveInst %s to %L\n" name loc))
    (let (inst)
        inst = (dbFindAnyInstByName (geGetEditCellView) name)
        (if inst then
          inst->xy = loc
          (if orient inst->orient = orient)
        else
          printf("ERROR: Unable to find %s. Need a buffer at %L\n" name, loc)
        )
        inst
    )
)

(defun ShiftInst (name loc @key (orient nil) (bufPrefix ""))
    (if boundp('debug) && debug >= 3 printf("ShiftInst %s to %L\n" name loc))
    (let (inst)
        inst = (dbFindAnyInstByName (geGetEditCellView) sprintf(nil "%s%s" bufPrefix name))
        (if inst then
          inst->xy = Ru_OffsetPoint(inst->xy loc)
          (if orient inst->orient = orient)
        else
          printf("ERROR: Unable to find %s%s. Need to shift buffer by %L\n" bufPrefix name loc)
        )
    )
    t
)

(defun Ru_MoveInstVH (prefix num loc @key (orient nil))
    (if boundp('debug) && debug >= 3 printf("moveInst %s to %L\n" name loc))
    (let (inst)
        inst = nil
        (foreach extra list("" "_V" "_H")
            instName = sprintf(nil "%s%s[%d]" prefix extra num)
            (if !inst inst = (dbFindAnyInstByName (geGetEditCellView) instName))
        )
        (if inst then
          inst->xy = loc
          (if orient inst->orient = orient)
        else
          printf("ERROR: Unable to find %s[%d]. Need a buffer at %L\n" prefix num loc)
        )
        inst
    )
)

(defun Ru_AlignX (locX)
    floor((locX+TrackPitch/64)/TrackPitch)*TrackPitch
)
(defun Ru_AlignY (locY)
    floor((locY+TrackPitch/64)/(2*TrackPitch))*2*TrackPitch
)

(defun Ru_AlignBufY (loc)
  locY = yCoord(loc)
  locX = xCoord(loc)
  locX:Ru_AlignY(locY)
)

(defun Ru_AlignBufXY (loc)
  locY = yCoord(loc)
  locX = xCoord(loc)
  round((locX-TrackPitch/2)/TrackPitch)*TrackPitch:Ru_AlignY(locY)
)


;very slow
(defun Ru_StringToInt (str)
  (let (p value)
    p = instring(str)
    fscanf(p "%d" value)
    value
  )
)

(defun Ru_StringIsInt (str)
  rexMatchp("[0-9]" str)
)


(defun Ru_SortChannelElByTrack (a b)
    a < b
)
(defun Ru_SortCarChannelElByTrack (a b)
    car(a) < car(b)
)
(defun Ru_SortChannelElByWireLength (a b)
    nth(6 a) < nth(6 b)
)

(defun Ru_SortChannelElByName (a b)
  (let (resulta resultb)
    resulta = parseString(a "[]")
    resultb = parseString(b "[]")
    (cond 
      (length(resulta)==2
        (if strcmp(car(resulta) car(resultb)) == 0 then
            Ru_StringToInt(cadr(resulta)) < Ru_StringToInt(cadr(resultb))
        else  strcmp(car(resulta) car(resultb)) < 0) 
      )
      (t (strcmp(a b) < 0))
    )
  )
)
(defun Ru_SortCarChannelElByName (a b)
    Ru_SortChannelElByName(cadr(a) cadr(b))
)

(defun Ru_CastToPatternName (name)
    sprintf(nil "smr_%s" name)
)
(defun Ru_LayoutToPatternName (name)
    (cond
        (name == "e1of4" sprintf(nil "smr_%s" name))
        (name == "e1of3" sprintf(nil "smr_%s" name))
        (name == "e1of2" sprintf(nil "smr_%s" name))
        (t name)
    )
)

(defun Ru_LayerToBusMetal (layer)
  (let (busMetal)
    (cond
      (layer == "M2" busMetal = "BusMetal23")
      (layer == "M3" busMetal = "BusMetal23")
      (layer == "M4" busMetal = "BusMetal45")
      (layer == "M5" busMetal = "BusMetal45")
      (layer == "M6" busMetal = "BusMetal67")
      (layer == "M7" busMetal = "BusMetal67")
      (t busMetal = "BusMetal67")
    )
    busMetal
  )
)


(defun Ru_GetNodeMap (nodeMap nodeList)
  (let (ret retOff e cnt nodeMatched)
    retOff = 0.0
    (foreach e nodeMap
       (if !ret then
           nodeMatched = t
           (for cnt 1 length(e)-1
               (if nodeMatched && nth(1 nth(cnt-1 nodeList)) != nth(0 nth(cnt e)) nodeMatched = nil)
               (if nodeMatched && !almostEqual(nth(2 nth(cnt-1 nodeList)) nth(1 nth(cnt e))) nodeMatched = nil)
           )
           (if nodeMatched ret = car(e))
       )
    )
    (if !ret then ;maybe it is not centered
        (foreach e nodeMap
           (if !ret then
               nodeMatched = t
               (for cnt 1 length(e)-1
                   (if nodeMatched && nth(1 nth(cnt-1 nodeList)) != nth(0 nth(cnt e)) nodeMatched = nil)
                   ;offset from the first entry
                   (if nodeMatched && !almostEqual(nth(2 nth(cnt-1 nodeList))-nth(2 nth(0 nodeList)) 
                                                   nth(1 nth(cnt e))-nth(1 nth(1 e)) ) nodeMatched = nil)
                   /*
                   printf("%L %L\n" nodeList e)
                   printf("nodeMatched %L %f %f = %f %f %f = %f\n" 
                          nodeMatched nth(2 nth(cnt-1 nodeList)) nth(2 nth(0 nodeList)) 
                                      nth(2 nth(cnt-1 nodeList))-nth(2 nth(0 nodeList))
                                      nth(1 nth(cnt e)) nth(1 nth(1 e))
                                      nth(1 nth(cnt e))-nth(1 nth(1 e)))
                  */
               )
               (if nodeMatched then
                   ret = car(e)
                   retOff = nth(2 nth(0 nodeList)) - nth(1 nth(1 e)); - 1.44
                   ;printf("retOff %L nodeList %L foundE %L\n" retOff nodeList e)
               )
           )
        )
    )
    (if ret then list(ret retOff) else ret)
  )
)

(defun Ru_GetNodePattern (nodeList)
  (let (ret nodeLen nodePatternMap e)
    e1of1Map = list(
        list("smr_e1of1" list("0" 1.32) list("e" 0.54))
        list("e1of1" list("0" 1.2) list("e" 1.44))
        list("e1of1a" list("0" -0.48) list("e" -0.9))
        list("e1of1b" list("0" -0.6) list("e" -0.18))
    )
    e1of2Map = list(
        list("smr_e1of2" list("0" 1.32) list("1" 1.68) list("e" 0.54))
;        list("e1of2" list("0" 0.0) list("1" 0.48) list("e" 0.24))
        list("inlv_e1of2_A" list("0" 0.84) list("1" 1.80) list("e" 1.32))
        list("inlv_e1of2_B" list("0" 1.08) list("1" 2.04) list("e" 1.56))
        list("smr_1of3" list("0" 1.32) list("1" 1.68) list("2" 2.04)) ;3 elements
    )
    e1of3Map = list(
        list("smr_e1of3" list("0" 1.32) list("1" 1.68) list("2" 2.04)  list("e" 0.54))
    )
    e1of4Map = list(
        list("smr_e1of4" list("0" 1.32) list("1" 1.68) list("2" 2.04)  list("3" 2.4)  list("e" 0.54))
;        list("e1of4" list("0" 0.0) list("1" 0.24) list("2" 0.72)  list("3" 0.96)  list("e" 0.48))
        list("inlv_e1of4_A" list("0" 0.36) list("1" 0.84) list("2" 1.8)  list("3" 2.28)  list("e" 1.32))
        list("inlv_e1of4_B" list("0" 0.6) list("1" 1.08) list("2" 2.04)  list("3" 2.52)  list("e" 1.56))
    )
    ret = nil
    nodeLen = length(nodeList)
    (cond
        (nodeLen == 2 ret = Ru_GetNodeMap(e1of1Map nodeList))
        (nodeLen == 3 ret = Ru_GetNodeMap(e1of2Map nodeList))
        (nodeLen == 4 ret = Ru_GetNodeMap(e1of3Map nodeList))
        (nodeLen == 5 ret = Ru_GetNodeMap(e1of4Map nodeList))
        ;(t printf("Ru_GetNodePattern: ERROR: Unhandled node pattern: %L. Please report so it can be added\n" nodeList))
    ) 
    (if !ret ret = list("UNKNOWN_PATTERN" 0.0))
    ret
  )
)

(defun matchedList (matchList name)
  (prog (el)
    (if !listp(matchList) then return(rexMatchp(matchList name))
    else
      (foreach el matchList
        (if rexMatchp(el name) return(t) )
      )
    )
    return(nil)
  )
)


; Pin(?pf "fc/hal/pins.il" ?cv halView ?debug 1 ?m "HD_L2F" ?c "HalToL2fHdChannel")
; Pin(?pf "fc/rxt_dist/pins.il" ?v rxtView ?sn t ?m "OUT_FT_SCA" ?c "RxtDistToScArFtC result2 %Lhannel")
(defun Pin (@key (M nil) (m nil) (o 0.0) (st nil) (sn nil) (sl nil) (pf nil) (c nil) (d nil) (e nil)
               (g nil) (map nil) (h nil) (v nil) (f nil) (w t) (l nil) (L nil) (p nil)
               (x nil) (y nil) (pinOffset nil)
               (help nil) (debug 0))
  (prog (entries shape busPrefix busPattern busArrayPitch bbox x0 y0 x1 y1 name start end temp postFix skip baseNode entryCnt
        busMetal points dir entry n match notMatch trackOff wireOff wireOffStr 
        cnt arrayCnt size view flipOffset offset offsetBase minOff
        channel channelName el aka channelTable DFII_dir cmdLine layer idx wireLen wireLenMax wireLenMin wireLenSum wireLenCnt
        filename inPort nextLine foundChannel result result2 arrayOffset nodeArrayPattern nodeEl iStr exactMatch
        nodeBaseName nodeTable lastName nodeList castStart castEnd castPattern layoutStart layoutEnd layoutPattern
        dumpPinPattern rangeX rangeY trackBaseOff pinName nameLen warnAbstract)

  (if !(help || h) then
    rexMagic(t)
    entries = nil
    chanFound = nil
    channelName = c
    dumpPinPattern = p
    match = m
    notMatch = M
    exactMatch = e
    trackOff = o
    rangeX = x
    rangeY = y
    (flipOffset = f)
    (if v then view = v
    else view = (geGetEditCellView))
    ; warn using abstract to generate pattern since the pins might be a polygon which incorrect generate the pattern
    warnAbstract = nil

     cmdLine = "Pin("
    ;cmdLine = strcat(cmdLine sprintf(nil "?v %L" view->cellName))
    (if pf cmdLine = strcat(cmdLine sprintf(nil " ?pf %L" pf)))
    (if m cmdLine = strcat(cmdLine sprintf(nil " ?m %s%L" (if listp(m) "list" "") m)))
    (if M cmdLine = strcat(cmdLine sprintf(nil " ?M %s%L" (if listp(M) "list" "") M)))
    (if e cmdLine = strcat(cmdLine sprintf(nil " ?e %L" e)))
    (if o!= 0.0 cmdLine = strcat(cmdLine sprintf(nil " ?o %L" o)))
    (if st cmdLine = strcat(cmdLine sprintf(nil " ?st %L" st)))
    (if d cmdLine = strcat(cmdLine sprintf(nil " ?d %L" d)))
    (if c cmdLine = strcat(cmdLine sprintf(nil " ?c %L" c)))
    (if p cmdLine = strcat(cmdLine sprintf(nil " ?p %L" p)))
    (if map cmdLine = strcat(cmdLine sprintf(nil " ?map %L" map)))
    (if g cmdLine = strcat(cmdLine sprintf(nil " ?g %L" g)))
    (if f cmdLine = strcat(cmdLine sprintf(nil " ?f %L" f)))
    (if x cmdLine = strcat(cmdLine sprintf(nil " ?x list%L" x)))
    (if y cmdLine = strcat(cmdLine sprintf(nil " ?y list%L" y)))
    cmdLine = strcat(cmdLine ")")
    (if pf then
        (if p error("Cannot have both ?pf and ?p option at the same time\n"))
        (if !channelName error("Must specify channelName when specify file\n"))
        DFII_dir = ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
        filename = (sprintf nil "%s/chip/alta/%s" DFII_dir pf)
        (printf "Reading file %s ... \n", filename)
        inPort = infile(filename) 
        (if !inPort error("Unable to open %s\n" filename))
        printf("Searching for %s\n" channelName)
        (when inPort 
          (while gets( nextLine inPort )
            nextLine = substring(nextLine 1 strlen(nextLine)-1) ;remove \n, don't know a better way
            (if foundChannel then
               (if !rexMatchp(";" nextLine) foundChannel = nil)
               (if rexMatchp("defchan" nextLine) foundChannel = nil)
               ;(if !rexMatchp("\\{\\}" nextLine) foundChannel = nil)
               (if debug >= 2  && !foundChannel printf("##### End of Channel: %L\n" nextLine))
            )
            (if foundChannel then
              (if debug >= 2 printf("LINE: [%L]\n" nextLine))
              el = parseString(nextLine ";+ =-")
              aka = nil
              (for cnt 2 length(el)-2
                  (if nth(cnt el) == "LAYOUT" aka = nth(cnt+1 el))
              )
              (foreach entry map
                  (if !aka && cadr(el) == car(entry) aka = cadr(entry))
              )
              (if debug >= 1 printf("EL: [%L] aka: %L\n" el aka))
; load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il") Pin(?pf "fc/l3_ar/pins.il" ?M "TCAM_" ?range list(-100 0) ?c "FfuToL3ArHdChannel_MID")
; Pin( ?pf "fc/l3_ar/output/pins.il" ?c "OutputOf0ToBsChannel_Hi" ?debug 5)

              (if length(el) >= 2 && (rexMatchp("^e1of" car(el)) || rexMatchp("^1of" car(el))) then
                  result = parseString(car(el) "[]")
                  temp = nil
                  (cond
                      (length(result) <= 2 
                          (if aka then temp = sprintf(nil "%s" aka))
                          nodeList = list(list(sprintf(nil "%s" nth(0 el)) sprintf(nil "%s" nth(1 el)) temp) )
                      )
                      (length(result) == 3 ;2-D array
                          nodeList = nil
                          (for cnt 0  Ru_StringToInt(nth(1 result))-1
                              (if aka then temp = sprintf(nil "%s[%d]" aka cnt))
                              nodeList = append(nodeList list(list(sprintf(nil "%s[%s]" nth(0 result) nth(2 result)) sprintf(nil "%s[%d]" nth(1 el) cnt) temp)))
                          )
                      )
                      (t error("Unsupport this pattern %L\n" el))
                  )
                  (foreach el nodeList
                      (if rexMatchp("^e1of4v" car(el)) then
                          result = parseString(car(el) "()")
                          (if !cadr(result) error("Is e1of4v(n) delared correctly?\n"))
                          (if !cadr(el) error("Unexpected nil in nodeName: %L \n" el ))
                          channel = append(channel list(list(sprintf(nil "e1of4[%s]" cadr(result)) sprintf(nil "%s.D" cadr(el)) nth(2 el))))
                          channel = append(channel list(list("e1of2" sprintf(nil "%s.V" cadr(el))  nth(2 el))))
                      else
                          channel = append(channel list(list(car(el) cadr(el) nth(2 el))))
                      )
                  )
              )
            )
            (if rexMatchp(sprintf(nil "defchan %s[^a-zA-Z0-9_]" channelName) nextLine) then
              foundChannel = t
              (if channel error("Multiple instance of defchan for %L found. %L\n" channelName nextLine))
              (if debug >= 2 printf("##### Channel Found: %L\n" nextLine))
            else
              (if debug >= 2 && rexMatchp("defchan" nextLine) then
                  printf("Found a [%L], looking for %L\n" nextLine channelName)
              )
            )
          )
        )

        (if g then
            printf("############################### CHANNEL PATTERN ###############################\n\n\n")
            printf("NOTE: Also copy the cmdLine below for reproducibility\n")
            printf(";Generated by just assigning sequential track order with cmdLine:\n" )
            printf(";  %s\n" cmdLine)
            printf("c = nil\n")
            cnt = 0
            (foreach el channel
                busMetal = "BusMetal67"
                busPattern = nth(0 el)
                result = parseString(busPattern "[]")
                (if debug >=2 printf("Entry: %L busPattern %L result %L\n" el busPattern result))
                (cond 
                  (length(result) ==  1
                      printf("c = (cons (list \".%s\" %s %d*TrackPitch %s) c)\n"
                             nth(1 el) Ru_CastToPatternName(busPattern) cnt busMetal)
                      cnt++
                  )
                  (length(result) ==  2
                      size = Ru_StringToInt(nth(1 result))
                      printf("(for i 0 %d\n" (size-1))
                      printf("    name = (sprintf nil \".%s%s\" i)\n" nth(1 el) "[%d]")
                      printf("    c = (cons (list name %s (i*%d+%d)*TrackPitch %s) c)\n"
                             Ru_CastToPatternName(nth(0 result)) 1 cnt busMetal)
                      cnt = cnt + size
                      printf(")\n")
                  )
                  (t printf("Unhandled busPattern of %L\n" busPattern))
                )
            )
            postFix = ""
            printf("%s%s = (DefChan c)\n" channelName postFix)
            printf("busPatterns = (cons (list \"%s%s\" %s%s) busPatterns)\n" channelName postFix channelName postFix)
            printf("\n\n")
            printf("Total channels: %L\n" cnt)
            return(t)
        else
           (if debug >= 1 then
                printf("Entries found for %s\n" channelName)
                (foreach el channel
                    printf("channel: %L\n" el)
                )
            )
           (if !channel error("ERROR: No entries found for %L\n" channelName))
        )
    )
(if debug == 5 return(t))

    channelTable = makeTable("channelTable" 0)
    (foreach ch channel
        el = nth(1 ch)
        channelTable[el] = nil
    )

    (foreach shape view->shapes
      (cond ((and shape->objType=="path"
             (or shape->lpp==BusPath23
                 shape->lpp==BusPath34
                 shape->lpp==BusPath45
                 shape->lpp==BusPath56
                 shape->lpp==BusPath67))
           ;printf("CellView %L points %L\n" shape->cellView->cellName shape->points)
           busPrefix = nil
           busPattern = nil
           busArrayPitch = nil
           (foreach prop shape->prop
              ;printf("prop: %L value: %L\n" prop->name prop->value)
              (if prop->name == "BusPrefix" busPrefix = prop->value)
              (if prop->name == "BusPattern" busPattern = prop->value)
              (if prop->name == "BusArrayPitch" busArrayPitch = prop->value)
           )
           points = shape->points
           dir = ru_getSegDir(car(points) cadr(points))
           (if debug >= 2 printf("%s Path: %L BusPrefix: %L BusPattern: %L BusArrayPitch: %L\n" 
                                 dir points busPrefix busPattern busArrayPitch))
           (if busPrefix && (d==nil || d == dir) && (match==nil || matchedList(match busPrefix))  then
               (if dir == "HOR" then offset = yCoord(car(points))
               else offset = xCoord(car(points)))
               offset = offset - trackOff*TrackPitch
               entry = list(list(offset busPrefix busPattern busArrayPitch shape->lpp dir))
               entries = append(entries entry)
               (foreach ch channel
                   el = nth(1 ch)
                   aka = nth(2 ch)
                   rexMagic(nil)
                   (if aka && rexMatchp(aka busPrefix) then
                       (if debug >=2 printf("Appending AKA NODE %L: %L\n" aka busPrefix))
                       channelTable[el] = append(channelTable[el] entry)
                   )
                   (if !aka && rexMatchp(el busPrefix) then
                       (if debug >=2 printf("Appending NODE %L: %L\n" el busPrefix))
                       result = parseString(busPrefix ".[]")
                       ;printf("el %L result %L\n" el result)
                       (if member(el result) then ;prevent a short name matching a long name
                           channelTable[el] = append(channelTable[el] entry)
                       )
                   )
                   rexMagic(t)
               ) 
           )
        )
        (shape->pin
            bbox = shape->bBox
            x0 = (car  (car  bbox))
            y0 = (cadr (car  bbox))
            x1 = (car  (cadr bbox))
            y1 = (cadr (cadr bbox))
            name = shape->net->name
            (if debug >=3 printf("(PinPlace \"%s\" (list %g:%g %g:%g) ?LPP (list \"%s\" \"%s\"))\n"
                                  name x0 y0 x1 y1 shape->layerName shape->purpose))
            
            (if !(name=="GND"||name=="Vdd") && (match == nil || matchedList(match name)) &&
                 (notMatch == nil || !matchedList(notMatch name))  then
               skip = nil
               (if abs(y1-y0) > abs(x1-x0) then
                  dir = "VERT"
                  offset = (x0+x1)/2
                  (if rangeY then
                      skip = !Ru_IsBetween(car(rangeY) y1 cadr(rangeY))
                  )
                  (if rangeX && !skip then
                      skip = !Ru_IsBetween(car(rangeX) x1 cadr(rangeX))
                  )
;printf("VERT:skip %L y1 %L %L x1 %L %L\n" skip y1 Ru_IsBetween(car(rangeY) y1 cadr(rangeY)) x1 Ru_IsBetween(car(rangeX) x1 cadr(rangeX)))
               else
                  dir = "HOR"
                  offset = (y0+y1)/2
                  (if rangeY then
                      skip = !Ru_IsBetween(car(rangeY) y1 cadr(rangeY))
                  )
                  (if rangeX && !skip then
                      skip = !Ru_IsBetween(car(rangeX) x1 cadr(rangeX))
                  )
;printf("HOR:skip %L y1 %L %L x1 %L %L\n" skip y1 Ru_IsBetween(car(rangeY) y1 cadr(rangeY)) x1 Ru_IsBetween(car(rangeX) x1 cadr(rangeX)))
               )
;  load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il")
               (if flipOffset offset = -offset)
               (if !skip && (d==nil || d == dir) &&
                   (l==nil || l == shape->layerName) &&
                   (L==nil || L != shape->layerName)  then
                   offset = offset - trackOff*TrackPitch
                   (if shape->net then
                       result = CheckLengthRouted(shape->net)
                   else
                       result = list(0 0)
                   )
(if debug == 100 return(shape))
                   entry = list(list(offset name "PIN" "PIN" shape->layerName dir (car(result)+cadr(result)) x0:y0))
                   entries = append(entries entry)
                   (foreach ch channel
                       el = nth(1 ch)
                       aka = nth(2 ch)
                       rexMagic(nil)
                       (if aka then
                           (if exactMatch then
                               temp = (aka==Ru_RemoveLastEl(Ru_RemoveLastEl(name ".") "[]"))
                           else temp = rexMatchp(aka name))
                           
                           (if temp then
                               (if debug >=2 printf("Appending AKA NODE %L: %L\n" aka name))
                               channelTable[el] = append(channelTable[el] entry)
                           )
                       else 
                           (if exactMatch then
                               ;remove node element and array index
                               result = Ru_RemoveLastEl(Ru_RemoveLastEl(name ".") "[]")
                               temp = (el == result)
                           else temp = rexMatchp(el name))
                           (if debug >= 2 printf("MATCH TEST el %L name %L result %L match %L\n" el name result temp))
                           (if temp then 
                               (if debug >= 2 printf("Appending NODE %L: %L\n" el name))
                               result = parseString(name ".[]")
                               result2 = parseString(el ".[]")
                               ;should match all the sub fields
                               (for cnt 0 length(result)-1
                                   (if nth(cnt result) == nth(0 result2) then
                                       (for idx 1 length(result2)-1
                                           (if nth(cnt+idx result) != nth(idx result2) temp = nil)
                                       ) 
                                   )
                               )
                               (if debug >= 2 printf("MATCH POS el %L result %L result2 %L match %L\n" el result result2 temp))
                               (if temp then
                                   channelTable[el] = append(channelTable[el] entry)
                               )
                           )
                       )
                       rexMagic(t)
                   )
               ) 
            )
         )

      ) ; cond
    ) ; foreach shapes
(if dumpPinPattern then
  (if st error("Does not support sorting by track when dumping pin pattern for all pins\n"))
  sn = t
  st = nil
)
showEntries = !channel || debug >= 2 || length(entries) < 100
    (if showEntries then
    ;must make a new copy, since channelTable is using the same one
    ;don't know proper way to make copy, reverse will do for now
    (if sl entries = sort(reverse(entries) 'Ru_SortChannelElByWireLength))
    (if st entries = sort(reverse(entries) 'Ru_SortCarChannelElByTrack))
    (if sn entries = sort(reverse(entries) 'Ru_SortCarChannelElByName))
    printf("\n############################### PINS FOUND ####################################\n")
    busPattern = nth(2 car(entries))
    (if busPattern == "PIN" then
        printf("%-40s %6s  %L %L %L\n" "Name" "Tracks" "Layer" "DIR" "wireLen")
    else
        printf("%-40s %6s %-20s %L %L %L\n" "Name" "Tracks" "busPattern" "busArrayPitch" "busMetal" "DIR")
    )
    minOff = 999999.0
    (foreach entry entries
        offset = nth(0 entry)
        busPrefix = nth(1 entry)
        busPattern = nth(2 entry)
        busArrayPitch = nth(3 entry)
        busMetal = nth(4 entry)
        dir = nth(5 entry)
        wireLen = nth(6 entry)
        (if busPattern == "PIN" then
            (if !pinOffset || (pinOffset && substring(busPrefix strlen(busPrefix) strlen(busPrefix))=="e") then
                printf("%-40s %6.2f  %L   %L %L\n" busPrefix offset/TrackPitch busMetal dir wireLen)
            )
        else
            printf("%-40s %6.2f %-20s %L %L %L\n" busPrefix offset/TrackPitch busPattern busArrayPitch busMetal dir)
        )
        (if offset < minOff then
            minOff = offset
            baseNode = entry
        )
    )
    )

    ;dump out all pins found
; load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il") Pin(?m "^LS")
; load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il") Pin(?d "HOR" ?y list(700.0 1850.0) ?p t ?c "CtrlToMidAbuttPins")
; load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il") Pin(?d "VERT" ?y list(1850.0 2000.0) ?p t ?c "MidToOutputAbuttPins")
   (if dumpPinPattern then
        printf("############################### PIN PATTERN ###############################\n\n\n")
        printf("NOTE: Also copy the cmdLine below for reproducibility\n")
        printf(";Generated from %s:%s with cmdLine:\n" view->cellName view->viewName )
        printf(";  %s\n" cmdLine)
        (if cadr(baseNode) then
            printf(";  Base pin location: %L at %L or %L Tracks\n" 
            car(Ru_SplitNodeName(cadr(baseNode))) nth(7 baseNode) car(ToTrack(list(nth(7 baseNode)))))
        )
        printf("c = nil\n")
        entryCnt = length(entries)
        cnt = 0
        (while cnt < entryCnt 
            entry = nth(cnt entries)
            offset = nth(0 entry)
            pinName = nth(1 entry)
            nameLen = strlen(pinName)
            busMetal = nth(4 entry)
            dir = nth(5 entry)
            wireLen = nth(6 entry)
            nodeBaseName = substring(pinName 1 nameLen-2)
            lastName = nodeBaseName
            busPattern =  nth(2 entry)
            (if busPattern != "PIN" error("##### ERROR: Not support pins from busWires with busPattern %L\n" busPattern))

            offsetBase = RoundDownTrack(offset)
            nextCnt = cnt
            busPattern = nil
            wireLenMax = -9999.0
            ;looking for a pattern
            nodeEl = substring(pinName nameLen nameLen)
            (if nodeEl == "0" then
                nodeTable = makeTable("nodeTable" 0)
                (while lastName == nodeBaseName && nextCnt < entryCnt
                     nodeEl = substring(pinName nameLen nameLen)
                     (if wireLen > wireLenMax wireLenMax = wireLen)
                     nodeTable[nodeEl] = list(list(nodeBaseName nodeEl offset-offsetBase offset layer wireLen))
                     ;printf("ADDING EL: %L entry %L\n" nodeEl nodeTable[nodeEl])
                     lastName = nodeBaseName
                     nextCnt++
                     pinName = nth(1 nth(nextCnt entries))
                     (if pinName then
                         offset = nth(0 nth(nextCnt entries))
                         nameLen = strlen(pinName)
                         nodeBaseName = substring(pinName 1 nameLen-2)
                         wireLen = nth(6  nth(nextCnt entries))
                     )
                )
                ;the .e should be last entry
                result = nil
                (foreach el list("0" "1" "2" "3" "e")
                    (if nodeTable[el]&&nodeTable[el]!=0 result = append(result nodeTable[el]))
                )
                busPattern = Ru_GetNodePattern(result)
                ;printf("cnt %L %L busPattern %L nodeTable %L\n" cnt nextCnt busPattern result)
            )

            (if busPattern&&car(busPattern) != "UNKNOWN_PATTERN" then
;printf("WireOff %L offsetBase %L minOff %L trackOff %L layer %L\n" wireOff offsetBase minOff trackOff busMetal)
                layoutPattern = car(busPattern)
                trackBaseOff = (offsetBase-RoundDownTrack(minOff))/TrackPitch
                (if almostEqual(cadr(busPattern) 0.0) then
                    temp = sprintf(nil "c = (cons (list \"%s\" %s %.0f*TrackPitch %s) c)"
                        lastName layoutPattern (trackBaseOff-trackOff) Ru_LayerToBusMetal(busMetal));
                else
                    wireOff = cadr(busPattern)
                    temp = sprintf(nil "c = (cons (list \"%s\" %s %.0f*TrackPitch%s%.3f %s) c)"
                       lastName car(busPattern) (trackBaseOff-trackOff) 
                       (if wireOff < 0.0 "" "+") wireOff Ru_LayerToBusMetal(busMetal));
                )
                printf("%-80s" temp)
                (if !integerp(wireLenMax) then printf(";%5.1f um\n" wireLenMax)
                else printf("\n"))
                 cnt = nextCnt
            else
                ;individual node
                offset = nth(0 entry)
                wireOff = offset - RoundDownTrack(minOff)
                trackBaseOff = RoundDownTrack(wireOff)/TrackPitch
                wireOff = (wireOff-trackBaseOff*TrackPitch) - TrackPitch/2
;printf("WireOff %L offset %L minOff %L trackOff %L layer %L\n" wireOff offset minOff trackOff busMetal)
                temp = sprintf(nil " c = (cons (list \"%s\" node_2W %.0f*TrackPitch%s%.3f %s) c)"
                        nth(1 entry)
                        (trackBaseOff-trackOff) 
                       (if wireOff < 0.0 "" "+") wireOff Ru_LayerToBusMetal(busMetal))
                printf("%-80s" temp)
                printf(";%5.1f um\n" wireLen)
                cnt++
            )
        )
        (if channelName then
            printf("%s = (DefChan c)\n" channelName)
            printf("busPatterns = (cons (list \"%s\" %s) busPatterns)\n" channelName channelName)
            printf("\n\n")
        )
    )
    (if channel then
        (if debug >= 2 printf("\n############################# CHANNEL ######################################\n"))
        minOff = 999999.0
        wireLenMax = -9999.0
        wireLenMin = 99999.9
        wireLenCnt = 0
        wireLenSum = 0.0
        (foreach ch channel
            el = nth(1 ch)
            (if channelTable[el] != nil then
                (foreach entry channelTable[el]
                    (if car(entry) < minOff then
                        minOff = car(entry)
                        baseNode = entry
                    )
                    wireLen = nth(6 entry)
                    (if wireLen < wireLenMin wireLenMin = wireLen)
                    (if wireLen > wireLenMax wireLenMax = wireLen)
                    wireLenSum = wireLenSum + wireLen
                    wireLenCnt++
                )
            )
        )
        (foreach ch channel
            el = nth(1 ch)
            (if debug >= 2 printf("############# %L ############\n" el))
            (if channelTable[el] == nil then
                (if debug >= 2 printf("No entry found for %L\n" el))
            else
                (foreach entry channelTable[el]
                    (if debug >= 2 printf("%s: %L\n" el entry))
                )
            )
        )
    )

    (if channel then
        printf("############################### CHANNEL PATTERN ###############################\n\n\n")
        printf("NOTE: Also copy the cmdLine below for reproducibility\n")
        printf(";Generated from %s:%s with cmdLine:\n" view->cellName view->viewName )
        printf(";  %s\n" cmdLine)
        (if !(integerp(wireLenMax) || wireLenCnt == 0) then
            printf(";  MaxWireLen: %.2f AvgWireLen: %.2f MinWireLen: %L\n" wireLenMax wireLenSum/wireLenCnt wireLenMin)
        else
            (if view->viewName=="layout" || view->viewName=="layout_pg" then
                printf("WARN: No connectivity?\n")
            )
        )
        (if cadr(baseNode) then
            printf(";  Base pin location: %L at %L or %L Tracks\n" 
            car(Ru_SplitNodeName(cadr(baseNode))) nth(7 baseNode) car(ToTrack(list(nth(7 baseNode)))))
        )
        printf("c = nil\n")
        (foreach ch channel
            el = nth(1 ch)
            (if channelTable[el] == nil then
                printf("######################## ERROR: No entry found for %L ######################\n" el)
            )
            ;from the pin pattern
            (if channelTable[el] && nth(2 car(channelTable[el])) == "PIN" then
                nodeTable = makeTable("nodeTable" 0)
                channelTable[el] = sort(channelTable[el] 'Ru_SortCarChannelElByName)
                (if debug >= 1 printf("############# %L ############\n" ch))
                lastName = ""
                nodeList = nil
                (foreach entry channelTable[el]
                    (if !cadr(entry) error("Nil second entry %L\n" entry))
                    result = parseString(cadr(entry) ".")
idx = 1
rexMagic(nil)
;too short of a name might falsely match
aka = nth(2 ch)
temp = (if aka aka el)
(if (strlen(temp) > 1) && rexMatchp(temp nth(0 result)) idx = 0)
(if temp == el idx = 0)
(if debug >= 2 printf("#### idx %d MATCH %L ch %L el %L result %L\n" idx temp ch el nth(0 result)))
rexMagic(t)
                    nodeBaseName = nth(idx result)
;  load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il")  Pin( ?pf "fc/next_hop/pins.il" ?e t ?c "SramNextHopToMgmtChannel")
;Pin( ?pf "fc/l3_ar/mid/pins.il" ?m "TRAP_" ?c "MidToOutputThMgmtChannel")
;Pin( ?pf "fc/l3_ar/mid/pins.il" ?m "TCAM_ADDR" ?c "SeqCtrlToMidChannel" ?debug 2)
;  load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il") Pin( ?pf "fc/l3_ar/proteus_ctrl/pins.il" ?m "MGMT_WR" ?c "SeqCtrlToMidChannel_H" ?debug 1)
;  load("/scratch/alta5/virtuoso/skill/layout/route/routepath.il") Pin(?pf "datapath/modify/stats/pins.il" ?c "HalToL2fHdChannel")
                    (for cnt idx+1 length(result)-2
                        nodeBaseName = strcat(nodeBaseName sprintf(nil ".%s" nth(cnt result)))
                    )
                    offset =  nth(0 entry)
                    layer = nth(4 entry)
                    wireLen = nth(6 entry)
                    nodeEl = car(last(result))
                    (cond
                        (nodeEl == "d[0]" nodeEl = "0")
                        (nodeEl == "d[1]" nodeEl = "1")
                        (nodeEl == "d[2]" nodeEl = "2")
                        (nodeEl == "d[3]" nodeEl = "3")
                    )
                    (if lastName != nodeBaseName then
                        offsetBase = RoundDownTrack(offset)
                        nodeTable[nodeBaseName] = list(list(nodeBaseName nodeEl offset-offsetBase offset layer wireLen))
                        nodeList = append(nodeList list(nodeBaseName))
                    else
                        nodeTable[nodeBaseName] = append(nodeTable[nodeBaseName] list(list(nodeBaseName nodeEl offset-offsetBase offset layer wireLen)))
                    )
                    lastName = nodeBaseName

                    (if debug >= 2 printf("%s: node: [%s] %L\n" el nodeBaseName entry))

                ) ;foreach
                arrayOffset = nil
                nodeArrayPattern = nil
                cnt = 0
                nodeList = sort(nodeList 'Ru_SortChannelElByName)
                (foreach entry nodeList
                    offset = (nth(3 car(nodeTable[entry])) - nth(3 car(nodeTable[car(nodeList)])))/TrackPitch*TrackPitch
                    (if cnt != 0 then
                        arrayOffset = append(arrayOffset list(offset))
                    )
                    temp = offset - floor((offset+TrackPitch/64)/TrackPitch)*TrackPitch
                    busPattern = Ru_GetNodePattern(nodeTable[entry])
                    nodeArrayPattern = append(nodeArrayPattern list(busPattern))
                    (if debug >= 1 printf("EL: %s: %L baseOff: %.2f %.2f %L\n" entry busPattern offset temp nodeTable[entry]))
                    cnt++
                )
                ;verify busPattern 
                (if !nth(0 ch) error("Nil first ch entry %L\n" ch))
                result = parseString(nth(0 ch) "[]")
                (if nth(1 result) then
                    result2 = parseString(cadr(result) ".")
                    (if cadr(result2) then castStart = car(result2) castEnd = cadr(result2)
                    else 
                        castStart = "0"
                        size = Ru_StringToInt(car(result2))
                        castEnd = sprintf(nil "%d" size-1)
                    )
                else
                    ;not an array
                    castStart = nil
                    castEnd = nil
                )
                castPattern = car(result)
                (if !car(nodeList) error("Nil first nodeList entry %L\n" nodeList))
                ;find layout array index
                result = parseString(car(nodeList) "[]")
                (if length(result) > 1 then
                    layoutStart = car(last(result))
                    layoutEnd = car(last(parseString(car(last(nodeList)) "[]")))
                else layoutStart = nil layoutEnd = nil
                )
                layoutPattern = car(car(nodeArrayPattern)) ;just assume the first one

                (if !rexMatchp(castPattern layoutPattern) then
                    (if w printf("cast pattern: %L layoutPattern: %L\n" castPattern layoutPattern))
                    (if w printf("###### ERROR: Cast pattern and layout pattern does not match for %L. Using individual element\n" el))
                )
                (if castStart!=layoutStart || castEnd!=layoutEnd then
                    (if w printf("castSize: %L..%L layoutSize: %L..%L\n" castStart castEnd layoutStart layoutEnd))
                    (if w printf("###### WARN: Cast pattern and layout size does not match for %L\n" el))
                   layoutStart = castStart ; using cast
                   layoutEnd = castEnd
                )
                minOff = minOff/TrackPitch*TrackPitch
                offset = nth(3 car(nodeTable[car(nodeList)]))
                layer = nth(4 car(nodeTable[car(nodeList)]))
                ;printf("NodeTable: %L\n" nodeTable[car(nodeList)])
                busMetal = Ru_LayerToBusMetal(layer)
;  load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il")  Pin( ?pf "fc/l3_ar/mid/pins.il" ?m "TCAM_" ?c "SeqCtrlToMidChannel" ?debug 2)
;  load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il") Pin( ?pf "fc/l3_ar/mid/pins.il" ?c "SeqCtrlToMidChannelTest" ?debug 1)
;  Pin( ?pf "fc/l3_ar/mid/pins.il" ?m "TCAM" ?c "SeqCtrlToMidChannel" ?debug 1)
;  Pin( ?pf "fc/l3_ar/mid/pins.il" ?m "TCAM_ADDR" ?c "SeqCtrlToMidChannel" ?debug 2)
                wireLenMax = -9999.9
                (foreach n nodeTable[car(nodeList)]
                    wireLen = nth(5 n)
                    (if wireLen > wireLenMax wireLenMax = wireLen)
                )
                (if !layoutEnd then ;not an array

                    (if layoutPattern == "UNKNOWN_PATTERN" then
                        warnAbstract = t
                        (if w printf("###### WARN: Please report if this pattern if known so it can be added. %L\n" nodeTable[car(nodeList)]))
                        cnt = 0
                        (foreach n nodeTable[car(nodeList)]
                            wireOff = nth(2 n) - TrackPitch/2
                            trackBaseOff = RoundDownTrack(offset-minOff)/TrackPitch
                            temp = sprintf(nil " c = (cons (list \".%s.%s\" node_2W %.0f*TrackPitch%s%.3f %s) c)"
                               el nth(1 n) (trackBaseOff-trackOff) 
                               (if wireOff < 0.0 "" "+") wireOff busMetal);
                            printf("%-80s" temp)
                            (if cnt == 0 && !integerp(wireLenMax) then printf(";%5.1f um\n" wireLenMax)
                            else printf("\n"))
                            cnt++
                        )
                    else
                        cnt = 0
                        trackBaseOff = RoundDownTrack(offset-minOff)/TrackPitch
                        (if almostEqual(cadr(nth(cnt nodeArrayPattern)) 0.0) then
                            temp = sprintf(nil "c = (cons (list \".%s\" %s %.0f*TrackPitch %s) c)"
                                el layoutPattern (trackBaseOff-trackOff) busMetal);
                        else
                            wireOff = cadr(nth(cnt nodeArrayPattern))
                            temp = sprintf(nil "c = (cons (list \".%s\" %s %.0f*TrackPitch%s%.3f %s) c)"
                               el car(nth(cnt nodeArrayPattern)) (trackBaseOff-trackOff) 
                               (if wireOff < 0.0 "" "+") wireOff busMetal);
                        )
                        printf("%-80s" temp)
                        (if !integerp(wireLenMax) then printf(";%5.1f um\n" wireLenMax)
                        else printf("\n"))
                    )
                else
                    result = nil
                    (for cnt 1 length(arrayOffset)-1
                        (if !result && !almostEqual(nth(0 arrayOffset)*(cnt+1) nth(cnt arrayOffset)) then
                            (if w printf("###### WARN: %s Can't use for loop: Inconsistent array pitch %L. expect: %f got: %f\n" el arrayOffset
                                   nth(0 arrayOffset)*(cnt+1)  nth(cnt arrayOffset) ))
                            result = t
                        )
                    )
                    (for cnt 1 length(nodeArrayPattern)-1
                        (if !result && car(nth(0 nodeArrayPattern)) != car(nth(cnt nodeArrayPattern)) then
                            (if w printf("###### WARN: %s Can't use for loop: Inconsistent array node pattern %L. expect: %L got: %L\n" el nodeArrayPattern
                                   nth(0 nodeArrayPattern)  nth(cnt nodeArrayPattern) ))
                            result = t
                        )
                    )
                    (if result then ;inconsistent pitch or node pattern
                        arrayCnt = 0
                        (foreach entry nodeList
                            offset = nth(3 car(nodeTable[entry]))
                            wireLenMax = -9999.9
                            (foreach n nodeTable[entry]
                                wireLen = nth(5 n)
                                (if wireLen > wireLenMax wireLenMax = wireLen)
                            )
                            busMetal = Ru_LayerToBusMetal(nth(4 car(nodeTable[entry])))
                            result2 = parseString(entry "[]")
                            idx = nth(length(result2)-1 result2)
                            (if w && idx != sprintf(nil "%d" arrayCnt) 
                                printf("###### WARN: Layout index %L is not the same as arrayCnt %d. %L\n" idx arrayCnt result2)) 
                            (if car(nth(arrayCnt nodeArrayPattern)) == "UNKNOWN_PATTERN" then
                                warnAbstract = t
                                (if w printf("###### WARN: Please report if this pattern if known so it can be added. %L\n" nodeTable[entry]))
                                cnt  = 0
                                (foreach n nodeTable[entry]
                                    wireOff = nth(2 n) - TrackPitch/2
                                    trackBaseOff = RoundDownTrack(offset-minOff)/TrackPitch
;printf("WireOff %L offset %L minOff %L trackOff %L track %L\n" wireOff offset minOff trackOff (RoundDownTrack(offset-minOff)/TrackPitch))
                                    temp = sprintf(nil " c = (cons (list \".%s[%s].%s\" node_2W %.1f*TrackPitch%s%.3f %s) c)"
                                            el idx nth(1 n)
                                            (trackBaseOff-trackOff) 
                                           (if wireOff < 0.0 "" "+") wireOff busMetal)
                                    printf("%-80s" temp)
                                    (if cnt == 0 && !integerp(wireLenMax) then printf(";%5.1f um\n" wireLenMax)
                                    else printf("\n"))
                                    cnt++
                                )
                            else
                                trackBaseOff = RoundDownTrack(offset-minOff+TrackPitch/2)/TrackPitch
                                (if almostEqual(cadr(nth(arrayCnt nodeArrayPattern)) 0.0) then
                                    temp = sprintf(nil "c = (cons (list \".%s[%s]\" %s %.0f*TrackPitch %s) c)"
                                       el idx car(nth(arrayCnt nodeArrayPattern)) (trackBaseOff-trackOff) busMetal);
                                else
                                    wireOff = cadr(nth(arrayCnt nodeArrayPattern))
;printf("ENTRY %L: wireOff %L offset %L minOff %L trackOff %L track %L %L nodeArrayPattern %L \n" entry  wireOff offset minOff trackOff ((offset-minOff)/TrackPitch-trackOff)  (RoundDownTrack(offset-minOff)/TrackPitch-trackOff) nodeArrayPattern)
                                    temp = sprintf(nil "c = (cons (list \".%s[%s]\" %s %.0f*TrackPitch%s%.3f %s) c)"
                                       el idx car(nth(arrayCnt nodeArrayPattern)) (trackBaseOff-trackOff) 
                                      (if wireOff < 0.0 "" "+") wireOff busMetal);
                                )
                                printf("%-80s" temp)
                                (if !integerp(wireLenMax) then printf(";%5.1f um\n" wireLenMax)
                                else printf("\n"))
                            )
                            arrayCnt++
                        )
                    else ;consistent pitch
                        (if !arrayOffset arrayOffset = list(1)) ;single entry array
                        printf("(for i %s %s\n" castStart castEnd)
                        iStr = "i"
                        (if castStart != "0" iStr = sprintf(nil "(i-%s)" castStart))
                        (if layoutPattern == "UNKNOWN_PATTERN" then
                            warnAbstract = t
                            (if w printf("###### WARN: Please report if this pattern if known so it can be added. %L\n" nodeTable[car(nodeList)]))
                            cnt = 0
                            (foreach n nodeTable[car(nodeList)]
                                wireOff = nth(2 n) - TrackPitch/2
                                trackBaseOff = RoundDownTrack(offset-minOff)/TrackPitch
                                printf("    name = (sprintf nil \".%s%s.%s\" i)\n" el "[%d]" nth(1 n))
                                temp = sprintf(nil "    c = (cons (list name node_2W (%.0f*%s+%.0f)*TrackPitch%s%.3f %s) c)"
                                        car(arrayOffset)/TrackPitch iStr (trackBaseOff-trackOff) 
                                       (if wireOff < 0.0 "" "+") wireOff busMetal)
                                printf("%-80s" temp)
                                (if cnt == 0 && !integerp(wireLenMax) then printf(";%5.1f um\n" wireLenMax)
                                else printf("\n"))
                                cnt++
                            )
                        else
                            wireOff = cadr(nth(0 nodeArrayPattern))
                            (if almostEqual(wireOff 0.0) then
                                wireOffStr = ""
                            else
                                wireOffStr = sprintf(nil "%s%.3f" (if wireOff < 0.0 "" "+") wireOff)
                            )
                            printf("    name = (sprintf nil \".%s%s\" i)\n" el "[%d]")
                            trackBaseOff = RoundDownTrack(offset-minOff)/TrackPitch
                            temp = sprintf(nil "    c = (cons (list name %s (%.0f*%s+%.0f)*TrackPitch%s %s) c)"
                                   layoutPattern car(arrayOffset)/TrackPitch iStr (trackBaseOff-trackOff)
                                   wireOffStr busMetal)
                            printf("%-80s" temp)
                            (if !integerp(wireLenMax) then printf(";%5.1f um\n" wireLenMax)
                            else printf("\n"))
                        )
                        printf(")\n")
                    )
                )
            else ;from the buswire pattern
                (foreach entry channelTable[el]
                    (cond
                       (nth(4 entry)==BusPath23 busMetal = "BusMetal23")
                       (nth(4 entry)==BusPath45 busMetal = "BusMetal45")
                       (nth(4 entry)==BusPath56 busMetal = "BusMetal56")
                       (t busMetal = "BusMetal67")
                    )
                    busPattern = nth(2 entry)
                    result = parseString(busPattern "[]")
                    ;verify busPattern 
                    result2 = parseString(nth(0 ch) "[]")
                    (if cadr(result)!=cadr(result2) || !rexMatchp(car(result2) car(result)) then
                        printf("cast: %L\nlayout: %L\n" ch entry)
                        printf("cast pattern %L\n" nth(0 ch))
                        printf("layout pattern %L\n" busPattern)
                        printf("###### ERROR: Cast pattern and layout pattern does not match for %L\n" el)
                    )
                    (cond 
                      (length(result) ==  1
                          printf("c = (cons (list \".%s\" %s %.0f*TrackPitch %s) c)\n"
                                 el Ru_LayoutToPatternName(nth(2 entry)) ((nth(0 entry)-minOff)/TrackPitch-trackOff) busMetal)
                      )
                      (length(result) ==  2
                          size = nth(1 result)
                          arrayOffset = nth(3 entry)/TrackPitch
                          temp = parseString(size ".")
                          (if length(temp) == 2 then
                            start = Ru_StringToInt(car(temp))
                            end = Ru_StringToInt(cadr(temp))
                          else
                            start = 0
                            end = Ru_StringToInt(size)
                          )
                          printf("(for i %d %d\n" start end)
                          printf("    name = (sprintf nil \".%s%s\" i)\n" el "[%d]")
                          printf("    c = (cons (list name %s (i*%.0f+%.0f)*TrackPitch %s) c)\n"
                                 Ru_LayoutToPatternName(nth(0 result)) arrayOffset ((nth(0 entry)-minOff)/TrackPitch-trackOff-start) busMetal)
                          printf(")\n")
                      )
                      (t printf("Unhandled busPattern of %L\n" busPattern))
                    )
                )
              (if length(channelTable[el]) > 1
                printf("###### WARN: More than one entry found for %L\n" el)
              )
            )
        )
        result = ""
        printf("%s%s = (DefChan c)\n" channelName result)
        printf("busPatterns = (cons (list \"%s%s\" %s%s) busPatterns)\n" channelName result channelName result)
        printf("\n\n")
        (if warnAbstract && view->viewName == "abstract" then
            printf("##################### WARN WARN WARN ########################\n")
            printf("Patterns with node element might be incorrect due to generated from abstract\n") 
        )
    )
  else 
    printf("\n\n")
    printf("Dump out pin offset from busWire view or any view containing pin pattern\n")
    printf("Auto generate a channel pattern given a channel definition\n")
    printf("Verify a pattern with the busWire view or any view containing pin pattern\n")
    printf("Usage: Pin([OPTION])\n")
    printf("       ?help t             - this help\n")
    printf("       ?st t               - Sort the output by trackOffset\n")
    printf("       ?sn t               - Sort the output by name\n")
    printf("       ?m <regexp>         - Filter entries with a regular expression or a list of regexp before further processing\n")
    printf("       ?M <regexp>         - Excluding entries with a regular expression or a list of regexp before further processing\n")
    printf("       ?v <view>           - Specify the view name instead of the current view\n")
    printf("       ?o <value>          - Normalize channel track offset with <value>\n")
    printf("       ?c <name>           - Specify channelName to build or verify\n")
    printf("       ?pf <name>          - Specify pins.il name relative to layout/tsmc65/dfII/chip/alta base directory\n")
    printf("                             Used to build channel pattern list. Must specify channelName also\n")
    printf("       ?p t                - Generate pattern for all pins found.\n")
    printf("       ?d <HOR,VERT>       - Only use pins in one direction, used for pins spanning multiple directions\n")
    printf("       ?l <layer>          - Only use pins with specified layer\n")
    printf("       ?L <layer>          - Only use pins not matching specified layer\n")
    printf("       ?x <list(low hi)>   - Only use pins with xCoord within this range")
    printf("       ?y <list(low hi)>   - Only use pins with yCoord within this range")
    printf("       ?pinOffset t        - Only print out the .e offset for manually creating pin pattern\n")
    printf("       ?g t                - Generate a pattern in order of the elements\n")
    printf("       ?w nil              - Use to suppress warning messages once deemed okay to copy bus pattern cleanly\n")
    printf("       ?f t                - Flip offset before generating pin pattern due to pin flipped\n")
    ;printf("       ?p <pattern>        - Verify the channel pattern with pins in a view(Not implemented yet)\n")
    printf("       ?map <map>          - Specify a list of (list(list<castEntryName> <layoutEntryName>))\n")
    printf("                             to associate the cast entry name to layout name, where it differs\n")
    printf("NOTE:\n")
    printf("In some instances the cast channel entry name does not match with the layout name,\n")
    printf("one can add a \"LAYOUT=<name>\" in pins.il to associate the cast entry name to the\n")
    printf("layout name as below. This is prefered since it will be reproducible. Or use map option\n")
    printf("   ;     e1of4v(3)         +ALU13_CMD_PROFILE;  LAYOUT=L3AR_ALU13_OP_PROFILE_TABLE_CMD\n")
    printf("Example: Pin(?pf \"fc/hal/pins.il\" ?cv halBusView ?m \"IN_HD\" ?c \"L2LookupToHalHdChannel\")\n")
    printf("         Obtain all the busPrefix containing \"IN_HD\" in busWires view specified by halBusView.\n")
    printf("         Using the result to generate channel pattern for L2LookupToHalHdChannel with definition in pins.il\n")
    printf("\n")
    printf("DefChan Example:\n")
    printf(";defchan ExampleChannel\n")
    printf(";  e1of4[4]             +Array;\n") 
    printf(";  e1of4[32..63]        +Array1;\n")
    printf(";  e1of4[4][2]          +2DArray;\n")

  ) ; help
  return(t)
  )

)


(defun ru_WithinGrid (value grid limit)
  (let (delta)
    delta = abs((value - round(value/grid)*grid))
    ((delta < limit) || delta > (grid-limit))
  )
)

(defun ru_AdjustSegLength (seg len)
  (let (dist x1 y1 x2 y2)
    x1 = xCoord(nth(0 seg))
    x2 = xCoord(nth(1 seg))
    y1 = yCoord(nth(0 seg))
    y2 = yCoord(nth(1 seg))
    dist = abs(x1-x2)+abs(y1-y2)
    (if dist < len/2 len = dist/2)
    (if almostEqual(x1 x2) then ;vert
        (if y1 > y2 then y2 = y2 + len
        else y2 = y2 - len)
    else
        (if x1 > x2 then x2 = x2 + len
        else x2 = x2 - len)
    )
    list(x1:y1 x2:y2)
  )
)

(defun ru_IsNameMatched (matchEntry wireName @key (exactMatch nil))
  (prog (entry result result2 idx minEntry maxEntry temp len cnt el matchEntryIntIndx)
     rexMagic(nil) ;we want to match [] in normal sense
;printf("######## IsNameMatched: %L %L rexMatchp %L\n" matchEntry wireName rexMatchp(matchEntry wireName) )
     (if (matchEntry == wireName || rexMatchp(matchEntry wireName) ) then
         rexMagic(t)
         return(t)
     )
     result = parseString(matchEntry "\\[]")
;printf("    IsNameMatched matchEntry %L wireName %L result %L\n" matchEntry wireName result)
     (if rexMatchp(car(result) wireName) then
         len = length(result)
         (if exactMatch then
             result2 = parseString(wireName "\\[]")
             (for cnt 0 len-2
                 (if nth(cnt result)!=nth(cnt result2) then
                     ;printf("    NOT EXACT MATCH at index %d %L %L\n" cnt  nth(cnt result) nth(cnt result2))
                     rexMagic(t)
                     return(nil)
                 )
             )
         )
         minEntry = nil

   matchEntryIntIndx = nil
  (if rexMatchp(".." nth(len-1 result)) matchEntryIntIndx = len-1) ;in the form of RD[0..15]
   rexMagic(t)
;Ru_StringIsInt requires rexMagic(t)
(if !matchEntryIntIndx then
(cond
  ( Ru_StringIsInt(nth(len-1 result)) matchEntryIntIndx = len-1)
  ( Ru_StringIsInt(nth(len-2 result)) 
    ;must match the last entry
    (if nth(len-1 result)!=nth(length(result2)-1 result2) return(nil))
    matchEntryIntIndx = len-2
  )
  (t matchEntryIntIndx = len)
)
)

;printf("    wireName: %L result %L result2 %L matchEntryIntIndx %L len %L\n"  wireName result result2 matchEntryIntIndx len)
        ;must match the last index
        (if matchEntryIntIndx < len then
            result = parseString(nth(matchEntryIntIndx result) ".")
            (if cadr(result) then
                minEntry = Ru_StringToInt(car(result))
                maxEntry = Ru_StringToInt(cadr(result))
            else
                minEntry = Ru_StringToInt(car(result))
                maxEntry = minEntry
            )
            result = parseString(wireName "[]")
            temp = nil
            (foreach el result
              (if !temp && Ru_StringIsInt(el) temp = el)
            )
            (if temp idx = Ru_StringToInt(temp))
        )
;printf("    wireName: %L matchEntry %L idx %L min %L max %L %L temp %L \n"  wireName matchEntry idx minEntry maxEntry result temp )
        (if !minEntry || (idx && idx >= minEntry && idx <= maxEntry) then
;printf("    FOUND wireName: %L idx %L min %L max %L matchEntry %L %L\n" wireName idx minEntry maxEntry matchEntry result)
            return(t)
        )
     )
    return(nil)
  )
)

(defun ru_GetFilterList (filterList wireName)
  (prog (entry)
    (foreach entry filterList
         (if ru_IsNameMatched(nth(0 entry) wireName ?exactMatch t) return(entry))
    )
    return(nil)
  )
)

(defun ru_GetSegException (segException wireName)
  (prog (segExcep)
    (foreach segExcep segException
         (if ru_IsNameMatched(nth(0 segExcep) wireName ?exactMatch t) return(segExcep))
    )
    return(nil)
  )
)

(defun Ru_GetLastField (name)
  (let (array entry)
    array = parseString(name ".")
    (if length(array) == 3 && rexMatchp("^DFT" car(array)) && (cadr(array)=="D"||cadr(array)=="C") then ;FIXME DFT
        entry = sprintf(nil "%s.%s" cadr(array) nth(2 array))
    else
        entry = nth((length(array)-1) array)
    )
    ;printf("Ru_GetLastField: Array %L entry %s\n" array entry)
    entry
  )
)
(defun Ru_GetFirstField (name)
  (let (array entry)
    array = parseString(name ".")
    entry = nth(0 array)
    ;printf("Ru_GetFirstField: Array %L entry %s\n" array entry)
    entry
  )
)

(defun Ru_gete1of1PinLLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(0.06:0.48 4.14:0.60))
        (field == "0" loc = list(0.06:1.26 4.14:1.38))
        (t printf("ERROR: Ru_gete1of1PinLLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)
(defun Ru_gete1of1PinRLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(0.06:3.36 4.14:3.48))
        (field == "0" loc = list(0.06:4.14 4.14:4.26))
        (t printf("ERROR: Ru_gete1of1PinRLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)

(defun Ru_gete1of2aPinLLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(0.06:4.14 3.12:4.26))
        (field == "0" loc = list(0.06:3.66 3.12:3.78))
        (field == "1" loc = list(0.06:4.62 3.12:4.74))
        (t printf("ERROR: Ru_gete1of2aPinLLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)
(defun Ru_gete1of2aPinRLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(0.06:7.02 3.12:7.14))
        (field == "0" loc = list(0.06:6.54 3.12:6.66))
        (field == "1" loc = list(0.06:7.50 3.12:7.62))
        (t printf("ERROR: Ru_gete1of2aPinRLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)

(defun Ru_gete1of4PinLLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(0.12:3.36 2.7:3.48))
        (field == "0" loc = list(0.12:4.14 2.7:4.26))
        (field == "1" loc = list(0.12:4.50 2.7:4.62))
        (field == "2" loc = list(0.12:4.86 2.7:4.98))
        (field == "3" loc = list(0.12:5.22 2.7:5.34))
        (t printf("ERROR: Ru_gete1of4PinLLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)
(defun Ru_gete1of4PinRLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(3.06:3.36 5.64:3.48))
        (field == "0" loc = list(3.06:4.14 5.64:4.26))
        (field == "1" loc = list(3.06:4.50 5.64:4.62))
        (field == "2" loc = list(3.06:4.86 5.64:4.98))
        (field == "3" loc = list(3.06:5.22 5.64:5.34))
        (t error("ERROR: Ru_gete1of4PinRLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)
(defun Ru_gete1of3aPinLLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(0.06:4.14 2.82:4.26))
        (field == "0" loc = list(0.06:3.18 2.82:3.30))
        (field == "1" loc = list(0.06:3.66 2.82:3.78))
        (field == "2" loc = list(0.06:4.62 2.82:4.74))
        (t error("ERROR: Ru_gete1of3aPinLLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)
(defun Ru_gete1of3aPinRLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(2.94:7.02 5.70:7.14))
        (field == "0" loc = list(2.94:6.06 5.70:6.18))
        (field == "1" loc = list(2.94:6.54 5.70:6.66))
        (field == "2" loc = list(2.94:7.50 5.70:7.62))
        (t error("ERROR: Ru_gete1of3aPinRLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)
(defun Ru_gete1of4aPinLLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(0.06:4.14 2.82:4.26))
        (field == "0" loc = list(0.06:3.18 2.82:3.30))
        (field == "1" loc = list(0.06:3.66 2.82:3.78))
        (field == "2" loc = list(0.06:4.62 2.82:4.74))
        (field == "3" loc = list(0.06:5.10 2.82:5.22))
        (t error("ERROR: Ru_gete1of4aPinLLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)
(defun Ru_gete1of4aPinRLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "e" loc = list(2.94:7.02 5.70:7.14))
        (field == "0" loc = list(2.94:6.06 5.70:6.18))
        (field == "1" loc = list(2.94:6.54 5.70:6.66))
        (field == "2" loc = list(2.94:7.50 5.70:7.62))
        (field == "3" loc = list(2.94:7.98 5.70:8.10))
        (t error("ERROR: Ru_gete1of4aPinRLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)
(defun Ru_getDftPinLLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "D.e" loc = list(0.06:3.36 3.12:3.48))
        (field == "D.0" loc = list(0.06:4.14 3.12:4.26))
        (field == "D.1" loc = list(0.06:4.5 3.12:4.62))
        (field == "C.0" loc = list(0.06+3.18:0.12*4 1.68+3.18:0.12*5)) ;not real pin location
        (field == "C.1" loc = list(0.06+3.18:2*TrackPitch+0.12*16.5 1.68+3.18:2*TrackPitch+0.12*17.5)) ;not real pin location
        (field == "C.2" loc = list(0.06+4.92:2*TrackPitch+0.12*19.5 1.68+4.92:2*TrackPitch+0.12*20.5)) ;not real pin location
        (t printf("ERROR: Ru_getDftPinLLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)
(defun Ru_getDftPinRLoc (pinName)
  (let (loc field)
    field = Ru_GetLastField(pinName) 
    (cond
        (field == "D.e" loc = list(0.06:6.24 3.12:6.36))
        (field == "D.0" loc = list(0.06:7.02 3.12:7.14))
        (field == "D.1" loc = list(0.06:7.38 3.12:7.5))
        (field == "C.0" loc = list(0.06+3.18:TrackPitch+0.12*18 1.68+3.18:TrackPitch+0.12*19)) ;not real pin location
        (field == "C.1" loc = list(0.06+3.18:3*TrackPitch+0.12*8 1.68+3.18:3*TrackPitch+0.12*9)) ;not real pin location
        (field == "C.2" loc = list(0.06+4.92:3*TrackPitch+0.12*12 1.68+4.92:3*TrackPitch+0.12*13)) ;not real pin location
        (t printf("ERROR: Ru_getDftPinRLoc: Unknown pin name [%s] field [%s]\n" pinName field))
    )
    loc
  )
)

procedure( offsetBox( box )
    (if debug >= 4 printf( "offsetBox %L %L\n" box offset) )
      let( ( llx lly urx ury )
            urx = xCoord( upperRight( box ))
            ury = yCoord( upperRight( box ))
            llx = xCoord( lowerLeft( box ))
            lly = yCoord( lowerLeft( box ))
            offx = xCoord(offset)
            offy = yCoord(offset)
            list((llx+offx):(lly+offy) (urx+offx):(ury+offy))
      ) ; let
)

(defun Ru_getPinLLoc (cellTypeName name orient)
  (let (loc flipX flipY)
    (cond
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of1.1000" ||
         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of1.1000" loc = Ru_gete1of1PinLLoc(name))
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of2.1000a" ||
         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of2.1000a" loc = Ru_gete1of2aPinLLoc(name))
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of3.1000a" ||
         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of3.1000a"  loc = Ru_gete1of3aPinLLoc(name))
;        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of4.1000" ||
;         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of4.1000"  loc = Ru_gete1of4PinLLoc(name))
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of4.1000a" ||
         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of4.1000a"  loc = Ru_gete1of4aPinLLoc(name))
        (cellTypeName == "lib.serial.scan.MBUF_ChanDft.1003" loc = Ru_getDftPinLLoc(name))
        (t error("ERROR: Ru_getPinLLoc: Unsupported cell type [%s]\n" cellTypeName))
    )
    flipX = (orient == "MX" || orient == "R180")
    flipY = (orient == "MY" || orient == "R180")
    (if flipY loc = list(-xCoord(lowerLeft(loc)):yCoord(lowerLeft(loc)) -xCoord(upperRight(loc)):yCoord(upperRight(loc))))
    (if flipX loc = list(xCoord(lowerLeft(loc)):-yCoord(lowerLeft(loc)) xCoord(upperRight(loc)):-yCoord(upperRight(loc))))
    loc
  )
)
(defun Ru_getPinRLoc (cellTypeName name orient)
  (let (loc flipX flipY)
    (cond
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of1.1000" ||
         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of1.1000" loc = Ru_gete1of1PinRLoc(name))
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of2.1000a" ||
         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of2.1000a" loc = Ru_gete1of2aPinRLoc(name))
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of3.1000a" ||
         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of3.1000a" loc = Ru_gete1of3aPinRLoc(name))
;        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of4.1000" ||
;         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of4.1000"  loc = Ru_gete1of4PinRLoc(name))
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of4.1000a" ||
         cellTypeName == "lib.buffer.half.DENSE_MBUF_1of4.1000a" loc = Ru_gete1of4aPinRLoc(name))
        (cellTypeName == "lib.serial.scan.MBUF_ChanDft.1003" loc = Ru_getDftPinRLoc(name))
        (t error("ERROR: Ru_getPinRLoc: Unsupported cell type [%s]\n" cellTypeName))
    )
    flipX = (orient == "MX" || orient == "R180")
    flipY = (orient == "MY" || orient == "R180")
    (if flipY loc = list(-xCoord(lowerLeft(loc)):yCoord(lowerLeft(loc)) -xCoord(upperRight(loc)):yCoord(upperRight(loc))))
    (if flipX loc = list(xCoord(lowerLeft(loc)):-yCoord(lowerLeft(loc)) xCoord(upperRight(loc)):-yCoord(upperRight(loc))))
    loc
  )
)

(defun ru_getCellType (name)
  (let (cellType)
    (cond
        (name == "lib.buffer.half.FAST_MBUF_1of1.1000" cellType = "FAST_MBUF_1of1")
        (name == "lib.buffer.half.DENSE_MBUF_1of1.1000" cellType = "DENSE_MBUF_1of1")
        (name == "lib.buffer.half.FAST_MBUF_1of2.1000a" cellType = "FAST_MBUF_1of2")
        (name == "lib.buffer.half.DENSE_MBUF_1of2.1000a" cellType = "DENSE_MBUF_1of2")
        (name == "lib.buffer.half.FAST_MBUF_1of2.1000" cellType = "FAST_MBUF_1of2")
        (name == "lib.buffer.half.DENSE_MBUF_1of2.1000" cellType = "DENSE_MBUF_1of2")
        (name == "lib.buffer.half.FAST_MBUF_1of3.1000a" cellType = "FAST_MBUF_1of3")
        (name == "lib.buffer.half.DENSE_MBUF_1of3.1000a" cellType = "DENSE_MBUF_1of3")
        (name == "lib.buffer.half.FAST_MBUF_1of4.1000" cellType = "FAST_MBUF_1of4")
        (name == "lib.buffer.half.DENSE_MBUF_1of4.1000" cellType = "DENSE_MBUF_1of4")
        (name == "lib.buffer.half.FAST_MBUF_1of4.1000a" cellType = "FAST_MBUF_1of4")
        (name == "lib.buffer.half.DENSE_MBUF_1of4.1000a" cellType = "DENSE_MBUF_1of4")
        (name == "lib.buffer.half.FAST_MBUF_4.1000" cellType = "FAST_MBUF_4")
        (name == "lib.buffer.half.FAST_MBUF_2_1of2.1000" cellType = "FAST_MBUF_2_1of2")
        (t error("ERROR: ru_getCellType: Unsupported cell type [%L]\n" name))
    )
    cellType
  )
)

(defun Ru_TrackOffset (value)
  (let (diff wirePitch grid)
    grid = TrackPitch
    diff = value - round(value/grid)*grid
    ;printf("value %f grid %f round %d diff %f\n" value grid round(value/grid) diff)
    (if diff < 0.0 diff = diff + TrackPitch)
    diff
  )
)

(defun ru_AlignTo (value grid)
  (let (diff wirePitch)
    wirePitch = TrackPitch/24
    diff = value - round(value/grid)*grid
    ;printf("value %f grid %f round %d diff %f\n" value grid round(value/grid) diff)
    (abs(diff) < wirePitch/128)
  )
)

(defun VerifyInstAlignment (@key (xgrid 2*TrackPitch) (ygrid 2*TrackPitch) 
                           (match "chip.alta") (exclude nil) (view nil) (debug nil)
                           (recurseList list("chip.alta.CORE-L76-R.1000"
                                             "chip.alta.datapath.DATAPATH-L76-R.1000"
                                             "chip.alta.fc.FRAME_CONTROL-L76-R.1000"
                                             "chip.alta.fc.early.EARLY-L76-R.1000"
                                             "chip.alta.fc.early.A-L76-R.1000"
                                             "chip.alta.fc.early.B-L76-R.1000"
                                             "chip.alta.fc.late.LATE-L76-R.1000"
                                             "chip.alta.fc.late.A-L76-R.1000"
                                             "chip.alta.fc.late.B-L76-R.1000"
                                             "chip.alta.fc.late.C-L76-R.1000"
                                             "chip.alta.fc.late.D-L76-R.1000"
                                           )
                           )
                           )
  (let (inst rView)
     (if !view view = (geGetEditCellView))
    (foreach inst view->instances
        (if debug printf("INST: %L-%L %L\n" inst->cellName inst->viewName inst->xy))
        (if (!match || (rexMatchp match inst->cellName)) &&
           !(exclude && (rexMatchp exclude inst->cellName)) then
          (if !ru_AlignTo(xCoord(inst->xy) xgrid) then
           printf("%L:%L xCoord %.1f is not aligned to %.1f-track\n" 
               inst->name inst->cellName xCoord(inst->xy)/TrackPitch xgrid/TrackPitch)
          )
          (if !ru_AlignTo(yCoord(inst->xy) ygrid) then
           printf("%L:%L yCoord %.1f is not aligned to %.1f-track\n" 
               inst->name inst->cellName yCoord(inst->xy)/TrackPitch ygrid/TrackPitch)
          )
        )
        (if member(inst->cellName recurseList) then
          printf("########### RecurseDown: %L-%L ############\n" inst->cellName inst->viewName)
          rView = dbOpenCellViewByType(inst->libName inst->cellName inst->viewName)
          VerifyInstAlignment(?view rView ?xgrid xgrid ?ygrid ygrid
                             ?match match ?exclude exclude ?recurseList recurseList)
                             
        )
    )
  )
t
)

(defun VerifyViasAlignment (@key (xgrid 1*TrackPitch) (ygrid 1*TrackPitch) 
                           (match nil) (exclude "chip.alta") (view nil) (debug nil)
                           )
  (let (inst rView)
    (if !view view = (geGetEditCellView))
    (foreach inst view->instances
        (if debug printf("INST: %L-%L %L\n" inst->cellName inst->viewName inst->xy))
        (if (!match || (rexMatchp match inst->cellName)) &&
           !(exclude && (rexMatchp exclude inst->cellName)) then
          (if xgrid && ru_AlignTo(xCoord(inst->xy) xgrid) then
           printf("%L:%L-%L xCoord %.1f is aligned to %.1f-track\n" 
               inst->name inst->cellName inst->xy xCoord(inst->xy)/TrackPitch xgrid/TrackPitch)
          )
          (if ygrid && ru_AlignTo(yCoord(inst->xy) ygrid) then
           printf("%L:%L-%L yCoord %.1f is aligned to %.1f-track.\n" 
               inst->name inst->cellName inst->xy yCoord(inst->xy)/TrackPitch ygrid/TrackPitch)
          )
        )
    )
  )
t
)

;compare location of two different viewName
(defun VerifyInstLoc (cViewName @key (debug nil)
                           (match "chip.alta") (exclude nil) (view nil)
                           (recurseList list("chip.alta.CORE-L76-R.1000"
                                             "chip.alta.datapath.DATAPATH-L76-R.1000"
                                             "chip.alta.fc.FRAME_CONTROL-L76-R.1000"
                                             "chip.alta.fc.early.EARLY-L76-R.1000"
                                             "chip.alta.fc.late.LATE-L76-R.1000"
                                           )
                           )
                           )
  (let (inst rView cView cInst cnt cellName)
    (if !view view = (geGetEditCellView))
    (cond
      (view == "chip.alta.fc.early.EARLY-L76-R.1000"
         cellList = list(view
                      "chip.alta.fc.early.A-L76-R.1000"
                      "chip.alta.fc.early.B-L76-R.1000"
                   )
      )
      (view == "chip.alta.fc.late.LATE-L76-R.1000"
         cellList = list(view
                       "chip.alta.fc.late.A-L76-R.1000"
                       "chip.alta.fc.late.B-L76-R.1000"
                       "chip.alta.fc.late.C-L76-R.1000"
                       "chip.alta.fc.late.D-L76-R.1000"
                   )
      )
      (t cellList = list(view))
    )
    (foreach cellName cellList
    printf("############# Verify cell locations of %L between %L and %L ############\n"
               cellName view->viewName cViewName)
    cView = dbOpenCellViewByType(view->libName cellName cViewName)
    (if !cView then 
      printf("Unable to open %L:%L\n" inst->cellName cViewName)
    else
      (foreach inst view->instances
        (if debug printf("INST: %L-%L %L\n" inst->cellName inst->viewName inst->xy))
        (if (!match || (rexMatchp match inst->cellName)) &&
           !(exclude && (rexMatchp exclude inst->cellName)) then
            cnt = 0
            (foreach cInst cView->instances
              (if (!match || (rexMatchp match cInst->cellName)) &&
                 !(exclude && (rexMatchp exclude cInst->cellName)) then
                (if inst->cellName == cInst->cellName then
                    cnt++
                    (if debug printf("CINST: %L-%L %L\n" cInst->cellName cInst->viewName cInst->xy))
                    (if inst->xy != cInst->xy then
                        printf("%L:%L-%L loc %L is not the same as %L:%L-%L loc %L\n"
                            inst->name inst->cellName inst->viewName inst->xy
                            cInst->name cInst->cellName cInst->viewName cInst->xy)
                    )
                )
              )
            )
            (if cnt == 0 then
                printf("%L:%L-%L loc %L is not found in %L\n"
                  inst->name inst->cellName inst->viewName inst->xy
                  cViewName)
            )
        )
        (if member(inst->cellName recurseList) then
          printf("########### RecurseDown: %L-%L ############\n" inst->cellName inst->viewName)
          rView = dbOpenCellViewByType(inst->libName inst->cellName inst->viewName)
          VerifyInstLoc(cViewName ?view rView
                             ?match match ?exclude exclude ?recurseList recurseList)
                             
        )
      )
    )
    );foreach cellList
  )
t
)

(defun DumpInst (@key (filterCell nil) (filterView nil))
  (let (inst view cnt add)
    view = (geGetEditCellView)
    cnt = 0
      (foreach inst view->instances
        add = nil
        (if (!filterView && !filterCell)  || (filterView && rexMatchp(filterView inst->viewName)) then
            add = t
        )
        (if (!filterView && !filterCell)  || (filterCell && rexMatchp(filterCell inst->cellName)) then
            add = t
        )
        (if add then
          printf("%-60s %L %L\n" inst->cellName inst->viewName inst->xy)
          cnt++
        )
      )
  cnt
  )
)

(defun ru_getTurnOffset (bufLoc cellTypeName pinName dirIn flipY bufInst)
  (let (offset off field wirePitch)
    wirePitch = TrackPitch/24
    (cond
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of1.1000" || cellTypeName == "lib.buffer.half.DENSE_MBUF_1of1.1000"  
            field = Ru_GetLastField(pinName)
            (cond  
              (ru_AlignTo(xCoord(bufLoc) TrackPitch)
                (cond
                  (field == "e" 
                      (if dirIn then offset = wirePitch*5
                      else offset = wirePitch*14)
                  )
                  (field == "0"
                      (if dirIn then offset = wirePitch*10
                      else offset = wirePitch*19)
                  )
                )
              )
              ((!flipY && ru_AlignTo((xCoord(bufLoc)-7*wirePitch) TrackPitch))
               (flipY && ru_AlignTo((xCoord(bufLoc)+7*wirePitch) TrackPitch))
                (cond
                  (field == "e" 
                      (if dirIn then offset = wirePitch*2
                      else offset = wirePitch*21)
                  )
                  (field == "0"
                      (if dirIn then offset = wirePitch*5
                      else offset = wirePitch*24)
                  )
                )
                (if ru_AlignTo(yCoord(bufLoc) 4*TrackPitch) offset = offset + 8*wirePitch)

              )
              (t  error("ERROR: ru_getTurnOffset: %s: Unsupported cell [%s] alignment. Must align to TrackPitch or offset by %f(7*wirePitch). Got %f wirePitch. flipX %L flipY %L bufLoc %L\n" 
                        bufInst->name cellTypeName 7*wirePitch (xCoord(bufLoc)-RoundDownTrack(xCoord(bufLoc)))/wirePitch flipX flipY xCoord(bufLoc)))
            )
            (if flipY offset = -offset)
        )
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of2.1000a" || cellTypeName == "lib.buffer.half.DENSE_MBUF_1of2.1000a"
            field = Ru_GetLastField(pinName)
            (cond
              (ru_AlignTo(xCoord(bufLoc) 2.88)
                ; buffer location is aligned to grid
                (cond
                  (field == "e" 
                    (if dirIn then offset = wirePitch*3
                    else offset = wirePitch*18)
                  )
                  (field == "0"
                    (if dirIn then offset = wirePitch*6
                    else offset = wirePitch*14)
                  )
                  (field == "1" 
                    (if dirIn then offset = wirePitch*10
                    else offset = wirePitch*21)
                  )
                )
              )
              ((!flipY && ru_AlignTo((xCoord(bufLoc)-4*wirePitch) 2.88)) ||
               (flipY && ru_AlignTo((xCoord(bufLoc)+4*wirePitch) 2.88))
                ; buffer location is aligned to grid
                (cond
                  (field == "e" 
                    (if dirIn then offset = wirePitch*1
                    else offset = wirePitch*15)
                  )
                  (field == "0"
                    (if dirIn then offset = wirePitch*5
                    else offset = wirePitch*11)
                  )
                  (field == "1" 
                    (if dirIn then offset = wirePitch*8
                    else offset = wirePitch*25)
                  )
                )
              )
              ((!flipY && ru_AlignTo((xCoord(bufLoc)-20*wirePitch) 2.88)) ||
               (flipY && ru_AlignTo((xCoord(bufLoc)+20*wirePitch) 2.88))
                ; buffer location is aligned to grid
                (cond
                  (field == "e" 
                    (if dirIn then offset = wirePitch*1
                    else offset = wirePitch*21)
                  )
                  (field == "0"
                    (if dirIn then offset = wirePitch*9
                    else offset = wirePitch*17)
                  )
                  (field == "1" 
                    (if dirIn then offset = wirePitch*13
                    else offset = wirePitch*25)
                  )
                )
              )
              (t  error("ERROR: ru_getTurnOffset: Unsupported cell [%s] alignment. Must align to 2.88 or offset by %f. Got %f wirePitch. flipX %L flipY %L\n"
                        cellTypeName 4*wirePitch (RoundDownTrack(xCoord(bufLoc))-xCoord(bufLoc))/wirePitch flipX flipY ))
            )
            (if flipY offset = -offset)
        )
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of3.1000a" || cellTypeName == "lib.buffer.half.DENSE_MBUF_1of3.1000a"
            field = Ru_GetLastField(pinName)
            (cond  
              (ru_AlignTo(xCoord(bufLoc) 2.88)
                ; buffer location is aligned to grid
                (cond
                  (field == "e" 
                    offset = (if flipY -wirePitch*18 wirePitch*6)
                  )
                  (field == "0"
                    offset = (if flipY -wirePitch*14 wirePitch*10)
                  )
                  (field == "1" 
                    offset = (if flipY -wirePitch*10 wirePitch*14)
                  )
                  (field == "2" 
                    offset = (if flipY -wirePitch*6 wirePitch*18)
                  )
                )
              )
              (t  error("ERROR: ru_getTurnOffset: Unsupported cell [%s] alignment. Must align to 2.88\n" cellTypeName ))
            )
            (if !dirIn offset = offset + (if flipY -TrackPitch TrackPitch))
        )
 
        (cellTypeName == "lib.buffer.half.FAST_MBUF_1of4.1000a" || cellTypeName == "lib.buffer.half.DENSE_MBUF_1of4.1000a" ;cellTypeName == "lib.buffer.half.FAST_MBUF_1of4.1000"
            field = Ru_GetLastField(pinName)
            (cond  
              (ru_AlignTo(xCoord(bufLoc) 2.88)
                ; buffer location is aligned to grid
                (cond
                  (field == "e" 
                    offset = (if flipY -wirePitch*19 wirePitch*3)
                  )
                  (field == "0"
                    offset = (if flipY -wirePitch*15 wirePitch*7)
                  )
                  (field == "1" 
                    offset = (if flipY -wirePitch*11 wirePitch*11)
                  )
                  (field == "2" 
                    offset = (if flipY -wirePitch*7 wirePitch*15)
                  )
                  (field == "3" 
                    offset = (if flipY -wirePitch*3 wirePitch*19)
                  )
                )
              )
              (t  error("ERROR: ru_getTurnOffset: Unsupported cell [%s] alignment. Must align to 2.88\n" cellTypeName ))
            )
            (if !dirIn offset = offset + (if flipY -TrackPitch TrackPitch))
        )
        (cellTypeName == "lib.serial.scan.MBUF_ChanDft.1003"
            field = Ru_GetLastField(pinName)
            (cond
              (ru_AlignTo(xCoord(bufLoc) 2.88)
                ; buffer location is aligned to grid
                (cond
                  (field == "D.e" 
                    (if dirIn then offset = (if flipY -wirePitch*3 wirePitch*3)
                    else offset = (if flipY -wirePitch*21 wirePitch*21)) ;swap with 1 to prevent vias violation
                  )
                  (field == "D.0"
                    (if dirIn then offset = (if flipY -wirePitch*6 wirePitch*6)
                    else offset = (if flipY -wirePitch*14 wirePitch*14))
                  )
                  (field == "D.1" 
                    (if dirIn then offset = (if flipY -wirePitch*10 wirePitch*10)
                    else offset = (if flipY -wirePitch*18 wirePitch*18))
                  )
                  (field == "C.0" 
                    (if dirIn then offset = (if flipY -wirePitch*28 wirePitch*28)
                    else offset = (if flipY -wirePitch*35 wirePitch*35))
                  )
                  (field == "C.1"
                    (if dirIn then offset = (if flipY -wirePitch*32 wirePitch*32)
                    else offset = (if flipY -wirePitch*39 wirePitch*39))
                  )
                  (field == "C.2" 
                    (if dirIn then offset = (if flipY -wirePitch*44 wirePitch*44)
                    else offset = (if flipY -wirePitch*52 wirePitch*52))
                  )
                )
              )
              (t  error("ERROR: ru_getTurnOffset: Unsupported cell [%s] alignment. Must align to 2.88 or offset by %f\n" cellTypeName 4*wirePitch))
            )
        )
        (t error("ERROR: ru_getTurnOffset: Unsupported cell type [%s]\n" cellTypeName))
    )
    (if !offset error("ERROR: ru_getTurnOffset: Unknown pin name [%s] field [%s]\n" pinName field))
    offset
  )
)


;#####################################################################################
(defun TerminateChannel (path busPatternsList busPrefix @key 
                           (filterList nil) (filterExcludeList nil) (forceViasDir nil) (forceLayerIdx -1) (doubleVias t) 
                           (debugStackVias nil) (flipX nil) (flipY nil) (busPosfix "") (debug 0))
  (let (numErr wirePitch forceLayer offsetTurn drawPath wireName nodeName result)
    (if debug >= 1 printf("TerminateChannel %s path %L\n" busPrefix path)) 
     pointTypes = (Ru_ClassifyPoints path)
    (if length(busPatternsList) != (length(path)-1) 
        error("busPatternsList (%d) must be the same as number of segments(%d)\n" 
              length(busPatternsList)  (length(path)-1))
    )
    wirePitch = TrackPitch/24
    numSeg = length(busPatternsList)
    declare(segLp[numSeg])
    declare(segLayerNum[numSeg+1])

    numErr = 0
    (foreach bp car(busPatternsList)
        chName = nth(0 bp)
        channel = nth(1 bp)
        ;printf("########## Looking for chName %L channel %L\n" chName channel)
        (foreach ch channel
            skip = nil
            forceLayer = nil
            segLp[0] = ch
            (if debug >= 3 printf("SegLp#%d: %L\n" 0 segLp[0]))
            node = nth(0 ch)

termWire = t
(if filterList then
    filterNode = ru_GetFilterList(filterList node)
    (if debug >= 3 && filterNode printf("%L Found in filter list: %L\n" node filterNode))
    (if debug >= 4 && !filterNode printf("%L Not Found in filter list\n" node))

    termWire = (filterNode!=nil)
    (if filterNode && nth(1 filterNode) == "OffsetTurn" offsetTurn = nth(2 filterNode))
)
(if filterExcludeList then
    filterNode = ru_GetFilterList(filterExcludeList node)
    (if debug >= 1 && filterNode printf("%L Found in filter exclude list: %L\n" node filterNode))
    (if debug >= 4 && !filterNode printf("%L Not Found in filter exclude list\n" node))

    termWire = !(filterNode!=nil)
)

(if termWire then
            width = nth(1 ch)
            offset = nth(2 ch)
            layer = nth(3 ch)
            (if forceLayerIdx == 0 && !forceLayer then forceLayer = layer)
            filteredNode = (if strlen(node)>1 substring(node 2) node)
            result = Ru_SplitNodeName(filteredNode)
            filteredNode = sprintf(nil "%s%s.%s" Ru_TrimLeadingDots(car(result)) busPosfix cadr(result))
            pattern = list(list(filteredNode cadr(car(node_2W)) offset nil)) ;(DefChan (BuildDefChanList nil filteredNode 1 node_2W offset nil))
            (if debug >= 2 printf("Node#0: [%L] width %L layer %L pattern %L\n" node width layer pattern))
            layerPatternList = list(layer pattern)
            altLayerPatternList = nil
            hv = nth(0 pointTypes)
            segLayerNum[0] = ru_layerToValue(car((if (hv=="EH"||hv=="VH") nth(0 layer) nth(1 layer))))
            segLayerNum[1] = segLayerNum[0]

            ;get each layer and pattern for each segment
            (for segNum 1 (numSeg-1)
                (if nth(segNum busPatternsList) then
                    segLp[segNum] = Ru_GetBusPatternEntry(nth(segNum busPatternsList) chName node)
                else
                    segLp[segNum] = segLp[0]
                )
                ch = segLp[segNum]
                (if ch then
                    node = nth(0 ch)
                    offset = nth(2 ch)
                    layer = nth(3 ch)
                    (if forceLayerIdx == segNum && !forceLayer then 
                        (if debug >= 2 printf("Layer is forced to %L\n" layer))
                        forceLayer = layer
                    )
                    filteredNode = (if strlen(node)>1 substring(node 2) node)
                    result = Ru_SplitNodeName(filteredNode)
                    filteredNode = sprintf(nil "%s%s.%s" Ru_TrimLeadingDots(car(result)) busPosfix cadr(result))
                    pattern = list(list(filteredNode cadr(car(node_2W)) offset nil)) ;(DefChan (BuildDefChanList nil filteredNode 1 node_2W offset nil))
                    (if debug >= 2 printf("Node#%d: [%L] width %L layer %L pattern %L\n" segNum node width layer pattern))
                    altLayerPatternList = append(altLayerPatternList list(list(layer pattern)))
                    hv = nth(segNum pointTypes)
                    segLayerNum[segNum] = ru_layerToValue(car((if (hv=="EH"||hv=="VH") nth(0 layer) nth(1 layer))))
                    segLayerNum[segNum+1] = segLayerNum[segNum]
                else
                    skip = t
                    numErr++
                    (if numErr < 20 printf("ERROR: Unable to find node %L in segment %d: %L\n" node segNum nth(segNum busPatternsList)))
                )
                (if debug >=2 printf("SegLp#%d: %L\n" segNum segLp[segNum]))
            )
            (if !skip then
                (for segNum 0 (numSeg-1)
                    fromLayer = segLayerNum[segNum]
                    toLayer = segLayerNum[segNum+1]
                    (if !toLayer toLayer = fromLayer)
                    viasList = GenViasList(fromLayer toLayer)
                    (if mod(fromLayer 2) == 1 then
                        layer = list(list(sprintf(nil "M%d" fromLayer)) list(sprintf(nil "M%d" toLayer)) viasList forceViasDir)
                    else
                        layer = list(list(sprintf(nil "M%d" toLayer)) list(sprintf(nil "M%d" fromLayer)) viasList forceViasDir)
                    )
                    (if debug >=2 printf("SegLp#%d: CfgLayer: %L fromLayer %d toLayer %d viasList: %L Layer %L\n"
                            segNum segLp[segNum] fromLayer toLayer viasList layer))
                    (if segNum == 0 layerPatternList = list(layer nth(1 layerPatternList)))
                )
                (if forceLayer then 
                    (if debug >=2 printf("Layer is forced to %L\n" forceLayer))
                    layerPatternList = list(forceLayer nth(1 layerPatternList))
                    tmpList = nil
                    (foreach el altLayerPatternList
                        tmpList = append(tmpList list(list(forceLayer nth(1 el))))
                    )
                    altLayerPatternList = tmpList
                )
                (if offsetTurn then
                    (if length(path) != 4 error("Only support OffsetTurn with 3 segments path, but got %d segments\n" length(path)-1))
                    (let (p1 p2)
                       p1 = nth(1 path)
                       p2 = nth(2 path)
                       (if almostEqual(xCoord(p1) xCoord(p2)) then
                           p1 = (xCoord(p1)+offsetTurn):yCoord(p1)
                           p2 = (xCoord(p2)+offsetTurn):yCoord(p2)
                       else
                           p1 = xCoord(p1):(yCoord(p1)+offsetTurn)
                           p2 = xCoord(p2):(yCoord(p2)+offsetTurn)
                       )
                       drawPath = list(nth(0 path) p1 p2 nth(3 path))
                       (if debug >= 1 printf("Path is offset to %L from %L\n" drawPath path))
                    )
                else
                    drawPath = path
                )
                nodeName = car(car(nth(1 layerPatternList)))
                wireName = sprintf(nil "%s%s" busPrefix nodeName)
                (if debug >=1 printf("%s path: %L layerPattern: %L altLayerPatternList: %L \n"
                                     wireName drawPath layerPatternList altLayerPatternList))
                (if debug >=2 printf("layer: %L pattern: %L\n" car(layerPatternList) nth(1 layerPatternList)))

                DrawChannel( nth(0 layerPatternList) nth(1 layerPatternList) busPrefix drawPath
                             ?flipX flipX ?flipY flipY
                             ?altLayerPatternList altLayerPatternList ?doubleVias doubleVias)
                (if debugStackVias then
                    (for segNum 0 length(altLayerPatternList)-1
                        (if segNum == 0 then 
                          layer = car(layerPatternList)
                          pattern = nth(1 layerPatternList)
                          nextPattern = nth(1 nth(0 altLayerPatternList))
                        else 
                          layer = car(nth(numSeg-1 altLayerPatternList))
                          pattern = nth(1 (nth(numSeg-1 altLayerPatternList)))
                          nextPattern = nth(1 (nth(numSeg altLayerPatternList)))
                        )
                        viasList = nth(2 layer)
                        (if !listp(viasList) viasList = list(viasList))
                        (if debug >= 2 printf("VIAS SEG#%d %s seg %L layer %L\n" segNum wireName nth(segNum+1 drawPath) viasList))
                        (foreach node_offset pattern
                            node = (car node_offset)
                            offset = (caddr node_offset)
                            nextNode = Ru_GetChannelEntry(nextPattern node)
                            nextOffset = (caddr nextNode)
                            hv = nth(numSeg+1 pointTypes)
                            (if hv == "EH"  || hv == "HV" then
                                offsetX = offset 
                                offsetY = nextOffset
                            else 
                                (if hv == "VE" || hv == "HE" then
                                    offsetX = nextOffset
                                    offsetY = nextOffset
                                else
                                    offsetX = nextOffset
                                    offsetY = offset
                                )
                            )
                            corner = Ru_OffsetPoint(nth(segNum+1 drawPath) 
                                       (if flipX -1 1)*(if (and hv!="HE" hv!="EH") offsetX 0):
                                       (if flipY -1 1)*(if (and hv!="VE" hv!="EV") offsetY 0))

                            seg = list(Ru_OffsetPoint(corner -1*wirePitch:0.0) Ru_OffsetPoint(corner 1*wirePitch:0.0))

                            (for nVias 1 length(viasList)-2
                                vias = nth(nVias viasList)
                                slayer = car(parseString(vias "_"))
                                DrawWire( list(list(slayer) list(slayer)) (AddSuffix busPrefix node) wirePitch seg
                                         ?flipX flipX ?flipY flipY)
                            )
                        )
                    )
                )
            )


);termWire
        ) ;foreach
        (if numErr > 20 printf("ERROR: Too many errors to list all\n"))
    )
  )
)





(defun Ru_MidX (box)
  ret = (xCoord(lowerLeft(box)) + xCoord(upperRight(box)))/2
)

(defun Ru_MidY (box)
  ret = (yCoord(lowerLeft(box)) + yCoord(upperRight(box)))/2
)

(defun Ru_OffsetBox (box offset)
let( ( llx lly urx ury )
    urx = xCoord( upperRight( box ))
    ury = yCoord( upperRight( box ))
    llx = xCoord( lowerLeft( box ))
    lly = yCoord( lowerLeft( box ))
    offx = xCoord(offset)
    offy = yCoord(offset)
    list((llx+offx):(lly+offy) (urx+offx):(ury+offy))
) ; let
)

(defun GenViasList (hl vl @key (forceDir nil))
  (let (viasList)
    (if hl < vl then
        (for l hl (vl-1)
        viasList = append(viasList list(sprintf(nil  "M%d_M%d" l+1 l)))
        )
    else
        (for l vl (hl-1)
            viasList = append(viasList list(sprintf(nil  "M%d_M%d" l+1 l)))
        )
    )
    (if forceDir then
        viasList = list(viasList forceDir)
    else viasList)
  )
)

;generate intermediate layers when not going to adjacent layer
(defun ru_GenLayerList (hl vl)
  (let (layerList)
    (cond
      (abs(hl-vl) <= 1
          layerList = nil
      )
      (hl < vl
        (for l (hl+1) (vl-1)
            layerList = append(layerList  list(list(list(sprintf(nil "M%d" l)) list(sprintf(nil "M%d" l)) nil nil)))
        )
      )
      (t
        (for l (vl+1) (hl-1)
            layerList = append(layerList  list(list(list(sprintf(nil "M%d" l)) list(sprintf(nil "M%d" l)) nil nil)))
        )
      )
    )
    layerList
  )
)

;points - can be one point or list of points
(defun DrawStackedVias (wireName points fromLayer toLayer @key (doubleVias t) (extend TrackPitch/2))
  (let (viasList viasPoint wirePitch)
    wirePitch = TrackPitch/24
    viasList = GenViasList(fromLayer toLayer)
    (if listp(car(point)) then
      viasPoint = Ru_OffsetPoint(length(points)-1 points)
      DrawVia(wireName viasList viasPoint wirePitch cuts ?doubleVias doubleVias)
    else
      ;single point
      DrawVia(wireName viasList points wirePitch cuts ?doubleVias doubleVias)
    )
  )
)

(defun Ru_LocateBuffers (bufPrefix busPattern range typeList @key (bufPosfix "") (bufMap nil) (debug 0) )
  (let (bufList lastName bp name bufName inst entry)
    lastName = ""
    (if !listp(typeList) typeList = list(typeList))
    (foreach bp busPattern
        name = nth(0 bp)
        (if strncmp(lastName name (strlen(name)-2)) != 0 then 
            bufName = nil
            (if bufMap then
                rexMagic(nil)
                (foreach entry bufMap
                    ;printf("Node %s: Entry %L\n" name entry)
                    (if !bufName && rexMatchp(nth(0 entry) substring(name 1 strlen(name)-2)) then
                        bufName = nth(1 entry)
                        ;printf("%s matched to buf %s\n" name bufName)
                     )
                )
                rexMagic(t)
            )
            (if !bufName bufName = sprintf(nil "%s%s%s" bufPrefix Ru_GetFirstField(name) bufPosfix))
            inst = (dbFindAnyInstByName (geGetEditCellView) bufName)
            (if inst && member(ru_getCellType(inst->master->cellName) typeList) then
                wireOff = nth(2 bp) + TrackPitch/2
                (if debug >= 1 printf("bufName %s wireOff %f range %L \n" bufName wireOff range))
                (if wireOff >= nth(0 range) && wireOff <= nth(1 range) then
                    ;printf(" %s: CellView: %s\n" bufName inst->master->cellName)
                    (if !member(bufName bufList) bufList = append(bufList list(bufName)))
                ) 
            )
            (if !inst then
                printf("Ru_LocateBuffers: No such buffer %L found. Node %s First Field %s\n" bufName name Ru_GetFirstField(name) )
indexList = parseString(name "[]")
printf("Index %L\n" indexList)
(if integerp(nth(1 indexList)) then
    printf("INDEX %d\n" nth(1 indexList))
)
            )
        )
        lastName = name
    )
    bufList
  )
)

(defun StackBuffersVert (bufList startLoc @key (offset nil) (orient nil) 
                         (postFix "") (errorMsg t) (reserveGap nil) (setProp nil))
  (let (loc entry bufName sizeY inst gap cnt val)
    sizeY = 4*TrackPitch
    loc = startLoc
    (foreach entry bufList
        bufName = strcat(entry postFix)
        inst = (dbFindAnyInstByName (geGetEditCellView) bufName)
        (if inst then
            (foreach gap reserveGap
                val = yCoord(loc)
                cnt = 0
                (while Ru_IsBetween(nth(0 gap) val nth(1 gap))
                    cnt++
                    printf("%L is in the reserve gap, advance #%d\n" bufName cnt)
                    loc = Ru_OffsetPoint(loc 0:(if offset offset sizeY))
                    val = yCoord(loc)
                )
            )
            inst->xy = loc
            (foreach prop setProp
                (dbReplaceProp inst car(prop) cadr(prop) nth(2 prop))
            )
            (if orient inst->orient = orient)
            sizeY = (if offset offset yCoord(upperRight(inst->bBox))-yCoord(lowerLeft(inst->bBox)))
            loc =  Ru_OffsetPoint(loc 0.0:sizeY)
        else
            (if errorMsg printf("ERROR: Unable to find %s. Need a buffer at %L\n" bufName loc))
        )
    )
    loc
  )
)

(defun StackBuffersHor (bufList startLoc @key (offset nil) (orient nil) 
                           (minDist nil) (postFix "") (errorMsg t) (setProp nil)
                           (offsetPattern nil) (numTrack 4) (e1of2Offset nil))
  (let (loc entry bufName sizeX inst nodeBaseName nodeName nodeOffset curOffY curOffYTracks maxOff)
    loc = startLoc
    curOffYTracks = 0
    maxOff = 0
    (if offsetPattern maxOff = 1000) ;hardcoded
    declare( baseY[maxOff+1])
    (for cnt 0 maxOff
      baseY[cnt] = 0
    )
    (foreach entry bufList
        bufName = strcat(entry postFix)
        inst = (dbFindAnyInstByName (geGetEditCellView) bufName)
        (if inst then
          (if offsetPattern then
              nodeBaseName = Ru_GetNodeBaseNameFromBufName(bufName)
              nodeName = sprintf(nil ".%s.e" nodeBaseName)
              nodeOffset = Ru_GetChannelNodeOffset(offsetPattern nodeName ?nestedArray nil)
              (if !nodeOffset error("%s: Unable to find %L in offsetPattern %L\n" bufName nodeName offsetPattern))
              nodeOffset = nodeOffset + TrackPitch/2
              curOffYTracks = floor((nodeOffset+TrackPitch/64)/(numTrack*TrackPitch))
              ;(if curOffY != floor(nodeOffset/offsetMod) then
              ;    curOffY =  nodeOffset-floor(nodeOffset/offsetMod)
              ;)
;printf("%s nodeOffset %L curOffY %L %L loc %L\n" bufName nodeOffset curOffY curOffYTracks loc)
          )
          loc = Ru_OffsetPoint(startLoc baseY[curOffYTracks]:curOffYTracks*numTrack*TrackPitch)
          (if e1of2Offset && rexMatchp("1of2" inst->master->cellName) then
              loc = OffsetPoint(loc e1of2Offset 0)
          )
          inst->xy = loc
          (foreach prop setProp
              (dbReplaceProp inst car(prop) cadr(prop) nth(2 prop))
          )
          (if orient inst->orient = orient)
           sizeX = (if offset offset xCoord(upperRight(inst->bBox))-xCoord(lowerLeft(inst->bBox)))
           (if minDist && sizeX < minDist sizeX = minDist)
           baseY[curOffYTracks] = baseY[curOffYTracks] + sizeX
           ;loc =  Ru_OffsetPoint(loc sizeX:0.0)
        else
          (if errorMsg printf("ERROR: Unable to find %s. Need a buffer at %L\n" bufName loc))
        )
    )
    loc
  )
)

(defun SpaceBuffers (bufPrefix firstIdx lastIdx spacingX spacingY)
  (let (inst incr)
    inst = (dbFindAnyInstByName (geGetEditCellView) sprintf(nil "%s[%d]" bufPrefix firstIdx))
    (if !inst  error("ERROR: Unable to find %s.\n" sprintf(nil "%s[%d]" bufPrefix firstIdx)))
    baseLoc = inst->xy
    (if firstIdx < lastIdx then
    (for i firstIdx+1 lastIdx
        incr = i - firstIdx
        MoveInst(sprintf(nil "%s[%d]" bufPrefix i) OffsetPoint(baseLoc (incr*spacingX) (incr*spacingY)))
    )
    else
    (for i lastIdx firstIdx-1
        incr = firstIdx-i
        MoveInst(sprintf(nil "%s[%d]" bufPrefix i) OffsetPoint(baseLoc (incr*spacingX) (incr*spacingY)))
    )
    )
  )
)

(defun CombineList (listA listB every)
  (let (listEntry cnt i entry)
    cnt = 0
    (while cnt < length(listA)
        (for i 0 every-1
            entry = nth(cnt+i listA)
            (if entry listEntry = append(listEntry list(entry)))
        )
        (for i 0 every-1
            entry = nth(cnt+i listB)
            (if entry listEntry = append(listEntry list(entry)))
        )
        cnt = cnt + every
    )
    (for i cnt length(listB)-1
        entry = nth(i listB)
        (if entry listEntry = append(listEntry list(entry)))
    )
    listEntry
  )
)

(defun Ru_SplitList (listEntry every)
  (let (leftList rightList)
    leftList = nil
    rightList = nil
    (for cnt 0 (length(listEntry)-1)
        (if mod(cnt every*2) < every then
          leftList = append(leftList list(nth(cnt listEntry)))
        else
          rightList = append(rightList list(nth(cnt listEntry)))
        )
    )
    list(leftList rightList)
  )
)

;Add channel overide the layer
(defun Ru_AddChannel (channel layer)
  (let (ret)
    (foreach el channel
        (if nth(0 el) != "GND" ret = append(ret list(list( nth(0 el)  list(list("" nth(1 el) 0)) nth(2 el) layer))))
    )
    ret = DefChan(ret)
  )
)


;These use global variable bundledList, node2BufMap, node2BufMap
(defun Ru_AddMbufList2A (prefix cnt)
  (let (name bufName)
    name = car(parseString(prefix "."))
    bundledList = append(bundledList list(sprintf(nil "%s[0..%d]" prefix cnt)) )
    (for i 0 cnt
        bufName = sprintf(nil "mbuf_%s[%d]" name i)
        mbufList = append(mbufList list(bufName))
        node2BufMap = append(node2BufMap list(list(sprintf(nil "%s[%d]" prefix i*2)  bufName)) )
        node2BufMap = append(node2BufMap list(list(sprintf(nil "%s[%d]" prefix i*2+1) bufName)) )
    )
  )
)
(defun Ru_AddMbufList2 (bufName node1 node2)
    bundledList = append(bundledList list(node1))
    bundledList = append(bundledList list(node2))
    mbufList = append(mbufList list(bufName))
    node2BufMap = append(node2BufMap list(list(node1 bufName)))
    node2BufMap = append(node2BufMap list(list(node2 bufName)))
)
(defun Ru_AddMbufList (bufName node1)
    bundledList = append(bundledList list(node1))
    mbufList = append(mbufList list(bufName))
    node2BufMap = append(node2BufMap list(list(node1 bufName)))
)

(defun Ru_DumpChannelList ()
  (let (entry)
    printf("%d entries\n" length(node2BufMap))
    printf("============= Channel List ===============\n")
    (foreach entry node2BufMap
        printf("        (list \".%s\"  inlv_e1of4_X 0*TrackPitch BusMetal67)\n" car(entry))
    )
    printf("============= Bundled List ===============\n")
    (foreach entry bundledList
        printf("              \"L.%s\" \"R.%s\"\n" entry entry)

    )
  )
)

; load("/home/user/tnguyen/alta4/virtuoso/skill/layout/route/routepath.il") 
(defun InstantiateSubCellsPowerGrid (@key (debug nil))
  (let (inst result cnt pgName)
    view = (geGetEditCellView)
    (foreach inst view->instances
        printf("%L\n" inst->cellName)
        result = parseString(inst->cellName ".")
        pgName = car(result)
        len = length(result)
        (for cnt 1 len-2
            pgName = strcat(pgName sprintf(nil ".%s" nth(cnt result)))
            (if cnt == len-3 pgName = strcat(pgName ".wires"))
        )
        cnt = len-1
        pgName = strcat(pgName sprintf(nil "_%s_POWER_GRID_TIEOFF" nth(cnt result)))
        printf("PG: %L\n" pgName)
        pgView  = dbOpenCellViewByType(inst->libName pgName "layout")
        (if pgView then
            dbCreateInst(view pgView strcat(inst->name ".tiehilow") inst->xy inst->orient 1)
        else
            printf("ERROR: Unable to open layout view for %L\n" pgName)
        )
    )
;chip.alta.route.ffu_to_nexthop.wires.PATH_FfuToNextHopHdChannel_1000_POWER_GRID_TIEOFF
;chip.alta.route.ffu_to_l3ar.wires.PATH_FfuToL3ArHdChannelA_1000_POWER_GRID_TIEOF
;chip.alta.route.ffu_to_l3ar.wires.PATH_FfuToL3ArHdChannelA_1000_POWER_GRID_TIEOFF
  )
)

(defun Ru_DrawChannel (layers pattern prefix points
                           @key (flipX nil) (flipY nil) (isPin nil)
                           (startVias nil) (endVias nil) (doubleVias t)
                           (altLayerPatternList nil)
                           (guideInstName nil) (guideInst nil))
  (let (newpoints node width offset index x y hv
        offsetX offsetY nextOffset curOffset lastLayer curLayer tmpx tmpy cuts nodeIdx
        curPattern nextLayer nextPattern stVias eVias segNum layer_override lastx lasty
        (types (ClassifyPoints points)))

    ; set guide instance
    guideInst = (GetGuideInst guideInst guideInstName)

    ; go through all nodes in channel
    nodeIdx = 0
    (foreach node_offset pattern
             node = (car node_offset)
             width = (cadr node_offset)
             offset = (caddr node_offset)
             layer_override = (cadddr node_offset)
             newpoints = nil
             index = 0
             segNum = 0 ; for multiple segment due to changing layers
             lastLayer = curLayer
             (foreach point points
                  x = (car point)
                  y = (cadr point)
                  hv = (nth index types)
                 (if altLayerPatternList || listp(startVias) || listp(endVias) then
                  ;altLayerPatternList either a list of patterns for each segments or a single one
                  ;if a single one it will be used for the second segment to the end
                  ;a nil entry in the altLayerPatternList will use the original layer and pattern
                  nextOffset = offset
                  curOffset = offset
                  curLayer = layers
                  (if length(altLayerPatternList) > 1
                  then
                      ;current pattern
                      (if index >= 2 && nth(1 nth(index-2 altLayerPatternList)) 
                      then
                          curLayerPattern = nth(index-2 altLayerPatternList)
                          curLayer =  nth(0 curLayerPattern )
                          curPattern = nth(1 curLayerPattern )
                          (if car(nth(nodeIdx curPattern)) == node then ;assume the same index order
                            curOffset = caddr(nth(nodeIdx curPattern))
                          else
                            (foreach cur_node_offset curPattern
                              (if node == (car cur_node_offset) then
                                  curOffset  = (caddr cur_node_offset)
                              )
                            )
                          )
                      )
                      ;next pattern
                      (if index >= 1 && nth(1 nth(index-1 altLayerPatternList)) 
                      then
                          nextLayer = nth(0 nth(index-1 altLayerPatternList))
                          nextPattern = nth(1 nth(index-1 altLayerPatternList))
                          (if car(nth(nodeIdx nextPattern)) == node then ;assume the same index order
                            nextOffset = caddr(nth(nodeIdx nextPattern))
                          else
                            (foreach next_node_offset nextPattern
                              (if node == (car next_node_offset) then
                                  nextOffset  = (caddr next_node_offset)
                              )
                            )
                          )
                      )
                  else
;  load "/home/user/tnguyen/alta4/virtuoso/skill/layout/bus/bus.il"
                      ;single alternate pattern
                      altLayerPattern = nth(0 altLayerPatternList)
                      altLayer = nth(0 altLayerPattern)
                      (if !altLayer altLayer = layers) ;for viasList
                      altPattern = nth(1 altLayerPattern)
                      (if !altPattern altPattern = pattern)
                      (if car(nth(nodeIdx altPattern)) == node then ;assume the same index order
                          altOffset = caddr(nth(nodeIdx altPattern))
                      else
                        (foreach alt_node_offset altPattern
                          (if node == (car alt_node_offset) then
                              altOffset  = (caddr alt_node_offset)
                          )
                        )
                      )
                      (if index <= 1
                      then curLayer = layers nextLayer = (if (index == 0) layers altLayer) curOffset = offset nextOffset = altOffset
                      else curLayer = altLayer nextLayer = altLayer curOffset = altOffset nextOffset = altOffset
                      )
                  )
                  (if hv == "EH"  || hv == "HV" then
                      offsetX = nextOffset
                      offsetY = curOffset
                  else 
                      (if hv == "VE" || hv == "HE" then
                          offsetX = curOffset
                          offsetY = curOffset
                      else
                          offsetX = curOffset
                          offsetY = nextOffset
                      )
                  )
                  ;layer info is in the startVias to draw multiple vias
                  stVias = (segNum == 0 && startVias == t)
                  (if (caddr curLayer) != (caddr lastLayer) && length(newpoints) >= 1 then
                      ;switch layers if via list is different
                      tmpx = x + (if flipX -1 1)*(if (and hv!="HE" hv!="EH") offsetX 0.0)
                      tmpy = y + (if flipY -1 1)*(if (and hv!="VE" hv!="EV") offsetY 0.0)
                      newpoints = (append newpoints (list (list ((lastx+tmpx)/2.0) ((lasty+tmpy)/2.0))))
                      (DrawWire lastLayer (AddSuffix prefix node)
                                          width newpoints
                                          ?isPin isPin ?startVias stVias ?endVias nil
                                          ?doubleVias doubleVias ?guideInst guideInst)
                      newpoints = (list (list ((lastx+tmpx)/2.0) ((lasty+tmpy)/2.0)))
                  )
                  lastLayer = curLayer
                  lastx= x+(if flipX -1 1)*(if (and hv!="HE" hv!="EH") offsetX 0)
                  lasty = y+(if flipY -1 1)*(if (and hv!="VE" hv!="EV") offsetY 0)
                  (if segNum == 0 && startVias && startVias!=t then
                     ;startVias can draw multiple vias or force direction
                      (if listp(car(startVias)) then
                          cuts = strcat("_2CUT_" cadr(startVias))
                          stVias = car(startVias)
                      else
                          cuts = (if (and hv!="HE" hv!="EH") "_2CUT_V" "_2CUT_H")
                          stVias = startVias
                      )
                      DrawVia((AddSuffix prefix node) stVias lastx:lasty 
                               width cuts ?doubleVias doubleVias ?stacked !doubleVias)
                      stVias = nil
                  )
                  eVias = endVias
                  (if segNum == length(points)-1 && endVias && endVias!=t then
                     ;endVias can draw multiple vias or force direction
                      (if listp(car(endVias)) then
                          cuts = strcat("_2CUT_" cadr(endVias))
                          eVias = car(endVias)
                      else
                          cuts = (if (and hv!="HE" hv!="EH") "_2CUT_V" "_2CUT_H")
                          eVias = endVias
                      )
                      DrawVia((AddSuffix prefix node) eVias lastx:lasty 
                               width cuts ?doubleVias doubleVias ?stacked !doubleVias)
                      eVias = nil
                  )
                  segNum++
              else
                  ; no alternate pattern
                  offsetX = offset
                  offsetY = offset
                  stVias = startVias
                  eVias = endVias
                  curLayer = layers
              )

              x = x+(if flipX -1 1)*(if (and hv!="HE" hv!="EH") offsetX 0)
              y = y+(if flipY -1 1)*(if (and hv!="VE" hv!="EV") offsetY 0)
              newpoints = (append newpoints (list (list x y)))
              index = index+1
          )
          ; draw wire for this node
          (DrawWire (if layer_override layer_override curLayer)
                    (AddSuffix prefix node) width newpoints
                    ?isPin isPin ?startVias stVias ?endVias eVias
                    ?doubleVias doubleVias ?guideInst guideInst)
      nodeIdx++
    )
  )
  t
  )


;Allow to extend end points
(defun Ru_DrawWire (layers name width points
                        @key (isPin nil) (startVias nil) (endVias nil) (doubleVias t) (vias2Cut t)
                        (beginExt nil) (endExt nil)
                        (guideInst nil))
  (let (horz_layer vert_layer via viadir last first lastIndex index cuts layer)
;Ru_ValidatePath(points  sprintf(nil "Ru_DrawWire: %L points %L" name points))
    horz_layer = (car layers)
    vert_layer = (cadr layers)
    via = (caddr layers)
    viadir = (cadddr layers)
    points = (CanonicalizePoints points)
    last = (car points)
    first = t
    lastIndex = (length points)-2
    index = 0
    (if vias2Cut cuts = (if (almostEqual (car (cadr points)) (car last)) "_2CUT_V" "_2CUT_H"))
    (if startVias (DrawVia name via last width cuts ?doubleVias doubleVias ?guideInst guideInst))
    (foreach point (cdr points)
             layer = (if (almostEqual (car point) (car last)) vert_layer horz_layer)
             (DrawSegment name layer width last point
                          (index!=0||beginExt) 
                          (index!=lastIndex || endExt) 
                          isPin ?guideInst guideInst)
             cuts = (if (and doubleVias viadir) (strcat "_2CUT_" viadir) "")
             (if (not first) (DrawVia name via last width cuts
                                      ?doubleVias doubleVias ?guideInst guideInst))
             first = nil
             (if vias2Cut cuts = (if (almostEqual (car point) (car last)) "_2CUT_V" "_2CUT_H"))
             last = point
             index = index+1
             )
    (if endVias (DrawVia name via last width cuts ?doubleVias doubleVias ?guideInst guideInst))
    )
  t
  )

(defun Ru_SegmentDirection (a b)
    (let (dx dy)
        dx = (nth 0 b) - (nth 0 a)
        dy = (nth 1 b) - (nth 1 a)
        (if (dx != 0.0) then
          dx = dx/(abs dx))
        (if (dy != 0.0) then
          dy = dy/(abs dy))
        list(floor(dx) floor(dy))))

(defun Ru_isMultiple (x m)
    (floor x/m) == (x/m))

(defun Ru_isPitchMultiple (x) 
    Ru_isMultiple(x 1.44))

(defun Ru_roundToMultiple (x n)
    (n * (round x/n)))

(defun Ru_ComputePathListFromGuide (@key (cellCat "chip.alta")
                                         (cellInst "chip.alta.ALTA-L76-R.1000") 
                                         (cellView "abstract")
                                         (guideName nil) 
                                         (trackWidth nil) 
                                         (trackOffset nil)
                                         (startingPoint nil)
                                         (endingPoint nil)
                                         (reverseList nil)
                                         (debug nil))
  (let (shape cv channelName points propName guidePath cx cy nx ny tx ty delta offset dir)

    (if !cellView
        then
            cv = (geGetEditCellView)
        else
            cv = (dbOpenCellViewByType cellCat cellInst cellView))
  
    (if guideName != nil
      then
        ; look for all paths in the current view
        (foreach shape cv->shapes

          ; process only shapes with type "path"
          (when shape->objType=="path"

             ; look for the "Channel" or "myProp" property
             propName = "Channel"
             channelName = (GetProp shape propName nil)
             (when channelName == nil
                propName    = "myProp"
                channelName = (GetProp shape propName nil)
             )
             ; typically the name is FooBar,.* so we trim up to the ,
             (if (channelName != nil) && ((nindex channelName ",") != nil)
               then
                (setq channelName (substring channelName 1 ((nindex channelName ",") - 1))))

             ; matching entry?
             (if (channelName == guideName) && (channelName != nil)
               then
                 (printf "Found guide path %s\n" channelName)

                 ; check points
                 (if Ru_isPitchMultiple((nth 0 startingPoint)) ||
                     Ru_isPitchMultiple((nth 1 startingPoint)) ||
                     Ru_isPitchMultiple((nth 0 endingPoint)) ||
                     Ru_isPitchMultiple((nth 1 endingPoint)) then
                     (printf "WARNING: starting or ending points are not multiples of 1.44\n"))

                 points = shape->points

                 (if reverseList then
                    points = reverse(points))
                 (printf "%d points in list\n" length(points))

                 delta = (trackWidth / 2) * 2.88
                 (printf "Computed delta as %3.2f\n" delta)
                 offset = (trackOffset * 2.88) + 1.44
                 (printf "Computed offset as %3.2f\n" offset)

                 ; clear starting path
                 guidePath = list(startingPoint)
                 cx = (nth 0 startingPoint)
                 cy = (nth 1 startingPoint)
                 (printf "Initial point %3.2f %3.2f\n" cx cy)

                 ; first point
                 dir = (Ru_SegmentDirection (nth 0 points) (nth 1 points))
                 (printf "Segment direction is (%d, %d)\n" (nth 0 dir) (nth 1 dir))

                 (if (nth 1 dir) == 0 
                    then
                        ; segment is left-to-right but does not extend to starting point
                        (if ((nth 0 (nth 0 points)) > cx) && ((nth 0 dir) > 0)
                            then
                                (printf "WARNING: first segment (rightward) does not extend to starting point\n"))

                        ; segment is right-to-left but does not extend to starting point
                        (if ((nth 0 (nth 0 points)) < cx) && ((nth 0 dir) < 0)
                            then
                                (printf "WARNING: first segment (leftward) does not extend to starting point\n"))

                        ; horizontal path
                        nx = cx
                        ; y-coordinate of 1st point in path
                        ny = (nth 1 (nth 0 points)) - delta + offset
                    else
                        ; segment is bottom-to-top but does not extend to starting point
                        (if ((nth 0 (nth 1 points)) > cx) && ((nth 1 dir) > 0)
                            then
                                (printf "WARNING: first segment (upward) does not extend to starting point\n"))

                        ; segment is top-to-bottom but does not extend to starting point
                        (if ((nth 0 (nth 1 points)) < cx) && ((nth 1 dir) < 0)
                            then
                                (printf "WARNING: first segment (downward) does not extend to starting point\n"))
                        ; vertical path
                        ny = cy
                        ; x-coordinate of 1st point in path
                        nx = (nth 0 (nth 0 points)) - delta + offset

                 ) ; end if
                    

                 ; add point and update
                 guidePath = append(guidePath list(list(nx ny)))
                 cx = nx
                 cy = ny
                 (printf "First segment point %3.2f %3.2f\n" cx cy)

                 (for i 1 (length points)-2
                   nx = (nth 0 (nth i points))
                   ny = (nth 1 (nth i points))
                   dir = (Ru_SegmentDirection (nth i points) (nth i+1 points))
                   (printf "Raw point #%d is %3.2f %3.2f, direction is (%d, %d)\n" i nx ny (nth 0 dir) (nth 1 dir))

                   (if (nth 1 dir) == 0 
                      then
                          ; horizontal path
                          nx = cx
                          ; y-coordinate of 1st point in path
                          ny = ny - delta + offset
                      else
                          ; vertical path
                          ny = cy
                          ; x-coordinate of 1st point in path
                          nx = nx - delta + offset

                   ) ; end if
                    

                   ; add point and update
                   guidePath = append(guidePath list(list(nx ny)))
                   cx = nx
                   cy = ny
                   (printf "Segment point #%d is %3.2f %3.2f\n" i cx cy)

                 )  ; end for

                 cx = (nth 0 endingPoint)
                 cy = (nth 1 endingPoint)
                 nx = (nth 0 (nth length(points)-1 points))
                 ny = (nth 1 (nth length(points)-1 points))

                 (if (nth 1 dir) == 0 
                    then
                        ; segment is left-to-right but does not extend to starting point
                        (if (nx < cx) && ((nth 0 dir) > 0)
                            then
                                (printf "WARNING: last segment (rightward) does not extend to ending point\n"))

                        ; segment is right-to-left but does not extend to starting point
                        (if (nx > cx) && ((nth 0 dir) < 0)
                            then
                                (printf "WARNING: last segment (leftward) does not extend to ending point\n"))

                        ; horizontal path
                        nx = cx
                        ; y-coordinate of last point in path
                        ny = ny - delta + offset
                    else
                        ; segment is bottom-to-top but does not extend to ending point
                        (if (ny < cy) && ((nth 1 dir) > 0)
                            then
                                (printf "WARNING: last segment (upward) does not extend to ending point\n"))

                        ; segment is top-to-bottom but does not extend to ending point
                        (if (ny > cy) && ((nth 1 dir) < 0)
                            then
                                (printf "WARNING: last segment (downward) does not extend to ending point\n"))
                        ; vertical path
                        ny = cy
                        ; x-coordinate of last point in path
                        nx = nx - delta + offset

                 ) ; end if

                 ; add point and update
                 guidePath = append(guidePath list(list(nx ny)))
                 (printf "Last segment point %3.2f %3.2f\n" nx ny)
                 guidePath = append(guidePath list(list(cx cy)))
                 (printf "Last point %3.2f %3.2f\n" cx cy)

                 ; round to 1.44 multiples
                 guidePath = mapcar('(lambda (x) (list Ru_roundToMultiple((nth 0 x) 1.44)
                                                       Ru_roundToMultiple((nth 1 x) 1.44))) guidePath)

                ; draw in prBound if needed
                (if debug then dbCreatePath(cv list("prBoundary") guidePath 0.5))

                (printf "----\n")
                (printf "list(\n")
                (for i 0 length(guidePath)-1
                    (printf "  list(%3.2f %3.2f) ; tracks (%3.2f %3.2f)\n" 
                        (nth 0 (nth i guidePath)) 
                        (nth 1 (nth i guidePath))
                        (nth 0 (nth i guidePath))/1.44 
                        (nth 1 (nth i guidePath))/1.44 ))
                (printf ")\n")

            ) ; end if

          ) ; end when

        ) ; end foreach
      else
        (printf "No guidename specified\n")
    ) ; end if

  ) ; end let
)

;wrapper function for CopyGuidePath
(defun GenObstructions ( propValue
                       @key (libName  "chip.alta")
                            (cellName "chip.alta.PATHS-L76-R.1000")
                            (viewType "abstract")
                            (deleteObs t) ;delete any existing obstructions
                            (segOff nil) ;adjust segment number
                            (reversePath nil) ;path is drawn reverse order
                     )

    CopyGuidePath(propValue ?libName libName ?cellName cellName ?viewType viewType
                            ?deleteObs deleteObs ?segOff segOff ?reversePath reversePath
                            ?addBound nil)

)

; copy a guide path from one cellview to the current
; identify potential obstruction and create them
; in the destination cellview 
(defun CopyGuidePath ( propValue
                       @key (libName  "chip.alta")
                            (cellName "chip.alta.PATHS-L76-R.1000")
                            (viewType "abstract")
                            (propList (list "Channel" "myProp"))
                            (cleanup  nil)
                            (deleteObs t) ;delete any existing obstructions
                            (segOff nil) ;adjust segment number
                            (reversePath nil) ;path is drawn reverse order
                            (addBound t) ;copy over pr bound
                     )
  (prog ( curView srcView channelName properties propName shape shapeCount 
          dstShape points p0 p1 x0 x1 y0 y1 width )

    ; determine the current cellview
    curView = ( geGetEditCellView )
    (when curView == nil
      (printf "No destination cellview selected!\n")
      (return nil)
      ) ; end (when curView ... )

    ; determine the source cellview
    srcView = ( dbOpenCellViewByType libName cellName viewType )
    (when srcView == nil
      (printf "Invalid source cellview: %s - %s - %s\n" libName cellName viewType)
      (return nil)
      ) ; end (when srcView ... )

    ; perform the initial cleanup of the destination cellview
    ; if required. That consists in deleting guidepaths featuring
    ; at least one of the properties in propList matching propValue
    (when cleanup == t    
      ( DeleteGuidePath propValue ?curView curView ?propList propList )
      ) ; end (when cleanup ... )
    (when deleteObs == t    
      ( DeleteObstructions ?all nil)
      ) ; end (when deleteObs ... )

    ; now look for all matching paths in the source cellView. 
    ; Every matching path will be copied over to the destination
    ; cellview. Also for every matching path, we'll be looking
    ; for other paths featuring at least one of the the properties
    ; in propList that intersect it. For each intersection an 
    ; obstruction will be created in the destination cellview
    (foreach shape srcView->shapes

      ; is this a matching path?
      channelName = (Ru_MatchPathShape shape propValue propList)
      (when channelName != nil
        ; yes it is. Copy it to the destination cellview to start with. 
        (if addBound 
          then
            dstShape = (dbCopyFig shape curView)
            (if dstShape != nil
              then
                ; copy successful. Now:

                ; 1. Transform the path in a polygon
                (dbConvertPathToPolygon dstShape)
                ; 2. change the layer to prBound
                dstShape->lpp = (list "prBoundary" "drawing")
                ; 3. save the destination cellview  
                (dbSave curView)

              else
                ; copy failed, just log that
                (printf "Couldn't copy shape %s" shape)
              ) ; end if (dbCopyFig ... )
          )
          ; crossing paths
          (printf ";------------------------------------------------\n")
          (printf ";Generated with: %s %L %s %s %s\n" 
                (if addBound "CopyGuidePath" "GenObstructions") propValue
                (if segOff sprintf(nil "?segOff %d" segOff) "" )
                (if reversePath sprintf(nil "?reversePath t") "" ) 
                (if cellName!="chip.alta.PATHS-L76-R.1000" sprintf(nil "?cellName %s" cellName) "")
              )
          (printf ";Obstructions for path %s:\n" channelName)
          (Ru_CreatePathObstructions shape srcView propList curView segOff reversePath)
        ) ; end (when (PathShapeMatch ... )
      ) ; end foreach
      (return t)
    ) ; end prog
  ) ; end CopyGuidePath


; delete guide paths and obstructions from a cellview
(defun DeleteGuidePath ( propValue 
                         @key (curView  (geGetEditCellView) )
                              (propList (list "Channel" "myProp")))
  (let ( shape )
    (foreach shape curView->shapes
      (if (Ru_MatchPathShape shape propValue propList) != nil
        then (dbDeleteObject shape)
        else 
          (when (Ru_isPathObstruction shape)
            (dbDeleteObject shape)
           ) ; end ( when (isBusOps ... )
         )  ; end (if ... )
      ) ; end (foreach ... )
      (dbSave curView)
    ) ; end let
  ) ; end DeleteGuidePath


; function to see if a given shape matches at least one 
; guide path's property
(defun Ru_MatchPathShape ( shape propValue propList )
  (let ( channelName properties propName regexp matches)

    ; create the regular expression for path property matching if needed
    matches = nil
    (when propValue != nil regexp = (sprintf nil "^%s" propValue))

    ; in order for this shape to match a given guide path
    ; three conditions have to be true
    ; 1. The shape object has to be of type "path" or "polygon"
    ; 2. The shape must feature one of the properties in propList
    ; 3. If a propValue is given the property must be equal to propValue

    (when (or shape->objType=="path" shape->objType=="polygon")
    
      ; look for any of the valid properties
      channelName = nil
      properties = propList

      ; scan the property list 
      (while (and channelName == nil properties != nil matches == nil) 
        propName   = (car properties)
        properties = (cdr properties)
        channelName = (GetProp shape propName nil)

        ; is this a matching path?
        (when (and channelName != nil 
                  ( or propValue == nil ( rexMatchp regexp channelName ) == t ))
          matches = t
          ) ; end (when channelName ... )
        )   ; end (while ... )
      ) ; end (when shape->objType ... )

      (if matches == t then channelName else nil)
    ) ; end let
  ) ; end Ru_MatchPathShape

; create obstructions for each path in a cellview crossing a given path
(defun Ru_CreatePathObstructions ( path srcView propList curView segOff reversePath)
  (let ( bbox dir points_i bbox_i points_j bbox_j shapet dstShape obs prop)

    ; output list of obstructions to console using the format
    ; needed by the single cell flow
    (printf "\nreserveGap = list(\n")

    points_i  = path->points
    (for i 0 (length points_i)-2
      bbox = (Ru_CreateBboxFromPathSegment 
                (nth i points_i) 
                (nth i+1 points_i) 
                path->width)
      bbox_i = (car  bbox)
      dir    = (cadr bbox)

      ; now copy the shapes to the destination cellview
      (foreach shape srcView->shapes
        (when (and shape != path 
                   (or ((Ru_MatchPathShape shape nil propList) != nil && shape->width != nil) ;a path
                       (shape->objType == "rect" && cadr(shape->lpp) == "boundary" && car(shape->lpp) != "M7") ;a bypass rectangle
                   ) ;or
              ) ;and
          points_j = shape->points
          (if shape->objType == "rect" points_j = list(1:1 2:2)) ;fake for the for loop
          (for j 0 (length points_j)-2
            (if shape->objType == "rect" then
              bbox_j = shape->bBox
              else
              bbox = (Ru_CreateBboxFromPathSegment 
                      (nth j points_j) 
                      (nth j+1 points_j) 
                      shape->width)
              bbox_j = (car bbox)
            )

            obs = (RectGetIntersection bbox_i bbox_j)
            (when obs != nil && (RectGetArea obs) >= TrackPitch**2/4
              ; create an obstruction in the destination cellview
              ; for the intersection between these two path segments
              (printf "    list( %d" (if reversePath ((length points_i)-1-i) i)+(if segOff segOff 0))
              (if dir == 0 
                then (printf "    %8.2f    %8.2f )\n" 
                             (leftEdge obs)   (rightEdge obs))
                else (printf "    %8.2f    %8.2f )\n" 
                             (bottomEdge obs) (topEdge obs))
                )
              dstShape = (dbCreateRect curView (list "OBS" "bus") obs)
              (if dstShape == nil 
                then (printf "Couldn't create obstruction\n")
                else 
                  (CreateObsProps ?shapes (list dstShape))
                  (if dir == 0
                    then 
                      prop = (dbGetPropByName dstShape "M2")
                      prop->value = "TRUE"
                      prop = (dbGetPropByName dstShape "M4")
                      prop->value = "TRUE"
                      prop = (dbGetPropByName dstShape "M6")
                      prop->value = "TRUE"
                    else
                      prop = (dbGetPropByName dstShape "M3")
                      prop->value = "TRUE"
                      prop = (dbGetPropByName dstShape "M5")
                      prop->value = "TRUE"
                      prop = (dbGetPropByName dstShape "M7")
                      prop->value = "TRUE"
                    ) ; end (if dir ... )
                   ;set property so we only delete autogenerated obstruction
                   (dbReplaceProp dstShape "AutoGenerated" "boolean" t)
                ) ; end ( if dstShape ... )
              ) ; end (when obs ... )
            ) ; end ( for j ... )
          )  ; end (when ... )
        ) ; end (foreach shape ... )
      )  ; end ( for i ... )
      (printf "    )\n")
      (dbSave curView)
    ) ; end let
  ) ; end Ru_CreatePathObstructions


; create a bounding box from a path segment
; identified by start point, end point, width
(defun Ru_CreateBboxFromPathSegment ( p0 p1 width )
  (let (bbox x0 x1 y0 y1 dir)
    x0 = (car  p0)
    y0 = (cadr p0)
    x1 = (car  p1)
    y1 = (cadr p1)
    (if !width || width < TrackPitch width = TrackPitch)
    ; determine the direction
    (if x1 == x0
      then
        ; we're moving vertically
        bbox = (list x0-width/2:(min y0 y1) x0+width/2:(max y0 y1))
        dir = 1
      else
        ; we're moving horizontally
        bbox = (list (min x0 x1):y0-width/2 (max x0 x1):y0+width/2)
        dir = 0
      )  ; end ( if x1 == x0 )
    (list bbox dir)
    ) ; end let
  ) ; end Ru_CreateBboxFromPathSegment



; check if a given shape is a path obstruction
(defun Ru_isPathObstruction ( shape )
  (let ( isPathObs )
    isPathObs = 
      (cond
        ((isBusObs shape) t)
        ((and shape->objType == "rect" shape->lpp = (list "M2" "boundary")) t)
        ((and shape->objType == "rect" shape->lpp = (list "M3" "boundary")) t)
        ((and shape->objType == "rect" shape->lpp = (list "M4" "boundary")) t)
        ((and shape->objType == "rect" shape->lpp = (list "M5" "boundary")) t)
        ((and shape->objType == "rect" shape->lpp = (list "M6" "boundary")) t)
        ((and shape->objType == "rect" shape->lpp = (list "M7" "boundary")) t)
        (t nil)
        ) ;end conditions

    isPathObs
    ) ; end let
  ) ; end Ru_isPathObstruction


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CreateSimpleAbstract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CreateBlackboxAbstract (@key (cv nil) (cutaroundpins nil) (abstractName "abstract") )
 (let (abstractcv name net fig term nonDwShapesCnt
       havem2pins havem3pins havem4pins havem5pins havem6pins havem7pins)
  (when !cv
    cv=(geGetEditCellView)
  )
  (if rexMatchp("^abstract" cv->viewName) error("Cannot create %L from %L\n" abstractName cv->viewName))
  abstractcv=(nrOpenCellViewWritable cv->libName cv->cellName abstractName)
  (when !abstractcv
    (printf "Couldn't edit abstract view for %s\n" cv->cellName)
    (error "Cellview not writable.\n")
  )
  (printf "Starting abstract of %s at %s\n" cv->cellName (getCurrentTime))
  (foreach shape abstractcv->shapes
    (when shape (dbDeleteObject shape) )
  )

  ;copy prbound to abstract and make keepouts
  (foreach prb (setof shape cv->shapes shape->lpp==BoundaryLPP)
           (dbCopyFig prb abstractcv)
           )
  (AbstractInvertKeepout Metal1byLPP cv abstractcv)
  (AbstractInvertKeepout Metal2byLPP cv abstractcv)
  (AbstractInvertKeepout Metal3byLPP cv abstractcv)
  (AbstractInvertKeepout Metal4byLPP cv abstractcv)
  (AbstractInvertKeepout Metal5byLPP cv abstractcv)
  (AbstractInvertKeepout Metal6byLPP cv abstractcv)
  (AbstractInvertKeepout Metal7byLPP cv abstractcv)

  ;copy pins to abstract
  havem2pins=nil
  havem3pins=nil
  havem4pins=nil
  havem5pins=nil
  havem6pins=nil
  havem7pins=nil
  nonDwShapesCnt = 0
  (foreach shape cv->shapes
    (when shape->pin
      (when shape->net->name!="GND" && shape->net->name!="Vdd"
        name=shape->net->name
        net=(dbFindNetByName abstractcv name)
        (when !net net=(dbCreateNet abstractcv name))
        term=(dbFindTermByName abstractcv name)
        (when !term (dbCreateTerm net name "inputOutput"))
        fig=(dbCopyFig shape abstractcv)
(if cadr(fig->lpp) != "drawing" then
    nonDwShapesCnt++
    fig->lpp = (list car(fig->lpp) "drawing")
)
        (dbCreatePin net fig)
      )
      (when shape->lpp==Metal2LPP || shape->lpp==Metal2pinLPP
        havem2pins=t
      )
      (when shape->lpp==Metal3LPP || shape->lpp==Metal3pinLPP
        havem3pins=t
      )
      (when shape->lpp==Metal4LPP || shape->lpp==Metal4pinLPP
        havem4pins=t
      )
      (when shape->lpp==Metal5LPP || shape->lpp==Metal5pinLPP
        havem5pins=t
      )
      (when shape->lpp==Metal6LPP || shape->lpp==Metal6pinLPP
        havem6pins=t
      )
      (when shape->lpp==Metal7LPP || shape->lpp==Metal7pinLPP
        havem7pins=t
      )
    )
  )
  (PinUtilOrientLabels abstractcv)

  ;chop keepouts around pins
  (when cutaroundpins
    (when havem2pins (AbstractCutAroundPins abstractcv Metal2LPP Metal2byLPP ?otherpinlpp Metal2pinLPP))
    (when havem3pins (AbstractCutAroundPins abstractcv Metal3LPP Metal3byLPP ?otherpinlpp Metal3pinLPP))
    (when havem4pins (AbstractCutAroundPins abstractcv Metal4LPP Metal4byLPP ?otherpinlpp Metal4pinLPP))
    (when havem5pins (AbstractCutAroundPins abstractcv Metal5LPP Metal5byLPP ?otherpinlpp Metal5pinLPP))
    (when havem6pins (AbstractCutAroundPins abstractcv Metal6LPP Metal6byLPP ?otherpinlpp Metal6pinLPP))
    (when havem7pins (AbstractCutAroundPins abstractcv Metal7LPP Metal7byLPP ?otherpinlpp Metal7pinLPP))
  )
  ;create power pins
  powerPinLoc = Ru_AlignX(Ru_MidX(abstractcv->bBox)):Ru_AlignY(Ru_MidY(abstractcv->bBox))
  (CreatePowerPins abstractcv "M7" powerPinLoc  t)
  (printf "Power pins location is at %L\n" powerPinLoc)
  (printf "Finished abstract of %s at %s\n" cv->cellName (getCurrentTime))
  (if nonDwShapesCnt!=0 printf("%d pins converted to drawing shapes\n" nonDwShapesCnt))
  (dbSave abstractcv)
 t
 )
)

