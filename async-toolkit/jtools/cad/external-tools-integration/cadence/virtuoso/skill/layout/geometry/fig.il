; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

            

(defun FigGetPolygonEdgesForFig ( Fig )
  (let (
       ( ObjType ( getq Fig objType ) ) )  
    (cond (
           ( equal ObjType "path" )
           ( PolygonGetEdges ( PathGetPolygonPoints
                               Fig ) ) )
          (
           ( equal ObjType "polygon" )
           ( PolygonGetEdges ( getq Fig points ) ) )
          ( 
           ( BBoxGetPolygonEdges ( getq Fig bBox ) ) ) ) ) )
    
(defun FigGetRectInsideFig ( Fig )
 (let (
       ( ObjType ( getq Fig objType ) ) )  
   (cond (
          ( equal ObjType "rect" )
          ( getq Fig bBox ) )
         (
          ( equal ObjType "inst" )
          ( getq Fig bBox ) )
         (
          ( equal ObjType "path" )
          ( PolygonGetRectInsidePolygon 
            ( PathGetPolygonPoints
              Fig ) ) )
         (
          ( equal ObjType "donut" )
          ( RectMakeFromCenter ( FigGetValidPoint Fig )
                               ( quotient ( difference ( getq Fig outerRadius ) ( getq Fig innerRadius ) ) 2.0 )
                               ( quotient ( difference ( getq Fig outerRadius ) ( getq Fig innerRadius ) ) 2.0 ) ) )
         (
          ( equal ObjType "polygon" )
          ( PolygonGetRectInsidePolygon ( getq Fig points ) ) )
         ( 
          ( getq Fig bBox ) ) ) ) )
      
        

(defun FigGetValidPoint ( Fig )
  (let (
        ( ObjType ( getq Fig objType ) ) )
  (cond (
         ( equal ObjType "rect" )
         ( BBoxGetCenter ( getq Fig bBox ) )
        )
        ( 
         ( equal ObjType "path" )
         ( cadr ( getq Fig points ) )
        )
        (
         ( equal ObjType "polygon" )
         ( car ( getq Fig points ) )
        )
        ( 
         ( equal ObjType "donut" )
         ( plus ( getq Fig xy ) ( quotient ( plus ( getq Fig innerRadius ) ( getq Fig outerRadius ) ) 2.0 ) )
        )
        (
         ( BBoxGetCenter ( getq Fig bBox ) ) ) ) ) )


(defun GetShapesFromOverlaps (overlaps)
 (let (shapes)
   shapes = (list nil)
   (foreach element overlaps
     (when element->objType=="rect" || element->objType=="polygon"
       shapes = (cons element shapes)
     )
     (when element->objType==nil
       shapes = (GetShapesFromOverlaps element)
     )
   )
   shapes
))
