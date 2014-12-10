/**
 * INTEL TOP SECRET
 * Copyright 2002-2012 Intel Corporation. All Rights Reserved.
 * $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/chain/unstack.il#2 $
 */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/*DOC
This module contains functions for the post processing of the SUPERSTACK, aimed at making the output chain of stacks/gates DRC correct.

The following issues are encountered with the current (TSMC65) SUPERSTACK when used in TSMC28nm tecnology

1) <Poly Contact Location>
   When two gates' contacts are shared, the smaller gate's diffusion contacts are turned off. 
   So the smaller gate's poly input contacts are needed to be spaced away from the diffusion, to avoid the DRCs.

2) <PO.R.8> <Poly interact Diffusion shape restrictions in TSMC28nm>
   The fabrication rules need the OD to be either of T, L or U shapes. 
   Thus the SUPERSTACK generated needs to go through post-processing step to detect the shared OD shapes and "UNSTACK" them to allowable shapes

3) <Re-ordering Stack/Gates prior to SUPERSTACK>
   This can be a pre-processing step for the SUPERSTACK to cluster and arrage the domino logic or the gates, based on a set priority. 

*/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Define Constants
(defvar SharedOverlap 0.18)
(defvar UnSharedOverlap 0.05)

;;;;;;;;;;;;;;;
; Compares the physical Y locations of two Instances
procedure( UnStackCompareLLY( InstX InstY )

  lessp(cadar(InstX~>bBox)  cadar(InstY~>bBox))

) 

;;;;;;;;;;;;;;;
; Sorts the list in incrementing order of physical Y location
procedure( UnStackSortYOrder( SelectedList )
 
   sort(SelectedList 'UnStackCompareLLY)

) 

;;;;;;;;;;;;;;;
;
procedure( UnStackInstSharedOnTop( Inst TopInst )
   let( ( InstOverlap )

   InstOverlap = round((cadadr(Inst~>bBox)-cadar(TopInst~>bBox))*100)/100.00   
 
   equal(InstOverlap SharedOverlap)
  )
)

;;;;;;;;;;;;;;;
;
procedure( UnStackInstSharedOnBottom( Inst BottomInst )
   let( ( InstOverlap )

   InstOverlap = round((cadadr(BottomInst~>bBox)-cadar(Inst~>bBox))*100)/100.00
   equal(InstOverlap SharedOverlap)
  )
)

;;;;;;;;;;;;;;;
;
procedure( UnStackGetNColumnWidth( Inst )
 
  let( ( NWidth 
         GateName 
        )

   NWidth = nil

   when( car(parseString(Inst~>cellName ".")) == "gate"
         GateName = nth(1 parseString(Inst~>cellName "."))
       )

   when( car(parseString(Inst~>cellName ".")) == "stack"
         GateName = strcat("NW" nth(2 parseString(nth(1 parseString(Inst~>cellName ".")) "_")))
       )
  
 case( GateName

          ( "INV" || "NOR2" || "NOR3" || "NW1"        
           when( dbGet(Inst "NW1")
             NWidth = dbGet(Inst "NW1") / UnStackGetFolds(Inst)
            )
           )

          ( "NAND2" || "C2" || "C2_STAT" || "NW2"       
           when( dbGet(Inst "NW2")
             NWidth = dbGet(Inst "NW2") / UnStackGetFolds(Inst)
            )
           )

          ( "NAND3" || "NW3"        
           when( dbGet(Inst "NW3")
             NWidth = dbGet(Inst "NW3") / UnStackGetFolds(Inst)
            )
           )

          ( "NAND4" || "NW4"         
           when( dbGet(Inst "NW4")
             NWidth = dbGet(Inst "NW4") / UnStackGetFolds(Inst)
            )
           )

          ( "NAND5" || "NW5"       
           when( dbGet(Inst "NW5")
             NWidth = dbGet(Inst "NW5") / UnStackGetFolds(Inst)
            )
           )

          ( "NW6"       
           when( dbGet(Inst "NW6")
             NWidth = dbGet(Inst "NW6") / UnStackGetFolds(Inst)
            )
           )
    )
  )
) 

;;;;;;;;;;;;;;;
;
procedure( UnStackGetPColumnWidth( Inst )
 
  let( ( PWidth 
         GateName 
        )

   PWidth = nil
  
   when( car(parseString(Inst~>cellName ".")) == "gate"
         GateName = nth(1 parseString(Inst~>cellName "."))
       )

   when( car(parseString(Inst~>cellName ".")) == "stack"
         GateName = strcat("PW" nth(2 parseString(nth(1 parseString(Inst~>cellName ".")) "_")))
       )

 case( GateName

          ( "INV" || "NAND2" || "NAND3" || "NAND4" || "NAND5" || "PW1"
           when( dbGet(Inst "PW1")
            PWidth = dbGet(Inst "PW1") / UnStackGetFolds(Inst)
            )
           )

          ( "NOR2" || "C2" || "C2_STAT" || "PW2"        
           when( dbGet(Inst "PW2")
            PWidth = dbGet(Inst "PW2") / UnStackGetFolds(Inst)
            )
           )

          ( "NOR3" || "PW3"        
           when( dbGet(Inst "PW3")
            PWidth = dbGet(Inst "PW3") / UnStackGetFolds(Inst)
            )
           )

          ( "PW4"        
           when( dbGet(Inst "PW4")
            PWidth = dbGet(Inst "PW4") / UnStackGetFolds(Inst)
            )
           )
    )
  )
)

;;;;;;;;;;;;;;;
;
procedure( UnStackGetFolds( Inst ) 
 
  let( ( Folds
         GateName 
         GateNum
        )

    GateName = nth(1 parseString(Inst~>cellName "."))

    case( GateName

          ( "INV"
             Folds = round((((cadadr(Inst~>bBox) - cadar(Inst~>bBox)) - 0.31) / 0.13) + 1)
           )
          ( "C2_STAT"
             Folds = 2
           )
          ( t 
             GateNum  = evalstring(substring(GateName strlen(GateName) 1))
             Folds = round(((((cadadr(Inst~>bBox) - cadar(Inst~>bBox)) - 0.31) / 0.13) + 1) / GateNum)
           )
      )
 Folds
    )
)

;;;;;;;;;;;;;;;
;
defun( UnStackAdjustPolyContacts ( SelectedList )

;defun( UnStackAdjustPolyContacts ()

  let( ( I
         PrevInst
         CurrInst
         NextInst
         SortedList
         TempSelectedList GateName
         NPolyAdjustTop
         PPolyAdjustTop
         NPolyAdjustBottom
         PPolyAdjustBottom
        ) 

    TempSelectedList = SelectedList
;   SelectedList = geGetSelectedSet()
    SortedList   = UnStackSortYOrder(TempSelectedList)
    
    UnStackReadListNames()
 
    for( I 0 length(SortedList)

         NPolyAdjustTop = 0.05
         PPolyAdjustTop = 0.05
         NPolyAdjustBottom = 0.05
         PPolyAdjustBottom = 0.05
 
         when( nth(I SortedList) != nil
            CurrInst = nth(I SortedList)
 
         GateName = nth(1 parseString(CurrInst~>cellName "."))

         when( I == 0 
               NextInst = nth(1 SortedList)
           when( UnStackInstSharedOnTop(CurrInst NextInst) && when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) lessp(UnStackGetPColumnWidth(CurrInst) UnStackGetPColumnWidth(NextInst)))
                 PPolyAdjustTop = PPolyAdjustTop + when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) (UnStackGetPColumnWidth(NextInst) - UnStackGetPColumnWidth(CurrInst))* 1e6) ; + dbGet(CurrInst "poly_contact_offset")
                 println(PPolyAdjustTop)
                )
           when( UnStackInstSharedOnTop(CurrInst NextInst) && when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(CurrInst) UnStackGetNColumnWidth(NextInst)))
                 NPolyAdjustTop = NPolyAdjustTop + when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) (UnStackGetNColumnWidth(NextInst) - UnStackGetNColumnWidth(CurrInst))* 1e6) ; + dbGet(CurrInst "poly_contact_offset")
                 println(NPolyAdjustTop)
                )   
                 dbSet(CurrInst NPolyAdjustTop "n_contact_offset") 
                 dbSet(CurrInst PPolyAdjustTop "p_contact_offset") 
             )

         when( I == (length(SortedList) - 1)
               PrevInst = nth(I-1 SortedList)
           when( UnStackInstSharedOnBottom(CurrInst PrevInst) && when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) lessp(UnStackGetPColumnWidth(CurrInst) UnStackGetPColumnWidth(PrevInst)))
                 PPolyAdjustBottom = PPolyAdjustBottom + when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) (UnStackGetPColumnWidth(PrevInst) - UnStackGetPColumnWidth(CurrInst)) * 1e6) ; + dbGet(CurrInst "poly_contact_offset")
                )
           when( UnStackInstSharedOnBottom(CurrInst PrevInst) && when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(CurrInst) UnStackGetNColumnWidth(PrevInst)))
                 NPolyAdjustBottom = NPolyAdjustBottom + when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) (UnStackGetNColumnWidth(PrevInst) - UnStackGetNColumnWidth(CurrInst)) * 1e6) ; + dbGet(CurrInst "poly_contact_offset")
                ) 
                 dbSet(CurrInst NPolyAdjustBottom "n_contact_offset") 
                 dbSet(CurrInst PPolyAdjustBottom "p_contact_offset") 
             )
 
         when( I > 0 && I < (length(SortedList) - 1)
           PrevInst = nth(I-1 SortedList)
           NextInst = nth(I+1 SortedList)
           when( UnStackInstSharedOnTop(CurrInst NextInst) && when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(CurrInst) UnStackGetNColumnWidth(NextInst)))
                 NPolyAdjustTop = NPolyAdjustTop + when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) (UnStackGetNColumnWidth(NextInst) - UnStackGetNColumnWidth(CurrInst)) * 1e6) ; + dbGet(CurrInst "poly_contact_offset")
                )
           when( UnStackInstSharedOnTop(CurrInst NextInst) && when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) lessp(UnStackGetPColumnWidth(CurrInst) UnStackGetPColumnWidth(NextInst)))
                 PPolyAdjustTop = PPolyAdjustTop + when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) (UnStackGetPColumnWidth(NextInst) - UnStackGetPColumnWidth(CurrInst)) * 1e6) ; + dbGet(CurrInst "poly_contact_offset")
                ) 
           when( UnStackInstSharedOnBottom(CurrInst PrevInst) && when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(CurrInst) UnStackGetNColumnWidth(PrevInst)))
                 NPolyAdjustBottom = NPolyAdjustBottom + when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) (UnStackGetNColumnWidth(PrevInst) - UnStackGetNColumnWidth(CurrInst)) * 1e6) ; + dbGet(CurrInst "poly_contact_offset")
                )           
           when( UnStackInstSharedOnBottom(CurrInst PrevInst) && when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) lessp(UnStackGetPColumnWidth(CurrInst) UnStackGetPColumnWidth(PrevInst)))
                 PPolyAdjustBottom = PPolyAdjustBottom + when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) (UnStackGetPColumnWidth(PrevInst) - UnStackGetPColumnWidth(CurrInst)) * 1e6) ; + dbGet(CurrInst "poly_contact_offset")
                ) 
           if( NPolyAdjustTop >= NPolyAdjustBottom
                  then
                       dbSet(CurrInst NPolyAdjustTop "n_contact_offset")     
                  else
                       dbSet(CurrInst NPolyAdjustBottom "n_contact_offset")
             )
           if( PPolyAdjustTop >= PPolyAdjustBottom
                  then
                       dbSet(CurrInst PPolyAdjustTop "p_contact_offset")     
                  else
                       dbSet(CurrInst PPolyAdjustBottom "p_contact_offset")
             )
           ) 
     )
    )
  )
)

;;;;;;;;;;;;;;;
;
defun( UnStackAdjustDiffusion (SelectList)
  let( ( I
         PrevInst
         CurrInst
         NextInst
         SortedList
         TempSelectedList

         NIsLTForm PIsLTForm NIsTForm PIsTForm NIsLUForm PIsLUForm NIsUForm PIsUForm 
         NIsStepForm PIsStepForm 
         )

    TempSelectedList = SelectList
    SortedList = UnStackSortYOrder(TempSelectedList)

         NIsLTForm = nil
         PIsLTForm  = nil
         NIsTForm  = nil
         PIsTForm  = nil
         NIsLUForm  = nil
         PIsLUForm  = nil
         NIsUForm  = nil
         PIsUForm  = nil

    for( I 0 length(SortedList)

         when( nth(I SortedList) != nil
               CurrInst = nth(I SortedList)

         when( I == 0 
               NextInst = nth(1 SortedList)
             when( UnStackInstSharedOnTop(CurrInst NextInst)
               NIsLTForm = when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(CurrInst) UnStackGetNColumnWidth(NextInst)))
               NIsLUForm = when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(NextInst) UnStackGetNColumnWidth(CurrInst)))
               PIsLTForm = when(UnStackGetPColumnWidth(NextInst)&&UnStackGetPColumnWidth(CurrInst) lessp(UnStackGetPColumnWidth(CurrInst) UnStackGetPColumnWidth(NextInst)))
               PIsLUForm = when(UnStackGetPColumnWidth(NextInst)&&UnStackGetPColumnWidth(CurrInst) lessp(UnStackGetPColumnWidth(NextInst) UnStackGetPColumnWidth(CurrInst)))
                 )
              )

         when( I > 0 && I < (length(SortedList) - 1)

        println(I)

               NextInst = nth(I+1 SortedList)

              if(  NIsLTForm || NIsLUForm || PIsLTForm || PIsLUForm
                then
                 if( NIsStepForm || PIsStepForm 
                   then
                     when( UnStackInstSharedOnTop(CurrInst NextInst)
                ;      when( (UnStackGetPColumnWidth(CurrInst) != UnStackGetPColumnWidth(NextInst)) || (UnStackGetNColumnWidth(CurrInst) != UnStackGetNColumnWidth(NextInst))
                             UnStackUnShareInstance(I+1 SortedList)
                   println("hello")
                             NIsStepForm = nil
                             PIsStepForm = nil
                             NIsTForm = nil
                             NIsUForm = nil
                             PIsTForm = nil
                             PIsUForm = nil
                             NIsLTForm = nil
                             NIsLUForm = nil
                             PIsLTForm = nil
                             PIsLUForm = nil
                 ;            )
                           )

                   else
                     when( UnStackInstSharedOnTop(CurrInst NextInst)
          NIsStepForm = (NIsLTForm && when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(CurrInst) UnStackGetNColumnWidth(NextInst)))) || (NIsLUForm && when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(NextInst) UnStackGetNColumnWidth(CurrInst))))
          PIsStepForm = (PIsLTForm && when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) lessp(UnStackGetPColumnWidth(CurrInst) UnStackGetPColumnWidth(NextInst)))) || (PIsLUForm && when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) lessp(UnStackGetPColumnWidth(NextInst) UnStackGetPColumnWidth(CurrInst))))
                          )
                   )                 
                 if( NIsTForm || NIsUForm || PIsTForm || PIsUForm 
                   then
                     when( UnStackInstSharedOnTop(CurrInst NextInst)
                       when( (UnStackGetPColumnWidth(CurrInst) != UnStackGetPColumnWidth(NextInst)) || (UnStackGetNColumnWidth(CurrInst) != UnStackGetNColumnWidth(NextInst))
                             UnStackUnShareInstance(I+1 SortedList)
                             NIsStepForm = nil
                             PIsStepForm = nil
                             NIsTForm = nil
                             NIsUForm = nil
                             NIsLTForm = nil
                             NIsLUForm = nil
                             PIsTForm = nil
                             PIsUForm = nil
                             PIsLTForm = nil
                             PIsLUForm = nil
                             )
                           )

                   else
                     when( UnStackInstSharedOnTop(CurrInst NextInst)
          NIsTForm = NIsLTForm && when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(NextInst) UnStackGetNColumnWidth(CurrInst)))
          NIsUForm = NIsLUForm && when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(CurrInst) UnStackGetNColumnWidth(NextInst)))
          PIsTForm = PIsLTForm && when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) lessp(UnStackGetPColumnWidth(NextInst) UnStackGetPColumnWidth(CurrInst)))
          PIsUForm = PIsLUForm && when(UnStackGetPColumnWidth(CurrInst)&&UnStackGetPColumnWidth(NextInst) lessp(UnStackGetPColumnWidth(CurrInst) UnStackGetPColumnWidth(NextInst)))
                          )
                   )   
               else 
                 when( UnStackInstSharedOnTop(CurrInst NextInst)
               NIsLTForm = when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(CurrInst) UnStackGetNColumnWidth(NextInst)))
               NIsLUForm = when(UnStackGetNColumnWidth(CurrInst)&&UnStackGetNColumnWidth(NextInst) lessp(UnStackGetNColumnWidth(NextInst) UnStackGetNColumnWidth(CurrInst)))
               PIsLTForm = when(UnStackGetPColumnWidth(NextInst)&&UnStackGetPColumnWidth(CurrInst) lessp(UnStackGetPColumnWidth(CurrInst) UnStackGetPColumnWidth(NextInst)))
               PIsLUForm = when(UnStackGetPColumnWidth(NextInst)&&UnStackGetPColumnWidth(CurrInst) lessp(UnStackGetPColumnWidth(NextInst) UnStackGetPColumnWidth(CurrInst)))              
                      )
                 )
             )
 
      )
    );for
  );let
t
);func


;;;;;;;;;;;;;;;
;
defun( UnStackUnShareInstance (Index SortedList)

  let( ( I
         CurrInst
         PrevInst
         NewYLoc
        )

    for( I Index length(SortedList)-1
         CurrInst = nth(I SortedList)
;         println(CurrInst~>name)
;         println(CurrInst~>orient)
           when( I==Index
                   PrevInst = nth(I-1 SortedList)
                   UnStackTurnContON(CurrInst CurrInst~>orient "BOTTOM")
                   UnStackTurnContON(PrevInst PrevInst~>orient "TOP")
                 )
         NewYLoc  = list(car(CurrInst~>xy) cadr(CurrInst~>xy)+(SharedOverlap - UnSharedOverlap))
         dbSet(CurrInst NewYLoc "xy")
     )

   t
   )
)
;;;;;;;;;;;;;;;
;
defun( UnStackTurnContON (Inst Orient Loc)

    case( Orient

          ( "R0" || "MY"
             when( Loc == "TOP"
                dbSet(Inst "FALSE" "share_top_Pcontacts")
                dbSet(Inst "FALSE" "share_top_Ncontacts")
              )
             when( Loc == "BOTTOM"
                dbSet(Inst "FALSE" "share_bottom_Pcontacts")
                dbSet(Inst "FALSE" "share_bottom_Ncontacts")
              )
            )

          ( "R180" || "MX"
             when( Loc == "TOP"
                dbSet(Inst "FALSE" "share_bottom_Pcontacts")
                dbSet(Inst "FALSE" "share_bottom_Ncontacts")
              )
             when( Loc == "BOTTOM"
                dbSet(Inst "FALSE" "share_top_Pcontacts")
                dbSet(Inst "FALSE" "share_top_Ncontacts")
              )
            )
    )
)
;;;;;;;;;;;;;;;
;
; defun( UnStackReadListNames ( SelectedList )

defun( UnStackReadListNames ()
  
 let( ( I 
       SelectedList
       SortedList
       )
 
  SelectedList = geGetSelectedSet()
  
  SortedList = UnStackSortYOrder(SelectedList)

  for( I 0 length(SortedList) 
    when( nth(I SortedList) != nil
     println((nth(I SortedList)~>name))
     println(UnStackGetFolds(nth(I SortedList)))
     println(UnStackGetNColumnWidth(nth(I SortedList)))
     println(UnStackGetPColumnWidth(nth(I SortedList)))
     )
   )

  ) ;let
t
) ;defun
