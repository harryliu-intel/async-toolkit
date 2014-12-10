/**
 * INTEL TOP SECRET
 * Copyright 2002-2012 Intel Corporation. All Rights Reserved.
 * $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/chain/group.il#0 $
 */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/*DOC
This module contains functions for the grouping the NMOS & PMOS chains based on the dynamic "numbered" nets.



*/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( GroupStackGetChains ()
  let( ( CellView 
         Inst ChainName
         ChainList
        ) 

     CellView = wcv()

     foreach( Inst CellView~>instances
         ChainName = nth(0 parseString(nth(1 parseString(Inst~>cellName ".")) "_"))
         when( ChainName == "NMOS" || ChainName == "PMOS"
            ChainList = cons(Inst ChainList)
          )
       )
    ChainList
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( GroupStackGetNumNets (SelectedList)

   let( ( Inst IdxOP
          NumList
          DNet
          SNet
          I
          DNetPresent
          SNetPresent
         )

    NumList = nil
    DNetPresent = nil
    SNetPresent = nil

    foreach(Inst SelectedList
         IdxOP = FindSDIdxs(Inst)
         SNet = nth(car(IdxOP) Inst~>conns~>net~>name)
         DNet = nth(cadr(IdxOP) Inst~>conns~>net~>name)
      when( DNet
        when( rexMatchp("^[0-9]" DNet) && rexMatchp("[0-9]$" DNet) 
              for( I 0 length(NumList)
                  when( nth(I NumList) == DNet
                       DNetPresent = t
                   )
                  )

               when( !DNetPresent
                     NumList = cons(DNet NumList) 
                     DNetPresent = nil
                   )
            )
           )

       when( SNet
        when( rexMatchp("^[0-9]" SNet) && rexMatchp("[0-9]$" SNet)
              for( I 0 length(NumList)
                    when( nth(I NumList) == SNet
                       SNetPresent = t
                   )
                  )

               when( !SNetPresent
                     NumList = cons(SNet NumList) 
                     SNetPresent = nil
                   )
            )
          )

                     DNetPresent = nil
                     SNetPresent = nil

        )

    NumList
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( FindSDIdxs (Inst)
  let( (SIdx DIdx I)
   
    for( I 0 length(Inst~>conns)-1
     when( nth(I Inst~>conns~>name) == "S"
        SIdx = I
      )
     when( nth(I Inst~>conns~>name) == "D"
        DIdx = I
      )
    ) 
    list(SIdx DIdx)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

procedure( GroupStackGetNumInst(SelectedList NumNet)
  let( ( SNet
         DNet IdxOP
         NInst
         TempList
        )
  
     TempList = '() 
     foreach( NInst SelectedList
            IdxOP = FindSDIdxs(NInst)
            SNet = nth(car(IdxOP) NInst~>conns~>net~>name)
            DNet = nth(cadr(IdxOP) NInst~>conns~>net~>name)       

               when( DNet == NumNet
                     TempList = cons(NInst TempList)
                     )

               when( SNet == NumNet
                     TempList = cons(NInst TempList)
                     )
       
               remove(NInst SelectedList)
    )

TempList
  ) ;let
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( GroupChains ( @key (SelectedList nil) (CellVue (UIGetCellView)) )
  let( ( NumNetsList SuperList ; SelectedList
         NumNet NumNetInsts
         NumNetsSortList
         I J K
         Len
         JIdx
         NewNet
         NetAdded
         )

    if( geGetSelectedSet(CellVue) != nil && SelectedList == nil
      then 
         SelectedList = RemoveGates(geGetSelectedSet(CellVue))
      else
         SelectedList = GroupStackGetChains()
       )

    DisColorSuperList(SelectedList)

    NumNetsList = GroupStackGetNumNets(SelectedList)
    NumNetsSortList = GroupStackSortNumNets(NumNetsList)

    printf("Num Nets ")
    println(NumNetsSortList)

   SuperList = GroupCreateSuperList(SelectedList NumNetsSortList)

   println(SuperList~>name)

   ColorSuperList(SuperList)
;   ArrangeColoredInsts(SelectedList)
   printf("Total Colors: ")
   println(length(SuperList)-1)


   length(SuperList)-1

 );let
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( GroupCreateSuperList (SelectedList NumNetsSortList)
 let( (  NumNetsList SuperList
         NumNet NumNetInsts
         I J K
         Len
         JIdx
         NewNet
         NetAdded
       )

    for( K 0 length(NumNetsSortList)
 
        NumNet = nth(K NumNetsSortList)
        JIdx = nil

        NumNetInsts = GroupStackGetNumInst(SelectedList NumNet)

        Len = length(SuperList)
        NewNet = t
        NetAdded = nil

        for( I 0 Len

        when( !NetAdded
         when( Len == 0
               SuperList = list(NumNetInsts) 
             )

         when( Len > 0

         for( J 0 Len
           when( SuperListNetPresent(nth(J SuperList) NumNet)
                 JIdx = J
               )
             )

             if( JIdx
                then 
                  SuperList = AddSuperListInst(SuperList NumNetInsts JIdx)
                  NetAdded = t                
                else
                  SuperList = AddSuperListNew(SuperList NumNetInsts)
                  NetAdded = t                
              )
           )
          )
         )
       )

  SuperList
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( GroupSortSuperList (SuperList)
 let( ( ModSuperList )

   ModSuperList

   for( I 0 length(SuperList)-1
     CurrGroup = nth(I SuperList)
     

     )
   
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procedure( GroupStackCompareGates( X Y )
 let( ( term )
  lessp(1 strcmp(car(setof(term X~>terminals term~>name=="G[0]"))~>net~>name car(setof(term Y~>terminals term~>name=="G[0]"))~>net~>name))
 )
) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procedure( IsNetNumNet( NetName )
  rexMatchp("^[0-9]" NetName) && rexMatchp("[0-9]$" NetName)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( gtemp (instlist)
  let( ( I outlist instc )

  for( I 1 6
    outlist = cons(length(setof(instc instlist length(instc~>terminals)==I+3)) outlist)

   )

  for( I 0 length(instlist)-1
    instc = nth(I instlist)
    when( I == 0 
       SNet = car(setof(term instc~>terminals  term~>name=="S"))~>net~>name
       DNet = car(setof(term instc~>terminals  term~>name=="D"))~>net~>name
        
      )
    
  )

outlist
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


defun( SuperListNetPresent (List Net)
  let( (ListNets)
    ListNets = GroupStackGetNumNets(List)
    nequal(member(Net ListNets) nil)
  );let
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( AddSuperListInst (List Insts Idx) 
   let( ( 
          I
          ChopList ChopListTop
          NewList OutList
         )
   NewList = append(nth(Idx List) Insts)
   OutList = subst(NewList nth(Idx List) List)
   OutList
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( AddSuperListNew (List Insts)
  let( ( NewList OutList )
     NewList = list(Insts)
     OutList = append(List NewList)   
     OutList
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

procedure( GroupStackCompareNumNets( X Y )
  lessp(evalstring(X) evalstring(Y))
) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

procedure( GroupStackSortNumNets( SelectedList )
   sort(SelectedList 'GroupStackCompareNumNets)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( ColorSuperList (SuperList)
   let( (
          I 
          J
          CurrList
          XGrid
          YGrid
          PrevY
         )

     XGrid = -PowerGridPitch

     for( I 0 length(SuperList)
       CurrList = nth(I SuperList)
       YGrid = 0
       PrevY = 0
       when( CurrList
         for( J 0 length(CurrList)
          when( PrevY != 0
            YGrid = PrevY + 0.04
             )
            when( nth(J CurrList) && (nth(J CurrList)~>StackColor != "FALSE"); && nth(J CurrList)~>StackColor != nil
              dbReplaceProp(nth(J CurrList) "StackColor" "int" I)
              dbSet(nth(J CurrList) "R0" "orient")  
              dbSet(nth(J CurrList) "TRUE" "MovedFig")  
              dbSet(nth(J CurrList) list(XGrid YGrid) "xy")
              PrevY = cadadr(nth(J CurrList)~>bBox)
             )
           )
          ) 
        XGrid = XGrid - PowerGridPitch
         )
      )
t
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( DisColorSuperList (SelectedList)
   let( ( Inst )
 
    foreach( Inst SelectedList
      dbDeletePropByName(Inst "StackColor")
      dbSet(Inst "FALSE" "MovedFig")  
    )
 
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( ArrangeColoredInsts (SelectedList)
  let( ( 
        Inst
        XCoordOffset
        NewLoc
        XGrid
       )

   XGrid = -1.52
   foreach( Inst SelectedList

        when( Inst~>StackColor
           XCoordOffset = (Inst~>StackColor + 1) * XGrid
           NewLoc = list(XCoordOffset cadr(Inst~>xy))
           dbSet(Inst NewLoc "xy")
         )
     )

   
 );let
t
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( RemoveGates (InstList)
  let( ( modList Inst )

    modList = InstList

    foreach( Inst InstList
      when( Inst~>libName == "gate"
          modList = remove(Inst modList)
       )
    )
   modList
  )
)
