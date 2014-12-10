#include "auto.h"
#include "enumlayout.h"
#include <assert.h>

extern char* calma_name( NODE* pNode ) ;

static void DoBlockPaint( BLOCK* pb, EnumerateLayoutPaintRectFunc PaintFunc, void* pClientData ) {
	int CurrRectIndex;
	RECT CurrRect;
	assert( pb != NULL );
	if ( pb->rects != NULL ) {
	  for ( CurrRectIndex = 0; CurrRectIndex < pb->rects->max; CurrRectIndex++ ) {
	    CurrRect = pb->rects->p.rect[CurrRectIndex];
	    rectangle_move( &CurrRect, pb->x, pb->y);
	    CurrRect.x0 = round( CurrRect.x0 );
	    CurrRect.y0 = round( CurrRect.y0 );
	    CurrRect.x1 = round( CurrRect.x1 );
	    CurrRect.y1 = round( CurrRect.y1 );
	    PaintFunc( Paints->p.paint[ CurrRect.paint ], CurrRect, pClientData );
	  }
	}
}	

static void GetLabelInfo( PaintType* pPaintTypeDest, 
			  RECT* pRectDest,
			  char* NameDest,
			  size_t DestLen,
			  char* ExtraDest,
			  size_t ExtraDestLen,
			  NODE* pNode,
			  BLOCK* pBlocks,
			  size_t NumBlocks,
			  PORT* pPort ) {
	BLOCK* pBlock;	
	char* SafeName ;

	assert( NameDest != NULL );

	/* we a potentially going to decrement DestLen by two below
	 * so we had better make sure those decrements won't cause an under flow
	 */
	assert( DestLen > 2 );

	DestLen--;


	SafeName = leak_malloc(strlen(pNode->name)+2);
	strcpy(SafeName,pNode->name);
	
	/* NOTE: pPort->extra has all the |blah|blah|blah parts of the label
	fprintf(stderr,"%s %s %d\n",pNode->name,pPort->extra,pNode->global);
	*/
	if (pNode->global) {
	  /* append a ! if the node is a global */
	  SafeName[strlen(SafeName)+1] = 0 ; 
	  SafeName[strlen(SafeName)] = '!' ;
	}

	strncpy( NameDest, SafeName, DestLen );
	leak_free( SafeName );

	if ( pPort->extra != NULL ) {
	  strncpy( ExtraDest, pPort->extra, ExtraDestLen );
	}
	else {
	  ExtraDest[0] = '\0' ;
	}

	/* Make sure the string is null terminated */
	NameDest[ DestLen - 1 ] = '\0' ;	
	ExtraDest[ ExtraDestLen - 1 ] = '\0' ;

	assert( pPaintTypeDest != NULL );
	*pPaintTypeDest = pPort->paint ;
	assert( pRectDest != NULL );

	if ( pPort->block >= 0 ) /* attached to a block which may have moved */
	{
	  assert( pPort->block < NumBlocks );
	  pBlock = & ( pBlocks[ pPort->block ] );
	  pRectDest->x0 = round( pBlock->x + pPort->x0 );
	  pRectDest->y0 = round( pBlock->y + pPort->y0 );
	  pRectDest->x1 = round( pBlock->x + pPort->x1 );
	  pRectDest->y1 = round( pBlock->y + pPort->y1 );
	}
	else {
	  /* floating label */
	  pRectDest->x0 = round( pPort->x0 );
	  pRectDest->y0 = round( pPort->y0 );
	  pRectDest->x1 = round( pPort->x1 );
	  pRectDest->y1 = round( pPort->y1 );
	}
}

/*
 * Blocknum can be less then zero.
 */
void RealEnumerateLayout( LIST* blocks, 
			  LIST* nodes, 
			  LIST* subcells,
			  int blocknum, 
			  const RealEnumerateLayoutParamsStruct* pEnumParams)
{
  int i,j;
  BLOCK *pb;
  RECT rect;
  NODE *pn;
  PORT *pp;
  
  char LabelName[STRMAX];
  char ExtraLabelName[STRMAX];
  PaintType LabelPaintType;
  
  assert( pEnumParams != NULL );
  
  /*** magfile header ***/
  if ( pEnumParams->m_BeginFunc != NULL ) {
    pEnumParams->m_BeginFunc( pEnumParams->m_pClientData ) ;
  }
  
  if ( pEnumParams->m_BeginSubCellsFunc != NULL ) {
    pEnumParams->m_BeginSubCellsFunc( pEnumParams->m_pClientData ) ;
  }
  
  if ( ( subcells != NULL ) && ( pEnumParams->m_SubCellFunc ) ) {
    for (i=0; i<subcells->max; i++) {
      pEnumParams->m_SubCellFunc( subcells->p.use[i], pEnumParams->m_pClientData );
    }
  }
  
  if ( pEnumParams->m_BeginPaintsFunc != NULL ) {
    if ( blocks->max != 0 ) {
      pEnumParams->m_BeginPaintsFunc( pEnumParams->m_pClientData ) ;
    }
  }
  /*** draw block rectangles ***/
  assert( blocks != NULL );
  /*
   * If we are supposed to be enumerating the painted rectangles.
   */
  if ( pEnumParams->m_PaintRectFunc != NULL ) {
    /*
     * if blocknum is less than zero we are supposed to output all the
     * blocks, otherwise we are supposed to only output one specific block specified
     * by the block num.
     */
    if ( blocknum >= 0 ) {
      pb = & ( blocks->p.block[blocknum] );
      DoBlockPaint( pb, pEnumParams->m_PaintRectFunc, pEnumParams->m_pClientData ) ;
    }
    else {
      /*
       * For all the blocks in the list of blocks
       */
      for ( i = 0; i < blocks->max; i++ ) {
	pb = & ( blocks->p.block[i] );
	DoBlockPaint( pb, pEnumParams->m_PaintRectFunc, pEnumParams->m_pClientData ) ;
      }
    }
  }
  
  if ( pEnumParams->m_BeginLabelsFunc != NULL ) {
    pEnumParams->m_BeginLabelsFunc( pEnumParams->m_pClientData );
  }
  
  if ( pEnumParams->m_LabelFunc != NULL ) {
    for ( i = 0 ; i < nodes->max; i++) {
      pn = & ( nodes->p.node[i] );  
      for ( j = 0 ; j < pn->ports->max ; j++ ) {
	pp = & ( pn->ports->p.port[j] );
	/*
	  Only enumberate labels that are for the
	  specified block.
	*/
	if ( ( blocknum < 0 ) || ( pp->block == blocknum ) ) {
	  GetLabelInfo( &LabelPaintType, 
			&rect, 
			LabelName,
			sizeof( LabelName ),
			ExtraLabelName,
			sizeof( ExtraLabelName ),
			pn, 
			blocks->p.block,
			blocks->max,
			pp );
	  pEnumParams->m_LabelFunc( Paints->p.paint[ LabelPaintType ], 
				    rect,
				    LabelName,
				    ExtraLabelName,
				    pEnumParams->m_pClientData );
	  
	}
      }
      
    }
  }
  if ( pEnumParams->m_EndFunc != NULL ){
    pEnumParams->m_EndFunc( pEnumParams->m_pClientData ) ;
  }
}

static void BeginOldToNewGlueFunc( void* pClientData ){
  EnumerateLayoutParamsStruct* pOldParams = ( EnumerateLayoutParamsStruct* ) pClientData ;
  

  if ( pOldParams->m_BeginFunc != NULL ) {
    pOldParams->m_BeginFunc( pOldParams->m_pClientData );
  }
}

static void BeginSubCellsOldToNewGlueFunc( void* pClientData ){
  EnumerateLayoutParamsStruct* pOldParams = ( EnumerateLayoutParamsStruct* ) pClientData ;
  
  if ( pOldParams->m_BeginSubCellsFunc != NULL ) {
    pOldParams->m_BeginSubCellsFunc( pOldParams->m_pClientData );
  }
}

static void SubCellOldToNewGlueFunc( USE use, void* pClientData ){
  EnumerateLayoutParamsStruct* pOldParams = ( EnumerateLayoutParamsStruct* ) pClientData ;
  
  if ( pOldParams->m_SubCellFunc != NULL ) {
    pOldParams->m_SubCellFunc( use, pOldParams->m_pClientData );
  }
}

static void BeginPaintsOldToNewGlueFunc( void* pClientData ){
  EnumerateLayoutParamsStruct* pOldParams = ( EnumerateLayoutParamsStruct* ) pClientData ;
  
  if ( pOldParams->m_BeginPaintsFunc != NULL ) {
    pOldParams->m_BeginPaintsFunc( pOldParams->m_pClientData );
  }
}

static void PaintRectOldToNewGlueFunc( PAINT Paint, RECT RectToPaint, void* pClientData ){
  EnumerateLayoutParamsStruct* pOldParams = ( EnumerateLayoutParamsStruct* ) pClientData ;
  
  if ( pOldParams->m_PaintRectFunc != NULL ) {
    pOldParams->m_PaintRectFunc( Paint, RectToPaint, pOldParams->m_pClientData );
  }
}

static void BeginLabelsOldToNewGlueFunc( void* pClientData ){
  EnumerateLayoutParamsStruct* pOldParams = ( EnumerateLayoutParamsStruct* ) pClientData ;
  
  if ( pOldParams->m_BeginLabelsFunc != NULL ) {
    pOldParams->m_BeginLabelsFunc( pOldParams->m_pClientData );
  }

}

static void LabelInfoOldToNewGlueFunc( PAINT LabelPaint, RECT LabelRect,
				       char* LabelName,
				       char* ExtraLabelName,
				       void* pClientData ){
  EnumerateLayoutParamsStruct* pOldParams = ( EnumerateLayoutParamsStruct* ) pClientData ;

  if ( pOldParams->m_LabelFunc != NULL ) {
    pOldParams->m_LabelFunc( LabelPaint, LabelRect, LabelName,  pOldParams->m_pClientData );
  }
}

static void EndOldToNewGlueFunc( void* pClientData ){
  EnumerateLayoutParamsStruct* pOldParams = ( EnumerateLayoutParamsStruct* ) pClientData ;
  
  if ( pOldParams->m_EndFunc != NULL ) {
    pOldParams->m_EndFunc( pOldParams->m_pClientData );
  }
}

void EnumerateLayout( LIST* blocks, LIST* nodes, LIST* subcells, 
		      int blocknum, const EnumerateLayoutParamsStruct* pEnumParams){
  RealEnumerateLayoutParamsStruct MyParams;
  MyParams.m_BeginFunc = BeginOldToNewGlueFunc ;
  MyParams.m_BeginSubCellsFunc = BeginSubCellsOldToNewGlueFunc ;
  MyParams.m_SubCellFunc = SubCellOldToNewGlueFunc ;
  MyParams.m_BeginPaintsFunc = BeginPaintsOldToNewGlueFunc ;
  MyParams.m_PaintRectFunc = PaintRectOldToNewGlueFunc ;
  MyParams.m_BeginLabelsFunc = BeginLabelsOldToNewGlueFunc ;
  MyParams.m_LabelFunc = LabelInfoOldToNewGlueFunc ;
  MyParams.m_EndFunc = EndOldToNewGlueFunc ;
  MyParams.m_pClientData = ( void * ) pEnumParams;
  
  RealEnumerateLayout( blocks, nodes, subcells, blocknum, &MyParams ) ;
}


