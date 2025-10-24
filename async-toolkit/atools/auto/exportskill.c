#include "auto.h"
#include "enumlayout.h"
#include "exportskill.h"
#define INDENT_DELTA 4

#define SKILL_EXPORT_LAMBDA 1

static unsigned long IsHandLayout( void ) {
  double* pHandLayoutParam ;
  unsigned int isHandLayout ;

  isHandLayout = 0;

  pHandLayoutParam = get_parm("handlayout") ;

  if ( pHandLayoutParam != NULL ) {
    isHandLayout = ( *pHandLayoutParam - 1.0 ) < 0.001 ;
  }
  return isHandLayout ;

}

struct export_skill_layout_enum_client_data{
  export_skill_layout_state* pExportState;
  char* m_BlockName ;
  long m_CurrImplicitGroupNum ;
};

typedef struct export_skill_layout_enum_client_data* Pesklecd;


static Pexport_skill_print_state 
GetPrintStateFromExportSkillState( export_skill_layout_state* pState ) {
  assert( pState != NULL );
  return & ( pState->m_PrintState ) ;
}

static Pexport_skill_print_state GetPrintStateFromEnumClientData( Pesklecd pData ) {
  assert( pData != NULL ) ;
  return GetPrintStateFromExportSkillState( pData->pExportState ) ;
}

static void InitPrintState( Pexport_skill_print_state pState, FILE* File ) {
  assert( pState != NULL ) ;
  pState->m_File = File ;
  pState->m_Indent = 0 ;
}

static void DeInitPrintState( Pexport_skill_print_state pState ) {
  fclose( pState->m_File );
}

static void IndentPrintState( Pexport_skill_print_state pState ) {
  assert( pState != NULL ) ;
  assert( pState->m_Indent + INDENT_DELTA >= pState->m_Indent ) ;
  pState->m_Indent += INDENT_DELTA ;
}

static void UnIndentPrintState( Pexport_skill_print_state pState ) {
  assert( pState != NULL );
  assert( pState->m_Indent >= INDENT_DELTA ) ;
  pState->m_Indent -= INDENT_DELTA ;
}

static void PrintIndent( FILE* pFile, unsigned long indent ) {
  int i ;
  for ( i = 0; i < indent ; i++ ) {
    fputc( ' ', pFile );
  }
}

static void PrintCodeLine( const char* code, Pexport_skill_print_state pPrintState ) {
  PrintIndent( pPrintState->m_File, pPrintState->m_Indent ) ;
  fprintf( pPrintState->m_File, "%s\n", code );
}

typedef void (*DoPaintRectFunc)( PAINT Paint, 
				 RECT RectToPaint, 
				 const char* CadenceLayerName,
				 void* pClientData,
				 const void* pTableClientData );

typedef struct _layermapping {
  const char* MagicName;
  const char* CadenceName;
  DoPaintRectFunc PaintFunc;
  const void* pTableClientData;
} layermapping;


static void DoTranslatedLayerPaint( PAINT Paint,
				    RECT RectToPaint,
				    const char* CadenceLayerName,
				    void* pClientData, 
				    const void* pTableClientData ) {
  Pesklecd pData = ( Pesklecd ) pClientData ;	
  char CodeBuffer[ 1024 ] ;
  safe_sprintf( CodeBuffer, "( dbCreateRect TheCellView \"%s\" ( list ( list %d %d ) ( list %d %d ) ) ) ; %d",
	   CadenceLayerName,
	   round( RectToPaint.x0 ),
	   round( RectToPaint.y0 ),
	   round( RectToPaint.x1 ),
	   round( RectToPaint.y1 ),
	   __LINE__ );	
  PrintCodeLine( CodeBuffer, GetPrintStateFromEnumClientData( pData ) );
}

static void CrackRect( RECT Src, 
		       double* left,
		       double* bottom,
		       double* width,
		       double* height ) {
  assert( left != NULL );
  assert( bottom != NULL );
  assert( width != NULL );
  assert( height != NULL );
  
  *width = fabs( Src.x1 - Src.x0 );
  *height = fabs( Src.y1 - Src.y0 ) ;
  
  *left = ( Src.x0 < Src.x1 ) ? Src.x0 : Src.x1 ;
  *bottom = ( Src.y0 < Src.y1 ) ? Src.y0 : Src.y1 ;

}


#define MIN_GATE_LENGTH 3.0
#define MIN_GATE_WIDTH 4.0 

static void DoTransistorPaint( const char* TransistorSkillFunc,
			       PAINT Paint,
			       RECT RectToPaint,
			       void* pClientData,
			       const void* pTableClientData ) {
  Pesklecd pData = ( Pesklecd ) pClientData ;	
  double width ;
  double height;
  double left;
  double bottom;
  double centerx;
  double centery;
  double glength;
  double gwidth;
  char* CodeBuffer;
  char* SourceName;
  char* DrainName;
  char* GateName;
  const char* NilStr = "nil";
  char* Source;
  char* Gate;
  char* Drain;
  const char* Rotation;
  const char* NoRotation = "R0";
  const char* RotatedRotation = "R90";
  const char* TransistorFormat = "( %s TheCellView %d %d ( list %f %f ) \"%s\" %s %s %s ) ; %d ";
  
  CrackRect( RectToPaint, &left, &bottom, &width, &height );

  if ( fabs( height - width ) < 0.001 ) {
    Rotation = NoRotation;
    SourceName = RectToPaint.abutS;
    DrainName = RectToPaint.abutN;
  }
  else if ( fabs( height - MIN_GATE_LENGTH ) < 0.001 ) {
    Rotation = NoRotation ;
    SourceName = RectToPaint.abutS;
    DrainName = RectToPaint.abutN;

    glength = height;
    gwidth = width ;
  }
  else if ( fabs( width - MIN_GATE_LENGTH ) < 0.001 ) {
    Rotation = RotatedRotation ;
    SourceName = RectToPaint.abutE;
    DrainName = RectToPaint.abutW;
    glength = width ;
    gwidth = height ;
  }
  else if ( fabs( height - MIN_GATE_WIDTH ) < 0.001 ) {
    Rotation = RotatedRotation ;
    SourceName = RectToPaint.abutE;
    DrainName = RectToPaint.abutW;
    glength = width ;
    gwidth = height ;
  }
  else if ( fabs( width - MIN_GATE_WIDTH ) < 0.001 ) {
    Rotation = NoRotation ;
    SourceName = RectToPaint.abutS;
    DrainName = RectToPaint.abutN;
    glength = height ;
    gwidth = width ;
  }
  else {
    Rotation = NoRotation ;
    SourceName = RectToPaint.abutS;
    DrainName = RectToPaint.abutN;
    glength = height ;
    gwidth = width ;
  }

  GateName = RectToPaint.name ;

  glength = height ;
  gwidth = width ;
  
  centerx = left + ( width / 2 ) ;
  centery = bottom + ( height / 2 ) ;
  

  Source = NULL ;
  Gate = NULL ;
  Drain = NULL ;

  if ( IsHandLayout() ) {
    SourceName = NULL ;
    DrainName = NULL ;
    GateName = NULL ;
  }

  if ( SourceName != NULL ) {
    const char* NodeName = SourceName;
    /* Make buffer strlen + 3, 2 quotes and a null. */
    if ( NodeName != NULL ) {
      Source = ( char* ) leak_malloc( ( strlen( NodeName ) + 3 ) * sizeof( char ) ) ;
      assert( Source != NULL );
      safe_sprintf( Source, "\"%s\"", NodeName );
    }
  }
  if ( Source == NULL ) {
    Source = ( char* ) leak_malloc( ( strlen( NilStr ) + 1 ) * sizeof( char ) ) ;
    assert( Source != NULL );
    strcpy( Source, NilStr );
  }

  if ( DrainName != NULL ) {
    const char* NodeName = DrainName;
    /* Make buffer strlen + 3, 2 quotes and a null. */ 
    if ( NodeName != NULL ) {
      Drain = ( char* ) leak_malloc( ( strlen( NodeName ) + 3 ) * sizeof( char ) ) ;
      assert( Drain != NULL );
      safe_sprintf( Drain, "\"%s\"", NodeName );
    }
  }

  if ( Drain == NULL ){
    Drain = ( char* ) leak_malloc( ( strlen( NilStr ) + 1 ) * sizeof( char ) ) ;
    assert( Drain != NULL );
    strcpy( Drain, NilStr );
  }

  if ( GateName != NULL ) {
    const char* NodeName = GateName;
    /* Make buffer strlen + 3, 2 quotes and a null. */ 
    if ( NodeName != NULL ) {
      Gate = ( char* ) leak_malloc( ( strlen( NodeName ) + 3 ) * sizeof( char ) ) ;
      assert( Gate != NULL );
      safe_sprintf( Gate, "\"%s\"", NodeName );
    }
  }
  
  if ( Gate == NULL ) {
    Gate = ( char* ) leak_malloc( ( strlen( NilStr ) + 1 ) * sizeof( char ) ) ;
    assert( Gate != NULL );
    strcpy( Gate, NilStr );
  }

  CodeBuffer = ( char * ) leak_malloc( ( strlen( TransistorSkillFunc ) + 
					 strlen( Source ) + strlen( Gate ) +
					 strlen( Drain ) + 9 
					 + 9 + 32 + 32 + strlen( Rotation ) + 9 +
					 strlen( TransistorFormat )  ) * ( sizeof( char ) ) );

  assert( CodeBuffer != NULL );
  
  safe_sprintf( CodeBuffer, TransistorFormat,
	   TransistorSkillFunc,
	   round( gwidth ),
	   round( glength ),
	   centerx,
	   centery,
	   Rotation,
	   Source,
	   Gate,
	   Drain,
	   __LINE__ );
  
  PrintCodeLine( CodeBuffer, GetPrintStateFromEnumClientData( pData ) );

  leak_free( CodeBuffer );

  if ( Source != NULL ) {
    leak_free( Source );
  }

  if ( Drain != NULL ) {
    leak_free( Drain );
  }

  if ( Gate != NULL ) {
    leak_free( Gate );
  }

}

static void DoNTransistorPaint( PAINT Paint,
				RECT RectToPaint,
				const char* CadenceLayerName, 
				void* pClientData,
				const void* pTableClientData ) {
  DoTransistorPaint( "CreateNMOS", Paint, RectToPaint, pClientData, pTableClientData );
}

static void DoPTransistorPaint( PAINT Paint, 
				RECT RectToPaint, 
				const char* CadenceLayerName,
				void* pClientData,
				const void* pTableClientData ) {
  DoTransistorPaint( "CreatePMOS", Paint, RectToPaint, pClientData, pTableClientData );
}

static void DoViaPaint( PAINT Paint, 
			RECT RectToPaint,
			const char* CadenceLayerName, 
			void* pClientData,
			const void* pTableClientData ) {
  Pesklecd pData = ( Pesklecd ) pClientData ;	
  double width ;
  double height;
  double left;
  double bottom;
  double right;
  double top;
  double centerx;
  double centery;
  char CodeBuffer[ 1024 ];

  
  CrackRect( RectToPaint, &left, &bottom, &width, &height );
  
  right = left + width ;
  top = bottom + height ;
  centerx = left + ( width / 2 ) ;
  centery = bottom + ( height / 2 ) ;
  

  safe_sprintf( CodeBuffer, "( MakePCellContact TheCellView \"%s\" ( list %f %f ) %d %d ) ; %d",
	   Paint.name, 
	   centerx, 
	   centery, 
	   round( width ),
	   round( height ),
	   __LINE__ );			
  PrintCodeLine( CodeBuffer, GetPrintStateFromEnumClientData( pData ) ) ;
}


static void export_skill_state_init_pin( export_skill_pin* pDest, 
					 const char* LayerName,
					 double x0,
					 double y0,
					 double x1,
					 double y1,
					 char* NetName,
					 long GroupNum
					 ) {

  
  
  pDest->m_LayerName = ( char* ) leak_malloc( ( strlen( LayerName ) + 1 ) * sizeof( char ) ) ;
  strcpy( pDest->m_LayerName, LayerName ) ;

  pDest->x0 = x0;
  pDest->y0 = y0;
  pDest->x1 = x1;
  pDest->y1 = y1;

  pDest->m_NetName = ( char* ) leak_malloc( ( strlen( NetName ) + 1 ) * sizeof( char ) ) ;

  strcpy( pDest->m_NetName, NetName ) ;

  pDest->m_GroupNum = GroupNum ;

}

static void export_skill_state_add_weak_pin( export_skill_layout_state* pState,
					     const char* LayerName,
					     double x0,
					     double y0,
					     double x1,
					     double y1,
					     char* NetName,
					     long GroupNum ) {
  export_skill_pin* pTempPins ;
  export_skill_pin* pNewPin ;
  assert( pState != NULL ) ;
  assert( LayerName != NULL );
  assert( NetName != NULL );
  
  assert( pState->m_pWeakPins != NULL );

  if ( pState->m_NumWeakPinsAllocated == pState->m_NumWeakPinsUsed ) {
    pState->m_NumWeakPinsAllocated <<= 1 ;


    pTempPins = ( export_skill_pin*) 
	leak_malloc( ( pState->m_NumWeakPinsAllocated ) * sizeof ( export_skill_pin ) ) ;

    if ( pTempPins != NULL ) {
      memcpy( pTempPins, 
	      pState->m_pWeakPins, 
	      pState->m_NumWeakPinsUsed * sizeof( export_skill_pin ) ) ;

      leak_free( pState->m_pWeakPins ) ;
      
      pState->m_pWeakPins = pTempPins ;
    }
    else { 
      printf( "out of memory" );
      assert( 0 ) ;
      exit( -1 ) ;
    }
  }

  assert( pState->m_NumWeakPinsAllocated > pState->m_NumWeakPinsUsed ) ;

  pNewPin = & ( pState->m_pWeakPins[ pState->m_NumWeakPinsUsed ] ) ;
  
  export_skill_state_init_pin( pNewPin, LayerName, x0, y0, x1, y1, NetName, GroupNum );
 
  ++ ( pState->m_NumWeakPinsUsed ) ;

}

static void export_skill_state_remove_weak_pin( export_skill_layout_state* pState,
						unsigned long which ) {
  assert( pState != NULL ) ;
  assert( pState->m_pWeakPins != NULL ) ;
  assert( pState->m_NumWeakPinsUsed != 0 ) ;

  leak_free( pState->m_pWeakPins[which].m_NetName ) ;
  leak_free( pState->m_pWeakPins[which].m_LayerName ) ;
  if ( ( pState->m_NumWeakPinsUsed - 1 ) != which ) {
    memcpy( & ( pState->m_pWeakPins[which] ), 
	    & ( pState->m_pWeakPins[ pState->m_NumWeakPinsUsed - 1 ] ),
	    sizeof( export_skill_pin ) ) ;
  }

  -- ( pState->m_NumWeakPinsUsed ) ;
}


static void export_skill_state_add_strong_pin( export_skill_layout_state* pState,
					       const char* LayerName,
					       double x0,
					       double y0,
					       double x1,
					       double y1,
					       char* NetName,
					       long GroupNum
					       ) {
  export_skill_pin* pTempPins ;
  export_skill_pin* pNewPin ;
  assert( pState != NULL ) ;
  
  assert( LayerName != NULL );
  assert( NetName != NULL );
  
  assert( pState->m_pStrongPins != NULL );

  if ( pState->m_NumStrongPinsAllocated == pState->m_NumStrongPinsUsed ) {
    pState->m_NumStrongPinsAllocated <<= 1 ;


    pTempPins = ( export_skill_pin*) 
	leak_malloc( ( pState->m_NumStrongPinsAllocated ) * sizeof ( export_skill_pin ) ) ;

    if ( pTempPins != NULL ) {
      memcpy( pTempPins, 
	      pState->m_pStrongPins, 
	      pState->m_NumStrongPinsUsed * sizeof( export_skill_pin ) ) ;

      leak_free( pState->m_pStrongPins ) ;
      
      pState->m_pStrongPins = pTempPins ;
    }
    else { 
      printf( "out of memory" );
      assert( 0 ) ;
      exit( -1 ) ;
    }
  }

  assert( pState->m_NumStrongPinsAllocated > pState->m_NumStrongPinsUsed ) ;

  pNewPin = & ( pState->m_pStrongPins[ pState->m_NumStrongPinsUsed ] ) ;

  export_skill_state_init_pin( pNewPin, LayerName, x0, y0, x1, y1, NetName, GroupNum );

  ++ ( pState->m_NumStrongPinsUsed ) ;

}

static void export_skill_state_remove_strong_pin( export_skill_layout_state* pState,
						unsigned long which ) {
  assert( pState != NULL ) ;
  assert( pState->m_pStrongPins != NULL ) ;
  assert( pState->m_NumStrongPinsUsed != 0 ) ;

  leak_free( pState->m_pStrongPins[which].m_NetName ) ;
  leak_free( pState->m_pStrongPins[which].m_LayerName ) ;
  if ( ( pState->m_NumStrongPinsUsed - 1 ) != which ) {
    memcpy( & ( pState->m_pStrongPins[which] ), 
	    & ( pState->m_pStrongPins[ pState->m_NumStrongPinsUsed - 1 ] ),
	    sizeof( export_skill_pin ) ) ;
  }

  -- ( pState->m_NumStrongPinsUsed ) ;
}




static const layermapping mappings[] = {
  { "metal1", "metal1", DoTranslatedLayerPaint, NULL },
  { "metal2", "metal2", DoTranslatedLayerPaint, NULL },
  { "metal3", "metal3", DoTranslatedLayerPaint, NULL },
  { "metal4", "metal4", DoTranslatedLayerPaint, NULL },
  { "metal5", "metal5", DoTranslatedLayerPaint, NULL },
  { "metal6", "metal6", DoTranslatedLayerPaint, NULL },
  { "metal7", "metal7", DoTranslatedLayerPaint, NULL },
  { "polysilicon", "poly", DoTranslatedLayerPaint, NULL },
  { "ndiffusion", "nactive", DoTranslatedLayerPaint, NULL },
  { "pdiffusion", "pactive", DoTranslatedLayerPaint, NULL },
  { "nwell", "nwell", DoTranslatedLayerPaint, NULL },
  { "pselect", "pselect", DoTranslatedLayerPaint, NULL },
  { "pwell", "pwell", DoTranslatedLayerPaint, NULL },
  { "nselect", "nselect", DoTranslatedLayerPaint, NULL },
  { "ntransistor", NULL, DoNTransistorPaint, NULL },
  { "ptransistor", NULL, DoPTransistorPaint, NULL },
  { "ndcontact", "", DoViaPaint, NULL },
  { "pdcontact", "", DoViaPaint, NULL },
  { "polycontact", "", DoViaPaint, NULL },
  { "m2contact", "", DoViaPaint, NULL },
  { "m3contact", "", DoViaPaint, NULL },
  { "m4contact", "", DoViaPaint, NULL },
  { "m5contact", "", DoViaPaint, NULL },
  { "m6contact", "", DoViaPaint, NULL }, 
  { "psubstratepcontact", "", DoViaPaint, NULL },
  { "nsubstratencontact", "", DoViaPaint, NULL }
};

#define NUM_LAYER_MAPPINGS ( sizeof(mappings) / sizeof( layermapping ) )


static unsigned char GetMapping( const char* MagLayerName, 
				 unsigned long* IndexDest ) {
  int i;
  
  i = 0 ;
  
  while ( ( i < NUM_LAYER_MAPPINGS ) &&
	  ( strcasecmp( MagLayerName, mappings[ i ].MagicName ) != 0 ) )
    {
      ++i ;
    }
  
  *IndexDest = i ;
  return ( i < NUM_LAYER_MAPPINGS ) ;
  
}

/* 17141 Murphy Suite 5c Irvine 92614 */

typedef const char* PCChar;


static void export_skill_layout_begin_func( void* pClientData ){

  char CodeBuff[ 1024 ];
  Pesklecd pData = ( Pesklecd ) pClientData ;

  safe_sprintf( CodeBuff, "( defun Block_%lu_Func ( BlockLibHandle ) ; %d ",
	   pData->pExportState->m_CurrBlockNum,
	   __LINE__ ) ;

  pData->pExportState->m_CurrBlockNum++;

  PrintCodeLine( CodeBuff, GetPrintStateFromEnumClientData( pData ) ) ;

  IndentPrintState(  GetPrintStateFromEnumClientData( pData ) ) ;
  
  PrintCodeLine( "( let", GetPrintStateFromEnumClientData( pData ) );
  

  IndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( "(", GetPrintStateFromEnumClientData( pData ) );

  IndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  safe_sprintf( CodeBuff, 
	   "( TheCellView ( dbOpenCellViewByType BlockLibHandle "
	   "\"%s\" \"layout\" \"maskLayout\" \"w\" ) ) ; %d",
	   pData->m_BlockName,
	   __LINE__ );
  PrintCodeLine( CodeBuff, GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( "( TheTerminalTable ( makeTable \"TermTable\" nil ) )",
		 GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( "( TheNetTable ( makeTable \"NetTable\" nil ) )",
		 GetPrintStateFromEnumClientData( pData ) ) ;

  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( ")", GetPrintStateFromEnumClientData( pData ) );
  
  PrintCodeLine( "( let",  GetPrintStateFromEnumClientData( pData )) ;
  IndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  
  PrintCodeLine( "(", GetPrintStateFromEnumClientData( pData ) );
  IndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  
  PrintCodeLine( " ( Figures ( list", GetPrintStateFromEnumClientData( pData ) ) ;
  IndentPrintState( GetPrintStateFromEnumClientData( pData ) );

}


static void export_skill_layout_paint_func( PAINT Paint, 
					    RECT RectToPaint, 
					    void* pClientData ) {
  unsigned long i;
  
  if (  GetMapping( Paint.name, &i ) ) {
    RectToPaint.x0 *= ( SKILL_EXPORT_LAMBDA );
    RectToPaint.y0 *= ( SKILL_EXPORT_LAMBDA );
    RectToPaint.x1 *= ( SKILL_EXPORT_LAMBDA );
    RectToPaint.y1 *= ( SKILL_EXPORT_LAMBDA );
    
    mappings[i].PaintFunc( Paint, RectToPaint,
			   mappings[i].CadenceName,
			   pClientData, mappings[i].pTableClientData );
  }

}
/* we don't need these right now but we might later
static void export_skill_layout_begin_paints_func( void *pClientData ){
	Pesklecd pData = ( Pesklecd ) pClientData ;	
	
}
static void export_skill_layout_begin_labels_func( void *pClientData ) {
	Pesklecd pData = ( Pesklecd ) pClientData ;	
}
*/

static void export_skill_get_label_name_part( const char* LabelNamePos,
					      char *pPartDest,
					      unsigned long destlen,
					      unsigned long* count,
					      unsigned long* founddelim,
					      char delim ) {
  const char* pSrc = LabelNamePos ;
  assert( LabelNamePos != NULL );
  assert( pPartDest != NULL ) ;
  assert( destlen > 0 );
  
  
  *count = 0 ;
  
     

  while ( ( (*pSrc) != '\0' ) && 
	  ( (*pSrc) != delim ) &&
	  ( (*count) < destlen ) ) {
    *pPartDest = *pSrc;
    ++pPartDest;
    ++pSrc;
    ++(*count);
  }

  *founddelim = ( *pSrc ==  delim ) ;

  if ( destlen <= *count ){
    --pPartDest ;
  }

  *pPartDest = '\0';

}

static void export_skill_parse_label_name( const char* LabelName, 
					   const char* LabelLayer,
					   unsigned long* bIsPin,
					   char *NetNameDest,
					   unsigned long NetNameDestLen,
					   char *LayerNameDest,
					   unsigned long LayerNameDestLen,
					   unsigned long* GroupNum,
					   unsigned long* bIsStrong,
					   Pesklecd pData ) {
  unsigned long delimfound;
  unsigned long partcount;

  char CommandStr[4] ;

  char* TempBuffer ;

  const char* pCurrPos;

  unsigned long TempBufferSize = ( strlen( LabelName ) + 1 ) * sizeof( char ) ;


  TempBuffer = leak_malloc( TempBufferSize );

  if ( TempBuffer != NULL ) {

    pCurrPos = LabelName ;
    export_skill_get_label_name_part( pCurrPos, 
				      NetNameDest,
				      NetNameDestLen,
				      &partcount,
				      &delimfound,
				      '|' ) ;
    if ( delimfound ) {
      pCurrPos += ( partcount + 1 );
      
      export_skill_get_label_name_part( pCurrPos,
					CommandStr,
					sizeof( CommandStr ),
					&partcount,
					&delimfound,
					'|');
      
      if ( ( delimfound ) && ( strcmp( CommandStr, "pin" ) == 0 ) ) {
	*bIsPin = 1;
	pCurrPos += ( partcount + 1 );

	export_skill_get_label_name_part( pCurrPos,
					  TempBuffer,
					  TempBufferSize,
					  &partcount,
					  &delimfound,
					  '|' ) ;
	if ( strcmp( TempBuffer, "s" ) == 0 ) {
	  *bIsStrong = 1 ;
	}
	else if ( strcmp( TempBuffer, "w" ) == 0 ) {
	  *bIsStrong = 0; 
	}
	else {
	  fprintf( stderr, "Invalid pin: %s\nGroup type not specified\n", LabelName );
	  *bIsPin = 0 ;
	}

	if ( delimfound ) {
	  pCurrPos += ( partcount + 1 ) ;
	  
	  export_skill_get_label_name_part( pCurrPos,
					    TempBuffer,
					    TempBufferSize,
					    &partcount,
					    &delimfound,
					    '|' ) ;
	  if ( partcount > 0 ) {
	   
	    pCurrPos += ( partcount + 1 ) ;
	    *GroupNum = strtol( TempBuffer, NULL, 10 );
	    
	    {
	      unsigned long MappingIndex;
	      if ( GetMapping( LabelLayer, &MappingIndex ) ) {
		strncpy( LayerNameDest, mappings[MappingIndex].CadenceName, LayerNameDestLen );
		LayerNameDest[ LayerNameDestLen - 1] = '\0';
	      }
	    }
	  }
	}
	else{
	  fprintf( stderr, "Invalid pin: %s\nNo group id specified\n", LabelName );
	  *bIsPin = 0 ;
	}
      }
      else{
	*bIsPin = 0 ;
      }
    }
    else{
      *bIsPin = 1 ;
      *bIsStrong = 1 ;
      *GroupNum = ( pData->m_CurrImplicitGroupNum - 1 ) ;
      --(pData->m_CurrImplicitGroupNum);
      strncpy( LayerNameDest, LabelLayer, LayerNameDestLen );
      LayerNameDest[ LayerNameDestLen - 1] = '\0';
      {
	unsigned long MappingIndex ;
	if ( GetMapping( LabelLayer, &MappingIndex ) ) {
	  strncpy( LayerNameDest, mappings[MappingIndex].CadenceName, LayerNameDestLen );
	  LayerNameDest[ LayerNameDestLen - 1] = '\0';
	}
      }
    }
    leak_free( TempBuffer ) ;
  }
}

static void export_skill_layout_label_info_func( PAINT LabelPaint, 
						 RECT LabelRect,
						 char* LabelName,
						 char* LabelNameExtra,
						 void* pClientData ) {
  Pesklecd pData = ( Pesklecd ) pClientData ;	
  export_skill_print_state* pPrintState;

  unsigned long bIsPin ;
  unsigned long bIsStrong ;
  char* NetName ;
  char* LayerName ;
  char* LabelNameTemp;
  long GroupID ;

  double Left;
  double Bottom;
  double Width;
  double Height;
 
  unsigned long LabelNameSize ;
  unsigned long LayerNameSize ;
  
  
  
  
  pPrintState = GetPrintStateFromEnumClientData( pData ) ;

  CrackRect( LabelRect, &Left, &Bottom, &Width, &Height ) ;


  LabelNameTemp = ( char* ) leak_malloc( ( strlen( LabelName ) +
					   strlen( LabelNameExtra ) +
					   2 ) * sizeof( char ) );
  
  if ( LabelNameTemp != NULL ) {
    strcpy( LabelNameTemp, LabelName );
    if ( strlen( LabelNameExtra ) != 0 ) {
      /* for whatever reason LabelNameExtra sometime comes with the '|' already prepended
       * so this is a hack to keep from adding an additional '|' in the label name
       */
      if ( (*LabelNameExtra) != '|' )
	strcat( LabelNameTemp, "|" );
      strcat( LabelNameTemp, LabelNameExtra ) ;
    }
    LabelNameSize = ( strlen( LabelNameTemp ) + 1 ) * sizeof( char ) ;
    
    LayerNameSize = ( strlen( LabelPaint.name ) + 1 ) * sizeof( char ) ;
    
    if ( LayerNameSize > LabelNameSize ) {
      LabelNameSize = LayerNameSize ;
    }
    
    NetName = leak_malloc( LabelNameSize );
    
    if ( NetName != NULL ) {
      LayerName = leak_malloc( LabelNameSize ) ;
      if ( LayerName != NULL ) {
	export_skill_parse_label_name( LabelNameTemp,
				       LabelPaint.name,
				       &bIsPin,
				       NetName,
				       LabelNameSize,
				       LayerName,
				       LayerNameSize,
				       &GroupID,
				       &bIsStrong,
				       pData );
	
	if ( bIsPin ) {
	  export_skill_layout_state* pExportState = pData->pExportState ;
	  
	  if ( ( Height > 0 ) && ( Width > 0 ) ) {
	    
	    if ( bIsStrong ) {
	      assert( pExportState != NULL ) ;
	      
	      assert( pExportState->m_pStrongPins != NULL ) ; 
	      
	      export_skill_state_add_strong_pin( pExportState, 
						 LayerName,
						 LabelRect.x0,
						 LabelRect.y0,
						 LabelRect.x1,
						 LabelRect.y1,
						 NetName,
						 GroupID ) ;
	    }
	    else{
	      assert( pExportState != NULL ) ;
	      
	      assert( pExportState->m_pStrongPins != NULL ) ; 
	      
	      export_skill_state_add_weak_pin( pExportState, 
					       LayerName,
					       LabelRect.x0,
					       LabelRect.y0,
					       LabelRect.x1,
					       LabelRect.y1,
					       NetName,
					       GroupID ) ;
	    }
	  }
	  else if ( IsHandLayout() ) {

	    const char* LabelOutputWithGroupFmt = 
	      "( dbCreateLabel TheCellView ( list \"%s\" \"label\" )"
	      " ( list %f %f ) \"%s%d\" \"lowerLeft\" \"R0\" \"euroStyle\" 1 )";
	    const char* LabelOutputWithOutGroupFmt = 
	      "( dbCreateLabel TheCellView ( list \"%s\" \"label\" )"
	      " ( list %f %f ) \"%s\" \"lowerLeft\" \"R0\" \"euroStyle\" 1 )";
	    
	    char* CodeBuffer = ( char * ) leak_malloc( ( strlen( LayerName ) +
							 strlen( NetName ) +
							 strlen( LabelOutputWithOutGroupFmt ) +
							 strlen( LabelOutputWithGroupFmt ) +
							 ( 12 * 3 ) + 1 )
						       * sizeof( char ) );
	    if ( CodeBuffer != NULL ) {

	      if ( GroupID < 0 ) {

		safe_sprintf( CodeBuffer, LabelOutputWithOutGroupFmt,
			 LayerName,
			 Left,
			 Bottom,
			 NetName );
	      }
	      else{
		safe_sprintf( CodeBuffer, LabelOutputWithGroupFmt,
			 LayerName,
			 Left,
			 Bottom,
			 NetName,
			 GroupID );
	      }
	      
	      PrintCodeLine( CodeBuffer, pPrintState );
	      
	      leak_free( CodeBuffer );
	    }
	  }
	}
	else{
	  printf( "WARNING: %s is not a valid pin label\n", LabelNameTemp ) ;
	}
      }
      leak_free( LayerName );
    }
    leak_free( NetName );
  }
  else{
    leak_free( LabelNameTemp ) ;
  }
  
}

static void export_skill_layout_copy_pin( export_skill_pin* pDest, export_skill_pin* pSrc ) {

  if ( pDest->m_NetName != NULL ) {
    leak_free( pDest->m_NetName ) ;
  }

  if ( pDest->m_LayerName != NULL ) {
    leak_free( pDest->m_LayerName ) ;
  }

  pDest->x0 = pSrc->x0;
  pDest->y0 = pSrc->y0;
  pDest->x1 = pSrc->x1;
  pDest->y1 = pSrc->y1;

  pDest->m_GroupNum = pSrc->m_GroupNum ;

  pDest->m_NetName = ( char * ) leak_malloc( ( strlen( pSrc->m_NetName ) + 1 ) * sizeof( char ) ) ;
  
  strcpy( pDest->m_NetName, pSrc->m_NetName ) ;


  pDest->m_LayerName = ( char * ) leak_malloc( ( strlen( pSrc->m_LayerName ) + 1 ) * sizeof( char ) ) ;
 
  strcpy( pDest->m_LayerName, pSrc->m_LayerName ) ;

 

}

/*  static void export_skill_print_pin( export_skill_pin* pPin ) { */
/*    printf( "%s %f %f %f %f %ld %s\n", */
/*  	  pPin->m_NetName, */
/*  	  pPin->x0, */
/*  	  pPin->y0, */
/*  	  pPin->x1, */
/*  	  pPin->y1, */
/*  	  pPin->m_GroupNum, */
/*  	  pPin->m_LayerName */
/*  	  ); */
/*  } */

/*  static void export_skill_print_pins( export_skill_pin* pPins, unsigned long num ) { */
/*    int i;  */
/*    for ( i=0 ; i<num ;i++ ){ */
/*      export_skill_print_pin( &(pPins[i]) ); */
/*    } */
/*  } */

static void export_skill_layout_output_pin( export_skill_print_state* pPrintState,
					    export_skill_pin* pPin,
					    const char* FormatStr ) {
  char* CodeBuffer;

  CodeBuffer = ( char* ) 
    leak_malloc( sizeof( char ) * ( ( 2 * strlen( pPin->m_NetName ) ) + 32 +
				    strlen( FormatStr ) ) ) ;
  
  assert( CodeBuffer != NULL ) ;
  
  assert( pPin->x1 > pPin->x0 );
  assert( pPin->y1 > pPin->y0 );

  safe_sprintf( CodeBuffer, 
	   FormatStr,
	   pPin->m_LayerName,
	   round( pPin->x0 ),
	   round( pPin->y0 ),
	   round( pPin->x1 ),
	   round( pPin->y1 ),
	   __LINE__ ) ;
  PrintCodeLine( CodeBuffer, pPrintState );
  
  leak_free( CodeBuffer ) ;
}

static void export_skill_layout_sort_pins( export_skill_pin* pPins,
					   unsigned long NumPins ) {
  unsigned long i;
  unsigned long j;
  export_skill_pin temppin;

  /*printf( "---------------------------------------------------\n");
  export_skill_print_pins( pPins, NumPins );
  printf( "---------------------------------------------------\n");
  */
  
  temppin.m_LayerName = NULL ;
  temppin.m_NetName = NULL ;

  assert( pPins != NULL );

  assert( NumPins > 0 ) ;
  /*Simple bubble sort, blah*/
  for( i = 0 ; i < NumPins ; i++ ) {
    for ( j = 0; j < ( NumPins - 1 ) ; j++ ) {
      if ( pPins[j].m_GroupNum > pPins[j+1].m_GroupNum ) {
	export_skill_layout_copy_pin( &temppin, &(pPins[j]) );
	export_skill_layout_copy_pin( &(pPins[j]), &(pPins[j+1] ) );
	export_skill_layout_copy_pin( &(pPins[j+1]), &temppin ) ;
      }
      else if ( pPins[j].m_GroupNum == pPins[j+1].m_GroupNum ) {
	if ( strcmp( pPins[j].m_NetName, pPins[j+1].m_NetName ) > 0 ) {
	  export_skill_layout_copy_pin( &temppin, &(pPins[j]) );
	  export_skill_layout_copy_pin( &(pPins[j]), &(pPins[j+1] ) );
	  export_skill_layout_copy_pin( &(pPins[j+1]), &temppin ) ;
	}
      }
    }
  }
  
  if ( temppin.m_LayerName != NULL ) {
    leak_free( temppin.m_LayerName ) ;
  }
  if ( temppin.m_NetName != NULL ) {
    leak_free( temppin.m_NetName );
  }

 /*   export_skill_print_pins( pPins, NumPins ); */
/*    printf( "---------------------------------------------------\n"); */
}

static void export_skill_layout_output_pins( export_skill_print_state* pPrintState,
					     export_skill_pin* pPins,
					     unsigned long NumPinsUsed,
					     const char* ConnectionStr ) {
  
  unsigned long GroupStartIndex ;
  unsigned long GroupEndIndex ;
  char* CodeBuffer;
  
  
  const char* PinExpStr = "( dbCreatePin ( getq CurrTerminal net ) ( dbCreateRect TheCellView \"%s\" ( list ( list %d %d ) ( list %d %d ) ) ) ) ; %d";

 

  export_skill_layout_sort_pins( pPins, NumPinsUsed );
  
  GroupStartIndex = 0;
  GroupEndIndex = 0;

  while( GroupStartIndex < NumPinsUsed ) {
    GroupEndIndex = GroupStartIndex ;
    
    while ( ( GroupEndIndex < NumPinsUsed ) && 
	    ( pPins[GroupStartIndex].m_GroupNum == pPins[GroupEndIndex].m_GroupNum ) &&
	    ( strcmp( pPins[GroupStartIndex].m_NetName, pPins[GroupEndIndex].m_NetName) == 0 )) {
      ++GroupEndIndex ;
    }
    
    if ( IsHandLayout() ){

      if ( pPins[GroupStartIndex].m_GroupNum < 0 ) {
	const char* TerminalExpStr = 
	  "( let ( ( CurrTerminal ( ConnectivityMakeTerminal TheCellView ( dbMakeNet TheCellView "
	  "( ImportCellCononicalizeNetName \"%s\" ) nil ) ) ) ) "
	  "( let ( ( PinList  ( list ; %d" ;
	
	CodeBuffer = ( char* ) 
	  leak_malloc( sizeof( char ) * ( ( 2 * strlen( pPins[GroupStartIndex].m_NetName ) ) + 16 + 
					  strlen( TerminalExpStr ) ) ) ;
	
	assert( CodeBuffer != NULL ) ;
	
	safe_sprintf( CodeBuffer, 
		 TerminalExpStr,
		 pPins[GroupStartIndex].m_NetName,
		 __LINE__ ) ;
      }
      else{
	const char* TerminalExpStr = 
	  "( let ( ( CurrTerminal ( ConnectivityMakeTerminal TheCellView ( dbMakeNet TheCellView "
	  "( ImportCellCononicalizeNetName \"%s%d\" ) nil ) ) ) ) "
	  "( let ( ( PinList  ( list ; %d" ;
	
	CodeBuffer = ( char* ) 
	  leak_malloc( sizeof( char ) * ( ( 2 * strlen( pPins[GroupStartIndex].m_NetName ) ) + 16 + 
					  strlen( TerminalExpStr ) ) ) ;
	
	assert( CodeBuffer != NULL ) ;
	
	safe_sprintf( CodeBuffer, 
		 TerminalExpStr,
		 pPins[GroupStartIndex].m_NetName,
		 pPins[GroupStartIndex].m_GroupNum,
		 __LINE__ ) ;
      }
    }
    else{

      const char* TerminalExpStr = 
	"( let ( ( CurrTerminal ( dbCreateTerm ( dbMakeNet TheCellView "
	"( ImportCellCononicalizeNetName \"&%d^%s\" ) nil )  nil \"inputOutput\" ) ) ) "
	"( let ( ( PinList  ( list ; %d" ;

      CodeBuffer = ( char* ) 
	leak_malloc( sizeof( char ) * ( ( 2 * strlen( pPins[GroupStartIndex].m_NetName ) ) + 16 + 
					strlen( TerminalExpStr ) ) ) ;

      assert( CodeBuffer != NULL ) ;
      
      safe_sprintf( CodeBuffer, 
	       TerminalExpStr,
	       pPins[GroupStartIndex].m_GroupNum,
	       pPins[GroupStartIndex].m_NetName,
	       __LINE__ ) ;
    }

    PrintCodeLine( CodeBuffer, pPrintState );
    IndentPrintState( pPrintState ) ;
    IndentPrintState( pPrintState ) ;
    leak_free( CodeBuffer ) ;

    while ( GroupStartIndex < GroupEndIndex ) {
      export_skill_layout_output_pin( pPrintState,
				      &(pPins[GroupStartIndex]),
				      PinExpStr );
      ++GroupStartIndex ;
    }
    PrintCodeLine( ")", pPrintState );
    UnIndentPrintState( pPrintState ) ; 
    UnIndentPrintState( pPrintState ) ;

    PrintCodeLine( ConnectionStr, pPrintState );
   
  }
}
/*   const char* ConnectLoopStr = ") ) ( foreach Pin PinList ( %s ( list Pin ) ) ) PinList ) )" ; */
/*    assert( pPrintState != NULL ); */ 

  
static void export_skill_layout_output_strong_pins( export_skill_layout_state* pState ){
  
  const char* ConnectStr = ") ) ( dbStronglyConnectPins PinList ) PinList ) )" ;
  
  if ( pState->m_NumStrongPinsUsed > 0 ) {
    export_skill_layout_output_pins( GetPrintStateFromExportSkillState( pState ),
				     pState->m_pStrongPins,
				     pState->m_NumStrongPinsUsed,
				     ConnectStr );
    while( pState->m_NumStrongPinsUsed > 0 ) {
      export_skill_state_remove_strong_pin( pState, 0 ) ;
    }
  }
}

static void export_skill_layout_output_weak_pins( export_skill_layout_state* pState ) {
 
  const char* ConnectStr =  ") ) ( foreach Pin PinList ( dbWeaklyConnectPins ( list Pin ) ) ) PinList ) )" ;
  
  if ( pState->m_NumWeakPinsUsed > 0 ){
    export_skill_layout_output_pins( GetPrintStateFromExportSkillState( pState ),
				     pState->m_pWeakPins,
				     pState->m_NumWeakPinsUsed,
				     ConnectStr );
    while( pState->m_NumWeakPinsUsed > 0 ) {
      export_skill_state_remove_weak_pin( pState, 0 ) ;
    }
  }
} 

static void export_skill_layout_end_func( void *pClientData ) {
  Pesklecd pData = ( Pesklecd ) pClientData ;	
  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( ") ) ", GetPrintStateFromEnumClientData( pData ) );
  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( ") ", GetPrintStateFromEnumClientData( pData ) );

  PrintCodeLine( "( ConnectivityMakeTerminalPinsMustConnects TheCellView )",
		 GetPrintStateFromEnumClientData( pData ) );

  PrintCodeLine( "( let ( ( PinListList ( list",  GetPrintStateFromEnumClientData( pData ) );
  IndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  IndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  export_skill_layout_output_strong_pins( pData->pExportState ) ;
  export_skill_layout_output_weak_pins( pData->pExportState ) ;
  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( ") ) )", GetPrintStateFromEnumClientData( pData ) );

  PrintCodeLine( "( dbComputeBBox TheCellView )", GetPrintStateFromEnumClientData( pData ) );
  PrintCodeLine( "( let ( ( BoundingBox ( getq TheCellView bBox ) ) )",
		 GetPrintStateFromEnumClientData( pData ) );
  IndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( "( let ( ( BoxLeft ( car ( car BoundingBox ) ) ) "
		 "( BoxBottom ( car ( cdr ( car BoundingBox ) ) ) ) "
		 "( CurrFigs ( copy ( getq TheCellView shapes ) ) ) )",
		 GetPrintStateFromEnumClientData( pData ) );
  IndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( "( leMergeShapes CurrFigs )", GetPrintStateFromEnumClientData( pData ) ) ;
 /*   PrintCodeLine( "( ImpliedLayerGenDeleteShapes CurrFigs )", GetPrintStateFromEnumClientData( pData ) ); */
  PrintCodeLine( "( leMoveCellViewOrigin TheCellView ( list 0 BoxBottom ) )", GetPrintStateFromEnumClientData( pData ) );
  /*PrintCodeLine( "( foreach Figure Figures ( dbMoveFig Figure TheCellView ( list ( list 0 ( difference 0 BoxBottom ) ) \"R0\" 1.0 ) ) )", GetPrintStateFromEnumClientData( pData ) ) ;

  PrintCodeLine( "( foreach PinList PinListList ( foreach Pin PinList ( dbMoveFig ( getq Pin fig ) TheCellView ( list ( list 0 ( difference 0 BoxBottom ) ) \"R0\" 1.0 ) ) ) )", GetPrintStateFromEnumClientData( pData ) ) ;
  */
  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( ") ", GetPrintStateFromEnumClientData( pData ) );
  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( ")", GetPrintStateFromEnumClientData( pData ) );
  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( ") )", GetPrintStateFromEnumClientData( pData ) );
  PrintCodeLine( "( dbSave TheCellView )", GetPrintStateFromEnumClientData( pData ) );
  PrintCodeLine( "TheCellView", GetPrintStateFromEnumClientData( pData ) );
  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( " ) ", GetPrintStateFromEnumClientData( pData ) );
  UnIndentPrintState( GetPrintStateFromEnumClientData( pData ) ) ;
  PrintCodeLine( " ) ", GetPrintStateFromEnumClientData( pData ) );
}

static void PrintBlockHeader( export_skill_layout_state* pState ) {
  Pexport_skill_print_state pPrintState;

  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  PrintCodeLine( ";;Skill file generated by auto to import a cell and all", pPrintState ) ;
  PrintCodeLine( ";; of its blocks in to cadence.", pPrintState );
 
}

static void PrintBlockFooter( export_skill_layout_state* pState ) {
  Pexport_skill_print_state pPrintState;

  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  UnIndentPrintState( pPrintState ) ;
  PrintCodeLine( ")", pPrintState ) ;

  PrintCodeLine( "( list", pPrintState );
  IndentPrintState( pPrintState ) ;
  PrintCodeLine( "StrutList", pPrintState );
  PrintCodeLine( "TopList", pPrintState );
  PrintCodeLine( "BottomList", pPrintState ) ;
  PrintCodeLine( "VerticalList", pPrintState );
  PrintCodeLine( "LeftList", pPrintState );
  PrintCodeLine( "RightList", pPrintState );
  PrintCodeLine( "InPlaceList", pPrintState );
  PrintCodeLine( "YPitch", pPrintState ) ;
  PrintCodeLine( "SubCells", pPrintState ) ;
  PrintCodeLine( "nil", pPrintState );
  PrintCodeLine( "nil", pPrintState );
  PrintCodeLine( "BoundBloat", pPrintState ) ;
  UnIndentPrintState( pPrintState ) ;
  PrintCodeLine( ")", pPrintState ) ;

  UnIndentPrintState( pPrintState ) ;
  PrintCodeLine( ")", pPrintState ) ;

  UnIndentPrintState( pPrintState ) ;
  PrintCodeLine( ")", pPrintState ) ;

  UnIndentPrintState( pPrintState ) ;
  PrintCodeLine( ")", pPrintState ) ;
}

static char* MakeSafeCellName( const char* Source ){
  char* ret = ( char * ) leak_malloc( strlen( Source ) * 4 ) ;
  
  if ( ret != NULL ) {
    const char* currChar;
    char* currDest;

    currChar = Source;
    
    currDest = ret;

    while ( ( *currChar != '.' )  && ( *currChar != '\0' ) ) {
      if ( *currChar == '(' ) {
	*currDest = '%';
	++currDest;
	*currDest = '4';
	++currDest;
	*currDest = '0';
	++currDest;
      }
      else if ( *currChar == ')' ) {
	*currDest = '%';
	++currDest;
	*currDest = '4';
	++currDest;
	*currDest = '1';
	++currDest;
      }
      else if ( *currChar == ',' ) {
	*currDest = '%';
	++currDest;
	*currDest = '4';
	++currDest;
	*currDest = '4';
	++currDest;
      }
      else{
	*currDest = *currChar ;
	++currDest;
      }
      ++currChar;
    }
    *currDest = '\0';
  }
  return ret;
}


bool export_skill_init_state( export_skill_layout_state* pStateToInit,
			      const char* FileName,
			      LIST* blocks,
			      LIST* nodes,
			      LIST* subcells,
			      int YPitch ) {
  bool ret = false ;
  FILE* TheFile;

  assert( pStateToInit != NULL );
  assert( FileName != NULL );

  TheFile = fopen( FileName, "w+" ) ;

  if ( TheFile != NULL ) {

    InitPrintState( GetPrintStateFromExportSkillState( pStateToInit ), TheFile ) ;

    pStateToInit->CellName = MakeSafeCellName( FileName ) ;
    if ( pStateToInit->CellName != NULL ) {

      pStateToInit->m_NumStrongPinsUsed = 0 ;
      pStateToInit->m_NumStrongPinsAllocated = 128 ;
      
      pStateToInit->m_pStrongPins = ( export_skill_pin*) 
	leak_malloc( ( pStateToInit->m_NumStrongPinsAllocated ) * sizeof ( export_skill_pin ) ) ;

      if ( pStateToInit->m_pStrongPins != NULL ) {

	pStateToInit->m_NumWeakPinsUsed = 0 ;
	pStateToInit->m_NumWeakPinsAllocated = 128 ;
	
	pStateToInit->m_pWeakPins = ( export_skill_pin*) 
	  leak_malloc( ( pStateToInit->m_NumWeakPinsAllocated ) * sizeof ( export_skill_pin ) ) ;

	if ( pStateToInit->m_pWeakPins != NULL ) {
	
	  assert( blocks != NULL );
	  assert( nodes != NULL );
      
	  pStateToInit->blocks     = blocks;
	  pStateToInit->nodes      = nodes;
	  pStateToInit->subcells   = subcells;
	  pStateToInit->m_CurrBlockNum = 0;
	  
	  pStateToInit->m_endblocks_called = 0 ;
	  
	  if ( YPitch > 0 ) {
	    pStateToInit->m_YPitch = YPitch ;
	  }
	  else{
	    pStateToInit->m_YPitch = 480 ;
	  }
	  ret = true ;
	  
	  PrintBlockHeader( pStateToInit ) ;
	}
      }
      else{
	fclose( TheFile ) ;
	leak_free( pStateToInit->m_pStrongPins ) ;
      }

    }
    else{
      fclose( TheFile ) ;
    }
  }
  return ret;
}

void export_skill_block_layout( export_skill_layout_state* pState,
				int blocknum ) {

  struct export_skill_layout_enum_client_data mydata;
  RealEnumerateLayoutParamsStruct EnumParams;

  assert( pState != NULL ) ;

  mydata.pExportState = pState;
  mydata.m_CurrImplicitGroupNum = 0;
  EnumParams.m_BeginFunc		=	export_skill_layout_begin_func;
  EnumParams.m_BeginSubCellsFunc	=	NULL;
  EnumParams.m_SubCellFunc		=	NULL;
  EnumParams.m_BeginPaintsFunc	        =	NULL ;
  EnumParams.m_PaintRectFunc		=	export_skill_layout_paint_func;
  EnumParams.m_BeginLabelsFunc	        =	NULL;
  EnumParams.m_LabelFunc		=	export_skill_layout_label_info_func ;
  EnumParams.m_EndFunc		        =	export_skill_layout_end_func;
  EnumParams.m_pClientData		=	&mydata;
 
  

  mydata.m_BlockName = ( char* ) leak_malloc( ( strlen( pState->CellName ) + 32 ) * 
						  sizeof( char ) ) ;

  if ( mydata.m_BlockName != NULL ) {
    safe_sprintf( mydata.m_BlockName, "%s[%d]", pState->CellName, blocknum ) ;
    RealEnumerateLayout( pState->blocks, 
			 pState->nodes, 
			 pState->subcells, 
			 blocknum, 
			 &EnumParams );

    leak_free( mydata.m_BlockName ) ;
  }

}

static void export_skill_output_subcell( export_skill_layout_state* pState,
				  const char* type,
				  const char* name,
				  int xlo,
				  int xhi,
				  int xsep,
				  int ylo,
				  int yhi,
				  int ysep,
				  TRANSFORM *pTransform ) {
  Pexport_skill_print_state pPrintState;

  char* CodeBuffer;
  
  const char* FormatStr ;
  char* SafeTypeName ;

  FormatStr = "( list \"%s\" \"%s\" ( list %d %d %d %d %d %d ( list ( list %d %d ) ( list %d %d ) ) ( list %d %d ) ) ) ; %d"; 

  assert( pState != NULL );
  
  SafeTypeName = MakeSafeCellName( type );

  if ( SafeTypeName != NULL ) {

      pPrintState = GetPrintStateFromExportSkillState( pState ) ;
      assert( pPrintState != NULL );

      CodeBuffer = leak_malloc( ( strlen( FormatStr ) + 
				  strlen( name ) + 
				  strlen( type ) +
				  32 +
				  ( 12 * 16 ) + 
				  1 ) * sizeof( char ) ) ;

      if ( CodeBuffer != NULL ) {
	  safe_sprintf( CodeBuffer, 
		   FormatStr, 
		   SafeTypeName, 
		   name,
		   xlo,
		   xhi,
		   xsep,
		   ylo,
		   yhi,
		   ysep,
		   pTransform->M[0][0],
		   pTransform->M[0][1],
		   pTransform->M[1][0],
		   pTransform->M[1][1],
		   pTransform->O[0],
		   pTransform->O[1],
		   __LINE__ ) ;
	  PrintCodeLine( CodeBuffer, pPrintState ) ;

	  leak_free( CodeBuffer );
      }
      leak_free( SafeTypeName );
  }

}


static void export_skill_subcellcallback( USE use, void* pClientData ) {
  export_skill_layout_state* pState =  ( export_skill_layout_state* ) pClientData ;
  export_skill_output_subcell( pState,
			       use.type,
			       use.name,
			       use.x0,
			       use.x1,
			       use.dx,
			       use.y0,
			       use.y1,
			       use.dy,
			       &(use.transform) );
}

static void export_skill_output_subcells( export_skill_layout_state* pState ) {
  struct export_skill_layout_enum_client_data mydata;
  Pexport_skill_print_state pPrintState;
  
  RealEnumerateLayoutParamsStruct EnumParams;

  assert( pState != NULL ) ;

  mydata.pExportState = pState;
  mydata.m_CurrImplicitGroupNum = 0;
  EnumParams.m_BeginFunc		=	NULL;
  EnumParams.m_BeginSubCellsFunc	=	NULL;
  EnumParams.m_SubCellFunc		=	export_skill_subcellcallback;
  EnumParams.m_BeginPaintsFunc	        =	NULL;
  EnumParams.m_PaintRectFunc		=	NULL;
  EnumParams.m_BeginLabelsFunc	        =	NULL;
  EnumParams.m_LabelFunc		=	NULL;
  EnumParams.m_EndFunc		        =	NULL;
  EnumParams.m_pClientData		=	pState;
  


  pPrintState = GetPrintStateFromExportSkillState( pState ) ;

  PrintCodeLine( "( SubCells", pPrintState ) ;
  
  IndentPrintState( pPrintState );

  PrintCodeLine( "( list", pPrintState ) ;

  IndentPrintState( pPrintState );

  RealEnumerateLayout( pState->blocks, 
		       pState->nodes, 
		       pState->subcells, 
		       -1, 
		       &EnumParams );

  UnIndentPrintState( pPrintState );
  PrintCodeLine( ")", pPrintState );

  UnIndentPrintState( pPrintState );
  PrintCodeLine( ")", pPrintState );
}

void export_skill_end_blocks( export_skill_layout_state* pState ) {
  unsigned long currblock;
  char CodeBuff[STRMAX];
  Pexport_skill_print_state pPrintState;

  pState->m_endblocks_called = 1;
  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  PrintCodeLine( "( defun ImportCurrentCell ( BlockLibHandle )",
		 pPrintState );
  IndentPrintState( pPrintState ) ;

  PrintCodeLine( "( let", pPrintState ) ;
  IndentPrintState( pPrintState ) ;
  PrintCodeLine( "(", pPrintState ) ;
  IndentPrintState( pPrintState ) ;
  

  PrintCodeLine( "StrutList", pPrintState );
  PrintCodeLine( "TopList", pPrintState );
  PrintCodeLine( "BottomList", pPrintState ) ;
  PrintCodeLine( "VerticalList", pPrintState );
  PrintCodeLine( "LeftList", pPrintState );
  PrintCodeLine( "RightList", pPrintState );
  PrintCodeLine( "InPlaceList", pPrintState );
  safe_sprintf( CodeBuff, "( YPitch %d ) ; %d ", pState->m_YPitch, __LINE__ ) ;
  PrintCodeLine( CodeBuff, pPrintState ) ;
  export_skill_output_subcells( pState );
  {
    double *pBloatParam = get_parm( "boundbloat" ) ;
    if ( pBloatParam != NULL ) {
      safe_sprintf( CodeBuff, "( BoundBloat %f ) ; %d",*pBloatParam, __LINE__  );
    }
    else{
      strcpy( CodeBuff, "( BoundBloat nil )" );
    }
    PrintCodeLine( CodeBuff, pPrintState );
  }

  UnIndentPrintState( pPrintState ) ;
  PrintCodeLine( ")", pPrintState );

  

  PrintCodeLine( "( list", pPrintState ) ;

  IndentPrintState( pPrintState ) ;
  PrintCodeLine( "( list", pPrintState ) ;
  IndentPrintState( pPrintState ) ;
  
  for( currblock=0 ; currblock < pState->m_CurrBlockNum ; currblock ++ ) {
    safe_sprintf( CodeBuff, "( unless ( null BlockLibHandle ) ( Block_%lu_Func BlockLibHandle ) ) ; %d", currblock, __LINE__ ) ;
    PrintCodeLine( CodeBuff, pPrintState ) ;
  }

}

void export_skill_deinit_state( export_skill_layout_state* pStateToDeInit ) {
  assert( pStateToDeInit != NULL );
  assert( pStateToDeInit->CellName != NULL );
  assert( pStateToDeInit->m_pStrongPins != NULL );
  assert( pStateToDeInit->m_pWeakPins != NULL );


  if ( ! ( pStateToDeInit->m_endblocks_called ) ) {
    export_skill_end_blocks( pStateToDeInit ) ;
  }
  PrintBlockFooter( pStateToDeInit ) ;
  
  DeInitPrintState( GetPrintStateFromExportSkillState( pStateToDeInit ) ) ;
  
  leak_free( pStateToDeInit->CellName );


  while( pStateToDeInit->m_NumWeakPinsUsed > 0 ) {
    export_skill_state_remove_weak_pin( pStateToDeInit, 0 ) ;
  }
  leak_free( pStateToDeInit->m_pWeakPins ) ;

  while( pStateToDeInit->m_NumStrongPinsUsed > 0 ) {
    export_skill_state_remove_strong_pin( pStateToDeInit, 0 ) ;
  }
  leak_free( pStateToDeInit->m_pStrongPins ) ;

  pStateToDeInit->CellName = NULL ;

}

void export_skill_output_strut_pin( export_skill_layout_state* pState,
				    const char* name ) {
  Pexport_skill_print_state pPrintState;

  char* CodeBuffer;

  const char* FormatStr = "( setq StrutList ( cons ( list \"%s\" ) StrutList ) ) ; %d"; 

  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  CodeBuffer = leak_malloc( ( strlen( FormatStr ) + strlen( name ) + 32 + 1 ) * sizeof( char ) ) ;

  if ( CodeBuffer != NULL ) {
    safe_sprintf( CodeBuffer, FormatStr, name, __LINE__ ) ;
    PrintCodeLine( "( let ()", pPrintState ) ;
    IndentPrintState( pPrintState ) ;
    PrintCodeLine( CodeBuffer, pPrintState ) ;
    PrintCodeLine( "nil", pPrintState ) ;
    UnIndentPrintState( pPrintState ) ;
    PrintCodeLine( ")", pPrintState ) ;

    leak_free( CodeBuffer );
  }

}

void export_skill_output_top_pin( export_skill_layout_state* pState,
				  const char* name ) {
  Pexport_skill_print_state pPrintState;

  char* CodeBuffer;

  const char* FormatStr = "( setq TopList ( cons ( list \"%s\" ) TopList ) ) ; %d"; 

  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  CodeBuffer = leak_malloc( ( strlen( FormatStr ) + strlen( name ) + 32 + 1 ) * sizeof( char ) ) ;

  if ( CodeBuffer != NULL ) {
    safe_sprintf( CodeBuffer, FormatStr, name , __LINE__ ) ;
    PrintCodeLine( "( let ()", pPrintState ) ;
    IndentPrintState( pPrintState ) ;
    PrintCodeLine( CodeBuffer, pPrintState ) ;
    PrintCodeLine( "nil", pPrintState ) ;
    UnIndentPrintState( pPrintState ) ;
    PrintCodeLine( ")", pPrintState ) ;

    leak_free( CodeBuffer );
  }

}

void export_skill_output_bottom_pin( export_skill_layout_state* pState,
				     const char* name ) {
  Pexport_skill_print_state pPrintState;

  char* CodeBuffer;

  const char* FormatStr = "( setq BottomList ( cons ( list \"%s\" ) BottomList ) ) ; %d"; 

  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  CodeBuffer = leak_malloc( ( strlen( FormatStr ) + strlen( name ) + 32 + 1 ) * sizeof( char ) ) ;

  if ( CodeBuffer != NULL ) {
    safe_sprintf( CodeBuffer, FormatStr, name, __LINE__ ) ;
    PrintCodeLine( "( let ()", pPrintState ) ;
    IndentPrintState( pPrintState ) ;
    PrintCodeLine( CodeBuffer, pPrintState ) ;
    PrintCodeLine( "nil", pPrintState ) ;
    UnIndentPrintState( pPrintState ) ;
    PrintCodeLine( ")", pPrintState ) ;

    leak_free( CodeBuffer );
  }

}

void export_skill_output_vertical_pin( export_skill_layout_state* pState,
				       const char* name )  {
  Pexport_skill_print_state pPrintState;

  char* CodeBuffer;

  const char* FormatStr = "( setq VerticalList ( cons ( list \"%s\" ) VerticalList ) ) ; %d"; 

  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  CodeBuffer = leak_malloc( ( strlen( FormatStr ) + strlen( name ) + 32 + 1 ) * sizeof( char ) ) ;

  if ( CodeBuffer != NULL ) {
    safe_sprintf( CodeBuffer, FormatStr, name, __LINE__ ) ;
    PrintCodeLine( "( let ()", pPrintState ) ;
    IndentPrintState( pPrintState ) ;
    PrintCodeLine( CodeBuffer, pPrintState ) ;
    PrintCodeLine( "nil", pPrintState ) ;
    UnIndentPrintState( pPrintState ) ;
    PrintCodeLine( ")", pPrintState ) ;

    leak_free( CodeBuffer );
  }

}

void export_skill_output_left_pin( export_skill_layout_state* pState,
				   const char* name,
				   int pos ) {
  Pexport_skill_print_state pPrintState;

  char* CodeBuffer;
  
  const char* FormatStr ;

  if ( pos > 0 ) {
    FormatStr = "( setq LeftList ( cons ( list \"%s\" %d ) LeftList ) ) ; %d"; 
  }
  else {
    FormatStr = "( setq LeftList ( cons ( list \"%s\" ) LeftList ) ) ; %d" ;
  }

  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  CodeBuffer = leak_malloc( ( strlen( FormatStr ) + strlen( name ) + 32 + 1 ) * sizeof( char ) ) ;

  if ( CodeBuffer != NULL ) {
    safe_sprintf( CodeBuffer, FormatStr, name, pos, __LINE__ ) ;
    PrintCodeLine( "( let ()", pPrintState ) ;
    IndentPrintState( pPrintState ) ;
    PrintCodeLine( CodeBuffer, pPrintState ) ;
    PrintCodeLine( "nil", pPrintState ) ;
    UnIndentPrintState( pPrintState ) ;
    PrintCodeLine( ")", pPrintState ) ;

    leak_free( CodeBuffer );
  }

}

void export_skill_output_right_pin( export_skill_layout_state* pState,
				    const char* name,
				    int pos ) {
  Pexport_skill_print_state pPrintState;

  char* CodeBuffer;
  
  const char* FormatStr ;

  if ( pos > 0 ) {
    FormatStr = "( setq RightList ( cons ( list \"%s\" %d ) RightList ) ) ; %d"; 
  }
  else {
    FormatStr = "( setq RightList ( cons ( list \"%s\" ) RightList ) ) ; %d " ;
  }

  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  CodeBuffer = leak_malloc( ( strlen( FormatStr ) + strlen( name ) + 16 + 1 ) * sizeof( char ) ) ;

  if ( CodeBuffer != NULL ) {
    safe_sprintf( CodeBuffer, FormatStr, name, pos, __LINE__ ) ;
    PrintCodeLine( "( let ()", pPrintState ) ;
    IndentPrintState( pPrintState ) ;
    PrintCodeLine( CodeBuffer, pPrintState ) ;
    PrintCodeLine( "nil", pPrintState ) ;
    UnIndentPrintState( pPrintState ) ;
    PrintCodeLine( ")", pPrintState ) ;

    leak_free( CodeBuffer );
  }

}

void export_skill_output_pin( export_skill_layout_state* pState,
			     const char* name,
				    int pos ) {
  export_skill_output_strut_pin( pState, name ) ;
}

void export_skill_output_inplace_pin( export_skill_layout_state* pState, const char* name ) {
  Pexport_skill_print_state pPrintState;
  
  char* CodeBuffer;
  
  const char* FormatStr ;

  FormatStr = "( setq InPlaceList ( cons ( list \"%s\" ) InPlaceList ) ) ; %d"; 
 

  assert( pState != NULL );
  
  pPrintState = GetPrintStateFromExportSkillState( pState ) ;
  assert( pPrintState != NULL );

  CodeBuffer = leak_malloc( ( strlen( FormatStr ) + strlen( name ) + 16 + 1 ) * sizeof( char ) ) ;

  if ( CodeBuffer != NULL ) {
    safe_sprintf( CodeBuffer, FormatStr, name, __LINE__ ) ;
    PrintCodeLine( "( let ()", pPrintState ) ;
    IndentPrintState( pPrintState ) ;
    PrintCodeLine( CodeBuffer, pPrintState ) ;
    PrintCodeLine( "nil", pPrintState ) ;
    UnIndentPrintState( pPrintState ) ;
    PrintCodeLine( ")", pPrintState ) ;

    leak_free( CodeBuffer );
  }
}



void export_skill_layout( const char *filename, 
			 LIST *blocks, 
			 LIST *nodes, 
			 LIST *subcells,
			 int blocknum)
{
  export_skill_layout_state mystate;

  if ( export_skill_init_state( &mystate,
				filename,
				blocks,
				nodes,
				subcells, 480 ) ) {
    if ( ( blocks->max != 0 ) ) {
      export_skill_block_layout( &mystate, blocknum ) ;
    }
    export_skill_deinit_state( &mystate ) ;
  }
}
