#include "auto.h"
#include "enumlayout.h"
struct write_layout_enum_client_data{
	FILE* m_pFile;
	RECT m_BoundingBox;
	char m_FirstPaint;
	PaintType m_LastPaint;
};

typedef struct write_layout_enum_client_data* Pwlecd;

static void write_layout_begin_func( void* pClientData ){
	Pwlecd pData = ( Pwlecd ) pClientData ;	
	fprintf( pData->m_pFile, "magic\ntech scmos\ntimestamp 0\n");
}

static void write_layout_subcell_func( USE use, void* pClientData ) {
	Pwlecd pData = ( Pwlecd ) pClientData ;	
	print_use( pData->m_pFile, use, 0 ) ;
}

static void write_layout_begin_paints_func( void *pClientData ){
	Pwlecd pData = ( Pwlecd ) pClientData ;	
	pData->m_BoundingBox.x0 = pData->m_BoundingBox.y0 = 1e10;
	pData->m_BoundingBox.x1 = pData->m_BoundingBox.y1 = -1e10;
	pData->m_FirstPaint = 1;
}

static void write_layout_paint_func( PAINT Paint, RECT RectToPaint, void* pClientData ) {
	Pwlecd pData = ( Pwlecd ) pClientData ;	

	/* small optimization to make output more human readable */
	if ( ( pData->m_FirstPaint == 1 ) || ( pData->m_LastPaint != RectToPaint.paint ) ) {
		fprintf( pData->m_pFile, "<< %s >>\n", Paint.name );
		pData->m_FirstPaint = 0;
		pData->m_LastPaint = RectToPaint.paint;
	}
	fprintf( pData->m_pFile, "rect %.0f %.0f %.0f %.0f\n", 
					RectToPaint.x0,RectToPaint.y0,
			   		RectToPaint.x1,RectToPaint.y1  );

	rectangle_union( & ( pData->m_BoundingBox ), &RectToPaint );
}

static void write_layout_begin_labels_func( void *pClientData ) {
	Pwlecd pData = ( Pwlecd ) pClientData ;	
	fprintf(pData->m_pFile, "<< checkpaint >>\nrect %.0f %.0f %.0f %.0f\n",
					pData->m_BoundingBox.x0,
					pData->m_BoundingBox.y0,
					pData->m_BoundingBox.x1,
					pData->m_BoundingBox.y1 );

	fprintf(pData->m_pFile, "<< labels >>\n");
}

static void write_layout_label_info_func( PAINT LabelPaint, RECT LabelRect,
				char* LabelName, void* pClientData ) {
  char* SafeName ;
  Pwlecd pData = ( Pwlecd ) pClientData ;	

  SafeName = ( char* ) leak_malloc( strlen( LabelName)+2);
  strcpy( SafeName, LabelName );
  strtok( SafeName, "|" );
  fprintf( pData->m_pFile, "rlabel %s %.0f %.0f %.0f %.0f %d %s\n", LabelPaint.name, 
	   LabelRect.x0 ,
	   LabelRect.y0 ,
	   LabelRect.x1 ,
	   LabelRect.y1 ,
	   LABEL_POSITION_NORTH,
	   SafeName );
  leak_free( SafeName );

}

static void write_layout_end_func( void *pClientData ) {
	Pwlecd pData = ( Pwlecd ) pClientData ;	
	fprintf( pData->m_pFile, "<< end >>\n");
}

void write_layout(char *filename, LIST *blocks, LIST *nodes, LIST *subcells, int blocknum)
{
	struct write_layout_enum_client_data mydata;
	EnumerateLayoutParamsStruct EnumParams;
	mydata.m_pFile = fopen( filename, "w+" );

	if ( mydata.m_pFile != NULL ) {

		EnumParams.m_BeginFunc				=	write_layout_begin_func;
		EnumParams.m_BeginSubCellsFunc		=	NULL;
		EnumParams.m_SubCellFunc			=	write_layout_subcell_func;
		EnumParams.m_BeginPaintsFunc		=	write_layout_begin_paints_func;
		EnumParams.m_PaintRectFunc			=	write_layout_paint_func;
		EnumParams.m_BeginLabelsFunc		=	write_layout_begin_labels_func;
		EnumParams.m_LabelFunc				=	write_layout_label_info_func;
		EnumParams.m_EndFunc				=	write_layout_end_func;
		EnumParams.m_pClientData			=	&mydata;

		EnumerateLayout( blocks, nodes, subcells, blocknum, &EnumParams );
		fclose( mydata.m_pFile );
	}
	else fprintf(stderr,"ERROR: can't open %s\n",filename);
}
