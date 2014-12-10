#ifndef ENUMERATE_LAYOUT_H
#define ENUMERATE_LAYOUT_H
/*
 * Pointer to a function that is called by EnumerateLayout before any elements of the
 * layout are enumerated.
 * The pClientData parameter is the pointer that you specied in the
 * m_pClientData member of the DrawParamsStruct you passed to EnumerateLayout.
 */
typedef void (*EnumerateLayoutBeginFunc)( void* pClientData ) ;
/*
 * Pointer to a function that is called by EnumerateLayout immediately before the
 * first call to your SubCellFunc.
 * The pClientData parameter is the pointer that you specied in the
 * m_pClientData member of the DrawParamsStruct you passed to EnumerateLayout.
 */
typedef void (*EnumerateLayoutBeginSubCellsFunc)( void* pClientData );
/*
 * Pointer to a function that is called by EnumerateLayout for each sub cell in the layout.
 * The pClientData parameter is the pointer that you specied in the
 * m_pClientData member of the DrawParamsStruct you passed to EnumerateLayout.
 */
typedef void (*EnumerateLayoutSubCellFunc)( USE use, void* pClientData );
/*
 * Pointer to a function that is called by EnumerateLayout immediately before the
 * first call to your PaintRectFunc.
 * The pClientData parameter is the pointer that you specied in the
 * m_pClientData member of the DrawParamsStruct you passed to EnumerateLayout.
 */
typedef void (*EnumerateLayoutBeginPaintsFunc)( void* pClientData );
/*
 * Pointer to a function that is called by EnumerateLayout for each painted rectangle in the layout.
 * The Paint parameter is the paint to be used to paint the rectangle.
 * The RectToPaint parameter is the rectangle to paint.
 * The pClientData parameter is the pointer that you specied in the
 * m_pClientData member of the DrawParamsStruct you passed to EnumerateLayout.
 */
typedef void (*EnumerateLayoutPaintRectFunc)( PAINT Paint, RECT RectToPaint, void* pClientData );
/*
 * Pointer to a function that is called by EnumerateLayout immediately before the
 * first call to your LabelFunc.
 * The pClientData parameter is the pointer that you specied in the
 * m_pClientData member of the DrawParamsStruct you passed to EnumerateLayout.
 */
typedef void (*EnumerateLayoutBeginLabelsFunc)( void* pClientData );
/*
 * Pointer to a function that is called by EnumerateLayout for each label in the layout.
 * The pClientData parameter is the pointer that you specied in the
 * m_pClientData member of the DrawParamsStruct you passed to EnumerateLayout.
 */
typedef void (*EnumerateLayoutOldLabelInfoFunc)( PAINT LabelPaint, RECT LabelRect,
						 char* LabelName, void* pClientData );

/*
 * Pointer to a function that is called by EnumerateLayout for each label in the layout.
 * The pClientData parameter is the pointer that you specied in the
 * m_pClientData member of the DrawParamsStruct you passed to EnumerateLayout.
 */
typedef void (*EnumerateLayoutLabelInfoFunc)( PAINT LabelPaint, RECT LabelRect,
					      char* LabelName,
					      char* ExtraLabelName,
					      void* pClientData );

/*
 * Pointer to a function that is called by EnumerateLayout after all the elements of the
 * layout are enumerated.
 * The pClientData parameter is the pointer that you specied in the
 * m_pClientData member of the DrawParamsStruct you passed to EnumerateLayout.
 */
typedef void (*EnumerateLayoutEndFunc)( void* pCliendData ) ; 

typedef struct _RealEnumerateLayoutParamsStruct {
  EnumerateLayoutBeginFunc                      m_BeginFunc;
  EnumerateLayoutBeginSubCellsFunc              m_BeginSubCellsFunc;
  EnumerateLayoutSubCellFunc                    m_SubCellFunc;
  EnumerateLayoutBeginPaintsFunc                m_BeginPaintsFunc;
  EnumerateLayoutPaintRectFunc                  m_PaintRectFunc;
  EnumerateLayoutBeginLabelsFunc                m_BeginLabelsFunc;
  EnumerateLayoutLabelInfoFunc                  m_LabelFunc;
  EnumerateLayoutEndFunc                        m_EndFunc;
  void*                                         m_pClientData;
} RealEnumerateLayoutParamsStruct;

typedef struct _EnumerateLayoutParamsStruct {
  EnumerateLayoutBeginFunc                      m_BeginFunc;
  EnumerateLayoutBeginSubCellsFunc              m_BeginSubCellsFunc;
  EnumerateLayoutSubCellFunc                    m_SubCellFunc;
  EnumerateLayoutBeginPaintsFunc                m_BeginPaintsFunc;
  EnumerateLayoutPaintRectFunc                  m_PaintRectFunc;
  EnumerateLayoutBeginLabelsFunc                m_BeginLabelsFunc;
  EnumerateLayoutOldLabelInfoFunc               m_LabelFunc;
  EnumerateLayoutEndFunc                        m_EndFunc;
  void*                                         m_pClientData;
} EnumerateLayoutParamsStruct;


void RealEnumerateLayout( LIST* blocks, LIST* nodes, LIST* subcells, 
			  int blocknum, const RealEnumerateLayoutParamsStruct* pEnumParams);

void EnumerateLayout( LIST* blocks, LIST* nodes, LIST* subcells, 
		      int blocknum, const EnumerateLayoutParamsStruct* pEnumParams);

#endif
