#ifndef EXPORT_SKILL_H
#define EXPORT_SKILL_H

#include "auto.h"

#include <stdbool.h>

struct _export_skill_print_state {
  unsigned long m_Indent;
  FILE* m_File;
} ;

typedef struct _export_skill_print_state export_skill_print_state ;
typedef export_skill_print_state* Pexport_skill_print_state ;

struct _export_skill_pin {
  char* m_LayerName ;
  double x0;
  double y0;
  double x1;
  double y1;
  
  char* m_NetName ;
  long m_GroupNum ;
};

typedef struct _export_skill_pin export_skill_pin ;

struct _export_skill_layout_state {
  export_skill_print_state m_PrintState;
  char* CellName ;
  LIST* blocks ;
  LIST* nodes ;
  LIST* subcells ;

  unsigned long m_CurrBlockNum ;

  export_skill_pin* m_pWeakPins ;
  unsigned long m_NumWeakPinsAllocated ;
  unsigned long m_NumWeakPinsUsed ;

  export_skill_pin* m_pStrongPins ;
  unsigned long m_NumStrongPinsAllocated ;
  unsigned long m_NumStrongPinsUsed ;

  int m_YPitch ;

  char m_endblocks_called ;
  
} ;

typedef struct _export_skill_layout_state export_skill_layout_state ;


bool export_skill_init_state( export_skill_layout_state* pStateToInit,
			      const char* FileName,
			      LIST* blocks,
			      LIST* nodes,
			      LIST* subcells,
			      int YPitch );

void export_skill_block_layout( export_skill_layout_state* pState,
				int blocknum );

void export_skill_end_blocks( export_skill_layout_state* pState ) ;

void export_skill_deinit_state( export_skill_layout_state* pStateToDeInit ) ;

void export_skill_output_strut_pin( export_skill_layout_state* pState,
				    const char* name ) ;

void export_skill_output_top_pin( export_skill_layout_state* pState,
				  const char* name ) ;

void export_skill_output_bottom_pin( export_skill_layout_state* pState,
				     const char* name ) ;

void export_skill_output_vertical_pin( export_skill_layout_state* pState,
				       const char* name ) ;

void export_skill_output_left_pin( export_skill_layout_state* pState,
				   const char* name,
				   int pos ) ;

void export_skill_output_right_pin( export_skill_layout_state* pState,
				    const char* name,
				    int pos ) ;

void export_skill_output_pin( export_skill_layout_state* pState,
			      const char* name,
			      int pos ) ;

void export_skill_output_inplace_pin( export_skill_layout_state* pState, const char* name );

/*void export_skill_output_ypitch( export_skill_layout_state* pState,
  int ypitch ) ;*/

void export_skill_layout( const char* FileName,
			  LIST* blocks,
			  LIST* nodes,
			  LIST* subcells,
			  int blocknum );



#endif
