INTERFACE ModelStagesC;
IMPORT Ctypes;
IMPORT Byte;

(* some ugly C-matching declarations here

   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   XXX                                         XXX
   XXX             HERE BE DRAGONS !!!!        XXX
   XXX                                         XXX
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

 *)

TYPE
  Info = RECORD (* model_stages.h : typedef ... model_stages_info_t *)
    top_map_name                      : Ctypes.char_star;
    stage_name                        : Ctypes.char_star;
    stage_func                        : Func;
    r_size, w_size, in_size, out_size : Ctypes.unsigned_long;
    next                              : UNTRACED REF Info;
  END;

  Func = PROCEDURE(r, w, in, out    : ADDRESS;
                   rx_data, tx_data : UNTRACED REF Varchar);
  (* model_stages.h : typedef ... model_stages_voidstar_func_t *)

  Varchar = RECORD (* varchar.h : typedef ... varchar_t *)
    data   : ADDRESS;
    length : Ctypes.unsigned_int;
  END;

<*EXTERNAL model_stages*>
VAR model_stages : UNTRACED REF Info;
  
CONST Brand = "ModelStagesC";

PROCEDURE Lookup(top_map_name, stage_name : TEXT; VAR info : Info) : BOOLEAN;

PROCEDURE CallStage(READONLY info   : Info;
                    r               : ADDRESS;
                    w               : ADDRESS;
                    READONLY in     : ARRAY OF Byte.T;
                    VAR      out    : REF ARRAY OF Byte.T;
                    READONLY rxData : ARRAY OF Byte.T;
                    VAR      txData : REF ARRAY OF Byte.T)
  RAISES { Error } ;

EXCEPTION Error(TEXT);
      
END ModelStagesC.
