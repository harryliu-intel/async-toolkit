INTERFACE StageModelServer;
IMPORT ModelServer;
IMPORT Pathname;
IMPORT ServerPacket AS Pkt;
IMPORT ModelServerSuper;
IMPORT UpdaterFactory;

(********************************************************************** 
 *
 *

   White Model Model Server -- for single-stage model servers 

   Main module, largely follows model_server.c from IES system in terms
   of interface.

   Author : Mika Nystrom <mika.nystroem@intel.com>
   January, 2019
                                                                      *
                                                                      *
 **********************************************************************)


CONST DefInfoFileName = ModelServer.DefInfoFileName;

TYPE
  T <: Public;

  Public = ModelServerSuper.T OBJECT
    topMapName : TEXT;
    stageName  : TEXT;
  METHODS
    init(stageName    : TEXT;
         factory      : UpdaterFactory.T;
         infoPath     : Pathname.T       := ".";
         quitOnLastClientExit            := FALSE;
         infoFileName : Pathname.T       := DefInfoFileName) : T;
    (* initialize object.  infoPath is a directory path where
       the host:port file is created with the filename given below
       by InfoFileName *)

    (****** abstract methods, implement in child type: ******)
    
  END;

CONST Brand = "StageModelServer";

END StageModelServer.
