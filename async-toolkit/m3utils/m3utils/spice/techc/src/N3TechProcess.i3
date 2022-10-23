INTERFACE N3TechProcess;

IMPORT TechProcess;
IMPORT N5TechProcess;

FROM TechProcess IMPORT TechUnknownCellPaths, TechUnknownCellNames;

CONST
  P = TechProcess.T {
  tranSufxs := N5TechProcess.P.tranSufxs,

  tranSize :="l=3n nfin=2 ppitch=0",

  hspiceModel :="cln3_1d2_sp_v1d0_2p2_usage.l",


  hspiceModelRoot :="/p/tech1/n3/tech-release/v1.0.10/models/1P18M_1X_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_5Y_hvhvh_2Yy2Yx1R1U_thin_curdl/hspice",

  cornNames :=N5TechProcess.P.cornNames,

  cellPaths := TechUnknownCellPaths,

  cellNames := TechUnknownCellNames,

  plugText :="vcc vssx"

      };

CONST
  Brand = "N3TechProcess";

END N3TechProcess.
                                


      
