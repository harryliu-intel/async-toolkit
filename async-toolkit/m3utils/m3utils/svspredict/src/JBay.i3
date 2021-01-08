INTERFACE JBay;
IMPORT N7Tech;
IMPORT Power;
IMPORT Wr;

(* what's below is for JBay B0 *)

PROCEDURE SetProgram(VAR params                  : Power.Params;
                     VAR Trunc                   : LONGREAL);
  
CONST Brand = "JBay";

CONST FinCounts = ARRAY N7Tech.Transistor OF LONGREAL {
  6.153d09,
  1.918d10,
  1.163d11
  };

CONST DevCounts = ARRAY N7Tech.Transistor OF LONGREAL {
  2.338d09,
  8.408d09,
  5.758d10
  };

CONST GoxArea = ARRAY N7Tech.Transistor OF LONGREAL {
  (* in square microns *)
  6.475d6,
  1.985d7,
  1.399d8
  };

CONST PowToContact = -50.0d-3;
  (* assumed delta between power and contact voltage, per Ram's 
     voltage stack, updated *)

PROCEDURE EvalSpecialCases(wr : Wr.T;
                           READONLY p : Power.Params;
                           medianSigma, worstSigma : LONGREAL);

PROCEDURE EvalCustomerMaxSpec(READONLY p : Power.Params;
                              worstSigma : LONGREAL) : LONGREAL;

END JBay.
