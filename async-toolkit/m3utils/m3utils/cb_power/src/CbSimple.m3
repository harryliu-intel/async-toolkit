MODULE CbSimple;
FROM Fmt IMPORT Int, F, FN; IMPORT Fmt;
IMPORT Debug;
IMPORT Wr;
IMPORT FileWr;

CONST LR = Fmt.LongReal;

PROCEDURE Multiplier(vratio, fratio : LONGREAL) : LONGREAL =
  BEGIN
    RETURN vratio * vratio * fratio
  END Multiplier;

PROCEDURE DoIt() =
  (* model is of a TF3-class switch with a PPS clock that can be
     swept for various minimum average packet sizes.

     3 models: 

     1 is single power domain

     2 is we split the PPS clock domain into its own p.d.

     3 is we split also the SRAMs into their own p.d.

     estimate power for each 
  *)
  CONST
    (* these are baseline power figures.
       somewhat arbitrarily chosen to make #s at 300B agree with
       Jon's power spreadsheet (744W split Vdd, 777W single p.d.)
    *)
    Pppsmax       =  348.0d0;  (* the part we scale completely *)
    Pserdesconst  =  110.0d0;  (* the SERDES : fixed power *)
    Prestmax      =  470.0d0;  (* the rest: voltage-scaled till Vminrest *)

    Vminsram      = 0.600d0; (* assumed SRAM Vmin *)
    Vminrest      = 0.750d0; (* min voltage for timing closure of non-PPS
                                logic in chip *)

    Fppsmax       = 1.50d0;  (* in GHz *)
    Apsmin        = 247;     (* in bytes *)
    Vmax          = 0.80d0;  (* in volts, assumed voltage at which we do
                                Apsmin @ Fppsmax *)
    
  VAR
    Vf, Ppsclock : LONGREAL;
    wr := FileWr.Open("simple.dat");
    
  PROCEDURE V(f : LONGREAL) : LONGREAL =
    BEGIN
      (* linear, based on 0.80V @ 1.5GHz, 0.70V @ 1.25GHz *)
      RETURN 0.55d0 / 1.5d0 * f + 0.25d0
    END V;
    
  PROCEDURE Ppps() : LONGREAL =
    (* baseline scenario, PPS domain has its own voltage, but 
       subject to SRAM Vmin *)
    BEGIN
      WITH vpps = MAX(Vminsram, Vf),
           vratio = vpps / Vmax,
           fratio = Ppsclock / Fppsmax,
           p      = Pppsmax * Multiplier(vratio,fratio) DO
        Wr.PutText(wr, F(",%s", LR(Ppsclock)));
        Wr.PutText(wr, F(",%s", LR(vpps)));
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END Ppps;

  PROCEDURE PppsSingle() : LONGREAL =
    (* worst case scenario, all domains same voltage *)
    BEGIN
      WITH vpps = MAX(Vminrest, MAX(Vminsram, Vf)),
           vratio = vpps / Vmax,
           fratio = Ppsclock / Fppsmax,
           p      = Pppsmax * Multiplier(vratio,fratio) DO
        Wr.PutText(wr, F(",%s", LR(Ppsclock)));
        Wr.PutText(wr, F(",%s", LR(vpps)));
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END PppsSingle;

  PROCEDURE PppsLow() : LONGREAL =
    (* best case scenario, split PPS voltage domain plus 
       split SRAM voltage domain *)
    BEGIN
      WITH vpps = Vf,
           vratio = vpps / Vmax,
           fratio = Ppsclock / Fppsmax,
           p      = Pppsmax * Multiplier(vratio,fratio) DO
        Wr.PutText(wr, F(",%s", LR(Ppsclock)));
        Wr.PutText(wr, F(",%s", LR(vpps)));
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END PppsLow;

  PROCEDURE Pserdes() : LONGREAL =
    BEGIN
      WITH p = Pserdesconst DO
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END Pserdes;

  PROCEDURE Prest() : LONGREAL =
    BEGIN
      WITH vrest = MAX(Vminrest, Vf),
           vratio = vrest / Vmax,
           p      = Prestmax * Multiplier(vratio, 1.0d0) DO
        Wr.PutText(wr, F(",%s", LR(vrest)));
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END Prest;
    
  BEGIN
    Wr.PutText(wr, "aps, fpps, vpps, ppps, fppsSingle, vppsSingle, pppsSingle, fppsLow, vppsLow, pppsLow, pserdes, vrest, prest, ptot, ptotSingle, ptotLow\n");

    (*

       below we sweep APS from 247 to 622 bytes
       
       why 622 bytes, you ask?

       because 595 MHz is the single-port packet rate at 84B per packet
       (64B data + 20B IFG)

       TF3 can't do this because it can't service one single port on
       back-to-back cycles, but would be theoretically possible with
       architecture changes

    *)


    FOR aps := 247 TO 622 (* 622 -> 595 MHz *) DO
      Wr.PutText(wr, F("%s", Int(aps)));
      
      Ppsclock := Fppsmax * FLOAT(Apsmin,LONGREAL)/FLOAT(aps,LONGREAL);
      Vf := V(Ppsclock);
      WITH p1 = Ppps(),
           p1s= PppsSingle(),
           p1l= PppsLow(),
           p2 = Pserdes(),
           p3 = Prest(),
           
           sum       = p1 + p2 + p3,  (* baseline assumption, SRAM Vmin limit *)
           sumSingle = p1s + p2 + p3, (* single power domain calc *)
           sumLow    = p1l + p2 + p3  (* assume NOT SRAM limited *)
       DO 
        Debug.Out(FN("%s %s %s %s %s %s %s",
                     ARRAY OF TEXT {
                    LR(FLOAT(aps,LONGREAL)),
                    LR(Ppsclock),
                    LR(Vf),
                    LR(p1),
                    LR(p2),
                    LR(p3),
                    LR(sum)} ));
        Wr.PutText(wr, F(",%s", LR(sum)));
        Wr.PutText(wr, F(",%s", LR(sumSingle)));
        Wr.PutText(wr, F(",%s", LR(sumLow)));
        Wr.PutChar(wr, '\n')
      END
    END;
    Wr.Close(wr)
  END DoIt;

BEGIN END CbSimple.
