(* $Id: ANOVA.i3,v 1.11 2010/04/22 08:28:55 mika Exp $ *)

INTERFACE ANOVA;
IMPORT FactorialDesign, FactorialDatumList, FactorialDatum, FactorialBindings;
IMPORT IntSet, FactorialValues;
IMPORT LongRealSeq;

TYPE
  Order = [-1..LAST(CARDINAL)];

  T <: Public;


  (* an Attachment is something that is attached to a certain experimental
     result.  It can be iterated over by using overI and overV below. 

     The Attachment is attached to the ANOVA.T by means of the Attacher,
     which is passed in at initialization and called back for each
     experimental result. 

     This stuff doesn't REALLY belong here.  The way this code *should*
     be structured is that there should be an abstract view of sub-designs.
     The ANOVA.T object should be a subtype of such an abstract view.
     A view of attachments should be another subtype.

     As evidence of the desirability of such an approach, many of the methods
     below have little to do with the SINGLE RESPONSE that is almost the
     definition of an ANOVA.  Multi-valued responses or non-LONGREAL
     responses are not mentioned at all.  Many of the methods would,
     however, work just fine for such types of responses.
  *)

  Attachment = BRANDED Brand & " Attachment" OBJECT END;

  Attacher = OBJECT METHODS
    callback(READONLY d : FactorialDatum.T;
             b : FactorialBindings.T) : Attachment;
  END;

  (* Basic factorial design functions follow... *)

  Super = OBJECT METHODS
    numVars() : CARDINAL;
    numValues() : CARDINAL; (* same as size(), below? *)

    size() : CARDINAL; (* total # of samples *)
    slots() : CARDINAL; (* total # of slots/diff excitation values *)
    varyingIndices() : IntSet.T;

    typeOf(idx : CARDINAL) : FactorialValues.Type;

    levels(treatment : CARDINAL) : CARDINAL;

    isQuantitative(treatment : CARDINAL) : BOOLEAN;
    (* returns TRUE if higher-order analysis works *)

    matching(READONLY overI, overV := EmptyArrC) : CARDINAL;
    (* how many samples match the given spec *)

    popDf() : CARDINAL;
    (* degrees of freedom of whole pop *)

    iterateAllFactorValues(READONLY factors : ARRAY OF CARDINAL;
                           iter : FactorValueIterator);
    
    attachmentMapMatching(READONLY treatments, atLevel : ARRAY OF CARDINAL;
                mapper : AttachmentMapper);
  END;

  FactorValueIterator = OBJECT METHODS
    callback(READONLY factors, vals : ARRAY OF CARDINAL)
  END;
  
  AttachmentMapper = OBJECT METHODS
    map(sample : Attachment)
  END;

  (* ANOVA functions follow... *)

  Mapper = OBJECT METHODS
    map(sample : LONGREAL)
  END;

  Public = Super OBJECT METHODS
    init(design : FactorialDesign.T; 
         data : FactorialDatumList.T;
         responseExpr : TEXT;
         attacher : Attacher := NIL) : T;
    (* responseExpr is a text expression parseable with cmd.y *)

    mapMatching(READONLY treatments, atLevel : ARRAY OF CARDINAL;
                mapper : Mapper);

    errorSS() : LONGREAL; (* sum of error SS in each cell *)

    totalSS() : LONGREAL; (* sum of all SS *)

    mapMatchingSamples(READONLY treatments, atLevel : ARRAY OF CARDINAL;
                       VAR increment, S, SS : LONGREAL);
    (* for each elem. treatments[i], if level for that treatment is at
       atLevel, add 1.0d0 to increment, response to S, and response^2 to SS *)


    interactionSS(READONLY orders : ARRAY OF Order) : LONGREAL;
    (* shouldn't we have an order for "treatments"---i.e., all levels of
       variable i? 
       hmm, make that -1...
    *)

    interactionEffect(READONLY orders : ARRAY OF Order) : LONGREAL;
    (* orders must not have -1s *)

    valueVersusAveEffect(index, value : CARDINAL) : LONGREAL;

    mean(READONLY overI, overV := EmptyArrC) : LONGREAL;

    allValues(READONLY overI, overV := EmptyArrC) : LongRealSeq.T;

    min(READONLY overI, overV := EmptyArrC) : LONGREAL;
    median(READONLY overI, overV := EmptyArrC) : LONGREAL;
    max(READONLY overI, overV := EmptyArrC) : LONGREAL;

    sdev(READONLY overI, overV := EmptyArrC; (*OUT*) VAR df : CARDINAL) : LONGREAL;
    (* returns degrees of freedom in VAR *)    

    confidence(READONLY overI, overV := EmptyArrC; level : LONGREAL) : Confidence;
    (* confidence interval under Student's T distribution *)

    ranked(READONLY overI, overV := EmptyArrC) : REF ARRAY OF LONGREAL;
    (* array of data, ranked by size *)

    overallSdev() : LONGREAL;
    (* overall std deviation (for pooled estimates) *)

  END;

  Confidence = RECORD
    mean, lower, lowerPooled, upperPooled, upper : LONGREAL;
  END;

CONST Brand = "ANOVA";
CONST EmptyArrC = ARRAY OF CARDINAL {};

END ANOVA.
