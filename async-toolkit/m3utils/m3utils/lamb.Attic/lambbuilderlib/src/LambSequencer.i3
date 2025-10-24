INTERFACE LambSequencer;
IMPORT LambCommandSeq AS CommandSeq;
IMPORT BitInteger;
IMPORT LambVerb AS Verb;

PROCEDURE Compile(prog          : CommandSeq.T;
                  VAR      seq  : ARRAY Verb.T OF REF ARRAY OF BitInteger.T);

END LambSequencer.
