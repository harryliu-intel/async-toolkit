INTERFACE TcamSequencer;
IMPORT CommandSeq;
IMPORT BitInteger;
IMPORT Verb;

PROCEDURE Compile(prog          : CommandSeq.T;
                  VAR      seq  : ARRAY Verb.T OF REF ARRAY OF BitInteger.T);

PROCEDURE AddKey(prog   : CommandSeq.T;
                 keyIdx : CARDINAL;
                 key    : TEXT;
                 w      : CARDINAL);


END TcamSequencer.
