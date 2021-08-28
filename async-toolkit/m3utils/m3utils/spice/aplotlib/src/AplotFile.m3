MODULE AplotFile;

(* 
   an aplot file consists of the following components

   <aplot header> AplotHeader.T
   <data> ARRAY OF AplotNode.T
   <directory> AplotDirectory.T

   for the formats NodeMajor*, StepMajor*, 
   the <directory> is null (unused/unneeded).

   All data items are written and read in little-endian (raw binary) format.

*)


BEGIN END AplotFile.
