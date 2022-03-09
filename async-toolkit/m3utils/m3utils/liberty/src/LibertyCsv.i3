INTERFACE LibertyCsv;
IMPORT SchemePair;

PROCEDURE ToList(csvString : TEXT) : SchemePair.T;

PROCEDURE ToCsv(lst : SchemePair.T) : TEXT;

CONST Brand = "LibertyCsv";

END LibertyCsv.
